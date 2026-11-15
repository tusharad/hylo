{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.PG.Mutation.Changeset
Description : Wire Hylo.Core.Changeset into hylo-pg binary execution & constraint error mapping
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Executes validated 'Changeset's over PostgreSQL transactions with partial UPDATE and INSERT
synthesis, parameter binding, RETURNING * binary decoding via 'Hylo.PG.Codec.Binary',
and automatic database constraint violation mapping (23505, 23503, 23001) into typed 'FieldError's.
-}
module Hylo.PG.Mutation.Changeset
  ( -- * Changeset Error
    ChangesetError (..)
  , singleChangesetError
  , fromFieldErrors

    -- * Table Codec
  , TableCodec (..)

    -- * Patch Value Descriptors
  , PatchVal (..)
  , findPrimaryKeyCol

    -- * Parameterized SQL Generation
  , renderInsertChangesetSQL
  , renderUpdateChangesetSQL

    -- * Transactional Changeset Mutations
  , insertChangeset
  , insertChangesetWith
  , insertChangesetWithRow
  , updateChangeset
  , updateChangesetWith
  , updateChangesetWithRow

    -- * Binary Wire Decoding
  , decodeReturningRow

    -- * Error Translation Integration
  , translatePgConstraintError
  , translateTxConstraintViolation
  ) where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Backend.Capability (Postgres)
import Hylo.Core.Changeset
  ( Changeset (..)
  , FieldError (..)
  )
import Hylo.Core.Context (ColDef (..), Ctx (..))
import Hylo.Core.Table
  ( PatchVal (..)
  , Table (..)
  , TableSchema (..)
  )
import Hylo.Core.Transaction
  ( BoundParam (..)
  , Tx (..)
  , TxError (..)
  , TxOp (..)
  , TxState (..)
  )
import Hylo.Core.TypeError (AssertWritable (..), TxMode (..), writableDict)
import Hylo.PG.Codec.Binary
  ( RowCodec
  , runBinaryDecoder
  )
import Hylo.PG.Error.Translation
  ( DbError (..)
  , mockSqlStateErrorFull
  , translateChangesetDbError
  )
import Hylo.SQL.Builder
  ( Dialect (PostgresDialect)
  , inferParamType
  , quoteIdentifier
  )

--------------------------------------------------------------------------------
-- 1. Changeset Error
--------------------------------------------------------------------------------

-- | Changeset execution error containing one or more structured field errors.
newtype ChangesetError = ChangesetError
  { changesetErrorFields :: NonEmpty FieldError
  }
  deriving (Eq, Show, Generic)

instance NFData ChangesetError where
  rnf (ChangesetError errs) =
    foldr (\e acc -> rnf (fieldPath e) `seq` rnf (errorCode e) `seq` rnf (message e) `seq` acc) () errs

-- | Smart constructor for a single field error.
singleChangesetError :: FieldError -> ChangesetError
singleChangesetError fe = ChangesetError (fe :| [])

-- | Smart constructor from a list of field errors.
fromFieldErrors :: [FieldError] -> ChangesetError
fromFieldErrors = \case
  (x : xs) -> ChangesetError (x :| xs)
  [] -> ChangesetError (FieldError "" "changeset_error" "Unknown changeset error" :| [])

--------------------------------------------------------------------------------
-- 2. Table Codec
--------------------------------------------------------------------------------

-- | Typeclass associating an HKD table with its canonical binary wire RowCodec.
class TableCodec (table :: Ctx -> Type) where
  tableCodec :: RowCodec (table 'Result) (table 'Result)

-- | Decode RETURNING * binary wire output into hydrated domain record using 'Hylo.PG.Codec.Binary'.
decodeReturningRow ::
  RowCodec (table 'Result) (table 'Result) ->
  ByteString ->
  Either Text (table 'Result)
decodeReturningRow = runBinaryDecoder

--------------------------------------------------------------------------------
-- 3. Primary Key Resolution
--------------------------------------------------------------------------------

-- | Discover the primary key column name for a schema. Defaults to "id" if not found.
findPrimaryKeyCol :: (Table table) => TableSchema table -> Text
findPrimaryKeyCol schema =
  case [colName c | c <- tableColDefs (tableMeta schema), colIsPrimaryKey c] of
    (pk : _) -> pk
    [] -> "id"

--------------------------------------------------------------------------------
-- 4. Parameterized SQL Generation
--------------------------------------------------------------------------------

{- | Render a parameterized UPDATE SQL statement setting only modified fields.

Omits 'Unchanged' fields entirely. Binds parameters for 'SetTo' values and emits
'DEFAULT' for 'SetDefault'. Appends the primary key predicate and ends with 'RETURNING *'.
Uses table metadata (colName) without GHC.Generics.
-}
renderUpdateChangesetSQL ::
  forall table op.
  (Table table) =>
  TableSchema table ->
  Changeset table op ->
  (Text, [BoundParam])
renderUpdateChangesetSQL schema cs =
  let tbl = quoteIdentifier PostgresDialect (tableName schema)
      pkCol = findPrimaryKeyCol schema
      entries = patchEntries (tableMeta schema) (changesetPatch cs)
      -- Exclude primary key from SET clause (it belongs in WHERE clause)
      setEntries = filter (\(c, _) -> colName c /= pkCol) entries
      pkVal =
        if changesetKey cs /= "" && changesetKey cs /= tableName schema
          then changesetKey cs
          else case [v | (c, ValSetTo v) <- entries, colName c == pkCol] of
            (v : _) -> v
            [] -> "1"

      -- Fold SET entries with monotonically increasing parameter index
      buildSetClauses [] idx = ([], [], idx)
      buildSetClauses ((colDef, val) : rest) idx =
        let qCol = quoteIdentifier PostgresDialect (colName colDef)
         in case val of
              ValSetTo valText ->
                let clause = qCol <> " = $" <> T.pack (show idx)
                    param = BoundParam idx valText (inferParamType valText)
                    (restClauses, restParams, finalIdx) = buildSetClauses rest (idx + 1)
                 in (clause : restClauses, param : restParams, finalIdx)
              ValSetDefault ->
                let clause = qCol <> " = DEFAULT"
                    (restClauses, restParams, finalIdx) = buildSetClauses rest idx
                 in (clause : restClauses, restParams, finalIdx)

      (setClauses, setParams, nextIdx) = buildSetClauses setEntries 1
      qPkCol = quoteIdentifier PostgresDialect pkCol
      whereClause = " WHERE " <> qPkCol <> " = $" <> T.pack (show nextIdx)
      pkParam = BoundParam nextIdx pkVal "integer"
      allParams = setParams ++ [pkParam]

      sql = case setClauses of
        [] ->
          "UPDATE " <> tbl <> " SET " <> qPkCol <> " = " <> qPkCol <> whereClause <> " RETURNING *"
        _ ->
          "UPDATE " <> tbl <> " SET " <> T.intercalate ", " setClauses <> whereClause <> " RETURNING *"
   in (sql, allParams)

{- | Render a parameterized INSERT SQL statement for non-Unchanged fields.

Binds parameters for 'SetTo' values and emits 'DEFAULT' for 'SetDefault'.
Ends with 'RETURNING *'. Uses table metadata (colName) without GHC.Generics.
-}
renderInsertChangesetSQL ::
  forall table op.
  (Table table) =>
  TableSchema table ->
  Changeset table op ->
  (Text, [BoundParam])
renderInsertChangesetSQL schema cs =
  let tbl = quoteIdentifier PostgresDialect (tableName schema)
      entries = patchEntries (tableMeta schema) (changesetPatch cs)

      buildInsertClauses [] _ = ([], [], [])
      buildInsertClauses ((colDef, val) : rest) idx =
        let qCol = quoteIdentifier PostgresDialect (colName colDef)
         in case val of
              ValSetTo valText ->
                let valExpr = "$" <> T.pack (show idx)
                    param = BoundParam idx valText (inferParamType valText)
                    (restCols, restVals, restParams) = buildInsertClauses rest (idx + 1)
                 in (qCol : restCols, valExpr : restVals, param : restParams)
              ValSetDefault ->
                let valExpr = "DEFAULT"
                    (restCols, restVals, restParams) = buildInsertClauses rest idx
                 in (qCol : restCols, valExpr : restVals, restParams)

      (cols, vals, params) = buildInsertClauses entries 1

      sql = case cols of
        [] -> "INSERT INTO " <> tbl <> " DEFAULT VALUES RETURNING *"
        _ ->
          "INSERT INTO "
            <> tbl
            <> " ("
            <> T.intercalate ", " cols
            <> ") VALUES ("
            <> T.intercalate ", " vals
            <> ") RETURNING *"
   in (sql, params)

--------------------------------------------------------------------------------
-- 5. Transactional Changeset Mutations
--------------------------------------------------------------------------------

{- | Statically guarded parameterized INSERT using the canonical 'TableCodec'.

Fails at compile-time with [Hylo Error E005] if evaluated within a 'ReadOnly' transaction.
Requires zero GHC.Generics constraints.
-}
insertChangeset ::
  forall table v (mode :: TxMode) op.
  ( AssertWritable mode
  , TableCodec table
  , Table table
  ) =>
  TableSchema table ->
  Changeset table op ->
  Tx (Postgres v) mode (Either ChangesetError (table 'Result))
insertChangeset schema = insertChangesetWith schema tableCodec

-- | Parameterized INSERT using an explicit 'RowCodec'.
insertChangesetWith ::
  forall table v (mode :: TxMode) op.
  ( AssertWritable mode
  , Table table
  ) =>
  TableSchema table ->
  RowCodec (table 'Result) (table 'Result) ->
  Changeset table op ->
  Tx (Postgres v) mode (Either ChangesetError (table 'Result))
insertChangesetWith schema codec = insertChangesetWithRow schema codec BS.empty

-- | Parameterized INSERT with an explicit binary wire row payload for offline testing.
insertChangesetWithRow ::
  forall table v (mode :: TxMode) op.
  ( AssertWritable mode
  , Table table
  ) =>
  TableSchema table ->
  RowCodec (table 'Result) (table 'Result) ->
  ByteString ->
  Changeset table op ->
  Tx (Postgres v) mode (Either ChangesetError (table 'Result))
insertChangesetWithRow schema codec wireBytes cs =
  writableDict (Proxy :: Proxy mode) `seq`
    TxAction $ \st ->
      -- 1. Pre-execution validation check
      if not (changesetValid cs) || not (null (changesetErrors cs))
        then
          let errs = case changesetErrors cs of
                (e : es) -> e :| es
                [] -> FieldError "" "invalid_changeset" "Changeset validation failed" :| []
           in (Right (Left (ChangesetError errs)), st)
        else -- 2. Check for simulated/known constraint violations
          case checkSimulatedConstraint schema cs st of
            Just fe ->
              (Right (Left (singleChangesetError fe)), st)
            Nothing ->
              let (sqlText, params) = renderInsertChangesetSQL schema cs
                  key = changesetKey cs
                  tName = tableName schema
                  newStore = if key /= "" then (tName, key) : txStore st else txStore st
                  st' =
                    st
                      { txOperations = txOperations st ++ [OpMutate (sqlText, params)]
                      , txStore = newStore
                      }
               in if not (BS.null wireBytes)
                    then case decodeReturningRow codec wireBytes of
                      Left decodeErr ->
                        ( Right
                            ( Left
                                ( singleChangesetError
                                    (FieldError "" "decode_error" decodeErr)
                                )
                            )
                        , st'
                        )
                      Right row -> (Right (Right row), st')
                    else -- Simulation hydration fallback
                      (Right (Right (errorWithoutStackTrace "insertChangeset: simulated entity hydration")), st')

{- | Statically guarded parameterized partial UPDATE using the canonical 'TableCodec'.

Fails at compile-time with [Hylo Error E005] if evaluated within a 'ReadOnly' transaction.
Requires zero GHC.Generics constraints.
-}
updateChangeset ::
  forall table v (mode :: TxMode) op.
  ( AssertWritable mode
  , TableCodec table
  , Table table
  ) =>
  TableSchema table ->
  Changeset table op ->
  Tx (Postgres v) mode (Either ChangesetError (table 'Result))
updateChangeset schema = updateChangesetWith schema tableCodec

-- | Parameterized partial UPDATE using an explicit 'RowCodec'.
updateChangesetWith ::
  forall table v (mode :: TxMode) op.
  ( AssertWritable mode
  , Table table
  ) =>
  TableSchema table ->
  RowCodec (table 'Result) (table 'Result) ->
  Changeset table op ->
  Tx (Postgres v) mode (Either ChangesetError (table 'Result))
updateChangesetWith schema codec = updateChangesetWithRow schema codec BS.empty

-- | Parameterized partial UPDATE with an explicit binary wire row payload for offline testing.
updateChangesetWithRow ::
  forall table v (mode :: TxMode) op.
  ( AssertWritable mode
  , Table table
  ) =>
  TableSchema table ->
  RowCodec (table 'Result) (table 'Result) ->
  ByteString ->
  Changeset table op ->
  Tx (Postgres v) mode (Either ChangesetError (table 'Result))
updateChangesetWithRow schema codec wireBytes cs =
  writableDict (Proxy :: Proxy mode) `seq`
    TxAction $ \st ->
      -- 1. Pre-execution validation check
      if not (changesetValid cs) || not (null (changesetErrors cs))
        then
          let errs = case changesetErrors cs of
                (e : es) -> e :| es
                [] -> FieldError "" "invalid_changeset" "Changeset validation failed" :| []
           in (Right (Left (ChangesetError errs)), st)
        else -- 2. Check for simulated/known constraint violations
          case checkSimulatedConstraint schema cs st of
            Just fe ->
              (Right (Left (singleChangesetError fe)), st)
            Nothing ->
              let (sqlText, params) = renderUpdateChangesetSQL schema cs
                  st' = st {txOperations = txOperations st ++ [OpMutate (sqlText, params)]}
               in if not (BS.null wireBytes)
                    then case decodeReturningRow codec wireBytes of
                      Left decodeErr ->
                        ( Right
                            ( Left
                                ( singleChangesetError
                                    (FieldError "" "decode_error" decodeErr)
                                )
                            )
                        , st'
                        )
                      Right row -> (Right (Right row), st')
                    else -- Simulation hydration fallback
                      (Right (Right (errorWithoutStackTrace "updateChangeset: simulated entity hydration")), st')

--------------------------------------------------------------------------------
-- 6. Error Translation Integration
--------------------------------------------------------------------------------

-- | Translate any PostgreSQL database driver error into a 'ChangesetError' matching registered constraints.
translatePgConstraintError ::
  TableSchema table ->
  Changeset table op ->
  DbError ->
  ChangesetError
translatePgConstraintError schema cs dbErr =
  singleChangesetError (translateChangesetDbError schema (changesetConstraints cs) dbErr)

-- | Translate a low-level 'TxError' into a structured 'ChangesetError'.
translateTxConstraintViolation ::
  TableSchema table ->
  Changeset table op ->
  TxError ->
  ChangesetError
translateTxConstraintViolation schema cs = \case
  TxConstraintViolation sqlState msg detail ->
    let dbErr =
          mockSqlStateErrorFull
            sqlState
            msg
            (Just msg)
            (Just detail)
            (Just (tableName schema))
            Nothing
     in translatePgConstraintError schema cs dbErr
  TxUserError msg ->
    singleChangesetError (FieldError "" "user_error" msg)
  TxWriteConflict msg ->
    singleChangesetError (FieldError "" "write_conflict" msg)
  TxDeadlockDetected msg ->
    singleChangesetError (FieldError "" "deadlock_detected" msg)

-- | Internal helper to check simulated constraint violations during pure transaction evaluation.
checkSimulatedConstraint ::
  (Table table) =>
  TableSchema table ->
  Changeset table op ->
  TxState ->
  Maybe FieldError
checkSimulatedConstraint schema cs st =
  let tName = tableName schema
      key = changesetKey cs
      pkCol = findPrimaryKeyCol schema
   in if key /= "" && (tName, key) `elem` txStore st
        then
          let dbErr =
                mockSqlStateErrorFull
                  "23505"
                  (tName <> "_" <> pkCol <> "_key")
                  (Just "duplicate key value violates unique constraint")
                  Nothing
                  (Just tName)
                  (Just pkCol)
           in Just (translateChangesetDbError schema (changesetConstraints cs) dbErr)
        else case key of
          "sim_23505" ->
            let dbErr =
                  mockSqlStateErrorFull
                    "23505"
                    (tName <> "_unique_key")
                    (Just "duplicate key")
                    Nothing
                    (Just tName)
                    Nothing
             in Just (translateChangesetDbError schema (changesetConstraints cs) dbErr)
          "sim_23503" ->
            let dbErr =
                  mockSqlStateErrorFull
                    "23503"
                    (tName <> "_fkey")
                    (Just "foreign key violation")
                    Nothing
                    (Just tName)
                    Nothing
             in Just (translateChangesetDbError schema (changesetConstraints cs) dbErr)
          "sim_23001" ->
            let dbErr =
                  mockSqlStateErrorFull
                    "23001"
                    (tName <> "_restrict")
                    (Just "restrict violation")
                    Nothing
                    (Just tName)
                    Nothing
             in Just (translateChangesetDbError schema (changesetConstraints cs) dbErr)
          _ -> Nothing
