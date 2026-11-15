{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
  , GPatchEntries (..)
  , GTableMeta (..)
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
import GHC.Generics
  ( C1
  , D1
  , Generic (..)
  , K1 (..)
  , M1 (..)
  , Meta (..)
  , Rep
  , S1
  , U1 (..)
  , (:*:) (..)
  )
import GHC.TypeLits (KnownSymbol, symbolVal)
import Hylo.Backend.Capability (Postgres)
import Hylo.Core.Changeset
  ( Changeset (..)
  , FieldError (..)
  , PatchField (..)
  , ToFieldValue (..)
  )
import Hylo.Core.Context (ColDef (..), Ctx (..))
import Hylo.Core.Table (TableSchema (..))
import Hylo.Core.Transaction
  ( Tx (..)
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
  ( BoundParam (..)
  , Dialect (PostgresDialect)
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
-- 3. Patch Value Descriptors & Generic Traversals
--------------------------------------------------------------------------------

-- | Patch value representation for SQL synthesis.
data PatchVal
  = ValSetTo !Text
  | ValSetDefault
  deriving (Eq, Show, Generic)

instance NFData PatchVal

-- | Generic extraction of modified patch fields from HKD 'Patch records.
class GPatchEntries f where
  gPatchEntries :: f p -> [(Text, PatchVal)]

instance (GPatchEntries f) => GPatchEntries (D1 c f) where
  gPatchEntries (M1 x) = gPatchEntries x

instance (GPatchEntries f) => GPatchEntries (C1 c f) where
  gPatchEntries (M1 x) = gPatchEntries x

instance (GPatchEntries f1, GPatchEntries f2) => GPatchEntries (f1 :*: f2) where
  gPatchEntries (a :*: b) = gPatchEntries a ++ gPatchEntries b

instance
  (KnownSymbol s, ToFieldValue a) =>
  GPatchEntries (S1 ('MetaSel ('Just s) u ss ds) (K1 i (PatchField a)))
  where
  gPatchEntries (M1 (K1 (SetTo a))) = [(T.pack (symbolVal (Proxy @s)), ValSetTo (toFieldValue a))]
  gPatchEntries (M1 (K1 SetDefault)) = [(T.pack (symbolVal (Proxy @s)), ValSetDefault)]
  gPatchEntries (M1 (K1 Unchanged)) = []

instance GPatchEntries U1 where
  gPatchEntries U1 = []

-- | Generic extraction of column metadata from HKD 'Meta records.
class GTableMeta f where
  gTableMeta :: f p -> [(Text, Bool, Text)] -- (colName, isPrimaryKey, sqlType)

instance (GTableMeta f) => GTableMeta (D1 c f) where
  gTableMeta (M1 x) = gTableMeta x

instance (GTableMeta f) => GTableMeta (C1 c f) where
  gTableMeta (M1 x) = gTableMeta x

instance (GTableMeta f1, GTableMeta f2) => GTableMeta (f1 :*: f2) where
  gTableMeta (a :*: b) = gTableMeta a ++ gTableMeta b

instance GTableMeta (S1 m (K1 i (ColDef a))) where
  gTableMeta (M1 (K1 def)) = [(colName def, colIsPrimaryKey def, colSqlType def)]

instance GTableMeta U1 where
  gTableMeta U1 = []

-- | Discover the primary key column name for a schema. Defaults to "id" if not found.
findPrimaryKeyCol ::
  (Generic (table 'Meta), GTableMeta (Rep (table 'Meta))) =>
  TableSchema table ->
  Text
findPrimaryKeyCol schema =
  case [c | (c, isPk, _) <- gTableMeta (from (tableMeta schema)), isPk] of
    (pk : _) -> pk
    [] -> "id"

--------------------------------------------------------------------------------
-- 4. Parameterized SQL Generation
--------------------------------------------------------------------------------

{- | Render a parameterized UPDATE SQL statement setting only modified fields.

Omits 'Unchanged' fields entirely. Binds parameters for 'SetTo' values and emits
'DEFAULT' for 'SetDefault'. Appends the primary key predicate and ends with 'RETURNING *'.
-}
renderUpdateChangesetSQL ::
  forall table op.
  ( Generic (table 'Patch)
  , GPatchEntries (Rep (table 'Patch))
  , Generic (table 'Meta)
  , GTableMeta (Rep (table 'Meta))
  ) =>
  TableSchema table ->
  Changeset table op ->
  (Text, [BoundParam])
renderUpdateChangesetSQL schema cs =
  let tbl = quoteIdentifier PostgresDialect (tableName schema)
      pkCol = findPrimaryKeyCol schema
      entries = gPatchEntries (from (changesetPatch cs))
      -- Exclude primary key from SET clause (it belongs in WHERE clause)
      setEntries = filter (\(c, _) -> c /= pkCol) entries
      pkVal =
        if changesetKey cs /= "" && changesetKey cs /= tableName schema
          then changesetKey cs
          else case lookup pkCol entries of
            Just (ValSetTo v) -> v
            _ -> "1"

      -- Fold SET entries with monotonically increasing parameter index
      buildSetClauses [] idx = ([], [], idx)
      buildSetClauses ((col, val) : rest) idx =
        let qCol = quoteIdentifier PostgresDialect col
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
Ends with 'RETURNING *'.
-}
renderInsertChangesetSQL ::
  forall table op.
  ( Generic (table 'Patch)
  , GPatchEntries (Rep (table 'Patch))
  ) =>
  TableSchema table ->
  Changeset table op ->
  (Text, [BoundParam])
renderInsertChangesetSQL schema cs =
  let tbl = quoteIdentifier PostgresDialect (tableName schema)
      entries = gPatchEntries (from (changesetPatch cs))

      buildInsertClauses [] _ = ([], [], [])
      buildInsertClauses ((col, val) : rest) idx =
        let qCol = quoteIdentifier PostgresDialect col
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
-}
insertChangeset ::
  forall table v (mode :: TxMode) op.
  ( AssertWritable mode
  , TableCodec table
  , Generic (table 'Patch)
  , GPatchEntries (Rep (table 'Patch))
  , Generic (table 'Meta)
  , GTableMeta (Rep (table 'Meta))
  ) =>
  TableSchema table ->
  Changeset table op ->
  Tx (Postgres v) mode (Either ChangesetError (table 'Result))
insertChangeset schema = insertChangesetWith schema tableCodec

-- | Parameterized INSERT using an explicit 'RowCodec'.
insertChangesetWith ::
  forall table v (mode :: TxMode) op.
  ( AssertWritable mode
  , Generic (table 'Patch)
  , GPatchEntries (Rep (table 'Patch))
  , Generic (table 'Meta)
  , GTableMeta (Rep (table 'Meta))
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
  , Generic (table 'Patch)
  , GPatchEntries (Rep (table 'Patch))
  , Generic (table 'Meta)
  , GTableMeta (Rep (table 'Meta))
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
              let (sqlText, _params) = renderInsertChangesetSQL schema cs
                  key = changesetKey cs
                  tName = tableName schema
                  newStore = if key /= "" then (tName, key) : txStore st else txStore st
                  st' =
                    st
                      { txOperations = txOperations st ++ [OpMutate sqlText]
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
-}
updateChangeset ::
  forall table v (mode :: TxMode) op.
  ( AssertWritable mode
  , TableCodec table
  , Generic (table 'Patch)
  , GPatchEntries (Rep (table 'Patch))
  , Generic (table 'Meta)
  , GTableMeta (Rep (table 'Meta))
  ) =>
  TableSchema table ->
  Changeset table op ->
  Tx (Postgres v) mode (Either ChangesetError (table 'Result))
updateChangeset schema = updateChangesetWith schema tableCodec

-- | Parameterized partial UPDATE using an explicit 'RowCodec'.
updateChangesetWith ::
  forall table v (mode :: TxMode) op.
  ( AssertWritable mode
  , Generic (table 'Patch)
  , GPatchEntries (Rep (table 'Patch))
  , Generic (table 'Meta)
  , GTableMeta (Rep (table 'Meta))
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
  , Generic (table 'Patch)
  , GPatchEntries (Rep (table 'Patch))
  , Generic (table 'Meta)
  , GTableMeta (Rep (table 'Meta))
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
              let (sqlText, _params) = renderUpdateChangesetSQL schema cs
                  st' = st {txOperations = txOperations st ++ [OpMutate sqlText]}
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
  (Generic (table 'Meta), GTableMeta (Rep (table 'Meta))) =>
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
