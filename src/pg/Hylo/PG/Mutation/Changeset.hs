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
  , insertChangesetWithFallback
  , updateChangeset
  , updateChangesetWith
  , updateChangesetWithRow
  , updateChangesetWithFallback

    -- * Live Connection Pool Execution
  , runInsertChangesetIO
  , runInsertChangesetIOWith
  , runUpdateChangesetIO
  , runUpdateChangesetIOWith

    -- * Binary Wire Decoding
  , decodeReturningRow

    -- * Error Translation Integration
  , translatePgConstraintError
  , translateTxConstraintViolation
  ) where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Hylo.Backend.Capability (Postgres)
import Hylo.Core.Changeset
  ( Changeset (..)
  , FieldError (..)
  )
import Hylo.Core.Context (Ctx (..))
import Hylo.Core.Query
  ( findPrimaryKeyCol
  , renderInsertChangesetSQL
  , renderUpdateChangesetSQL
  )
import Hylo.Core.Table
  ( PatchVal (..)
  , Table (..)
  , TableSchema (..)
  )
import Hylo.Core.Transaction
  ( Tx (..)
  , TxError (..)
  , TxOp (..)
  , TxState (..)
  )
import Hylo.Core.TypeError (AssertWritable (..), TxMode (..), writableDict)
import Hylo.PG.Codec.Binary
  ( RowCodec
  , TableCodec (..)
  , runBinaryDecoder
  )
import Hylo.PG.Connection
  ( Pool (..)
  , executePreparedQuery
  )
import Hylo.PG.Error.Translation
  ( DbError (..)
  , mockSqlStateErrorFull
  , translateChangesetDbError
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

-- Note: 'TableCodec' is re-exported from 'Hylo.PG.Codec.Binary'.

-- | Decode RETURNING * binary wire output into hydrated domain record using 'Hylo.PG.Codec.Binary'.
decodeReturningRow ::
  RowCodec (table 'Result) (table 'Result) ->
  ByteString ->
  Either Text (table 'Result)
decodeReturningRow = runBinaryDecoder

--------------------------------------------------------------------------------
-- 3. Transactional Changeset Mutations
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

-- | Parameterized INSERT with a fallback entity for simulated / offline execution.
insertChangesetWithFallback ::
  forall table v (mode :: TxMode) op.
  ( AssertWritable mode
  , Table table
  ) =>
  TableSchema table ->
  table 'Result ->
  Changeset table op ->
  Tx (Postgres v) mode (Either ChangesetError (table 'Result))
insertChangesetWithFallback schema fallback cs =
  writableDict (Proxy :: Proxy mode) `seq`
    TxAction $ \st ->
      if not (changesetValid cs) || not (null (changesetErrors cs))
        then
          let errs = case changesetErrors cs of
                (e : es) -> e :| es
                [] -> FieldError "" "invalid_changeset" "Changeset validation failed" :| []
           in (Right (Left (ChangesetError errs)), st)
        else case checkSimulatedConstraint schema cs st of
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
             in (Right (Right fallback), st')

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

-- | Parameterized partial UPDATE with a fallback entity for simulated / offline execution.
updateChangesetWithFallback ::
  forall table v (mode :: TxMode) op.
  ( AssertWritable mode
  , Table table
  ) =>
  TableSchema table ->
  table 'Result ->
  Changeset table op ->
  Tx (Postgres v) mode (Either ChangesetError (table 'Result))
updateChangesetWithFallback schema fallback cs =
  writableDict (Proxy :: Proxy mode) `seq`
    TxAction $ \st ->
      if not (changesetValid cs) || not (null (changesetErrors cs))
        then
          let errs = case changesetErrors cs of
                (e : es) -> e :| es
                [] -> FieldError "" "invalid_changeset" "Changeset validation failed" :| []
           in (Right (Left (ChangesetError errs)), st)
        else case checkSimulatedConstraint schema cs st of
          Just fe ->
            (Right (Left (singleChangesetError fe)), st)
          Nothing ->
            let (sqlText, params) = renderUpdateChangesetSQL schema cs
                st' = st {txOperations = txOperations st ++ [OpMutate (sqlText, params)]}
             in (Right (Right fallback), st')

--------------------------------------------------------------------------------
-- 6. Live Connection Pool Execution
--------------------------------------------------------------------------------

-- | Execute an INSERT changeset against live PostgreSQL and return the hydrated domain record.
runInsertChangesetIO ::
  forall table op.
  (Table table, TableCodec table) =>
  Pool ->
  TableSchema table ->
  Changeset table op ->
  IO (Either ChangesetError (table 'Result))
runInsertChangesetIO pool schema =
  runInsertChangesetIOWith pool schema tableCodec

-- | Execute an INSERT changeset against live PostgreSQL using an explicit 'RowCodec'.
runInsertChangesetIOWith ::
  forall table op.
  (Table table) =>
  Pool ->
  TableSchema table ->
  RowCodec (table 'Result) (table 'Result) ->
  Changeset table op ->
  IO (Either ChangesetError (table 'Result))
runInsertChangesetIOWith pool schema codec cs =
  if not (changesetValid cs) || not (null (changesetErrors cs))
    then
      let errs = case changesetErrors cs of
            (e : es) -> e :| es
            [] -> FieldError "" "invalid_changeset" "Changeset validation failed" :| []
       in pure (Left (ChangesetError errs))
    else do
      let (sqlText, params) = renderInsertChangesetSQL schema cs
          numCols = length (tableColDefs (tableMeta schema))
      res <- executePreparedQuery pool sqlText params codec numCols
      case res of
        Left txErr -> pure (Left (translateTxConstraintViolation schema cs txErr))
        Right [] ->
          pure
            ( Left
                (singleChangesetError (FieldError "" "no_rows_returned" "INSERT ... RETURNING * returned no rows"))
            )
        Right (row : _) -> pure (Right row)

-- | Execute an UPDATE changeset against live PostgreSQL and return the hydrated domain record.
runUpdateChangesetIO ::
  forall table op.
  (Table table, TableCodec table) =>
  Pool ->
  TableSchema table ->
  Changeset table op ->
  IO (Either ChangesetError (table 'Result))
runUpdateChangesetIO pool schema =
  runUpdateChangesetIOWith pool schema tableCodec

-- | Execute an UPDATE changeset against live PostgreSQL using an explicit 'RowCodec'.
runUpdateChangesetIOWith ::
  forall table op.
  (Table table) =>
  Pool ->
  TableSchema table ->
  RowCodec (table 'Result) (table 'Result) ->
  Changeset table op ->
  IO (Either ChangesetError (table 'Result))
runUpdateChangesetIOWith pool schema codec cs =
  if not (changesetValid cs) || not (null (changesetErrors cs))
    then
      let errs = case changesetErrors cs of
            (e : es) -> e :| es
            [] -> FieldError "" "invalid_changeset" "Changeset validation failed" :| []
       in pure (Left (ChangesetError errs))
    else do
      let (sqlText, params) = renderUpdateChangesetSQL schema cs
          numCols = length (tableColDefs (tableMeta schema))
      res <- executePreparedQuery pool sqlText params codec numCols
      case res of
        Left txErr -> pure (Left (translateTxConstraintViolation schema cs txErr))
        Right [] ->
          pure
            (Left (singleChangesetError (FieldError "" "not_found" "UPDATE ... RETURNING * returned no rows")))
        Right (row : _) -> pure (Right row)

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
