{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.SQLite.Connection
Description : Lightweight SQLite 3.38+ connection pool and statement executor
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides an in-memory or file-backed SQLite execution bridge matching Pool and
Tx SQLite mode a, supporting polymorphic Query execution, transactional savepoints,
commit/rollback hooks, and positional JSON array decoding for multi-level nested queries.
-}
module Hylo.SQLite.Connection
  ( -- * Connection Pool
    Pool (..)
  , createPool
  , createMemoryPool
  , closePool
  , withPool

    -- * Statement Execution
  , executeSql

    -- * Transaction Runners
  , runTx
  , runTxReadOnly
  , runTxReadWrite

    -- * Query Row Hydration
  , selectRows
  , selectRowsTx
  , selectRowsTxWith
  , select

    -- * Data Conversions & Error Handling
  , boundParamToSqlData
  , sqlDataToAeson
  , translateSqliteError
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.DeepSeq (NFData (..))
import Control.Exception (bracket, try)
import qualified Data.Aeson as Aeson
import Data.Foldable (toList)
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Database.SQLite3 as S
import qualified Database.SQLite3.Direct as SD
import Hylo.Backend.Capability (SQLite)
import Hylo.Core.Query.Monad (Query (..), initialQueryState, runQuery)
import Hylo.Core.Transaction
  ( BoundParam (..)
  , Tx (..)
  , TxError (..)
  , TxMode (..)
  , TxOp (..)
  , TxState (..)
  , executeTx
  , initialTxState
  )
import Hylo.PG.Codec.Binary (Selectable (..), runPositional)
import Hylo.PG.Transaction.Hooks (executeCommitHooks, executeRollbackHooks)
import Hylo.SQL.AST (Projectable (..))
import Hylo.SQL.Builder (CompiledQuery (..), Dialect (SQLiteDialect), compileQuery)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- 1. Connection Pool
--------------------------------------------------------------------------------

-- | SQLite connection pool descriptor wrapping an active Database handle in an MVar.
data Pool = Pool
  { poolConnectionString :: !Text
  , poolDatabase :: !(MVar S.Database)
  }

instance Eq Pool where
  Pool c1 _ == Pool c2 _ = c1 == c2

instance Show Pool where
  showsPrec d (Pool c _) =
    showParen (d > 10) $
      showString "Pool { poolConnectionString = "
        . shows c
        . showString " }"

instance NFData Pool where
  rnf (Pool c _) = rnf c

-- | Create a SQLite connection pool for an in-memory or file-backed database.
createPool :: Text -> IO Pool
createPool connStr = do
  db <- S.open connStr
  S.exec db "PRAGMA foreign_keys = ON;"
  mvar <- newMVar db
  pure (Pool connStr mvar)

-- | Convenience helper creating an in-memory SQLite pool.
createMemoryPool :: IO Pool
createMemoryPool = createPool ":memory:"

-- | Explicitly close the SQLite database handle held by the pool.
closePool :: Pool -> IO ()
closePool p = withMVar (poolDatabase p) S.close

-- | Safely borrow the active SQLite database handle within a scoped computation.
withPool :: Pool -> (S.Database -> IO (Either TxError a)) -> IO (Either TxError a)
withPool p = withMVar (poolDatabase p)

--------------------------------------------------------------------------------
-- 2. Statement Execution
--------------------------------------------------------------------------------

-- | Execute arbitrary raw SQL (DDL, schema creation, PRAGMA) against the pool.
executeSql :: Pool -> Text -> IO (Either TxError ())
executeSql p sqlText = withPool p $ \db -> do
  res <- try (S.exec db sqlText)
  case res of
    Left (err :: S.SQLError) -> pure (Left (translateSqliteError err))
    Right () -> pure (Right ())

--------------------------------------------------------------------------------
-- 3. Transaction Runners
--------------------------------------------------------------------------------

-- | Execute a transaction computation against the SQLite connection pool.
runTx ::
  Pool ->
  TxMode ->
  Tx SQLite mode a ->
  IO (Either TxError a)
runTx targetPool _mode tx = withPool targetPool $ \db -> do
  let (res, st) = executeTx tx initialTxState
  case res of
    Left err -> do
      executeRollbackHooks st
      pure (Left err)
    Right val -> do
      beginRes <- trySqlite db "BEGIN TRANSACTION;"
      case beginRes of
        Left err -> do
          executeRollbackHooks st
          pure (Left err)
        Right () -> do
          opsRes <- runOps db (txOperations st)
          case opsRes of
            Left err -> do
              _ <- trySqlite db "ROLLBACK;"
              executeRollbackHooks st
              pure (Left err)
            Right () -> do
              commitRes <- trySqlite db "COMMIT;"
              case commitRes of
                Left err -> do
                  _ <- trySqlite db "ROLLBACK;"
                  executeRollbackHooks st
                  pure (Left err)
                Right () -> do
                  executeCommitHooks st
                  pure (Right val)
  where
    runOps _ [] = pure (Right ())
    runOps db (op : rest) = case op of
      OpMutate (sqlText, params) -> do
        res <- executeStatement db sqlText params
        case res of
          Left err -> pure (Left err)
          Right () -> runOps db rest
      OpQuery (sqlText, params) -> do
        res <- executeStatement db sqlText params
        case res of
          Left err -> pure (Left err)
          Right () -> runOps db rest
      OpSavepoint _ name -> do
        res <- trySqlite db ("SAVEPOINT \"" <> name <> "\";")
        case res of
          Left err -> pure (Left err)
          Right () -> runOps db rest
      OpReleaseSavepoint _ name -> do
        res <- trySqlite db ("RELEASE SAVEPOINT \"" <> name <> "\";")
        case res of
          Left err -> pure (Left err)
          Right () -> runOps db rest
      OpRollbackSavepoint _ name -> do
        res <- trySqlite db ("ROLLBACK TO SAVEPOINT \"" <> name <> "\";")
        case res of
          Left err -> pure (Left err)
          Right () -> runOps db rest

-- | Execute a 'ReadOnly' transaction against the SQLite pool.
runTxReadOnly :: Pool -> Tx SQLite 'ReadOnly a -> IO (Either TxError a)
runTxReadOnly p = runTx p ReadOnly

-- | Execute a 'ReadWrite' transaction against the SQLite pool.
runTxReadWrite :: Pool -> Tx SQLite 'ReadWrite a -> IO (Either TxError a)
runTxReadWrite p = runTx p ReadWrite

--------------------------------------------------------------------------------
-- 4. Query Row Hydration
--------------------------------------------------------------------------------

-- | Execute a typed Query against SQLite and hydrate multi-level domain rows.
selectRows ::
  forall expr res.
  (Projectable expr, Selectable expr res) =>
  Pool ->
  Query SQLite expr ->
  IO (Either TxError [res])
selectRows targetPool q = withPool targetPool $ \db -> do
  let compiled = compileQuery SQLiteDialect q
      (expr, _) = runQuery q initialQueryState
  res <-
    try $
      bracket (S.prepare db compiled.compiledSQL) S.finalize $ \stmt -> do
        S.bind stmt (map boundParamToSqlData (toList compiled.compiledParams))
        let fetchRows acc = do
              stepRes <- S.step stmt
              case stepRes of
                SD.Done -> pure (Right (reverse acc))
                SD.Row -> do
                  colVals <- S.columns stmt
                  let aesonVals = map sqlDataToAeson colVals
                  case runPositional (selectablePositionalDecoder expr) aesonVals of
                    Left err -> pure (Left (TxUserError ("Decoding error: " <> err)))
                    Right row -> fetchRows (row : acc)
        fetchRows []
  case res of
    Left (err :: S.SQLError) -> pure (Left (translateSqliteError err))
    Right val -> pure val

-- | Transaction-aware counterpart to 'selectRows' recording 'OpQuery' and returning empty fallback rows.
selectRowsTx ::
  forall expr res (mode :: TxMode).
  (Projectable expr, Selectable expr res) =>
  Query SQLite expr ->
  Tx SQLite mode [res]
selectRowsTx = selectRowsTxWith []

-- | Transaction-aware counterpart to 'selectRows' recording 'OpQuery' and returning the given fallback rows.
selectRowsTxWith ::
  forall expr res (mode :: TxMode).
  (Projectable expr, Selectable expr res) =>
  [res] ->
  Query SQLite expr ->
  Tx SQLite mode [res]
selectRowsTxWith fallback q =
  let (expr, _) = runQuery q initialQueryState
      _dec = selectablePositionalDecoder expr
   in _dec `seq` TxAction $ \st ->
        let compiled = compileQuery SQLiteDialect q
            op = OpQuery (compiled.compiledSQL, toList compiled.compiledParams)
            st' = st {txOperations = txOperations st ++ [op]}
         in (Right fallback, st')

-- | Monadic query execution combinator within transactions, hydrating rows into domain results.
select ::
  forall expr res (mode :: TxMode).
  (Projectable expr, Selectable expr res) =>
  Query SQLite expr ->
  Tx SQLite mode [res]
select = selectRowsTx

--------------------------------------------------------------------------------
-- 5. Helper Functions & Error Translation
--------------------------------------------------------------------------------

-- | Execute a prepared statement without returning result rows.
executeStatement :: S.Database -> Text -> [BoundParam] -> IO (Either TxError ())
executeStatement db sqlText params = do
  res <-
    try $
      bracket (S.prepare db sqlText) S.finalize $ \stmt -> do
        S.bind stmt (map boundParamToSqlData params)
        _ <- S.step stmt
        pure ()
  case res of
    Left (err :: S.SQLError) -> pure (Left (translateSqliteError err))
    Right () -> pure (Right ())

-- | Execute raw SQL wrapped in exception handling.
trySqlite :: S.Database -> Text -> IO (Either TxError ())
trySqlite db sqlText = do
  res <- try (S.exec db sqlText)
  case res of
    Left (err :: S.SQLError) -> pure (Left (translateSqliteError err))
    Right () -> pure (Right ())

-- | Translate SQLite C-API errors into structured Hylo 'TxError'.
translateSqliteError :: S.SQLError -> TxError
translateSqliteError (S.SQLError err details ctx) =
  let msg = if T.null ctx then details else details <> " (" <> ctx <> ")"
   in case err of
        SD.ErrorConstraint -> TxConstraintViolation "23505" msg details
        SD.ErrorBusy -> TxWriteConflict msg
        SD.ErrorLocked -> TxDeadlockDetected msg
        _ -> TxUserError msg

-- | Convert a generic 'BoundParam' into a typed SQLite 'S.SQLData'.
boundParamToSqlData :: BoundParam -> S.SQLData
boundParamToSqlData (BoundParam _ val pType)
  | pType == "integer" || pType == "bigint" = case readMaybe (T.unpack val) of
      Just n -> S.SQLInteger n
      Nothing -> S.SQLText val
  | pType == "numeric" || pType == "double precision" = case readMaybe (T.unpack val) of
      Just (d :: Double) -> S.SQLFloat d
      Nothing -> S.SQLText val
  | pType == "boolean" = case T.toLower val of
      "true" -> S.SQLInteger 1
      "false" -> S.SQLInteger 0
      "1" -> S.SQLInteger 1
      "0" -> S.SQLInteger 0
      _ -> S.SQLText val
  | otherwise = S.SQLText val

-- | Convert a SQLite 'S.SQLData' column value into an 'Aeson.Value' for positional decoding.
sqlDataToAeson :: S.SQLData -> Aeson.Value
sqlDataToAeson = \case
  S.SQLInteger n -> Aeson.Number (fromIntegral n)
  S.SQLFloat d -> Aeson.Number (fromFloatDigits d)
  S.SQLText t
    | (T.isPrefixOf "[" t && T.isSuffixOf "]" t) || (T.isPrefixOf "{" t && T.isSuffixOf "}" t) ->
        case Aeson.decodeStrict' (TE.encodeUtf8 t) of
          Just val -> val
          Nothing -> Aeson.String t
    | otherwise -> Aeson.String t
  S.SQLBlob bs -> case Aeson.decodeStrict' bs of
    Just val -> val
    Nothing -> Aeson.String (TE.decodeUtf8With TE.lenientDecode bs)
  S.SQLNull -> Aeson.Null
