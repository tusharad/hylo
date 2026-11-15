{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.Connection
Description : PostgreSQL connection pool and real transaction runner (Hasql)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Connection
  ( Pool (..)
  , createPool
  , createConnectedPool
  , closePool
  , defaultPool
  , pool
  , runTx
  , executeSql
  , executeSession
  , translateHasqlError
  )
where

import Control.DeepSeq (NFData (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Hasql.Connection as C
import qualified Hasql.Connection.Setting as S
import qualified Hasql.Connection.Setting.Connection as SC
import qualified Hasql.Session as Sess
import Hylo.Core.Query (Postgres)
import Hylo.Core.Transaction
  ( Tx (..)
  , TxError (..)
  , TxMode (..)
  , TxOp (..)
  , TxState (..)
  , executeTx
  , initialTxState
  )
import Hylo.PG.Transaction.Hooks (executeCommitHooks, executeRollbackHooks)

-- | PostgreSQL connection pool descriptor.
data Pool = Pool
  { poolConnectionString :: !Text
  , poolMaxConnections :: !Int
  , poolActiveConnection :: !(Maybe C.Connection)
  }

instance Eq Pool where
  Pool c1 m1 _ == Pool c2 m2 _ = c1 == c2 && m1 == m2

instance Show Pool where
  showsPrec d (Pool c m conn) =
    showParen (d > 10) $
      showString "Pool { poolConnectionString = "
        . shows c
        . showString ", poolMaxConnections = "
        . shows m
        . showString ", poolActive = "
        . shows (isJust conn)
        . showString " }"

instance NFData Pool where
  rnf (Pool c m _) = rnf c `seq` rnf m

{- | Create a connection pool descriptor. Attempts immediate connection;
if PostgreSQL is available, stores the active connection.
-}
createPool :: Text -> IO Pool
createPool connStr = do
  connRes <- C.acquire [S.connection (SC.string connStr)]
  case connRes of
    Right conn -> pure $ Pool connStr 10 (Just conn)
    Left _ -> pure $ Pool connStr 10 Nothing

{- | Create a connection pool that strictly expects an active connection,
returning Left on connection failure.
-}
createConnectedPool :: Text -> IO (Either TxError Pool)
createConnectedPool connStr = do
  connRes <- C.acquire [S.connection (SC.string connStr)]
  case connRes of
    Right conn -> pure $ Right (Pool connStr 10 (Just conn))
    Left err -> pure $ Left (TxUserError ("Connection failed: " <> T.pack (show err)))

-- | Explicitly release any active connection held by the pool.
closePool :: Pool -> IO ()
closePool (Pool _ _ (Just conn)) = C.release conn
closePool _ = pure ()

-- | Default connection pool for local execution and test environments.
defaultPool :: Pool
defaultPool = Pool "postgresql://localhost:5432/hylo_test" 10 Nothing

-- | Top-level default pool reference.
pool :: Pool
pool = defaultPool

{- | Execute a transaction computation against the connection pool.
If the pool has an active PostgreSQL connection, statements and savepoints
are executed on real PostgreSQL. Otherwise, it safely falls back to pure state simulation.
-}
runTx ::
  Pool ->
  TxMode ->
  Tx Postgres mode a ->
  IO (Either TxError a)
runTx (Pool _ _ (Just conn)) mode tx = do
  let (res, st) = executeTx tx initialTxState
  case res of
    Left err -> do
      executeRollbackHooks st
      pure (Left err)
    Right val -> do
      let beginSql = case mode of
            ReadOnly -> "BEGIN TRANSACTION READ ONLY;"
            ReadWrite -> "BEGIN TRANSACTION READ WRITE;"
      beginRes <- Sess.run (Sess.sql beginSql) conn
      case beginRes of
        Left qErr -> do
          executeRollbackHooks st
          pure (Left (translateHasqlError qErr))
        Right () -> do
          opsRes <- runOps (txOperations st)
          case opsRes of
            Left err -> do
              _ <- Sess.run (Sess.sql "ROLLBACK;") conn
              executeRollbackHooks st
              pure (Left err)
            Right () -> do
              commitRes <- Sess.run (Sess.sql "COMMIT;") conn
              case commitRes of
                Left qErr -> do
                  executeRollbackHooks st
                  pure (Left (translateHasqlError qErr))
                Right () -> do
                  executeCommitHooks st
                  pure (Right val)
  where
    runOps [] = pure (Right ())
    runOps (op : rest) = case op of
      OpMutate sqlText -> do
        res <- Sess.run (Sess.sql (TE.encodeUtf8 sqlText)) conn
        case res of
          Left qErr -> pure (Left (translateHasqlError qErr))
          Right () -> runOps rest
      OpQuery sqlText -> do
        res <- Sess.run (Sess.sql (TE.encodeUtf8 sqlText)) conn
        case res of
          Left qErr -> pure (Left (translateHasqlError qErr))
          Right () -> runOps rest
      OpSavepoint _ name -> do
        res <- Sess.run (Sess.sql (TE.encodeUtf8 ("SAVEPOINT \"" <> name <> "\";"))) conn
        case res of
          Left qErr -> pure (Left (translateHasqlError qErr))
          Right () -> runOps rest
      OpReleaseSavepoint _ name -> do
        res <- Sess.run (Sess.sql (TE.encodeUtf8 ("RELEASE SAVEPOINT \"" <> name <> "\";"))) conn
        case res of
          Left qErr -> pure (Left (translateHasqlError qErr))
          Right () -> runOps rest
      OpRollbackSavepoint _ name -> do
        res <- Sess.run (Sess.sql (TE.encodeUtf8 ("ROLLBACK TO SAVEPOINT \"" <> name <> "\";"))) conn
        case res of
          Left qErr -> pure (Left (translateHasqlError qErr))
          Right () -> runOps rest
runTx (Pool _ _ Nothing) _ tx = do
  let (res, st) = executeTx tx initialTxState
  case res of
    Left err -> do
      executeRollbackHooks st
      pure (Left err)
    Right val -> do
      executeCommitHooks st
      pure (Right val)

-- | Execute a raw SQL statement against the pool's active connection.
executeSql :: Pool -> Text -> IO (Either TxError ())
executeSql (Pool _ _ (Just conn)) sqlText = do
  res <- Sess.run (Sess.sql (TE.encodeUtf8 sqlText)) conn
  case res of
    Left qErr -> pure (Left (translateHasqlError qErr))
    Right () -> pure (Right ())
executeSql (Pool _ _ Nothing) _ = pure (Right ())

-- | Execute an arbitrary Hasql Session against the pool's active connection.
executeSession :: Pool -> Sess.Session a -> IO (Either TxError a)
executeSession (Pool _ _ (Just conn)) sess = do
  res <- Sess.run sess conn
  case res of
    Left qErr -> pure (Left (translateHasqlError qErr))
    Right a -> pure (Right a)
executeSession (Pool _ _ Nothing) _ =
  pure (Left (TxUserError "executeSession: no active PostgreSQL connection in pool"))

-- | Translate Hasql/PostgreSQL driver errors into structured Hylo TxError.
translateHasqlError :: Sess.SessionError -> TxError
translateHasqlError = \case
  Sess.QueryError _ _ cmdErr -> case cmdErr of
    Sess.ResultError (Sess.ServerError code msg mDetail _ _) ->
      let sqlState = TE.decodeUtf8 code
          message = TE.decodeUtf8 msg
          detail = maybe "" TE.decodeUtf8 mDetail
       in case sqlState of
            "23505" -> TxConstraintViolation "23505" message detail
            "23503" -> TxConstraintViolation "23503" message detail
            "23514" -> TxConstraintViolation "23514" message detail
            "40001" -> TxSerializationFailure message
            "40P01" -> TxDeadlockDetected message
            _ -> TxUserError (message <> if detail /= "" then " (" <> detail <> ")" else "")
    _ -> TxUserError (T.pack (show cmdErr))
  Sess.PipelineError pipErr -> TxUserError ("PipelineError: " <> T.pack (show pipErr))
