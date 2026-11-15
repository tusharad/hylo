{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
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
  , runTxAttempt
  , executeSql
  , executeSession
  , translateHasqlError
  , interpolateParams
  , boundParamValueEncoder
  , buildParamsEncoder
  , mkPreparedStatement
  , mkPreparedQueryStatement
  , assembleRowBytes
  , decodeRowWithCodec
  , executePreparedQuery
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isDigit)
import Data.Functor.Contravariant (contramap)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Hasql.Connection as C
import qualified Hasql.Connection.Setting as S
import qualified Hasql.Connection.Setting.Connection as SC
import qualified Hasql.Decoders as Dec
import qualified Hasql.Encoders as Enc
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt
import Hylo.Core.Codec (RowCodec, runBinaryDecoder)
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
import Hylo.PG.Transaction.Hooks (executeCommitHooks, executeRollbackHooks)
import Hylo.SQL.Builder (formatParamValue)
import Text.Read (readMaybe)

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
  Tx db mode a ->
  IO (Either TxError a)
runTx targetPool mode tx = do
  let beginSql = case mode of
        ReadOnly -> "BEGIN TRANSACTION READ ONLY;"
        ReadWrite -> "BEGIN TRANSACTION READ WRITE;"
  (res, finalSt) <- runTxAttempt targetPool beginSql initialTxState tx
  case res of
    Left err -> do
      executeRollbackHooks finalSt
      pure (Left err)
    Right val -> do
      executeCommitHooks finalSt
      pure (Right val)

{- | Execute a single transaction attempt against a pool with a specific BEGIN command.
If the pool has an active connection, executes against PostgreSQL; otherwise evaluates purely.
Returns the result along with the final TxState.
-}
runTxAttempt ::
  Pool ->
  Text ->
  TxState ->
  Tx db mode a ->
  IO (Either TxError a, TxState)
runTxAttempt (Pool _ _ (Just conn)) beginSql st0 tx = do
  let (res, st) = executeTx tx st0
  case res of
    Left err -> pure (Left err, st)
    Right val -> do
      beginRes <- Sess.run (Sess.sql (TE.encodeUtf8 beginSql)) conn
      case beginRes of
        Left qErr -> do
          _ <- Sess.run (Sess.sql "ROLLBACK;") conn
          pure (Left (translateHasqlError qErr), st)
        Right () -> do
          opsRes <- runOps (txOperations st)
          case opsRes of
            Left err -> do
              _ <- Sess.run (Sess.sql "ROLLBACK;") conn
              pure (Left err, st)
            Right () -> do
              commitRes <- Sess.run (Sess.sql "COMMIT;") conn
              case commitRes of
                Left qErr -> do
                  _ <- Sess.run (Sess.sql "ROLLBACK;") conn
                  pure (Left (translateHasqlError qErr), st)
                Right () -> pure (Right val, st)
  where
    runOps [] = pure (Right ())
    runOps (op : rest) = case op of
      OpMutate (sqlText, params) -> do
        res <- executeOp sqlText params
        case res of
          Left err -> pure (Left err)
          Right () -> runOps rest
      OpQuery (sqlText, params) -> do
        res <- executeOp sqlText params
        case res of
          Left err -> pure (Left err)
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
    executeOp sqlText params
      | null params && T.isInfixOf ";" (T.dropEnd 1 (T.stripEnd sqlText)) = do
          res <- Sess.run (Sess.sql (TE.encodeUtf8 sqlText)) conn
          pure (either (Left . translateHasqlError) Right res)
      | otherwise = do
          let stmt = mkPreparedStatement (TE.encodeUtf8 sqlText) params
          res <- Sess.run (Sess.statement params stmt) conn
          pure (either (Left . translateHasqlError) Right res)
runTxAttempt (Pool _ _ Nothing) _ st0 tx =
  pure (executeTx tx st0)

-- | Substitute bound parameters into SQL using single-pass tokenization without recursion or digit prefix collision.
interpolateParams :: Text -> [BoundParam] -> Text
interpolateParams sql [] = sql
interpolateParams sql params = go sql
  where
    paramMap = Map.fromList [(paramIndex p, formatParamValue (paramValue p) (paramType p)) | p <- params]
    go t = case T.break (== '$') t of
      (before, rest)
        | T.null rest -> before
        | otherwise ->
            let afterDollar = T.drop 1 rest
                (digits, remaining) = T.span isDigit afterDollar
             in if T.null digits
                  then before <> "$" <> go afterDollar
                  else case readMaybe (T.unpack digits) of
                    Just idx
                      | Just formatted <- Map.lookup idx paramMap ->
                          before <> formatted <> go remaining
                    _ -> before <> "$" <> digits <> go remaining

-- | Value encoder for a single bound parameter based on its inferred logical/SQL type.
boundParamValueEncoder :: Text -> Enc.Value Text
boundParamValueEncoder pType = case pType of
  "integer" -> contramap parseOrZero Enc.int4
  "int4" -> contramap parseOrZero Enc.int4
  "bigint" -> contramap parseOrZero Enc.int8
  "int8" -> contramap parseOrZero Enc.int8
  "smallint" -> contramap parseOrZero Enc.int2
  "int2" -> contramap parseOrZero Enc.int2
  "boolean" -> contramap (\t -> t == "true" || t == "TRUE" || t == "t") Enc.bool
  "bool" -> contramap (\t -> t == "true" || t == "TRUE" || t == "t") Enc.bool
  "double precision" -> contramap parseOrZero Enc.float8
  "float8" -> contramap parseOrZero Enc.float8
  "numeric" -> contramap parseOrZero Enc.float8
  "real" -> contramap parseOrZero Enc.float4
  "float4" -> contramap parseOrZero Enc.float4
  "bytea" -> contramap TE.encodeUtf8 Enc.bytea
  "unknown" -> contramap TE.encodeUtf8 Enc.unknown
  _ -> Enc.text
  where
    parseOrZero :: (Num a, Read a) => Text -> a
    parseOrZero = fromMaybe 0 . readMaybe . T.unpack

-- | Construct a Hasql parameter encoder for a dynamic list of bound parameters.
buildParamsEncoder :: [BoundParam] -> Enc.Params [BoundParam]
buildParamsEncoder ps =
  mconcat
    [ contramap
        (\xs -> if i < length xs then paramValue (xs !! i) else "")
        (Enc.param (Enc.nonNullable (boundParamValueEncoder (paramType p))))
    | (i, p) <- zip [0 ..] ps
    ]

-- | Build a prepared Hasql statement sending [BoundParam] and discarding results.
mkPreparedStatement :: ByteString -> [BoundParam] -> Stmt.Statement [BoundParam] ()
mkPreparedStatement sqlBS ps =
  Stmt.Statement sqlBS (buildParamsEncoder ps) Dec.noResult True

-- | Decode a single column as raw nullable bytes.
rawColumnDecoder :: Dec.Row (Maybe ByteString)
rawColumnDecoder = Dec.column (Dec.nullable (Dec.custom (\_ bs -> Right bs)))

-- | Assemble raw PostgreSQL wire column payloads into a framed binary row byte string.
assembleRowBytes :: [Maybe ByteString] -> ByteString
assembleRowBytes cols =
  BSL.toStrict (Builder.toLazyByteString (mconcat columnBuilders))
  where
    columnBuilders =
      [ case mbs of
          Nothing -> Builder.int32BE (-1)
          Just bs -> Builder.int32BE (fromIntegral (BS.length bs)) <> Builder.byteString bs
      | mbs <- cols
      ]

-- | Decode a row from PostgreSQL wire column values using a 'RowCodec'.
decodeRowWithCodec :: RowCodec r a -> Int -> Dec.Row a
decodeRowWithCodec codec numCols = do
  cols <- replicateM numCols rawColumnDecoder
  let rowBytes = assembleRowBytes cols
  case runBinaryDecoder codec rowBytes of
    Left err -> fail (T.unpack err)
    Right val -> pure val

-- | Build a prepared Hasql statement sending [BoundParam] and decoding result rows via 'RowCodec'.
mkPreparedQueryStatement ::
  ByteString ->
  [BoundParam] ->
  RowCodec r a ->
  Int ->
  Stmt.Statement [BoundParam] [a]
mkPreparedQueryStatement sqlBS params codec numCols =
  Stmt.Statement
    sqlBS
    (buildParamsEncoder params)
    (Dec.rowList (decodeRowWithCodec codec numCols))
    True

-- | Execute a prepared query against PostgreSQL connection pool and hydrate domain rows.
executePreparedQuery ::
  Pool ->
  Text ->
  [BoundParam] ->
  RowCodec r a ->
  Int ->
  IO (Either TxError [a])
executePreparedQuery (Pool _ _ (Just conn)) sqlText params codec numCols = do
  let stmt = mkPreparedQueryStatement (TE.encodeUtf8 sqlText) params codec numCols
  res <- Sess.run (Sess.statement params stmt) conn
  case res of
    Left qErr -> pure (Left (translateHasqlError qErr))
    Right rows -> pure (Right rows)
executePreparedQuery (Pool _ _ Nothing) _ _ _ _ =
  pure (Left (TxUserError "executePreparedQuery: no active PostgreSQL connection in pool"))

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
            "23001" -> TxConstraintViolation "23001" message detail
            "23514" -> TxConstraintViolation "23514" message detail
            "40001" -> TxWriteConflict message
            "40P01" -> TxDeadlockDetected message
            _ -> TxUserError (message <> if detail /= "" then " (" <> detail <> ")" else "")
    _ -> TxUserError (T.pack (show cmdErr))
  Sess.PipelineError pipErr -> TxUserError ("PipelineError: " <> T.pack (show pipErr))
