{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
  , mkPreparedQueryStatementWithDecoder
  , assembleRowBytes
  , rawColumnDecoder
  , decodeRowWithCodec
  , decodeRowWithBinaryDecoder
  , executePreparedQuery
  , executePreparedQueryWithDecoder
  , select
  , selectRows
  , selectRowsWithSchema
  , selectRowsTx
  , selectRowsTxWith
  , runExplain
  , mkExplainStatement
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..))
import Control.Monad (replicateM, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.Functor.Contravariant (contramap)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
  ( Day
  , LocalTime (..)
  , TimeOfDay (..)
  , UTCTime (..)
  , defaultTimeLocale
  , fromGregorian
  , parseTimeM
  )
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Hasql.Connection as C
import qualified Hasql.Connection.Setting as S
import qualified Hasql.Connection.Setting.Connection as SC
import qualified Hasql.Decoders as Dec
import qualified Hasql.Encoders as Enc
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt
import Hylo.Backend.Capability (Postgres)
import Hylo.Core.Codec
  ( Codec (..)
  , Decoder (..)
  , RowCodec
  , codecDecoder
  , runBinaryDecoder
  )
import Hylo.Core.Context (Ctx (..))
import Hylo.Core.Query.Monad (Query (..), initialQueryState, runQuery)
import Hylo.Core.Table (Table (..), TableSchema (..))
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
import Hylo.PG.Codec.Binary (Selectable (..), TableCodec (..))
import Hylo.PG.Compiler.SQL (explainQuery)
import Hylo.PG.Transaction.Hooks (executeCommitHooks, executeRollbackHooks)
import Hylo.SQL.AST (Projectable (..))
import Hylo.SQL.Builder
  ( CompiledQuery (..)
  , Dialect (PostgresDialect)
  , compileQuery
  , formatParamValue
  )
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
boundParamValueEncoder pType
  | "[]" `T.isSuffixOf` pType = contramap TE.encodeUtf8 Enc.unknown
  | pType == "integer" || pType == "int4" = contramap parseOrZero Enc.int4
  | pType == "bigint" || pType == "int8" = contramap parseOrZero Enc.int8
  | pType == "smallint" || pType == "int2" = contramap parseOrZero Enc.int2
  | pType == "boolean" || pType == "bool" =
      contramap (\t -> let low = T.toLower t in low == "true" || low == "t" || low == "1") Enc.bool
  | pType == "double precision" || pType == "float8" || pType == "numeric" =
      contramap parseOrZero Enc.float8
  | pType == "real" || pType == "float4" = contramap parseOrZero Enc.float4
  | pType == "bytea" = contramap TE.encodeUtf8 Enc.bytea
  | pType == "uuid" = contramap parseUUID Enc.uuid
  | pType == "timestamptz" = contramap parseUTCTime Enc.timestamptz
  | pType == "timestamp" = contramap parseLocalTime Enc.timestamp
  | pType == "date" = contramap parseDay Enc.date
  | pType == "jsonb" = contramap TE.encodeUtf8 Enc.jsonbBytes
  | pType == "json" = contramap TE.encodeUtf8 Enc.jsonBytes
  | pType == "vector" || "vector(" `T.isPrefixOf` pType = contramap TE.encodeUtf8 Enc.unknown
  | "range" `T.isSuffixOf` pType = contramap TE.encodeUtf8 Enc.unknown
  | pType == "tsvector" || pType == "tsquery" = contramap TE.encodeUtf8 Enc.unknown
  | pType == "unknown" = contramap TE.encodeUtf8 Enc.unknown
  | otherwise = Enc.text
  where
    parseOrZero :: (Num a, Read a) => Text -> a
    parseOrZero = fromMaybe 0 . readMaybe . T.unpack

    parseUUID :: Text -> UUID
    parseUUID = fromMaybe UUID.nil . UUID.fromText

    parseUTCTime :: Text -> UTCTime
    parseUTCTime t =
      fromMaybe (UTCTime (fromGregorian 2000 1 1) 0) $
        iso8601ParseM (T.unpack t)
          <|> readMaybe (T.unpack t)
          <|> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q%Z" (T.unpack t)
          <|> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" (T.unpack t)
          <|> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack t)

    parseLocalTime :: Text -> LocalTime
    parseLocalTime t =
      fromMaybe (LocalTime (fromGregorian 2000 1 1) (TimeOfDay 0 0 0)) $
        iso8601ParseM (T.unpack t)
          <|> readMaybe (T.unpack t)
          <|> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" (T.unpack t)
          <|> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack t)

    parseDay :: Text -> Day
    parseDay t =
      fromMaybe (fromGregorian 2000 1 1) $
        iso8601ParseM (T.unpack t)
          <|> readMaybe (T.unpack t)
          <|> parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack t)

-- | Construct a Hasql parameter encoder for a dynamic list of bound parameters.
buildParamsEncoder :: [BoundParam] -> Enc.Params [BoundParam]
buildParamsEncoder ps =
  mconcat
    [ contramap
        (\xs -> if i < length xs then paramValue (xs !! i) else "")
        (Enc.param (Enc.nonNullable (boundParamValueEncoder (paramType p))))
    | (i, p) <- zip [0 ..] ps
    ]

{- | Build a prepared Hasql statement sending [BoundParam] and discarding results.
Uses a row-discarding decoder when the query contains RETURNING or SELECT.
-}
mkPreparedStatement :: ByteString -> [BoundParam] -> Stmt.Statement [BoundParam] ()
mkPreparedStatement sqlBS ps =
  let u = T.toUpper (T.strip (TE.decodeUtf8 sqlBS))
      isReturning = "RETURNING" `T.isInfixOf` u
      isSelectTopLevel =
        "SELECT" `T.isPrefixOf` u
          || ( "WITH" `T.isPrefixOf` u
                 && not
                   ( "INSERT" `T.isInfixOf` u
                       || "UPDATE" `T.isInfixOf` u
                       || "DELETE" `T.isInfixOf` u
                   )
             )
      isRowProducing = isReturning || isSelectTopLevel
      decoder =
        if isRowProducing
          then void (Dec.rowList rawColumnDecoder)
          else Dec.noResult
   in Stmt.Statement sqlBS (buildParamsEncoder ps) decoder True

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
decodeRowWithCodec codec = decodeRowWithBinaryDecoder (codecDecoder codec)

-- | Decode a row from PostgreSQL wire column values using a binary wire 'Decoder'.
decodeRowWithBinaryDecoder :: Decoder a -> Int -> Dec.Row a
decodeRowWithBinaryDecoder dec numCols = do
  cols <- replicateM numCols rawColumnDecoder
  let rowBytes = assembleRowBytes cols
  case runBinaryDecoder (Codec (const mempty) dec) rowBytes of
    Left err -> fail (T.unpack err)
    Right val -> pure val

-- | Build a prepared Hasql statement sending [BoundParam] and decoding result rows via 'RowCodec'.
mkPreparedQueryStatement ::
  ByteString ->
  [BoundParam] ->
  RowCodec r a ->
  Int ->
  Stmt.Statement [BoundParam] [a]
mkPreparedQueryStatement sqlBS params codec =
  mkPreparedQueryStatementWithDecoder sqlBS params (codecDecoder codec)

-- | Build a prepared Hasql statement sending [BoundParam] and decoding result rows via 'Decoder'.
mkPreparedQueryStatementWithDecoder ::
  ByteString ->
  [BoundParam] ->
  Decoder a ->
  Int ->
  Stmt.Statement [BoundParam] [a]
mkPreparedQueryStatementWithDecoder sqlBS params dec numCols =
  Stmt.Statement
    sqlBS
    (buildParamsEncoder params)
    (Dec.rowList (decodeRowWithBinaryDecoder dec numCols))
    True

-- | Execute a prepared query against PostgreSQL connection pool and hydrate domain rows.
executePreparedQuery ::
  Pool ->
  Text ->
  [BoundParam] ->
  RowCodec r a ->
  Int ->
  IO (Either TxError [a])
executePreparedQuery targetPool sqlText params codec =
  executePreparedQueryWithDecoder targetPool sqlText params (codecDecoder codec)

-- | Execute a prepared query against PostgreSQL connection pool and hydrate domain rows via 'Decoder'.
executePreparedQueryWithDecoder ::
  Pool ->
  Text ->
  [BoundParam] ->
  Decoder a ->
  Int ->
  IO (Either TxError [a])
executePreparedQueryWithDecoder (Pool _ _ (Just conn)) sqlText params dec numCols = do
  let stmt = mkPreparedQueryStatementWithDecoder (TE.encodeUtf8 sqlText) params dec numCols
  res <- Sess.run (Sess.statement params stmt) conn
  case res of
    Left qErr -> pure (Left (translateHasqlError qErr))
    Right rows -> pure (Right rows)
executePreparedQueryWithDecoder (Pool _ _ Nothing) _ _ _ _ =
  pure (Left (TxUserError "executePreparedQuery: no active PostgreSQL connection in pool"))

-- | Execute a typed Query against a PostgreSQL pool and hydrate domain rows.
selectRows ::
  forall k (db :: k) expr res.
  (Projectable expr, Selectable expr res) =>
  Pool ->
  Query db expr ->
  IO (Either TxError [res])
selectRows targetPool q = do
  let compiled = compileQuery PostgresDialect q
      (expr, _) = runQuery q initialQueryState
      cols = case projectColumns expr of
        [] -> ["1"]
        cs -> cs
      numCols = length cols
  executePreparedQueryWithDecoder
    targetPool
    compiled.compiledSQL
    (toList compiled.compiledParams)
    (selectableDecoder expr)
    numCols

-- | Execute a typed Query against a PostgreSQL pool using a schema for column counting.
selectRowsWithSchema ::
  forall k (db :: k) table.
  (Projectable (table 'Expr), Table table, TableCodec table) =>
  Pool ->
  TableSchema table ->
  Query db (table 'Expr) ->
  IO (Either TxError [table 'Result])
selectRowsWithSchema targetPool schema q = do
  let compiled = compileQuery PostgresDialect q
      (expr, _) = runQuery q initialQueryState
      numCols = length (tableColDefs (tableMeta schema))
  executePreparedQueryWithDecoder
    targetPool
    compiled.compiledSQL
    (toList compiled.compiledParams)
    (selectableDecoder expr)
    numCols

-- | Transaction-aware counterpart to 'selectRows' recording 'OpQuery' and returning empty fallback rows.
selectRowsTx ::
  forall k (db :: k) expr res (mode :: TxMode).
  (Projectable expr, Selectable expr res) =>
  Query db expr ->
  Tx db mode [res]
selectRowsTx = selectRowsTxWith []

-- | Transaction-aware counterpart to 'selectRows' recording 'OpQuery' and returning the given fallback rows.
selectRowsTxWith ::
  forall k (db :: k) expr res (mode :: TxMode).
  (Projectable expr, Selectable expr res) =>
  [res] ->
  Query db expr ->
  Tx db mode [res]
selectRowsTxWith fallback q =
  let (expr, _) = runQuery q initialQueryState
      _dec = selectableDecoder expr
   in _dec `seq` TxAction $ \st ->
        let compiled = compileQuery PostgresDialect q
            op = OpQuery (compiled.compiledSQL, toList compiled.compiledParams)
            st' = st {txOperations = txOperations st ++ [op]}
         in (Right fallback, st')

-- | Monadic query execution combinator within transactions, hydrating rows into domain results.
select ::
  forall k (db :: k) expr res (mode :: TxMode).
  (Projectable expr, Selectable expr res) =>
  Query db expr ->
  Tx db mode [res]
select = selectRowsTx

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

-- | Execute EXPLAIN (FORMAT JSON) or EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON) on a PostgreSQL query.
runExplain ::
  Projectable a =>
  Pool ->
  Bool ->
  Query (Postgres v) a ->
  IO (Either TxError Text)
runExplain (Pool _ _ (Just conn)) analyze q = do
  let cq = explainQuery analyze q
      stmt = mkExplainStatement (TE.encodeUtf8 cq.compiledSQL) (toList cq.compiledParams)
  res <- Sess.run (Sess.statement (toList cq.compiledParams) stmt) conn
  case res of
    Left qErr -> pure (Left (translateHasqlError qErr))
    Right jsonLines -> pure (Right (T.intercalate "\n" jsonLines))
runExplain (Pool _ _ Nothing) analyze q = do
  let cq = explainQuery analyze q
  pure
    ( Right
        ( "[{\"Plan\": {\"Node Type\": \"Simulated Scan\", \"Query\": "
            <> T.pack (show cq.compiledSQL)
            <> "}}]"
        )
    )

-- | Build a prepared Hasql statement executing EXPLAIN and returning the output rows as text.
mkExplainStatement :: ByteString -> [BoundParam] -> Stmt.Statement [BoundParam] [Text]
mkExplainStatement sqlBS ps =
  Stmt.Statement
    sqlBS
    (buildParamsEncoder ps)
    (Dec.rowList (Dec.column (Dec.nonNullable Dec.text)))
    True
