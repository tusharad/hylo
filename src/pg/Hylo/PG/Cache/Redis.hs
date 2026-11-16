{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.Cache.Redis
Description : Redis read-through caching engine and onCommit invalidation combinators
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Cache.Redis
  ( -- * Read-Through Cache Combinators
    selectCachedById
  , selectCachedByIdWith
  , selectCachedByIdWithFetcher
  , selectManyCachedByIds
  , selectManyCachedByIdsWith
  , selectManyCachedByIdsWithFetcher

    -- * Transaction-Scoped Cache Query Combinators
  , selectCachedByIdTx
  , selectManyCachedByIdsTx

    -- * Helper Combinators
  , defaultPkExtractor
  , findPrimaryKeyColName
  , findPrimaryKeyColType
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core
  ( BoundParam (..)
  , ColDef (..)
  , Ctx (..)
  , Id (..)
  , RedisCachePolicy (..)
  , RowCodec
  , Table (..)
  , TableSchema (..)
  , ToFieldValue (..)
  , Tx (..)
  , TxError (..)
  , TxMode (..)
  , TxOp (..)
  , TxState (..)
  , colName
  , colSqlType
  , runBinaryDecoder
  , runBinaryEncoder
  , tableName
  )
import Hylo.PG.Codec.Binary (TableCodec (..))
import Hylo.PG.Connection (Pool, executePreparedQuery)
import Hylo.Redis
  ( RedisClient (..)
  , tableCacheKey
  )
import Hylo.SQL.Builder
  ( Dialect (PostgresDialect)
  , quoteIdentifier
  )

-- | Extract the primary key string from a hydrated record using 'tablePrimaryKey' or schema lookup.
defaultPkExtractor :: forall table. (Table table) => TableSchema table -> table 'Result -> Text
defaultPkExtractor _ row = fromMaybe "" (tablePrimaryKey row)

-- | Locate the primary key column name in a table schema, defaulting to @"id"@.
findPrimaryKeyColName :: Table table => TableSchema table -> Text
findPrimaryKeyColName schema =
  case [colName c | c <- tableColDefs (tableMeta schema), colIsPrimaryKey c] of
    (c : _) -> c
    [] -> "id"

-- | Locate the primary key SQL type in a table schema, defaulting to @"bigint"@.
findPrimaryKeyColType :: Table table => TableSchema table -> Text
findPrimaryKeyColType schema =
  case [colSqlType c | c <- tableColDefs (tableMeta schema), colIsPrimaryKey c] of
    (t : _) -> if T.null t then "bigint" else t
    [] -> "bigint"

-- | Read a single record by primary key through Redis cache with live PostgreSQL fallback.
selectCachedById ::
  forall table pk.
  (Table table, TableCodec table, ToFieldValue pk) =>
  RedisClient ->
  Pool ->
  TableSchema table ->
  Id table pk ->
  IO (Either TxError (Maybe (table 'Result)))
selectCachedById client pool schema =
  selectCachedByIdWith client pool schema tableCodec (defaultPkExtractor schema)

-- | Read a single record by primary key through Redis cache with an explicit 'RowCodec'.
selectCachedByIdWith ::
  forall table pk.
  (Table table, ToFieldValue pk) =>
  RedisClient ->
  Pool ->
  TableSchema table ->
  RowCodec (table 'Result) (table 'Result) ->
  (table 'Result -> Text) ->
  Id table pk ->
  IO (Either TxError (Maybe (table 'Result)))
selectCachedByIdWith client pool schema codec pkExtractor reqId =
  let numCols = length (tableColDefs (tableMeta schema))
      fetcher sql params = executePreparedQuery pool sql params codec numCols
   in selectCachedByIdWithFetcher client fetcher schema codec pkExtractor reqId

-- | Read a single record by primary key through Redis cache with a custom query fetcher.
selectCachedByIdWithFetcher ::
  forall table pk.
  (Table table, ToFieldValue pk) =>
  RedisClient ->
  (Text -> [BoundParam] -> IO (Either TxError [table 'Result])) ->
  TableSchema table ->
  RowCodec (table 'Result) (table 'Result) ->
  (table 'Result -> Text) ->
  Id table pk ->
  IO (Either TxError (Maybe (table 'Result)))
selectCachedByIdWithFetcher client fetcher schema codec pkExtractor reqId = do
  case tableCachePolicy schema of
    Nothing -> do
      res <- selectManyCachedByIdsWithFetcher client fetcher schema codec pkExtractor [reqId]
      pure (listToMaybe <$> res)
    Just policy -> do
      let key = tableCacheKey policy schema (toFieldValue reqId)
      mBytes <- redisGet client key
      case mBytes of
        Just bs -> case runBinaryDecoder codec bs of
          Right row -> pure (Right (Just row))
          Left _ -> do
            res <- selectManyCachedByIdsWithFetcher client fetcher schema codec pkExtractor [reqId]
            pure (listToMaybe <$> res)
        Nothing -> do
          res <- selectManyCachedByIdsWithFetcher client fetcher schema codec pkExtractor [reqId]
          pure (listToMaybe <$> res)

-- | Read multiple records by primary keys with pipelined Redis MGET, PostgreSQL fallback, and SETEX backfill.
selectManyCachedByIds ::
  forall table pk.
  (Table table, TableCodec table, ToFieldValue pk) =>
  RedisClient ->
  Pool ->
  TableSchema table ->
  [Id table pk] ->
  IO (Either TxError [table 'Result])
selectManyCachedByIds client pool schema =
  selectManyCachedByIdsWith client pool schema tableCodec (defaultPkExtractor schema)

-- | Read multiple records by primary keys with an explicit 'RowCodec' and custom PK extractor.
selectManyCachedByIdsWith ::
  forall table pk.
  (Table table, ToFieldValue pk) =>
  RedisClient ->
  Pool ->
  TableSchema table ->
  RowCodec (table 'Result) (table 'Result) ->
  (table 'Result -> Text) ->
  [Id table pk] ->
  IO (Either TxError [table 'Result])
selectManyCachedByIdsWith client pool schema codec pkExtractor ids =
  let numCols = length (tableColDefs (tableMeta schema))
      fetcher sql params = executePreparedQuery pool sql params codec numCols
   in selectManyCachedByIdsWithFetcher client fetcher schema codec pkExtractor ids

-- | Read multiple records by primary keys using a custom query fetcher.
selectManyCachedByIdsWithFetcher ::
  forall table pk.
  (Table table, ToFieldValue pk) =>
  RedisClient ->
  (Text -> [BoundParam] -> IO (Either TxError [table 'Result])) ->
  TableSchema table ->
  RowCodec (table 'Result) (table 'Result) ->
  (table 'Result -> Text) ->
  [Id table pk] ->
  IO (Either TxError [table 'Result])
selectManyCachedByIdsWithFetcher client fetcher schema codec pkExtractor ids
  | null ids = pure (Right [])
  | otherwise = case tableCachePolicy schema of
      Nothing -> queryPostgresDirectly
      Just policy -> do
        let keyIdPairs = [(tableCacheKey policy schema (toFieldValue reqId), reqId) | reqId <- ids]
            keys = map fst keyIdPairs
        cachedBytesList <- redisMGet client keys
        let decodedResults =
              [ case mbs of
                  Just bs -> case runBinaryDecoder codec bs of
                    Right row -> Just (toFieldValue reqId, row)
                    Left _ -> Nothing
                  Nothing -> Nothing
              | ((_, reqId), mbs) <- zip keyIdPairs cachedBytesList
              ]
            missIds = [reqId | ((_, reqId), Nothing) <- zip keyIdPairs decodedResults]
        if null missIds
          then do
            let hitMap = Map.fromList (catMaybes decodedResults)
                orderedHits = [r | reqId <- ids, Just r <- [Map.lookup (toFieldValue reqId) hitMap]]
            pure (Right orderedHits)
          else do
            let missPks = map toFieldValue missIds
                sql = buildSelectAnySql schema
                pkType = findPrimaryKeyColType schema
                param = BoundParam 1 ("{" <> T.intercalate "," missPks <> "}") (pkType <> "[]")
            pgRes <- fetcher sql [param]
            case pgRes of
              Left err -> pure (Left err)
              Right fetchedRows -> do
                -- Backfill missing keys in Redis via pipelined SETEX
                let setexItems =
                      [ ( tableCacheKey policy schema (pkExtractor row)
                        , rcpTtlSeconds policy
                        , runBinaryEncoder codec row
                        )
                      | row <- fetchedRows
                      ]
                redisSetExMany client setexItems

                -- Merge cached hits and newly fetched rows preserving requested ID ordering
                let hitMap = Map.fromList (catMaybes decodedResults)
                    fetchedMap = Map.fromList [(pkExtractor r, r) | r <- fetchedRows]
                    allMap = Map.union hitMap fetchedMap
                    orderedAll = [r | reqId <- ids, Just r <- [Map.lookup (toFieldValue reqId) allMap]]
                pure (Right orderedAll)
  where
    queryPostgresDirectly = do
      let allPks = map toFieldValue ids
          sql = buildSelectAnySql schema
          pkType = findPrimaryKeyColType schema
          param = BoundParam 1 ("{" <> T.intercalate "," allPks <> "}") (pkType <> "[]")
      pgRes <- fetcher sql [param]
      case pgRes of
        Left err -> pure (Left err)
        Right rows -> do
          let rowMap = Map.fromList [(pkExtractor r, r) | r <- rows]
              ordered = [r | reqId <- ids, Just r <- [Map.lookup (toFieldValue reqId) rowMap]]
          pure (Right ordered)

-- | Construct the parameterized @SELECT ... WHERE "id" = ANY($1)@ query.
buildSelectAnySql :: Table table => TableSchema table -> Text
buildSelectAnySql schema =
  let tName = quoteIdentifier PostgresDialect (tableName schema)
      pkCol = quoteIdentifier PostgresDialect (findPrimaryKeyColName schema)
      cols = case [quoteIdentifier PostgresDialect (colName c) | c <- tableColDefs (tableMeta schema)] of
        [] -> ["*"]
        cs -> cs
      colList = T.intercalate ", " cols
   in "SELECT " <> colList <> " FROM " <> tName <> " WHERE " <> pkCol <> " = ANY($1)"

-- | Transaction-aware simulation counterpart for 'selectCachedById'.
selectCachedByIdTx ::
  forall k (db :: k) (mode :: TxMode) table pk.
  (Table table, ToFieldValue pk) =>
  TableSchema table ->
  Maybe (table 'Result) ->
  Id table pk ->
  Tx db mode (Maybe (table 'Result))
selectCachedByIdTx schema fallback reqId =
  TxAction $ \st ->
    let sql = buildSelectAnySql schema
        pkType = findPrimaryKeyColType schema
        param = BoundParam 1 ("{" <> toFieldValue reqId <> "}") (pkType <> "[]")
        st' = st {txOperations = txOperations st ++ [OpQuery (sql, [param])]}
     in (Right fallback, st')

-- | Transaction-aware simulation counterpart for 'selectManyCachedByIds'.
selectManyCachedByIdsTx ::
  forall k (db :: k) (mode :: TxMode) table pk.
  (Table table, ToFieldValue pk) =>
  TableSchema table ->
  [table 'Result] ->
  [Id table pk] ->
  Tx db mode [table 'Result]
selectManyCachedByIdsTx schema fallback ids =
  TxAction $ \st ->
    let sql = buildSelectAnySql schema
        pkType = findPrimaryKeyColType schema
        param = BoundParam 1 ("{" <> T.intercalate "," (map toFieldValue ids) <> "}") (pkType <> "[]")
        st' = st {txOperations = txOperations st ++ [OpQuery (sql, [param])]}
     in (Right fallback, st')
