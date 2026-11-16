{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{- |
Module      : Hylo.PG.Cache.RedisSpec
Description : Test suite for User Story 3: Transaction-safe Redis caching & onCommit invalidation
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Cache.RedisSpec (spec) where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Monad (forM_)
import Data.Function ((&))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Core
import Hylo.Core.Query (PGVersion (..), Postgres, deleteById, deleteWhere)
import Hylo.PG
import Hylo.Redis
import Test.Hspec

default (Text, Int64)

--------------------------------------------------------------------------------
-- 4-Column HKD Entity for Read-Through Caching & Invalidation Testing
--------------------------------------------------------------------------------

data CacheAccountT (ctx :: Ctx) = CacheAccount
  { id :: !(Col ctx (Id CacheAccountT Int64))
  , name :: !(Col ctx Text)
  , email :: !(Col ctx Text)
  , score :: !(Col ctx Int64)
  }
  deriving (Generic)

type CacheAccount = CacheAccountT 'Result

deriving instance Eq CacheAccount
deriving instance Show CacheAccount
deriving instance Eq (CacheAccountT 'Patch)
deriving instance Show (CacheAccountT 'Patch)

cacheAccountSchema :: TableSchema CacheAccountT
cacheAccountSchema =
  tableSchema "cache_accounts" $
    CacheAccount
      { id = primaryKey "id" "bigint"
      , name = column "name" "text"
      , email = column "email" "text"
      , score = column "score" "bigint"
      }

instance Table CacheAccountT where
  toTableExpr alias meta =
    CacheAccount
      { id = colSpecExpr alias meta.id
      , name = colSpecExpr alias meta.name
      , email = colSpecExpr alias meta.email
      , score = colSpecExpr alias meta.score
      }
  diffTable old new =
    CacheAccount
      { id = diffField old.id new.id
      , name = diffField old.name new.name
      , email = diffField old.email new.email
      , score = diffField old.score new.score
      }
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.name
    , eraseColDef m.email
    , eraseColDef m.score
    ]
  patchEntries meta patch =
    concat
      [ patchEntry meta.id patch.id
      , patchEntry meta.name patch.name
      , patchEntry meta.email patch.email
      , patchEntry meta.score patch.score
      ]
  tablePrimaryKey acc = Just (toFieldValue acc.id)

instance TableCodec CacheAccountT where
  tableCodec =
    dimap
      (\acc -> (acc.id, acc.name, acc.email, acc.score))
      (\(i, n, e, s) -> CacheAccount i n e s)
      (quadCodec (idCodec int64Codec) textCodec textCodec int64Codec)

spec :: Spec
spec = describe "User Story 3: Transaction-Safe Redis Read-Through Caching" $ do
  describe "Cache Policy Metadata (Hylo.Core.Table)" $ do
    it "defaults tableCachePolicy to Nothing in tableSchema" $ do
      tableCachePolicy cacheAccountSchema `shouldBe` Nothing

    it "attaches RedisCachePolicy via withRedisCache" $ do
      let policy = RedisCachePolicy "user_cache" 300
          cachedSchema = withRedisCache policy cacheAccountSchema
      tableCachePolicy cachedSchema `shouldBe` Just policy
      rcpKeyPrefix policy `shouldBe` "user_cache"
      rcpTtlSeconds policy `shouldBe` 300

    it "formulates canonical Redis cache key '<prefix>:<tableName>:<pk>'" $ do
      let policy = RedisCachePolicy "app" 600
          key = tableCacheKey policy cacheAccountSchema "42"
      key `shouldBe` "app:cache_accounts:42"

  describe "Read-Through Cache Combinator: selectManyCachedByIds" $ do
    it
      "executes 1 Redis MGET, 1 PostgreSQL query for 20 misses, 1 pipelined SETEX, and returns all 100 in order"
      $ do
        (client, storeVar, historyVar) <- newMockRedis
        let policy = RedisCachePolicy "cache" 3600
            cachedSchema = withRedisCache policy cacheAccountSchema
            all100Ids = [Id i :: Id CacheAccountT Int64 | i <- [1 .. 100]]

        -- 1. Prepopulate Redis with 80 records (IDs 1..80)
        let redisAccounts =
              [ CacheAccount
                  (Id i)
                  ("User " <> T.pack (show i))
                  ("user" <> T.pack (show i) <> "@example.com")
                  (i * 10)
              | i <- [1 .. 80]
              ]
        forM_ redisAccounts $ \acc -> do
          let key = tableCacheKey policy cachedSchema (toFieldValue acc.id)
              encoded = runBinaryEncoder tableCodec acc
          redisSet client key encoded

        -- Clear command history after prepopulation
        atomically $ writeTVar historyVar []

        -- 2. Mock PostgreSQL query fetcher for the 20 misses (IDs 81..100)
        dbQueryCountRef <- newIORef (0 :: Int)
        capturedSqlRef <- newIORef ("" :: Text)
        capturedParamsRef <- newIORef ([] :: [BoundParam])

        let mockDbFetcher sql params = do
              modifyIORef' dbQueryCountRef (+ 1)
              writeIORef capturedSqlRef sql
              writeIORef capturedParamsRef params
              -- PostgreSQL returns the missing rows in non-sequential order to verify sorting preservation
              let missAccounts =
                    [ CacheAccount
                        (Id i)
                        ("User " <> T.pack (show i))
                        ("user" <> T.pack (show i) <> "@example.com")
                        (i * 10)
                    | i <- [100, 99 .. 81]
                    ]
              pure (Right missAccounts)

        -- 3. Execute selectManyCachedByIdsWithFetcher
        res <-
          selectManyCachedByIdsWithFetcher
            client
            mockDbFetcher
            cachedSchema
            tableCodec
            (defaultPkExtractor cachedSchema)
            all100Ids

        -- 4. Verify all 100 domain records are returned in exact original ID order (1..100)
        case res of
          Left err -> expectationFailure ("Expected successful hydration, got error: " ++ show err)
          Right hydratedRows -> do
            length hydratedRows `shouldBe` 100
            let expected100 =
                  [ CacheAccount
                      (Id i)
                      ("User " <> T.pack (show i))
                      ("user" <> T.pack (show i) <> "@example.com")
                      (i * 10)
                  | i <- [1 .. 100]
                  ]
            hydratedRows `shouldBe` expected100

        -- 5. Verify Redis command history: exactly 1 Redis MGET was executed
        history <- readTVarIO historyVar
        let mgetCmds = [cmd | cmd@(RedisMGet _) <- history]
        length mgetCmds `shouldBe` 1
        case mgetCmds of
          [RedisMGet keys] -> length keys `shouldBe` 100
          _ -> expectationFailure "Expected exactly 1 RedisMGet"

        -- 6. Verify PostgreSQL query: exactly 1 query executed with WHERE "id" = ANY($1)
        dbQueries <- readIORef dbQueryCountRef
        dbQueries `shouldBe` 1
        sql <- readIORef capturedSqlRef
        T.unpack sql `shouldContain` "SELECT"
        T.unpack sql `shouldContain` "WHERE \"id\" = ANY($1)"

        params <- readIORef capturedParamsRef
        case params of
          [p] -> do
            paramType p `shouldBe` "bigint[]"
            -- Verify parameter contains all 20 miss IDs
            let paramVal = paramValue p
            T.unpack paramVal `shouldContain` "81"
            T.unpack paramVal `shouldContain` "100"
          _ -> expectationFailure ("Expected exactly 1 bound parameter, got: " ++ show (length params))

        -- 7. Verify pipelined Redis SETEX for the 20 misses
        let setexCmds = [cmd | cmd@(RedisSetExMany _) <- history]
        length setexCmds `shouldBe` 1
        case setexCmds of
          [RedisSetExMany items] -> do
            length items `shouldBe` 20
            -- Check TTL is 3600 from policy
            all (\(_, ttl, _) -> ttl == 3600) items `shouldBe` True
          _ -> expectationFailure "Expected exactly 1 pipelined RedisSetExMany"

        -- 8. Verify Redis store now contains all 100 keys
        finalStore <- readTVarIO storeVar
        Map.size finalStore `shouldBe` 100

  describe "Single Record Read-Through: selectCachedById" $ do
    it "serves hit directly from Redis without calling PostgreSQL" $ do
      (client, _, _) <- newMockRedis
      let policy = RedisCachePolicy "cache" 3600
          cachedSchema = withRedisCache policy cacheAccountSchema
          user = CacheAccount (Id 7) "James" "james@example.com" 700
          key = tableCacheKey policy cachedSchema "7"
      redisSet client key (runBinaryEncoder tableCodec user)

      dbCallsRef <- newIORef (0 :: Int)
      let mockFetcher _ _ = modifyIORef' dbCallsRef (+ 1) >> pure (Right [])

      res <-
        selectCachedByIdWithFetcher
          client
          mockFetcher
          cachedSchema
          tableCodec
          (defaultPkExtractor cachedSchema)
          (Id (7 :: Int64))
      res `shouldBe` Right (Just user)
      readIORef dbCallsRef `shouldReturn` 0

  describe "Automatic onCommit Invalidation: updateChangeset" $ do
    it "retains cached pre-transaction value in Redis when transaction rolls back / fails" $ do
      (client, _, _) <- newMockRedis
      let policy = RedisCachePolicy "cache" 3600
          cachedSchema = withRedisCache policy cacheAccountSchema
          key = tableCacheKey policy cachedSchema "42"
      -- Pre-populate cached value in Redis
      redisSet client key "pre_transaction_cached_wire_bytes"

      -- Build an update changeset
      let patch = CacheAccount Unchanged (SetTo "Bob") Unchanged Unchanged
          cs = change @'UpdateOp patch & withUniqueKey "42"
      let tx :: Tx (Postgres 'PG16) 'ReadWrite (Either ChangesetError CacheAccount)
          tx = do
            _ <- withTxRedis client (updateChangeset cachedSchema cs)
            txFail (TxUserError "simulated_transaction_abort")

      let (res, finalSt) = executeTx tx initialTxState
      res `shouldBe` Left (TxUserError "simulated_transaction_abort")

      -- Invalidation hook must NOT execute on rollback
      executeRollbackHooks finalSt
      mVal <- redisGet client key
      mVal `shouldBe` Just "pre_transaction_cached_wire_bytes"

    it "evicts stale cached key immediately after COMMIT when transaction succeeds" $ do
      (client, _, _) <- newMockRedis
      let policy = RedisCachePolicy "cache" 3600
          cachedSchema = withRedisCache policy cacheAccountSchema
          key = tableCacheKey policy cachedSchema "42"
          dummyAcc = CacheAccount (Id 42) "Bob" "bob@example.com" 999
      -- Pre-populate cached value in Redis
      redisSet client key "pre_transaction_cached_wire_bytes"

      let patch = CacheAccount Unchanged (SetTo "Bob") Unchanged Unchanged
          cs = change @'UpdateOp patch & withUniqueKey "42"
      let tx :: Tx (Postgres 'PG16) 'ReadWrite (Either ChangesetError CacheAccount)
          tx = withTxRedis client (updateChangesetWithFallback cachedSchema dummyAcc cs)

      let (res, finalSt) = executeTx tx initialTxState
      res `shouldBe` Right (Right dummyAcc)

      -- Commit hooks run strictly once following successful outer commit
      executeCommitHooks finalSt
      mVal <- redisGet client key
      mVal `shouldBe` Nothing

  describe "Automatic onCommit Invalidation: deleteWhere and deleteById" $ do
    it "deleteWhere evicts stale key on COMMIT and retains on rollback" $ do
      (client, _, _) <- newMockRedis
      let policy = RedisCachePolicy "cache" 3600
          cachedSchema = withRedisCache policy cacheAccountSchema
          key = tableCacheKey policy cachedSchema "55"
      redisSet client key "value_55"

      -- Rollback case: key retained
      let abortTx :: Tx (Postgres 'PG16) 'ReadWrite ()
          abortTx = do
            withTxRedis
              client
              (deleteWhere cachedSchema (\acc -> acc.id ==. lit (Id 55 :: Id CacheAccountT Int64)))
            txFail (TxUserError "abort")
      let (resAbort, stAbort) = executeTx abortTx initialTxState
      resAbort `shouldBe` Left (TxUserError "abort")
      executeRollbackHooks stAbort
      redisGet client key `shouldReturn` Just "value_55"

      -- Commit case: key evicted
      let commitTx :: Tx (Postgres 'PG16) 'ReadWrite ()
          commitTx =
            withTxRedis
              client
              (deleteWhere cachedSchema (\acc -> acc.id ==. lit (Id 55 :: Id CacheAccountT Int64)))
      let (resCommit, stCommit) = executeTx commitTx initialTxState
      resCommit `shouldBe` Right ()
      executeCommitHooks stCommit
      redisGet client key `shouldReturn` Nothing

    it "deleteById evicts stale key on COMMIT" $ do
      (client, _, _) <- newMockRedis
      let policy = RedisCachePolicy "cache" 3600
          cachedSchema = withRedisCache policy cacheAccountSchema
          key = tableCacheKey policy cachedSchema "77"
      redisSet client key "value_77"

      let commitTx :: Tx (Postgres 'PG16) 'ReadWrite ()
          commitTx = withTxRedis client (deleteById cachedSchema (Id (77 :: Int64)))
      let (res, finalSt) = executeTx commitTx initialTxState
      res `shouldBe` Right ()
      executeCommitHooks finalSt
      redisGet client key `shouldReturn` Nothing
