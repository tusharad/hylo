{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.Redis
Description : Redis key-value backend adapter, client abstraction, and mock engine
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Redis
  ( -- * Backend Marker
    Redis

    -- * Redis Command Descriptors
  , RedisCmd (..)
  , renderRedisCmd

    -- * Redis Client Abstraction
  , RedisClient (..)

    -- * Ambient & Transaction-Scoped Redis Client
  , setDefaultRedisClient
  , getDefaultRedisClient
  , clearDefaultRedisClient
  , withTxRedis
  , setTxRedis

    -- * In-Memory Mock Redis Engine
  , newMockRedis

    -- * TableSchema Integration & Key Formulation
  , redisTableKey
  , tableCacheKey
  , redisHGetCol
  , redisHSetCol
  ) where

import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  )
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Hylo.Backend.Capability (Redis)
import Hylo.Core
  ( ColDef (..)
  , RedisCachePolicy (..)
  , TableSchema (..)
  , Tx
  , colName
  , setTxExtension
  , tableName
  )
import Hylo.Core.Text (decodeUtf8Lenient)
import System.IO.Unsafe (unsafePerformIO)

-- | Abstract commands supported by the Hylo Redis engine.
data RedisCmd
  = RedisGet !Text
  | RedisMGet ![Text]
  | RedisSet !Text !ByteString
  | RedisSetEx !Text !Int !ByteString
  | RedisSetExMany ![(Text, Int, ByteString)]
  | RedisDel !Text
  | RedisDelMany ![Text]
  | RedisHGet !Text !Text
  | RedisHSet !Text !Text !ByteString
  deriving (Eq, Show, Generic)

instance NFData RedisCmd

-- | Render a Redis command into its canonical textual representation.
renderRedisCmd :: RedisCmd -> Text
renderRedisCmd = \case
  RedisGet k -> "GET " <> k
  RedisMGet ks -> "MGET " <> T.unwords ks
  RedisSet k v -> "SET " <> k <> " " <> decodeUtf8Lenient v
  RedisSetEx k ttl v -> "SETEX " <> k <> " " <> T.pack (show ttl) <> " " <> decodeUtf8Lenient v
  RedisSetExMany items ->
    "PIPELINE " <> T.intercalate "; " [renderRedisCmd (RedisSetEx k t v) | (k, t, v) <- items]
  RedisDel k -> "DEL " <> k
  RedisDelMany ks -> "DEL " <> T.unwords ks
  RedisHGet k f -> "HGET " <> k <> " " <> f
  RedisHSet k f v -> "HSET " <> k <> " " <> f <> " " <> decodeUtf8Lenient v

-- | First-class client interface for interacting with Redis.
data RedisClient = RedisClient
  { redisGet :: !(Text -> IO (Maybe ByteString))
  , redisMGet :: !([Text] -> IO [Maybe ByteString])
  , redisSet :: !(Text -> ByteString -> IO ())
  , redisSetEx :: !(Text -> Int -> ByteString -> IO ())
  , redisSetExMany :: !([(Text, Int, ByteString)] -> IO ())
  , redisDel :: !(Text -> IO ())
  , redisDelMany :: !([Text] -> IO ())
  }
  deriving (Typeable)

-- | Global default 'RedisClient' used when no transaction-scoped client is attached.
defaultRedisClientRef :: IORef (Maybe RedisClient)
{-# NOINLINE defaultRedisClientRef #-}
defaultRedisClientRef = unsafePerformIO (newIORef Nothing)

-- | Register the global default 'RedisClient'.
setDefaultRedisClient :: RedisClient -> IO ()
setDefaultRedisClient client = writeIORef defaultRedisClientRef (Just client)

-- | Retrieve the current global default 'RedisClient', if configured.
getDefaultRedisClient :: IO (Maybe RedisClient)
getDefaultRedisClient = readIORef defaultRedisClientRef

-- | Clear the global default 'RedisClient'.
clearDefaultRedisClient :: IO ()
clearDefaultRedisClient = writeIORef defaultRedisClientRef Nothing

-- | Attach a 'RedisClient' to a transaction's dynamic context.
withTxRedis :: forall k (db :: k) mode a. RedisClient -> Tx db mode a -> Tx db mode a
withTxRedis client tx = do
  setTxExtension "redis_client" client
  tx

-- | Set the active 'RedisClient' in the current transaction.
setTxRedis :: forall k (db :: k) mode. RedisClient -> Tx db mode ()
setTxRedis = setTxExtension "redis_client"

-- | Create a thread-safe in-memory mock 'RedisClient' with full command audit logging.
newMockRedis :: IO (RedisClient, TVar (Map Text ByteString), TVar [RedisCmd])
newMockRedis = do
  storeVar <- newTVarIO Map.empty
  historyVar <- newTVarIO []
  let client =
        RedisClient
          { redisGet = \k -> atomically $ do
              modifyTVar' historyVar (\h -> h ++ [RedisGet k])
              Map.lookup k <$> readTVar storeVar
          , redisMGet = \ks -> atomically $ do
              modifyTVar' historyVar (\h -> h ++ [RedisMGet ks])
              store <- readTVar storeVar
              pure [Map.lookup k store | k <- ks]
          , redisSet = \k v -> atomically $ do
              modifyTVar' historyVar (\h -> h ++ [RedisSet k v])
              modifyTVar' storeVar (Map.insert k v)
          , redisSetEx = \k ttl v -> atomically $ do
              modifyTVar' historyVar (\h -> h ++ [RedisSetEx k ttl v])
              modifyTVar' storeVar (Map.insert k v)
          , redisSetExMany = \items -> atomically $ do
              modifyTVar' historyVar (\h -> h ++ [RedisSetExMany items])
              modifyTVar' storeVar (\st -> foldl' (\m (k, _, v) -> Map.insert k v m) st items)
          , redisDel = \k -> atomically $ do
              modifyTVar' historyVar (\h -> h ++ [RedisDel k])
              modifyTVar' storeVar (Map.delete k)
          , redisDelMany = \ks -> atomically $ do
              modifyTVar' historyVar (\h -> h ++ [RedisDelMany ks])
              modifyTVar' storeVar (\st -> foldl' (flip Map.delete) st ks)
          }
  pure (client, storeVar, historyVar)

-- | Formulate a canonical Redis key from a 'TableSchema' and record ID.
redisTableKey :: TableSchema table -> Text -> Text
redisTableKey schema recId = tableName schema <> ":" <> recId

-- | Formulate a canonical Redis cache key using a 'RedisCachePolicy', schema, and record PK string.
tableCacheKey :: RedisCachePolicy -> TableSchema table -> Text -> Text
tableCacheKey policy schema pkVal = rcpKeyPrefix policy <> ":" <> tableName schema <> ":" <> pkVal

-- | Construct a Redis HGET command for a specific table column.
redisHGetCol :: TableSchema table -> Text -> ColDef a -> RedisCmd
redisHGetCol schema recId col = RedisHGet (redisTableKey schema recId) (colName col)

-- | Construct a Redis HSET command for a specific table column and binary payload.
redisHSetCol :: TableSchema table -> Text -> ColDef a -> ByteString -> RedisCmd
redisHSetCol schema recId col = RedisHSet (redisTableKey schema recId) (colName col)
