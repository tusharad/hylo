{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.Redis
Description : Redis key-value backend adapter and command algebra
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

    -- * TableSchema Integration
  , redisTableKey
  , redisHGetCol
  , redisHSetCol
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Hylo.Backend.Capability (Redis)
import Hylo.Core (ColDef (..), TableSchema (..))

-- | Abstract commands supported by the Hylo Redis engine.
data RedisCmd
  = RedisGet !Text
  | RedisSet !Text !ByteString
  | RedisDel !Text
  | RedisHGet !Text !Text
  | RedisHSet !Text !Text !ByteString
  deriving (Eq, Show, Generic)

-- | Render a Redis command into its canonical textual representation.
renderRedisCmd :: RedisCmd -> Text
renderRedisCmd = \case
  RedisGet k -> "GET " <> k
  RedisSet k v -> "SET " <> k <> " " <> decodeUtf8Lenient v
  RedisDel k -> "DEL " <> k
  RedisHGet k f -> "HGET " <> k <> " " <> f
  RedisHSet k f v -> "HSET " <> k <> " " <> f <> " " <> decodeUtf8Lenient v
  where
    decodeUtf8Lenient = TE.decodeUtf8With (\_ _ -> Just '?')

-- | Formulate a canonical Redis key from a 'TableSchema' and record ID.
redisTableKey :: TableSchema table -> Text -> Text
redisTableKey schema recId = tableName schema <> ":" <> recId

-- | Construct a Redis HGET command for a specific table column.
redisHGetCol :: TableSchema table -> Text -> ColDef a -> RedisCmd
redisHGetCol schema recId col = RedisHGet (redisTableKey schema recId) (colName col)

-- | Construct a Redis HSET command for a specific table column and binary payload.
redisHSetCol :: TableSchema table -> Text -> ColDef a -> ByteString -> RedisCmd
redisHSetCol schema recId col = RedisHSet (redisTableKey schema recId) (colName col)
