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
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- | Phantom type identifying the Redis database engine.
data Redis

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
