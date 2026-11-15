{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.PG.JSONB
Description : Type-safe PostgreSQL JSONB combinators and parameter binding
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides type-safe operators and combinators for PostgreSQL JSONB expressions,
ensuring all literal values and filters compile through SqlBuilderM as
parameterized $1, $2 bind parameters.
-}
module Hylo.PG.JSONB
  ( -- * Literals
    litJsonb
  , litJsonbText

    -- * JSONB Operators
  , jsonbContains
  , jsonbContainedBy
  , jsonbHasKey
  , jsonbHasAnyKey
  , jsonbHasAllKeys
  , jsonbGet
  , jsonbGetText
  , jsonbGetPath
  , jsonbGetPathText

    -- * Symbolic Infix Operators
  , (@>)
  , (@>.)
  , (<@)
  , (<@.)
  , (?.)
  , (->.)
  , (->>)
  , (->>.)
  , (#>.)
  , (#>>.)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Hylo.Core.Expr (QExpr (..), litTyped)

-- | Construct a typed JSONB expression from any 'Aeson.ToJSON' value, bound as a $1 parameter.
litJsonb :: Aeson.ToJSON a => a -> QExpr Aeson.Value
litJsonb val = litTyped (TE.decodeUtf8 (BSL.toStrict (Aeson.encode val))) "jsonb"

-- | Construct a typed JSONB expression from raw JSON text, bound as a $1 parameter.
litJsonbText :: Text -> QExpr Aeson.Value
litJsonbText txt = litTyped txt "jsonb"

-- | PostgreSQL JSONB containment test (@>). True if left JSONB contains right JSONB.
jsonbContains :: QExpr Aeson.Value -> QExpr Aeson.Value -> QExpr Bool
jsonbContains = BinOpExpr "@>"

-- | Infix alias for 'jsonbContains'.
(@>) :: QExpr Aeson.Value -> QExpr Aeson.Value -> QExpr Bool
(@>) = jsonbContains

infix 4 @>

-- | Dotted infix alias for 'jsonbContains'.
(@>.) :: QExpr Aeson.Value -> QExpr Aeson.Value -> QExpr Bool
(@>.) = jsonbContains

infix 4 @>.

-- | PostgreSQL JSONB contained-by test (<@). True if left JSONB is contained within right JSONB.
jsonbContainedBy :: QExpr Aeson.Value -> QExpr Aeson.Value -> QExpr Bool
jsonbContainedBy = BinOpExpr "<@"

-- | Infix alias for 'jsonbContainedBy'.
(<@) :: QExpr Aeson.Value -> QExpr Aeson.Value -> QExpr Bool
(<@) = jsonbContainedBy

infix 4 <@

-- | Dotted infix alias for 'jsonbContainedBy'.
(<@.) :: QExpr Aeson.Value -> QExpr Aeson.Value -> QExpr Bool
(<@.) = jsonbContainedBy

infix 4 <@.

-- | PostgreSQL JSONB key existence test (?). True if string key exists in top-level JSONB object.
jsonbHasKey :: QExpr Aeson.Value -> QExpr Text -> QExpr Bool
jsonbHasKey = BinOpExpr "?"

-- | Dotted infix alias for 'jsonbHasKey'.
(?.) :: QExpr Aeson.Value -> QExpr Text -> QExpr Bool
(?.) = jsonbHasKey

infix 4 ?.

-- | PostgreSQL JSONB has-any-key test (?|). True if any of the string keys exist in JSONB object.
jsonbHasAnyKey :: QExpr Aeson.Value -> QExpr [Text] -> QExpr Bool
jsonbHasAnyKey = BinOpExpr "?|"

-- | PostgreSQL JSONB has-all-keys test (?&). True if all string keys exist in JSONB object.
jsonbHasAllKeys :: QExpr Aeson.Value -> QExpr [Text] -> QExpr Bool
jsonbHasAllKeys = BinOpExpr "?&"

{- | PostgreSQL JSONB element / field extraction (->) returning JSONB.
Supports key text (object lookup) or integer index (array lookup).
-}
jsonbGet :: QExpr Aeson.Value -> QExpr a -> QExpr Aeson.Value
jsonbGet = BinOpExpr "->"

-- | Infix operator for 'jsonbGet'.
(->.) :: QExpr Aeson.Value -> QExpr a -> QExpr Aeson.Value
(->.) = jsonbGet

infixl 8 ->.

-- | PostgreSQL JSONB element / field extraction (->>) returning Text.
jsonbGetText :: QExpr Aeson.Value -> QExpr a -> QExpr Text
jsonbGetText = BinOpExpr "->>"

-- | Infix operator for 'jsonbGetText'.
(->>) :: QExpr Aeson.Value -> QExpr a -> QExpr Text
(->>) = jsonbGetText

infixl 8 ->>

-- | Dotted infix operator for 'jsonbGetText'.
(->>.) :: QExpr Aeson.Value -> QExpr a -> QExpr Text
(->>.) = jsonbGetText

infixl 8 ->>.

-- | PostgreSQL JSONB path extraction (#>) returning JSONB.
jsonbGetPath :: QExpr Aeson.Value -> QExpr [Text] -> QExpr Aeson.Value
jsonbGetPath = BinOpExpr "#>"

-- | Infix operator for 'jsonbGetPath'.
(#>.) :: QExpr Aeson.Value -> QExpr [Text] -> QExpr Aeson.Value
(#>.) = jsonbGetPath

infixl 8 #>.

-- | PostgreSQL JSONB path extraction as text (#>>) returning Text.
jsonbGetPathText :: QExpr Aeson.Value -> QExpr [Text] -> QExpr Text
jsonbGetPathText = BinOpExpr "#>>"

-- | Infix operator for 'jsonbGetPathText'.
(#>>.) :: QExpr Aeson.Value -> QExpr [Text] -> QExpr Text
(#>>.) = jsonbGetPathText

infixl 8 #>>.
