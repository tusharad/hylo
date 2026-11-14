{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Use <$>" #-}

{- |
Module      : Hylo.Core.CodecSpec
Description : Specification tests for Epic 2: Zero-Generics Linear-Time Codec Engine
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.CodecSpec
  ( spec
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core
  ( Col
  , Ctx (..)
  , Id (..)
  , TableCatalogDef (..)
  , TableSchema (..)
  , column
  , describeCol
  , nullableColumn
  , primaryKey
  , tableCatalog
  , tableSchema
  , toColumnCatalogDef
  )
import Hylo.Core.Codec
import Hylo.PG.Codec.Binary
import Test.Hspec
import Test.QuickCheck

-- | Test HKD Table for Zero-Generics record marshalling
data UserT (ctx :: Ctx) = User
  { id :: !(Col ctx (Id UserT Int64))
  , email :: !(Col ctx Text)
  , age :: !(Col ctx Int64)
  , note :: !(Col ctx (Maybe Text))
  }

deriving instance Eq (UserT 'Result)
deriving instance Show (UserT 'Result)

type User = UserT 'Result

userCodec :: Codec User User
userCodec =
  User
    <$> field (.id) (idCodec int64Codec)
    <*> field (.email) textCodec
    <*> field (.age) int64Codec
    <*> field (.note) (nullable textCodec)

spec :: Spec
spec = describe "Epic 2: Zero-Generics Linear-Time Codec Engine" $ do
  describe "2.4 Executable Oracle Specification" $ do
    it "guarantees round-trip equality across binary wire encoding and decoding" $
      property $ \(v :: (Int64, Double, String, Bool)) ->
        let encoded = runBinaryEncoder compositeCodec v
            decoded = runBinaryDecoder compositeCodec encoded
         in decoded == Right v

    it "decodes NULL byte markers strictly to Nothing without runtime exceptions" $
      let nullBytes = BS.pack [0xFF, 0xFF, 0xFF, 0xFF] -- libpq -1 NULL length marker
          decoded = runBinaryDecoder (nullable int64Codec) nullBytes
       in decoded `shouldBe` Right Nothing

  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 1: Binary Codec Round-Trip Invariant on Int64" $
      property $ \(v :: Int64) ->
        runBinaryDecoder int64Codec (runBinaryEncoder int64Codec v) == Right v

    it "Law 1: Binary Codec Round-Trip Invariant on Double" $
      property $ \(v :: Double) ->
        runBinaryDecoder doubleCodec (runBinaryEncoder doubleCodec v) == Right v

    it "Law 1: Binary Codec Round-Trip Invariant on Bool" $
      property $ \(v :: Bool) ->
        runBinaryDecoder boolCodec (runBinaryEncoder boolCodec v) == Right v

    it "Law 1: Binary Codec Round-Trip Invariant on Text" $
      property $ \(s :: String) ->
        let t = T.pack s
         in runBinaryDecoder textCodec (runBinaryEncoder textCodec t) == Right t

    it "Law 3: Nullability Preservation Invariant (non-null preservation)" $
      property $ \(x :: Int64) ->
        let encVal = runBinaryEncoder int64Codec x
            decNull = runBinaryDecoder (nullable int64Codec) encVal
         in decNull == Right (Just x)

    it "rejects NULL byte marker for non-nullable codec with descriptive error" $
      let nullBytes = BS.pack [0xFF, 0xFF, 0xFF, 0xFF]
          decoded = runBinaryDecoder int64Codec nullBytes
       in decoded `shouldBe` Left "Unexpected NULL value for non-nullable column"

  describe "Zero-Generics Record Codec with HKD" $ do
    it "marshals and hydrates domain record in linear time without GHC.Generics" $ do
      let alice =
            User
              { id = Id 101
              , email = "alice@example.com"
              , age = 30
              , note = Just "Engineering lead"
              }
          bob =
            User
              { id = Id 102
              , email = "bob@example.com"
              , age = 25
              , note = Nothing
              }
      let encodedAlice = runBinaryEncoder userCodec alice
          decodedAlice = runBinaryDecoder userCodec encodedAlice
      decodedAlice `shouldBe` Right alice

      let encodedBob = runBinaryEncoder userCodec bob
          decodedBob = runBinaryDecoder userCodec encodedBob
      decodedBob `shouldBe` Right bob

  describe "Profunctor & Applicative Laws" $ do
    it "satisfies Applicative identity law" $
      property $ \(x :: Int64) ->
        let c = pure id <*> int64Codec
         in runBinaryDecoder c (runBinaryEncoder int64Codec x) == Right x

    it "satisfies Profunctor dimap isomorphism" $
      property $ \(x :: Int64) ->
        let c = dimap (+ 1) (subtract 1) int64Codec
         in runBinaryDecoder c (runBinaryEncoder c x) == Right x

  describe "Defensive Error Boundaries (Zero Runtime Exceptions)" $ do
    it "handles buffer underflow during length header safely" $
      let truncatedHeader = BS.pack [0x00, 0x00]
          decoded = runBinaryDecoder int64Codec truncatedHeader
       in decoded
            `shouldBe` Left "Unexpected end of input: expected 4 bytes, but only 2 available"

    it "handles buffer underflow during payload safely" $
      let -- 4 bytes length header = 8, but only 4 bytes payload provided
          truncatedPayload = BS.pack [0x00, 0x00, 0x00, 0x08, 0x01, 0x02, 0x03, 0x04]
          decoded = runBinaryDecoder int64Codec truncatedPayload
       in decoded
            `shouldBe` Left "Unexpected end of input: expected 8 bytes, but only 4 available"

    it "rejects invalid negative column lengths (< -1)" $
      let invalidNegLen = BS.pack [0xFF, 0xFF, 0xFF, 0xFE] -- -2 in big-endian
          decoded = runBinaryDecoder int64Codec invalidNegLen
       in decoded `shouldBe` Left "Invalid negative column length: -2"

    it "rejects trailing unparsed bytes" $
      let extraBytes = BS.pack [0x00, 0x00, 0x00, 0x01, 0x01, 0x99, 0x88] -- bool + 2 trailing
          decoded = runBinaryDecoder boolCodec extraBytes
       in decoded `shouldBe` Left "Trailing unparsed bytes (2 bytes remaining)"

  describe "Catalog AST Synthesis" $ do
    it "derives TableCatalogDef matching schema metadata" $ do
      let schema :: TableSchema UserT
          schema =
            tableSchema "users" $
              User
                { id = primaryKey "id" "bigint"
                , email = column "email" "text" & describeCol "Unique user email"
                , age = column "age" "integer"
                , note = nullableColumn "note" "text"
                }
            where
              (&) x f = f x
          catalog =
            tableCatalog
              schema.tableName
              [ toColumnCatalogDef schema.tableMeta.id
              , toColumnCatalogDef schema.tableMeta.email
              , toColumnCatalogDef schema.tableMeta.age
              , toColumnCatalogDef schema.tableMeta.note
              ]
      catalog.catalogTableName `shouldBe` "users"
      catalog.catalogPrimaryKey `shouldBe` ["id"]
      length catalog.catalogColumns `shouldBe` 4
