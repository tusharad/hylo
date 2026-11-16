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

import Control.Exception (evaluate)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
  ( Day
  , NominalDiffTime
  , UTCTime (..)
  , addUTCTime
  , fromGregorian
  )
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
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
import Hylo.PG.Vector (Embedding (..))
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

  describe "User Story 12: PostgreSQL Binary Wire Codecs" $ do
    it "Law 1: Binary Codec Round-Trip Invariant on UUID (OID 2950)" $
      forAll genUUID $ \u ->
        runBinaryDecoder uuidCodec (runBinaryEncoder uuidCodec u) == Right u

    it "Law 1: Binary Codec Round-Trip Invariant on UTCTime pre-2000 (OID 1184)" $
      forAll genPre2000UTCTime $ \t ->
        runBinaryDecoder utcTimeCodec (runBinaryEncoder utcTimeCodec t) == Right t

    it "Law 1: Binary Codec Round-Trip Invariant on UTCTime post-2000 (OID 1184)" $
      forAll genPost2000UTCTime $ \t ->
        runBinaryDecoder utcTimeCodec (runBinaryEncoder utcTimeCodec t) == Right t

    it "Law 1: Binary Codec Round-Trip Invariant on Day (OID 1082)" $
      forAll genDay $ \d ->
        runBinaryDecoder dayCodec (runBinaryEncoder dayCodec d) == Right d

    it "Law 1: Binary Codec Round-Trip Invariant on Aeson.Value jsonb (OID 3802)" $
      forAll genJsonValue $ \j ->
        runBinaryDecoder jsonbCodec (runBinaryEncoder jsonbCodec j) == Right j

    it "Law 1: Binary Codec Round-Trip Invariant on Aeson.Value json (OID 114)" $
      forAll genJsonValue $ \j ->
        runBinaryDecoder jsonCodec (runBinaryEncoder jsonCodec j) == Right j

    it "Law 1: Binary Codec Round-Trip Invariant on Embedding \"text-embedding-3-small\" 1536" $
      forAll genEmbedding1536 $ \emb ->
        runBinaryDecoder embeddingCodec (runBinaryEncoder embeddingCodec emb) == Right emb

    it "Law 1: Binary Codec Round-Trip Invariant on 1-D binary arrays [Int64]" $
      property $ \(xs :: [Int64]) ->
        runBinaryDecoder (arrayCodec int64Codec) (runBinaryEncoder (arrayCodec int64Codec) xs) == Right xs

    it "Law 1: Binary Codec Round-Trip Invariant on 1-D binary arrays [Text]" $
      property $ \(strs :: [String]) ->
        let xs = map T.pack strs
         in runBinaryDecoder (arrayCodec textCodec) (runBinaryEncoder (arrayCodec textCodec) xs) == Right xs

    it "decodes empty binary array [] correctly" $
      let enc = runBinaryEncoder (arrayCodec int64Codec) []
          dec = runBinaryDecoder (arrayCodec int64Codec) enc
       in dec `shouldBe` Right []

  describe "User Story 19: Custom ADT / PostgreSQL ENUM Column Codecs" $ do
    it "Law 1: enumCodec round-trips every registered OrderStatus constructor" $
      forAll (elements [minBound .. maxBound] :: Gen OrderStatus) $ \s ->
        runBinaryDecoder orderStatusCodec (runBinaryEncoder orderStatusCodec s) == Right s

    it "enumCodec encodes each constructor to its registered framed UTF-8 label" $ do
      runBinaryEncoder orderStatusCodec Pending `shouldBe` runBinaryEncoder textCodec "pending"
      runBinaryEncoder orderStatusCodec Shipped `shouldBe` runBinaryEncoder textCodec "shipped"
      runBinaryEncoder orderStatusCodec Delivered `shouldBe` runBinaryEncoder textCodec "delivered"

    it "enumCodec decodes framed text labels back to constructors" $ do
      runBinaryDecoder orderStatusCodec (runBinaryEncoder textCodec "pending") `shouldBe` Right Pending
      runBinaryDecoder orderStatusCodec (runBinaryEncoder textCodec "shipped") `shouldBe` Right Shipped
      runBinaryDecoder orderStatusCodec (runBinaryEncoder textCodec "delivered")
        `shouldBe` Right Delivered

    it "enumCodec fails descriptively on unknown labels" $
      case runBinaryDecoder orderStatusCodec (runBinaryEncoder textCodec "paused") of
        Left err -> do
          T.unpack err `shouldContain` "Unknown enum label"
          T.unpack err `shouldContain` "paused"
        Right v -> expectationFailure ("Expected decode failure, got: " <> show v)

    it "enumCodec raises a descriptive error when encoding an unregistered constructor" $
      evaluate (runBinaryEncoder (enumCodec [] :: HyloCodec OrderStatus) Pending)
        `shouldThrow` anyException

    it "boundedEnumCodec derives snake_case labels from constructors" $ do
      runBinaryEncoder boundedOrderStatusCodec Pending `shouldBe` runBinaryEncoder textCodec "pending"
      runBinaryEncoder boundedOrderStateCodec AwaitingPickup
        `shouldBe` runBinaryEncoder textCodec "awaiting_pickup"

    it "boundedEnumCodec is equivalent to enumCodec over the derived label table" $
      forAll (elements [minBound .. maxBound] :: Gen OrderStatus) $ \s ->
        runBinaryEncoder boundedOrderStatusCodec s == runBinaryEncoder orderStatusCodec s

    it "boundedEnumCodec round-trips every constructor of a multi-word sum type" $
      forAll (elements [minBound .. maxBound] :: Gen OrderState) $ \s ->
        runBinaryDecoder boundedOrderStateCodec (runBinaryEncoder boundedOrderStateCodec s) == Right s

    it "pgEnumCodec registers round-trips through the text-protocol OID fallback" $ do
      runBinaryDecoder (pgEnumCodec orderStatusPairs) (runBinaryEncoder textCodec "delivered")
        `shouldBe` Right Delivered
      runBinaryEncoder (pgEnumCodec orderStatusPairs) Delivered
        `shouldBe` runBinaryEncoder textCodec "delivered"

-- -----------------------------------------------------------------------------
-- User Story 19 QuickCheck Fixtures
-- -----------------------------------------------------------------------------

-- | User Story 19 fixture: a sum type mapping to a PostgreSQL ENUM column.
data OrderStatus = Pending | Shipped | Delivered
  deriving (Eq, Show, Bounded, Enum)

-- | User Story 19 fixture: multi-word constructor names for snake_case derivation.
data OrderState = AwaitingPickup | InTransit | DeliveredSafe
  deriving (Eq, Show, Bounded, Enum)

-- | Registered (label, constructor) pairs for 'OrderStatus'.
orderStatusPairs :: [(Text, OrderStatus)]
orderStatusPairs = [("pending", Pending), ("shipped", Shipped), ("delivered", Delivered)]

-- | Explicit lookup-table codec for 'OrderStatus'.
orderStatusCodec :: HyloCodec OrderStatus
orderStatusCodec = enumCodec orderStatusPairs

-- | Derived codec for 'OrderStatus' via 'boundedEnumCodec'.
boundedOrderStatusCodec :: HyloCodec OrderStatus
boundedOrderStatusCodec = boundedEnumCodec

-- | Derived codec for the multi-word 'OrderState' fixture.
boundedOrderStateCodec :: HyloCodec OrderState
boundedOrderStateCodec = boundedEnumCodec

-- -----------------------------------------------------------------------------
-- User Story 12 QuickCheck Generators
-- -----------------------------------------------------------------------------

-- | QuickCheck generator for UUIDs.
genUUID :: Gen UUID
genUUID = UUID.fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- | QuickCheck generator for Days.
genDay :: Gen Day
genDay = do
  y <- choose (1970, 2050)
  m <- choose (1, 12)
  d <- choose (1, 28)
  pure (fromGregorian y m d)

-- | QuickCheck generator for pre-2000 UTCTime timestamps (negative microseconds).
genPre2000UTCTime :: Gen UTCTime
genPre2000UTCTime = do
  micros <- choose (-946728000000000, -1)
  let diff = fromRational (micros % 1000000) :: NominalDiffTime
  pure (addUTCTime diff (UTCTime (fromGregorian 2000 1 1) 0))

-- | QuickCheck generator for post-2000 UTCTime timestamps (positive microseconds).
genPost2000UTCTime :: Gen UTCTime
genPost2000UTCTime = do
  micros <- choose (0, 1104633600000000)
  let diff = fromRational (micros % 1000000) :: NominalDiffTime
  pure (addUTCTime diff (UTCTime (fromGregorian 2000 1 1) 0))

-- | QuickCheck generator for recursive Aeson.Value.
genJsonValue :: Gen Aeson.Value
genJsonValue = sized $ \n ->
  if n <= 1
    then
      oneof
        [ pure Aeson.Null
        , Aeson.Bool <$> arbitrary
        , Aeson.Number . fromInteger <$> choose (-100000, 100000)
        , Aeson.String . T.pack <$> listOf (elements ['a' .. 'z'])
        ]
    else
      oneof
        [ pure Aeson.Null
        , Aeson.Bool <$> arbitrary
        , Aeson.Number . fromInteger <$> choose (-100000, 100000)
        , Aeson.String . T.pack <$> listOf (elements ['a' .. 'z'])
        , Aeson.Array . Vector.fromList <$> resize (min n 5) (listOf genJsonValue)
        , do
            kvs <- resize (min n 5) $ listOf $ do
              k <- listOf1 (elements ['a' .. 'z'])
              v <- genJsonValue
              pure (Key.fromString k, v)
            pure (Aeson.Object (KeyMap.fromList kvs))
        ]

-- | QuickCheck generator for Embedding "text-embedding-3-small" 1536.
genEmbedding1536 :: Gen (Embedding "text-embedding-3-small" 1536)
genEmbedding1536 = do
  floats <- vectorOf 1536 (choose (-10.0, 10.0))
  pure (Embedding (Vector.fromList floats))
