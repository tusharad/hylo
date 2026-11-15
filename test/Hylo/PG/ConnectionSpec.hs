{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.ConnectionSpec
Description : Test suite for Real Hasql.Statement Execution, Row Hydration, and Parameterization
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.ConnectionSpec (spec) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Sequence (fromList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Hasql.Statement as Stmt
import Hylo.Core.Codec
  ( RowCodec
  , field
  , runBinaryDecoder
  , runBinaryEncoder
  )
import Hylo.Core.Transaction (BoundParam (..), TxError (..))
import Hylo.PG.Codec.Binary
  ( boolCodec
  , doubleCodec
  , int64Codec
  , textCodec
  )
import Hylo.PG.Connection
  ( assembleRowBytes
  , defaultPool
  , executePreparedQuery
  , interpolateParams
  , mkPreparedQueryStatement
  , mkPreparedStatement
  )
import Hylo.SQL.Builder (CompiledQuery (..), previewQuery)
import Test.Hspec

-- | Sample domain record for row hydration testing.
data TestRow = TestRow
  { rowId :: !Int64
  , rowName :: !Text
  , rowScore :: !Double
  , rowActive :: !Bool
  }
  deriving (Eq, Show)

testRowCodec :: RowCodec TestRow TestRow
testRowCodec =
  TestRow
    <$> field rowId int64Codec
    <*> field rowName textCodec
    <*> field rowScore doubleCodec
    <*> field rowActive boolCodec

spec :: Spec
spec = describe "User Story 10: Real Hasql.Statement Execution & Row Hydration" $ do
  describe "Acceptance Criterion 1: 12-Parameter Binding & Prevention of Corruption" $ do
    it "Binding 12 parameters ($1 through $12) in a single query never corrupts $10, $11, or $12" $ do
      let params =
            [ BoundParam 1 "one" "text"
            , BoundParam 2 "two" "text"
            , BoundParam 3 "three" "text"
            , BoundParam 4 "four" "text"
            , BoundParam 5 "five" "text"
            , BoundParam 6 "six" "text"
            , BoundParam 7 "seven" "text"
            , BoundParam 8 "eight" "text"
            , BoundParam 9 "nine" "text"
            , BoundParam 10 "ten" "text"
            , BoundParam 11 "eleven" "text"
            , BoundParam 12 "twelve" "text"
            ]
          sql =
            "SELECT * FROM items WHERE c1 = $1 AND c2 = $2 AND c3 = $3 AND c4 = $4 \
            \AND c5 = $5 AND c6 = $6 AND c7 = $7 AND c8 = $8 AND c9 = $9 \
            \AND c10 = $10 AND c11 = $11 AND c12 = $12"
          result = T.unpack (interpolateParams sql params)

      -- Verify that $10, $11, $12 are accurately replaced with their own values
      result `shouldContain` "c10 = 'ten'"
      result `shouldContain` "c11 = 'eleven'"
      result `shouldContain` "c12 = 'twelve'"
      result `shouldContain` "c1 = 'one'"

      -- Strictly verify no prefix collision corruption ($10 turned into 'one'0)
      result `shouldNotContain` "'one'0"
      result `shouldNotContain` "'one'1"
      result `shouldNotContain` "'one'2"
      result `shouldNotContain` "$10"
      result `shouldNotContain` "$11"
      result `shouldNotContain` "$12"

    it "previewQuery interpolates 12 parameters without corrupting $10, $11, or $12" $ do
      let params =
            [ BoundParam 1 "val_1" "text"
            , BoundParam 2 "val_2" "text"
            , BoundParam 3 "val_3" "text"
            , BoundParam 4 "val_4" "text"
            , BoundParam 5 "val_5" "text"
            , BoundParam 6 "val_6" "text"
            , BoundParam 7 "val_7" "text"
            , BoundParam 8 "val_8" "text"
            , BoundParam 9 "val_9" "text"
            , BoundParam 10 "val_10" "text"
            , BoundParam 11 "val_11" "text"
            , BoundParam 12 "val_12" "text"
            ]
          sql = "SELECT $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12"
          cq = CompiledQuery sql (fromList params)
          preview = T.unpack (previewQuery cq)

      preview `shouldContain` "'val_10'"
      preview `shouldContain` "'val_11'"
      preview `shouldContain` "'val_12'"
      preview `shouldNotContain` "'val_1'0"
      preview `shouldNotContain` "'val_1'1"
      preview `shouldNotContain` "'val_1'2"

    it "does not recursively replace $ patterns occurring inside parameter values" $ do
      let params =
            [ BoundParam 1 "price is $100" "text"
            , BoundParam 10 "regular price" "text"
            ]
          sql = "SELECT $1, $10"
          result = interpolateParams sql params

      result `shouldBe` "SELECT 'price is $100', 'regular price'"

  describe "Hasql.Statement Encoders & Row Hydration via Hylo.PG.Codec.Binary" $ do
    it "constructs prepared Hasql.Statement for 12 bound parameters" $ do
      let params = [BoundParam i ("val_" <> T.pack (show i)) "text" | i <- [1 .. 12]]
          stmt =
            mkPreparedStatement
              "INSERT INTO test VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)"
              params
      -- Verify statement can be constructed and contains prepared parameter encoder
      case stmt of
        Stmt.Statement _ _ _ preparable -> preparable `shouldBe` True

    it "assembles raw column wire bytes and decodes row via RowCodec" $ do
      let expectedRow = TestRow 42 "hydrated_entity" 99.5 True
          -- Generate binary wire bytes using canonical binary encoder
          wireBytes = runBinaryEncoder testRowCodec expectedRow
          -- Decode round-trip
          decoded = runBinaryDecoder testRowCodec wireBytes
      decoded `shouldBe` Right expectedRow

    it "assembleRowBytes frames nullable column payloads according to PostgreSQL wire protocol" $ do
      let colBytes1 = Just "alice"
          colBytes2 = Nothing
          colBytes3 = Just "secret"
          assembled = assembleRowBytes [colBytes1, colBytes2, colBytes3]
      -- Column 1: 4-byte length (5) followed by "alice"
      -- Column 2: 4-byte length (-1 / 0xFFFFFFFF) for NULL
      -- Column 3: 4-byte length (6) followed by "secret"
      BS.length assembled `shouldBe` (4 + 5 + 4 + 0 + 4 + 6)

    it "mkPreparedQueryStatement creates a prepared statement decoding domain entities" $ do
      let params = [BoundParam 1 "42" "bigint"]
          stmt =
            mkPreparedQueryStatement
              "SELECT id, name, score, active FROM users WHERE id = $1"
              params
              testRowCodec
              4
      case stmt of
        Stmt.Statement sqlBS _ _ preparable -> do
          preparable `shouldBe` True
          sqlBS `shouldBe` "SELECT id, name, score, active FROM users WHERE id = $1"

    it "executePreparedQuery safely returns TxUserError when pool is offline" $ do
      let pool = defaultPool
          params = [BoundParam 1 "1" "bigint"]
      res <- executePreparedQuery pool "SELECT 1" params testRowCodec 4
      case res of
        Left (TxUserError msg) -> T.unpack msg `shouldContain` "no active PostgreSQL connection"
        _ -> expectationFailure "Expected TxUserError for offline pool"
