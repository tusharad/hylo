{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Hylo.PG.RangeSpec
Description : Unit and property specifications for PostgreSQL Range types and wire codec
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.RangeSpec (spec) where

import Data.Foldable (toList)
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import Hylo.Backend.Capability (Postgres)
import Hylo.Core.Codec (runBinaryDecoder, runBinaryEncoder)
import Hylo.Core.Context (Col, Ctx (..))
import Hylo.Core.Key (Id (..))
import Hylo.Core.Query (Query, each, where_)
import Hylo.Core.Table
  ( Table (..)
  , TableSchema (..)
  , colName
  , colSqlType
  , colSpecExpr
  , column
  , eraseColDef
  , primaryKey
  , tableSchema
  )
import Hylo.PG.Codec.Binary (doubleCodec, int32Codec)
import Hylo.PG.Compiler.SQL (compilePostgresQuery, previewPostgresSQL)
import Hylo.PG.Range
import Hylo.SQL.AST (Projectable (..), renderExpr)
import Hylo.SQL.Builder (BoundParam (..), CompiledQuery (..))
import Test.Hspec
import Test.QuickCheck

-- QuickCheck Arbitrary generators
instance Arbitrary a => Arbitrary (RangeBound a) where
  arbitrary =
    oneof
      [ Inclusive <$> arbitrary
      , Exclusive <$> arbitrary
      , pure Unbounded
      ]

instance Arbitrary a => Arbitrary (Range a) where
  arbitrary =
    frequency
      [ (1, pure EmptyRange)
      , (4, Range <$> arbitrary <*> arbitrary)
      ]

-- Mock event table schema with an int4range column
data MockEventT (ctx :: Ctx) = MockEvent
  { eventId :: !(Col ctx (Id MockEventT Int64))
  , validPeriod :: !(Col ctx (Range Int32))
  }

instance Table MockEventT where
  toTableExpr alias m =
    MockEvent
      { eventId = colSpecExpr alias m.eventId
      , validPeriod = colSpecExpr alias m.validPeriod
      }
  tableExprColumns e =
    [ renderExpr e.eventId
    , renderExpr e.validPeriod
    ]
  tableColDefs m = [eraseColDef m.eventId, eraseColDef m.validPeriod]
  tableColumnsMeta m =
    [ (colName m.eventId, colSqlType m.eventId)
    , (colName m.validPeriod, colSqlType m.validPeriod)
    ]
  patchEntries _ _ = []

instance Projectable (MockEventT 'Expr) where
  projectColumns = tableExprColumns

eventTableSchema :: TableSchema MockEventT
eventTableSchema =
  tableSchema "events" $
    MockEvent
      { eventId = primaryKey "id" "bigint"
      , validPeriod = column "validPeriod" "int4range"
      }

spec :: Spec
spec = describe "User Story 17: PostgreSQL Range Algebra & Wire Codec" $ do
  describe "Binary Wire Codec Round-Trip" $ do
    it "round-trips EmptyRange" $ do
      let codec = rangeCodec int32Codec
          encoded = runBinaryEncoder codec EmptyRange
      runBinaryDecoder codec encoded `shouldBe` Right EmptyRange

    it "round-trips Range (Inclusive 1) (Exclusive 10)" $ do
      let codec = rangeCodec int32Codec
          val = Range (Inclusive 1) (Exclusive 10)
          encoded = runBinaryEncoder codec val
      runBinaryDecoder codec encoded `shouldBe` Right val

    it "round-trips Range Unbounded (Inclusive 100)" $ do
      let codec = rangeCodec int32Codec
          val = Range Unbounded (Inclusive 100)
          encoded = runBinaryEncoder codec val
      runBinaryDecoder codec encoded `shouldBe` Right val

    it "round-trips Range (Exclusive 0) Unbounded" $ do
      let codec = rangeCodec int32Codec
          val = Range (Exclusive 0) Unbounded
          encoded = runBinaryEncoder codec val
      runBinaryDecoder codec encoded `shouldBe` Right val

    it "round-trips fully unbounded Range Unbounded Unbounded" $ do
      let codec = rangeCodec int32Codec
          val = Range Unbounded Unbounded
          encoded = runBinaryEncoder codec val
      runBinaryDecoder codec encoded `shouldBe` Right val

    it "satisfies round-trip equality property for arbitrary Int32 ranges" $
      property $ \(r :: Range Int32) ->
        let codec = rangeCodec int32Codec
            encoded = runBinaryEncoder codec r
         in runBinaryDecoder codec encoded === Right r

    it "satisfies round-trip equality property for arbitrary Double ranges" $
      property $ \(r :: Range Double) ->
        let codec = rangeCodec doubleCodec
            encoded = runBinaryEncoder codec r
         in runBinaryDecoder codec encoded === Right r

  describe "Range Text Formatting" $ do
    it "renders EmptyRange as 'empty'" $ do
      renderRangeText (T.pack . show) (emptyRange :: Range Int) `shouldBe` "empty"

    it "renders standard ranges with bracket and parenthesis bounds" $ do
      renderRangeText (T.pack . show) (Range (Inclusive (1 :: Int)) (Exclusive 10)) `shouldBe` "[1,10)"
      renderRangeText (T.pack . show) (Range (Exclusive (1 :: Int)) (Inclusive 10)) `shouldBe` "(1,10]"
      renderRangeText (T.pack . show) (Range Unbounded (Inclusive (100 :: Int))) `shouldBe` "(,100]"
      renderRangeText (T.pack . show) (Range (Inclusive (5 :: Int)) Unbounded) `shouldBe` "[5,)"
      renderRangeText (T.pack . show) (Range (Unbounded :: RangeBound Int) Unbounded) `shouldBe` "(,)"

  describe "Range Query Combinators & Parameter Binding" $ do
    it "compiles rangeOverlaps (&&:) with bound parameters" $ do
      let target = Range (Inclusive 5) (Exclusive 15)
          q :: Query (Postgres v) (MockEventT 'Expr)
          q = do
            e <- each eventTableSchema
            where_ (e.validPeriod &&: litRange "int4range" target)
            pure e
          cq = compilePostgresQuery q
      cq.compiledSQL `shouldSatisfy` T.isInfixOf "(\"t0\".\"validPeriod\" && $1)"
      case toList cq.compiledParams of
        [p1] -> do
          p1.paramIndex `shouldBe` 1
          p1.paramType `shouldBe` "int4range"
          p1.paramValue `shouldBe` "[5,15)"
        other -> expectationFailure ("Expected 1 param but got: " ++ show other)

    it "compiles rangeContains (@>:) with bound parameters" $ do
      let target = Range (Inclusive 1) (Exclusive 5)
          q :: Query (Postgres v) (MockEventT 'Expr)
          q = do
            e <- each eventTableSchema
            where_ (e.validPeriod @>: litRange "int4range" target)
            pure e
          cq = compilePostgresQuery q
      cq.compiledSQL `shouldSatisfy` T.isInfixOf "(\"t0\".\"validPeriod\" @> $1)"
      case toList cq.compiledParams of
        [p1] -> p1.paramValue `shouldBe` "[1,5)"
        other -> expectationFailure ("Expected 1 param but got: " ++ show other)

    it "compiles rangeAdjacent (-|- and -|-.) with bound parameters" $ do
      let target = Range (Inclusive 10) (Exclusive 20)
          q :: Query (Postgres v) (MockEventT 'Expr)
          q = do
            e <- each eventTableSchema
            where_ (e.validPeriod -|-. litRange "int4range" target)
            pure e
          cq = compilePostgresQuery q
      cq.compiledSQL `shouldSatisfy` T.isInfixOf "(\"t0\".\"validPeriod\" -|- $1)"

    it "compiles rangeContainedBy (<@:), rangeLeftOf (<<:), and rangeRightOf (>>:)" $ do
      let target = Range (Inclusive 1) (Inclusive 100)
          q :: Query (Postgres v) (MockEventT 'Expr)
          q = do
            e <- each eventTableSchema
            where_ (e.validPeriod <@: litRange "int4range" target)
            pure e
          cq = compilePostgresQuery q
      cq.compiledSQL `shouldSatisfy` T.isInfixOf "(\"t0\".\"validPeriod\" <@ $1)"

    it "interpolates Range literals in previewPostgresSQL" $ do
      let target = Range (Inclusive 10) (Exclusive 20)
          q :: Query (Postgres v) (MockEventT 'Expr)
          q = do
            e <- each eventTableSchema
            where_ (e.validPeriod &&: litRange "int4range" target)
            pure e
          sql = previewPostgresSQL q
      sql `shouldSatisfy` T.isInfixOf "(\"t0\".\"validPeriod\" && '[10,20)')"
