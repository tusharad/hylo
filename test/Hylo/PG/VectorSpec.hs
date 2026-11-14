{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{-# HLINT ignore "Avoid restricted function" #-}

{- |
Module      : Hylo.PG.VectorSpec
Description : Test suite for Epic 15: Type-Level pgvector Support
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.VectorSpec (spec) where

import Control.DeepSeq (rnf)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Vector as Vector
import GHC.TypeNats (natVal)
import Hylo.Core.Expr (QExpr (..), colExpr)
import Hylo.Core.Query.AST (renderExpr)
import Hylo.PG.Vector
  ( DistanceMetric (..)
  , Embedding (..)
  , VectorDistance (..)
  , VectorIndexType (..)
  , decodeBinaryVector
  , embeddingDim
  , embeddingFromList
  , encodeBinaryVector
  , mkEmbedding
  , renderVectorSql
  , renderVectorText
  , (<#>)
  , (<->)
  , (<=>)
  )
import Test.Hspec
import Test.QuickCheck
import Test.ShouldNotTypecheck (shouldNotTypecheckWith)

default (Text, Int64, Double)

-- | Arbitrary instance generating valid 3-dimensional embeddings for property testing.
instance Arbitrary (Embedding "test-model" 3) where
  arbitrary = do
    f1 <- choose (-100.0, 100.0)
    f2 <- choose (-100.0, 100.0)
    f3 <- choose (-100.0, 100.0)
    pure (Embedding (Vector.fromList [f1, f2, f3]))

-- | Arbitrary instance generating valid 4-dimensional embeddings.
instance Arbitrary (Embedding "test-model" 4) where
  arbitrary = do
    f1 <- choose (-100.0, 100.0)
    f2 <- choose (-100.0, 100.0)
    f3 <- choose (-100.0, 100.0)
    f4 <- choose (-100.0, 100.0)
    pure (Embedding (Vector.fromList [f1, f2, f3, f4]))

spec :: Spec
spec = describe "Epic 15: Type-Level pgvector Support" $ do
  describe "15.4 Executable Oracle Specification" $ do
    it "statically rejects vector distance between different models" $ do
      shouldNotTypecheckWith "[Hylo Error E007]" $
        let v1 = undefined :: QExpr (Embedding "text-embedding-3-small" 1536)
            v2 = undefined :: QExpr (Embedding "bge-large-en" 1024)
            forbidden = cosineDistance v1 v2
         in rnf forbidden

    it "encodes vector to pgvector binary wire format in IEEE 754 big-endian" $ do
      let vec = Embedding (Vector.fromList [1.0, 2.0, 3.0]) :: Embedding "test" 3
          bytes = encodeBinaryVector vec
      BS.take 4 bytes `shouldBe` BS.pack [0x00, 0x03, 0x00, 0x00] -- dim = 3, unused = 0
  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 1: Metric Identity Laws (Reflexivity: cosineDistance(v, v) == 0.0)" $ do
      property $ \(v :: Embedding "test-model" 3) -> do
        cosineDistance v v `shouldBe` 0.0

    it "Law 1: Metric Identity Laws (Symmetry: cosineDistance(v1, v2) == cosineDistance(v2, v1))" $ do
      property $ \(v1 :: Embedding "test-model" 3, v2 :: Embedding "test-model" 3) -> do
        let d12 = cosineDistance v1 v2
            d21 = cosineDistance v2 v1
        abs (d12 - d21) `shouldSatisfy` (< 1e-6)

    it "Law 1: Metric Identity Laws (L2 Reflexivity & Symmetry)" $ do
      property $ \(v1 :: Embedding "test-model" 3, v2 :: Embedding "test-model" 3) -> do
        l2Distance v1 v1 `shouldBe` 0.0
        let d12 = l2Distance v1 v2
            d21 = l2Distance v2 v1
        abs (d12 - d21) `shouldSatisfy` (< 1e-6)

    it "Law 2: Dimension Preservation Law (Vector.length(unEmbedding v) == natVal(Proxy @dim))" $ do
      property $ \(v :: Embedding "test-model" 3) -> do
        fromIntegral (Vector.length (unEmbedding v)) `shouldBe` natVal (Proxy @3)
        embeddingDim v `shouldBe` 3

    it "Law 2: Dimension Preservation Law (4-dimensional preservation)" $ do
      property $ \(v :: Embedding "test-model" 4) -> do
        fromIntegral (Vector.length (unEmbedding v)) `shouldBe` natVal (Proxy @4)
        embeddingDim v `shouldBe` 4

    it "Law 3: Type-Level Model Exclusivity (Dimension mismatch compile-time rejection)" $ do
      shouldNotTypecheckWith "[Hylo Error E007]" $
        let v1 = undefined :: QExpr (Embedding "same-model" 512)
            v2 = undefined :: QExpr (Embedding "same-model" 768)
            forbidden = cosineDistance v1 v2
         in rnf forbidden

  describe "Comprehensive pgvector Binary Wire Codec Engine" $ do
    it "guarantees round-trip encoding and decoding equality" $ do
      property $ \(v :: Embedding "test-model" 3) -> do
        let encoded = encodeBinaryVector v
            decoded = (decodeBinaryVector encoded :: Either String (Embedding "test-model" 3))
        decoded `shouldBe` Right v

    it "round-trips 4-dimensional embeddings cleanly" $ do
      property $ \(v :: Embedding "test-model" 4) -> do
        let encoded = encodeBinaryVector v
            decoded = (decodeBinaryVector encoded :: Either String (Embedding "test-model" 4))
        decoded `shouldBe` Right v

    it "decodes standard IEEE 754 float values accurately" $ do
      let vec = Embedding (Vector.fromList [1.0, 2.0, 3.0]) :: Embedding "test" 3
          bytes = encodeBinaryVector vec
      BS.length bytes `shouldBe` 16
      (decodeBinaryVector bytes :: Either String (Embedding "test" 3)) `shouldBe` Right vec

    it "defensively rejects truncated buffers smaller than 4-byte header" $ do
      let shortBytes = BS.pack [0x00, 0x03, 0x00]
      isLeft (decodeBinaryVector shortBytes :: Either String (Embedding "test" 3)) `shouldBe` True

    it "defensively rejects buffers with mismatched dimension in header" $ do
      let vec = Embedding (Vector.fromList [1.0, 2.0, 3.0]) :: Embedding "test" 3
          bytes = encodeBinaryVector vec
      isLeft (decodeBinaryVector bytes :: Either String (Embedding "test" 4)) `shouldBe` True

    it "defensively rejects buffers with trailing or truncated payload bytes" $ do
      let vec = Embedding (Vector.fromList [1.0, 2.0, 3.0]) :: Embedding "test" 3
          bytes = encodeBinaryVector vec
          truncated = BS.take 15 bytes
          extended = BS.snoc bytes 0x00
      isLeft (decodeBinaryVector truncated :: Either String (Embedding "test" 3)) `shouldBe` True
      isLeft (decodeBinaryVector extended :: Either String (Embedding "test" 3)) `shouldBe` True

  describe "Safe Embedding Construction & Dimension Checks" $ do
    it "mkEmbedding succeeds when vector length matches dimension" $ do
      let v = Vector.fromList [1.0, 2.0, 3.0]
          mEmb = (mkEmbedding v :: Maybe (Embedding "openai" 3))
      case mEmb of
        Just emb -> unEmbedding emb `shouldBe` v
        Nothing -> expectationFailure "Expected Just embedding but got Nothing"

    it "mkEmbedding fails when vector length mismatches dimension" $ do
      let v = Vector.fromList [1.0, 2.0]
          mEmb = (mkEmbedding v :: Maybe (Embedding "openai" 3))
      mEmb `shouldBe` Nothing

    it "embeddingFromList constructs typed embedding from float list" $ do
      let mEmb = (embeddingFromList [0.1, 0.2, 0.3] :: Maybe (Embedding "gemini" 3))
      case mEmb of
        Just emb -> Vector.toList (unEmbedding emb) `shouldBe` [0.1, 0.2, 0.3]
        Nothing -> expectationFailure "Expected Just embedding but got Nothing"

  describe "Distance Operators & SQL AST Compilation" $ do
    it "compiles Cosine distance operator (<=>)" $ do
      let colA = colExpr "docs_0" "embedding" :: QExpr (Embedding "openai-ada" 1536)
          colB = colExpr "queries_0" "query_vec" :: QExpr (Embedding "openai-ada" 1536)
          dist = colA <=> colB
          sql = renderExpr dist
      sql `shouldBe` "(\"docs_0\".\"embedding\" <=> \"queries_0\".\"query_vec\")"

    it "compiles L2 Euclidean distance operator (<->)" $ do
      let colA = colExpr "docs_0" "embedding" :: QExpr (Embedding "bge-small" 384)
          colB = colExpr "queries_0" "query_vec" :: QExpr (Embedding "bge-small" 384)
          dist = colA <-> colB
          sql = renderExpr dist
      sql `shouldBe` "(\"docs_0\".\"embedding\" <-> \"queries_0\".\"query_vec\")"

    it "compiles Negative Inner Product distance operator (<#>)" $ do
      let colA = colExpr "docs_0" "embedding" :: QExpr (Embedding "cohere-embed" 1024)
          colB = colExpr "queries_0" "query_vec" :: QExpr (Embedding "cohere-embed" 1024)
          dist = colA <#> colB
          sql = renderExpr dist
      sql `shouldBe` "(\"docs_0\".\"embedding\" <#> \"queries_0\".\"query_vec\")"

    it "calculates pure negative inner product matching pgvector semantics" $ do
      let v1 = Embedding (Vector.fromList [1.0, 2.0]) :: Embedding "test" 2
          v2 = Embedding (Vector.fromList [3.0, 4.0]) :: Embedding "test" 2
      innerProductDistance v1 v2 `shouldBe` -11.0

    it "renders vector text and SQL representations correctly" $ do
      let vec = Embedding (Vector.fromList [1.0, 2.5, -3.0]) :: Embedding "test" 3
      renderVectorText vec `shouldBe` "[1.0, 2.5, -3.0]"
      renderVectorSql vec `shouldBe` "'[1.0, 2.5, -3.0]'::vector(3)"

    it "supports VectorIndexType and DistanceMetric descriptors" $ do
      rnf HNSW `shouldBe` ()
      rnf IVFFlat `shouldBe` ()
      rnf CosineMetric `shouldBe` ()
      rnf L2Metric `shouldBe` ()
      rnf InnerProductMetric `shouldBe` ()
