{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{-# HLINT ignore "Avoid restricted function" #-}

{- |
Module      : Hylo.PG.Search.HybridSpec
Description : Test suite for Epic 16: Single-Trip Hybrid RRF Search
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Search.HybridSpec (spec) where

import Control.DeepSeq (rnf)
import Data.Int (Int64)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Hylo.Core.Context (Col, Ctx (..))
import Hylo.Core.Expr (QExpr (..), colExpr)
import Hylo.Core.Key (Id (..))
import Hylo.Core.Query (Query (..))
import Hylo.Core.Table (Table (..), TableSchema (..), colSpecExpr, column, primaryKey, tableSchema)
import Hylo.PG.Compiler.SQL (compilePostgresQuery, renderSQL)
import Hylo.PG.Search.Hybrid
import Hylo.SQL.AST (Projectable (..), renderExpr)
import Hylo.SQL.Builder (BoundParam (..), CompiledQuery (..))
import Hylo.Test.ShouldNotTypecheck (shouldNotTypecheckWith)
import Test.Hspec
import Test.QuickCheck

default (Text, Int64, Double)

-- | HKD Table: Searchable document entity for hybrid RRF search.
data DocumentT (ctx :: Ctx) = Document
  { id :: !(Col ctx (Id DocumentT Int64))
  , body :: !(Col ctx Text)
  , embedding :: !(Col ctx (Embedding "text-embedding-3-small" 1536))
  }

instance Table DocumentT where
  toTableExpr alias m =
    Document
      { id = colSpecExpr alias m.id
      , body = colSpecExpr alias m.body
      , embedding = colSpecExpr alias m.embedding
      }
  tableExprColumns d =
    [ renderExpr d.id
    , renderExpr d.body
    , renderExpr d.embedding
    ]

instance Projectable (DocumentT 'Expr) where
  projectColumns = tableExprColumns

documentTable :: TableSchema DocumentT
documentTable =
  tableSchema "documents" $
    Document
      { id = primaryKey "id" "bigint"
      , body = column "body" "text"
      , embedding = column "embedding" "vector(1536)"
      }

spec :: Spec
spec = describe "Epic 16: Single-Trip Hybrid RRF Search" $ do
  describe "16.4 Executable Oracle Specification" $ do
    it "compiles single-query BM25 and HNSW vector Reciprocal Rank Fusion" $ do
      let queryText = "haskell functional programming"
          queryEmb = sampleEmbedding :: Embedding "text-embedding-3-small" 1536
          q =
            hybridSearchRRF
              (.body)
              queryText
              (.embedding)
              queryEmb
              60 -- k smoothing constant
              20 -- limit
          queryAction = q documentTable
          sql = renderSQL queryAction
      sql
        `shouldContain` "ts_rank_cd(to_tsvector('english', \"documents\".\"body\"), to_tsquery('english', $1))"
      sql `shouldContain` "(\"documents\".\"embedding\" <=> $2)"
      sql `shouldContain` "(1.0 / (60 + \"lex_rank\")) + (1.0 / (60 + \"vec_rank\")) AS \"rrf_score\""
      let compiled = compilePostgresQuery queryAction
      Seq.length compiled.compiledParams `shouldBe` 2
      let p1 = Seq.index compiled.compiledParams 0
          p2 = Seq.index compiled.compiledParams 1
      p1.paramIndex `shouldBe` 1
      p1.paramValue `shouldBe` queryText
      p1.paramType `shouldBe` "text"
      p2.paramIndex `shouldBe` 2
      p2.paramValue `shouldBe` renderVectorText queryEmb
      p2.paramType `shouldBe` "vector"

  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 1: Monotonicity of Reciprocal Rank Fusion (Better ranks yield higher/equal RRF score)" $ do
      property $ \(Positive (r1a :: Int), Positive (r1b :: Int), Positive (diffA :: Int), Positive (diffB :: Int)) -> do
        let r2a = r1a + diffA
            r2b = r1b + diffB
        isMonotonicRRF 60.0 (r1a, r1b) (r2a, r2b) `shouldBe` True

    it "Law 2: Score Range Bound (0 < RRF_k(d) <= |Modalities| / (k + 1))" $ do
      property $ \(Positive (kVal :: Int), Positive (r1 :: Int), Positive (r2 :: Int)) -> do
        let kDbl = fromIntegral (max 1 kVal)
        isBoundedRRF kDbl r1 r2 `shouldBe` True

    it "Law 3: Equivalence of Symmetric Search (RRF(r1, r2) == RRF(r2, r1))" $ do
      property $ \(Positive (kVal :: Int), Positive (r1 :: Int), Positive (r2 :: Int)) -> do
        let kDbl = fromIntegral (max 1 kVal)
        isSymmetricRRF kDbl r1 r2 `shouldBe` True

  describe "Static Type-Level Diagnostics (E007, E008)" $ do
    it "statically rejects vector distance between mismatched models with [Hylo Error E007]" $ do
      shouldNotTypecheckWith "[Hylo Error E007]" $
        let v1 = undefined :: QExpr (Embedding "text-embedding-3-small" 1536)
            v2 = undefined :: QExpr (Embedding "bge-large-en" 1024)
            forbidden = cosineDistance v1 v2
         in rnf forbidden

    it "statically rejects hybridSearchRRF on unsupported database backends with [Hylo Error E008]" $ do
      shouldNotTypecheckWith "[Hylo Error E008]" $
        let queryText = "search text"
            queryEmb = sampleEmbedding :: Embedding "text-embedding-3-small" 1536
            q =
              hybridSearchRRF
                (.body)
                queryText
                (.embedding)
                queryEmb
                60
                20 ::
                TableSchema DocumentT -> Query SQLite (HybridSearchResult DocumentT "text-embedding-3-small" 1536)
            forbidden = q documentTable
         in rnf forbidden

  describe "Comprehensive Single-Trip Query Synthesis & Options" $ do
    it "renders custom smoothing constant k into SQL" $ do
      let queryText = "concurrency stm"
          queryEmb = sampleEmbedding :: Embedding "text-embedding-3-small" 1536
          q =
            hybridSearchRRF
              (.body)
              queryText
              (.embedding)
              queryEmb
              45 -- custom k = 45
              10 -- limit = 10
          sql = renderSQL (q documentTable)
      sql `shouldContain` "(1.0 / (45 + \"lex_rank\")) + (1.0 / (45 + \"vec_rank\")) AS \"rrf_score\""
      sql `shouldContain` "LIMIT 10"

    it "supports custom language and distance metric via hybridSearchRRFWith" $ do
      let cfg =
            defaultHybridSearchConfig
              { hscLanguage = "french"
              , hscDistanceMetric = "<->"
              , hscCandidateLimit = 250
              }
          queryText = "programmation fonctionnelle"
          queryEmb = sampleEmbedding :: Embedding "text-embedding-3-small" 1536
          q =
            hybridSearchRRFWith
              cfg
              (.body)
              queryText
              (.embedding)
              queryEmb
              60
              15
          sql = renderSQL (q documentTable)
      sql `shouldContain` "to_tsvector('french', \"documents\".\"body\")"
      sql `shouldContain` "to_tsquery('french', $1)"
      sql `shouldContain` "(\"documents\".\"embedding\" <-> $2)"
      sql `shouldContain` "LIMIT 250"
      sql `shouldContain` "LIMIT 15"

    it "correctly projects columns via HybridSearchResult Projectable instance" $ do
      let res =
            HybridSearchResult
              { searchRecord = toTableExpr "documents" (tableMeta documentTable)
              , searchLexRank = colExpr "lexical_search" "lex_rank"
              , searchVecRank = colExpr "vector_search" "vec_rank"
              , searchRRFScore = colExpr "" "rrf_score"
              }
          cols = projectColumns res
      cols `shouldContain` ["\"documents\".\"body\""]
      cols `shouldContain` ["\"lexical_search\".\"lex_rank\""]
      cols `shouldContain` ["\"vector_search\".\"vec_rank\""]
      cols `shouldContain` ["\"rrf_score\""]

  describe "Pure RRF Mathematical Combinators & Weights" $ do
    it "computes standard RRF score over rank pairs correctly" $ do
      -- k = 60, r1 = 1, r2 = 1 -> (1/61) + (1/61) = 2/61
      let score = computeRRF2 60.0 1 1
      abs (score - (2.0 / 61.0)) `shouldSatisfy` (< 1e-9)

    it "computes weighted RRF correctly" $ do
      -- k = 60, (w1=2.0, r1=1), (w2=1.0, r2=1) -> (2/61) + (1/61) = 3/61
      let score = weightedRRF 60.0 [(2.0, 1), (1.0, 1)]
      abs (score - (3.0 / 61.0)) `shouldSatisfy` (< 1e-9)

    it "handles zero or missing ranks gracefully" $ do
      computeRRF2 60.0 0 1 `shouldBe` (1.0 / 61.0)
      computeRRF 60.0 [] `shouldBe` 0.0
      weightedRRF 60.0 [] `shouldBe` 0.0
