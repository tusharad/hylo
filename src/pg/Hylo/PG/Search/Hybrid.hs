{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.PG.Search.Hybrid
Description : Single-Trip Hybrid Search Engine (BM25 Full-Text + Vector Reciprocal Rank Fusion)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides single-trip hybrid search combining lexical full-text BM25 search
(@ts_rank_cd@ on @to_tsvector@ / @to_tsquery@) and semantic vector similarity
search (@<=>@ on @pgvector@) fused in-database via Reciprocal Rank Fusion (RRF).

Executes as a single PostgreSQL query utilizing window functions and Common Table
Expressions (CTEs) to concurrently leverage GIN and HNSW/IVFFlat indexes without
requiring external search infrastructure or linear score calibration.
-}
module Hylo.PG.Search.Hybrid
  ( -- * Hybrid Search Combinators
    hybridSearchRRF
  , hybridSearchRRFWith

    -- * SQL Rendering
  , renderHybridSearchSQL
  , HybridSearchAction (..)

    -- * Search Results & Configuration
  , HybridSearchResult (..)
  , HybridSearchConfig (..)
  , defaultHybridSearchConfig

    -- * Backend Capabilities
  , SupportsHybridSearch (..)
  , SQLite

    -- * Utilities & Sample Generation
  , sampleEmbedding
  , extractColName

    -- * Re-exports
  , module Hylo.PG.Search.RRF
  , module Hylo.PG.Vector
  ) where

import Control.DeepSeq (NFData (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import GHC.TypeNats (natVal)
import Hylo.Backend.Capability (SQLite)
import Hylo.Core.Context (Ctx (..))
import Hylo.Core.Expr (QExpr (..))
import Hylo.Core.Query (Postgres, Query (..), QueryState (..))
import Hylo.Core.Query.AST (Projectable (..), renderExpr)
import Hylo.Core.Table (Table (..), TableSchema (..))
import Hylo.Core.TypeError (AssertBackendCapability (..))
import Hylo.PG.Search.RRF
import Hylo.PG.Vector

-- | Backend capability certifying database support for single-trip hybrid search.
class SupportsHybridSearch db where
  hybridSearchDict :: Proxy db -> ()

instance SupportsHybridSearch (Postgres v) where
  hybridSearchDict _ = ()

instance SupportsHybridSearch Postgres where
  hybridSearchDict _ = ()

instance (AssertBackendCapability "HybridSearchRRF" "SQLite" 'False) => SupportsHybridSearch SQLite where
  hybridSearchDict _ =
    backendCapabilityDict (Proxy @"HybridSearchRRF") (Proxy @"SQLite") (Proxy @'False)

-- | Configuration settings for hybrid lexical and vector search compilation.
data HybridSearchConfig = HybridSearchConfig
  { hscLanguage :: !Text
  -- ^ Full-text search configuration dictionary (default: @"english"@).
  , hscDistanceMetric :: !Text
  -- ^ PostgreSQL vector distance operator (default: @"<=>"@ for cosine distance).
  , hscCandidateLimit :: !Int
  -- ^ Maximum candidates retrieved from each individual index (default: 100).
  , hscLexicalParamIndex :: !Int
  -- ^ Positional parameter index for the lexical query string (default: 1).
  , hscVectorParamIndex :: !Int
  -- ^ Positional parameter index for the vector embedding (default: 2).
  }
  deriving (Eq, Show, Generic)

instance NFData HybridSearchConfig

-- | Default hybrid search configuration.
defaultHybridSearchConfig :: HybridSearchConfig
defaultHybridSearchConfig =
  HybridSearchConfig
    { hscLanguage = "english"
    , hscDistanceMetric = "<=>"
    , hscCandidateLimit = 100
    , hscLexicalParamIndex = 1
    , hscVectorParamIndex = 2
    }

-- | Fused search result record tracking hydrated entity and ranking metrics.
data HybridSearchResult table (model :: Symbol) (dim :: Nat) = HybridSearchResult
  { searchRecord :: !(table 'Expr)
  -- ^ Projected table columns in 'Expr context.
  , searchLexRank :: !(QExpr Int)
  -- ^ Rank from the lexical BM25 full-text branch (1-based).
  , searchVecRank :: !(QExpr Int)
  -- ^ Rank from the vector similarity branch (1-based).
  , searchRRFScore :: !(QExpr Double)
  -- ^ Fused Reciprocal Rank Fusion score.
  }
  deriving (Generic)

instance (NFData (table 'Expr)) => NFData (HybridSearchResult table model dim)

instance (Table table) => Projectable (HybridSearchResult table model dim) where
  projectColumns res =
    tableExprColumns (searchRecord res)
      ++ [ renderExpr (searchLexRank res)
         , renderExpr (searchVecRank res)
         , renderExpr (searchRRFScore res)
         ]

-- | Operational description of a hybrid search query for SQL serialization.
data HybridSearchAction table (model :: Symbol) (dim :: Nat) = HybridSearchAction
  { hsaSchema :: !(TableSchema table)
  , hsaTextColumn :: !Text
  , hsaTextQuery :: !Text
  , hsaEmbColumn :: !Text
  , hsaEmbQuery :: !(Embedding model dim)
  , hsaK :: !Int
  , hsaLimit :: !Int
  , hsaConfig :: !HybridSearchConfig
  }
  deriving (Generic)

instance (NFData (TableSchema table)) => NFData (HybridSearchAction table model dim)

-- | Construct a zero-filled sample embedding of matching dimension for query synthesis.
sampleEmbedding :: forall model dim. (KnownNat dim) => Embedding model dim
sampleEmbedding =
  let d = fromIntegral (natVal (Proxy @dim))
   in Embedding (Vector.replicate d 0.0)

-- | Extract the target column name from a 'QExpr'.
extractColName :: QExpr a -> Text
extractColName (ColExpr _ col) = col
extractColName (BinOpExpr _ l _) = extractColName l
extractColName (UnaryOpExpr _ e) = extractColName e
extractColName (LiftNullableExpr e) = extractColName e
extractColName (CoalesceExpr e _) = extractColName e
extractColName other = renderExpr other

-- | Render a 'HybridSearchAction' into a single-trip PostgreSQL query with CTEs and RRF score.
renderHybridSearchSQL ::
  forall table model dim.
  HybridSearchAction table model dim ->
  Text
renderHybridSearchSQL act =
  let tName = tableName (hsaSchema act)
      quotedTable = "\"" <> tName <> "\""
      textField = hsaTextColumn act
      embField = hsaEmbColumn act
      cfg = hsaConfig act
      lang = hscLanguage cfg
      metric = hscDistanceMetric cfg
      candLimit = hscCandidateLimit cfg
      kVal = hsaK act
      resLimit = hsaLimit act
      lexParam = "$" <> T.pack (show (hscLexicalParamIndex cfg))
      vecParam = "$" <> T.pack (show (hscVectorParamIndex cfg))

      lexRankExpr =
        "ts_rank_cd(to_tsvector('"
          <> lang
          <> "', "
          <> quotedTable
          <> ".\""
          <> textField
          <> "\"), to_tsquery('"
          <> lang
          <> "', "
          <> lexParam
          <> "))"
      vecDistExpr =
        "(" <> quotedTable <> ".\"" <> embField <> "\" " <> metric <> " " <> vecParam <> ")"
      rrfScoreExpr =
        "(1.0 / ("
          <> T.pack (show kVal)
          <> " + \"lex_rank\")) + (1.0 / ("
          <> T.pack (show kVal)
          <> " + \"vec_rank\")) AS \"rrf_score\""

      sql =
        "WITH \"lexical_search\" AS (\n"
          <> "  SELECT "
          <> quotedTable
          <> ".*, ROW_NUMBER() OVER (ORDER BY "
          <> lexRankExpr
          <> " DESC) AS \"lex_rank\"\n"
          <> "  FROM "
          <> quotedTable
          <> "\n"
          <> "  WHERE to_tsvector('"
          <> lang
          <> "', "
          <> quotedTable
          <> ".\""
          <> textField
          <> "\") @@ to_tsquery('"
          <> lang
          <> "', "
          <> lexParam
          <> ")\n"
          <> "  LIMIT "
          <> T.pack (show candLimit)
          <> "\n"
          <> "),\n"
          <> "\"vector_search\" AS (\n"
          <> "  SELECT "
          <> quotedTable
          <> ".*, ROW_NUMBER() OVER (ORDER BY "
          <> vecDistExpr
          <> " ASC) AS \"vec_rank\"\n"
          <> "  FROM "
          <> quotedTable
          <> "\n"
          <> "  ORDER BY "
          <> vecDistExpr
          <> " ASC\n"
          <> "  LIMIT "
          <> T.pack (show candLimit)
          <> "\n"
          <> ")\n"
          <> "SELECT "
          <> quotedTable
          <> ".*, "
          <> rrfScoreExpr
          <> "\n"
          <> "FROM \"lexical_search\"\n"
          <> "FULL OUTER JOIN \"vector_search\" ON \"lexical_search\".\"id\" = \"vector_search\".\"id\"\n"
          <> "ORDER BY \"rrf_score\" DESC\n"
          <> "LIMIT "
          <> T.pack (show resLimit)
   in sql

-- | Single-query Reciprocal Rank Fusion (RRF) combining BM25 full-text and vector search.
hybridSearchRRF ::
  forall model dim table db.
  ( SupportsHybridSearch db
  , Table table
  ) =>
  (table 'Expr -> QExpr Text) ->
  Text ->
  (table 'Expr -> QExpr (Embedding model dim)) ->
  Embedding model dim ->
  Int ->
  Int ->
  TableSchema table ->
  Query db (HybridSearchResult table model dim)
hybridSearchRRF = hybridSearchRRFWith defaultHybridSearchConfig

-- | Generalized hybrid search combinator accepting custom 'HybridSearchConfig'.
hybridSearchRRFWith ::
  forall model dim table db.
  ( SupportsHybridSearch db
  , Table table
  ) =>
  HybridSearchConfig ->
  (table 'Expr -> QExpr Text) ->
  Text ->
  (table 'Expr -> QExpr (Embedding model dim)) ->
  Embedding model dim ->
  Int ->
  Int ->
  TableSchema table ->
  Query db (HybridSearchResult table model dim)
hybridSearchRRFWith cfg textSel queryText embSel queryEmb k limitCount schema =
  let tName = tableName schema
      tExpr = toTableExpr tName (tableMeta schema)
      textCol = extractColName (textSel tExpr)
      embCol = extractColName (embSel tExpr)
      act =
        HybridSearchAction
          { hsaSchema = schema
          , hsaTextColumn = textCol
          , hsaTextQuery = queryText
          , hsaEmbColumn = embCol
          , hsaEmbQuery = queryEmb
          , hsaK = k
          , hsaLimit = limitCount
          , hsaConfig = cfg
          }
      renderedSql = renderHybridSearchSQL act
      res =
        HybridSearchResult
          { searchRecord = tExpr
          , searchLexRank = ColExpr "lexical_search" "lex_rank"
          , searchVecRank = ColExpr "vector_search" "vec_rank"
          , searchRRFScore = ColExpr "" "rrf_score"
          }
   in Query $ \s ->
        hybridSearchDict (Proxy @db) `seq`
          let s' = s {qsCustomQuery = Just renderedSql}
           in (res, s')
