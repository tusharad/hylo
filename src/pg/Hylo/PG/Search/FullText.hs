{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.PG.Search.FullText
Description : Standalone PostgreSQL Full-Text Search algebra and combinators
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides type-safe operators and combinators for PostgreSQL Full-Text Search (FTS),
including 'to_tsvector', 'to_tsquery', 'plainto_tsquery', 'websearch_to_tsquery',
match predicate (@@), and relevance ranking ('ts_rank').
-}
module Hylo.PG.Search.FullText
  ( -- * Phantom FTS Types
    TsVector
  , TsQuery

    -- * Literals
  , litTsVector
  , litTsQuery

    -- * Vector Generation
  , toTsVector
  , toTsVectorWithCfg

    -- * Query Parsing
  , toTsQuery
  , toTsQueryWithCfg
  , plainToTsQuery
  , phraseToTsQuery
  , websearchToTsQuery

    -- * Match Predicate & Ranking
  , ftsMatches
  , (@@)
  , (@@.)
  , tsRank
  , tsRankCd
  ) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hylo.Core.Expr (QExpr (..), func2Expr, lit, litTyped)

-- | Phantom type representing a PostgreSQL text search vector ('tsvector').
data TsVector = TsVector
  deriving (Eq, Show, Generic)

instance NFData TsVector

-- | Phantom type representing a PostgreSQL text search query ('tsquery').
data TsQuery = TsQuery
  deriving (Eq, Show, Generic)

instance NFData TsQuery

-- | Construct a literal 'TsVector' bound as a $1 parameter.
litTsVector :: Text -> QExpr TsVector
litTsVector txt = litTyped txt "tsvector"

-- | Construct a literal 'TsQuery' bound as a $1 parameter.
litTsQuery :: Text -> QExpr TsQuery
litTsQuery txt = litTyped txt "tsquery"

-- | Convert text into a 'TsVector' using the database default text search configuration.
toTsVector :: QExpr Text -> QExpr TsVector
toTsVector txt = AggOpExpr "to_tsvector" txt Nothing

-- | Convert text into a 'TsVector' using an explicit text search configuration (e.g. @"english"@).
toTsVectorWithCfg :: Text -> QExpr Text -> QExpr TsVector
toTsVectorWithCfg cfg = func2Expr "to_tsvector" (lit cfg)

-- | Parse text into a 'TsQuery' using Boolean operators (&, |, !).
toTsQuery :: QExpr Text -> QExpr TsQuery
toTsQuery txt = AggOpExpr "to_tsquery" txt Nothing

-- | Parse text into a 'TsQuery' with an explicit text search configuration.
toTsQueryWithCfg :: Text -> QExpr Text -> QExpr TsQuery
toTsQueryWithCfg cfg = func2Expr "to_tsquery" (lit cfg)

-- | Parse unformatted plain text into a 'TsQuery' (words joined by &).
plainToTsQuery :: QExpr Text -> QExpr TsQuery
plainToTsQuery txt = AggOpExpr "plainto_tsquery" txt Nothing

-- | Parse text into an exact phrase 'TsQuery' (words joined by <->).
phraseToTsQuery :: QExpr Text -> QExpr TsQuery
phraseToTsQuery txt = AggOpExpr "phraseto_tsquery" txt Nothing

-- | Parse user search box input into a 'TsQuery' using web search syntax (unquoted words, "phrases", or OR).
websearchToTsQuery :: QExpr Text -> QExpr TsQuery
websearchToTsQuery txt = AggOpExpr "websearch_to_tsquery" txt Nothing

-- | PostgreSQL full-text search match predicate (@@). True if vector matches query.
ftsMatches :: QExpr TsVector -> QExpr TsQuery -> QExpr Bool
ftsMatches = BinOpExpr "@@"

-- | Infix operator for 'ftsMatches'.
(@@) :: QExpr TsVector -> QExpr TsQuery -> QExpr Bool
(@@) = ftsMatches

infix 4 @@

-- | Dotted infix operator for 'ftsMatches'.
(@@.) :: QExpr TsVector -> QExpr TsQuery -> QExpr Bool
(@@.) = ftsMatches

infix 4 @@.

-- | Compute full-text search relevance score using PostgreSQL @ts_rank(vector, query)@.
tsRank :: QExpr TsVector -> QExpr TsQuery -> QExpr Double
tsRank = func2Expr "ts_rank"

-- | Compute full-text search cover density relevance score using PostgreSQL @ts_rank_cd(vector, query)@.
tsRankCd :: QExpr TsVector -> QExpr TsQuery -> QExpr Double
tsRankCd = func2Expr "ts_rank_cd"
