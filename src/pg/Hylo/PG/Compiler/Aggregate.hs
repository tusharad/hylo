{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.PG.Compiler.Aggregate
Description : PostgreSQL-specific aggregate expression and clause compilation
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.Aggregate
  ( -- * Aggregate SQL Compilation
    renderAggregateExpr
  , renderGroupByClause
  , renderHavingClause
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core.Expr (QExpr (..))
import Hylo.Core.Query.AST (renderExpr)
import Hylo.SQL.Query.Aggregate (AggExpr (..))

-- | Render an aggregate expression to PostgreSQL SQL text.
renderAggregateExpr :: AggExpr a -> Text
renderAggregateExpr (AggExpr e) = renderExpr e

-- | Render a list of GROUP BY column expressions.
renderGroupByClause :: [Text] -> Text
renderGroupByClause = \case
  [] -> ""
  cols -> " GROUP BY " <> T.intercalate ", " cols

-- | Render a list of HAVING predicate expressions.
renderHavingClause :: [QExpr Bool] -> Text
renderHavingClause = \case
  [] -> ""
  (h : hs) ->
    let combined = foldl (BinOpExpr "AND") h hs
     in " HAVING " <> renderExpr combined
