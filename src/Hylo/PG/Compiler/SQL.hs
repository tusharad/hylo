{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.PG.Compiler.SQL
Description : High-level SQL code generator from operational Query AST
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.SQL
  ( renderSQL
  , renderSQLText
  ) where

import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core.Expr (QExpr (..))
import Hylo.Core.Query (Postgres)
import Hylo.Core.Query.AST
  ( FromItem (..)
  , LateralBinding (..)
  , Projectable (..)
  , renderExpr
  )
import Hylo.Core.Query.Monad
  ( Query (..)
  , QueryState (..)
  , initialQueryState
  )
import Hylo.PG.Compiler.CTE (renderCTEs)
import Hylo.PG.Compiler.JoinFlattener
  ( flattenJoins
  , renderFromItem
  )

-- | Render a Query into polymorphic string/text SQL representation.
renderSQL :: (Projectable a, IsString s) => Query Postgres a -> s
renderSQL q = fromString (T.unpack (renderSQLText q))

-- | Render a Query into strict Text SQL representation.
renderSQLText :: Projectable a => Query Postgres a -> Text
renderSQLText q =
  let (res, st) = runQuery q initialQueryState
      cteClause = renderCTEs (qsCTEs st)
      projections = projectColumns res
      selectClause =
        "SELECT "
          <> (if null projections then "1" else T.intercalate ", " projections)
      (mFrom, wherePreds) = flattenJoins (qsRelations st) (qsPredicates st)
      mFromWithLaterals = case mFrom of
        Nothing -> case qsLaterals st of
          [] -> Nothing
          (l : ls) ->
            Just $
              foldl
                (\acc (LateralBinding a sql) -> LeftJoinLateral acc sql a)
                (FromTable (lbAlias l) (lbAlias l))
                ls
        Just baseItem ->
          Just $
            foldl
              (\acc (LateralBinding a sql) -> LeftJoinLateral acc sql a)
              baseItem
              (qsLaterals st)
      fromClause = case mFromWithLaterals of
        Nothing -> ""
        Just item -> " FROM " <> renderFromItem item
      whereClause = case wherePreds of
        [] -> ""
        (p : ps) ->
          let combined = foldl (BinOpExpr "AND") p ps
           in " WHERE " <> renderExpr combined
      groupByClause = case qsGroupBy st of
        [] -> ""
        cols -> " GROUP BY " <> T.intercalate ", " cols
      havingClause = case qsHaving st of
        [] -> ""
        (h : hs) ->
          let combined = foldl (BinOpExpr "AND") h hs
           in " HAVING " <> renderExpr combined
      orderByClause = case qsOrderBy st of
        [] -> ""
        orders -> " ORDER BY " <> T.intercalate ", " orders
   in cteClause
        <> selectClause
        <> fromClause
        <> whereClause
        <> groupByClause
        <> havingClause
        <> orderByClause
