{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.SQLite.Compiler
Description : High-level SQL code generator for SQLite dialect from operational Query AST
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.SQLite.Compiler
  ( renderSQLiteSQL
  , renderSQLiteSQLText
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core.Expr (QExpr (..))
import Hylo.Core.Query.AST
  ( FromItem (..)
  , LateralBinding (..)
  , Projectable (..)
  , SQLite
  , renderExpr
  )
import Hylo.Core.Query.CTE (renderCTEs)
import Hylo.Core.Query.Monad
  ( Query (..)
  , QueryState (..)
  , initialQueryState
  )
import Hylo.SQL.Compiler.JoinFlattener
  ( flattenJoins
  , renderFromItem
  )

-- | Render a Query into strict Text SQL representation for SQLite.
renderSQLiteSQL :: Projectable a => Query SQLite a -> Text
renderSQLiteSQL = renderSQLiteSQLText

-- | Render a Query into strict Text SQL representation for SQLite.
renderSQLiteSQLText :: Projectable a => Query SQLite a -> Text
renderSQLiteSQLText q =
  let (res, st) = runQuery q initialQueryState
   in case qsUpsert st of
        Just upsertSql -> upsertSql
        Nothing ->
          case qsCustomQuery st of
            Just customSql -> customSql
            Nothing ->
              let cteClause = renderCTEs (qsCTEs st)
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
                  limitClause = case qsLimit st of
                    Nothing -> ""
                    Just n -> " LIMIT " <> T.pack (show n)
                  offsetClause = case qsOffset st of
                    Nothing -> ""
                    Just n -> " OFFSET " <> T.pack (show n)
               in cteClause
                    <> selectClause
                    <> fromClause
                    <> whereClause
                    <> groupByClause
                    <> havingClause
                    <> orderByClause
                    <> limitClause
                    <> offsetClause
