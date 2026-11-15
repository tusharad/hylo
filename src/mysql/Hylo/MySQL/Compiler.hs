{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.MySQL.Compiler
Description : High-level SQL code generator for MySQL dialect from operational Query AST
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.MySQL.Compiler
  ( renderMySQLSQL
  , renderMySQLSQLText
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Backend.Capability (MySQL)
import Hylo.Core.Expr (QExpr (..))
import Hylo.Core.Query.Monad
  ( Query (..)
  , QueryState (..)
  , initialQueryState
  )
import Hylo.SQL.AST
  ( FromItem (..)
  , LateralBinding (..)
  , Projectable (..)
  , renderExpr
  )
import Hylo.SQL.Compiler.JoinFlattener
  ( flattenJoins
  , renderFromItem
  )
import Hylo.SQL.Query.CTE (renderCTEs)

-- | Render a Query into strict Text SQL representation for MySQL.
renderMySQLSQL :: Projectable a => Query MySQL a -> Text
renderMySQLSQL = renderMySQLSQLText

-- | Render a Query into strict Text SQL representation for MySQL.
renderMySQLSQLText :: Projectable a => Query MySQL a -> Text
renderMySQLSQLText q =
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
                  limitOffsetClause = case (qsLimit st, qsOffset st) of
                    (Nothing, Nothing) -> ""
                    (Just lim, Nothing) -> " LIMIT " <> T.pack (show lim)
                    (Nothing, Just off) -> " LIMIT 18446744073709551615 OFFSET " <> T.pack (show off)
                    (Just lim, Just off) -> " LIMIT " <> T.pack (show lim) <> " OFFSET " <> T.pack (show off)
               in cteClause
                    <> selectClause
                    <> fromClause
                    <> whereClause
                    <> groupByClause
                    <> havingClause
                    <> orderByClause
                    <> limitOffsetClause
