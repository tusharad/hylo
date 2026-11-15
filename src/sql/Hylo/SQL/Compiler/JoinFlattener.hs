{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.SQL.Compiler.JoinFlattener
Description : Algebraic Join Flattener rewriting monadic Cartesian products into ANSI SQL-92 JOIN trees
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.SQL.Compiler.JoinFlattener
  ( generateScopes
  , flattenJoins
  , renderFromItem
  ) where

import Data.List (partition)
import Data.Text (Text)
import Hylo.Core.Expr (QExpr (..))
import Hylo.SQL.AST
  ( FromItem (..)
  , RelationBinding (..)
  , exprAliases
  , generateScopes
  , renderExpr
  )

{- | Flatten a sequence of relation bindings and predicates into an ANSI SQL JOIN tree
and remaining unconsumed WHERE filter predicates.
-}
flattenJoins ::
  [RelationBinding] ->
  [QExpr Bool] ->
  (Maybe FromItem, [QExpr Bool])
flattenJoins [] preds = (Nothing, preds)
flattenJoins (r0 : rs) initialPreds =
  let baseFrom = FromTable (rbTableName r0) (rbAlias r0)
      initialJoinedAliases = [rbAlias r0]
      (finalFrom, remainingPreds, _) =
        foldl step (baseFrom, initialPreds, initialJoinedAliases) rs
   in (Just finalFrom, remainingPreds)
  where
    step (curFrom, curPreds, joinedAliases) r =
      let rAlias = rbAlias r
          validScopeAliases = rAlias : joinedAliases
          isJoinPred p =
            let aliases = exprAliases p
             in rAlias `elem` aliases && all (`elem` validScopeAliases) aliases
          (joinPreds, otherPreds) = partition isJoinPred curPreds
          nextFrom = case joinPreds of
            [] -> CrossJoin curFrom (FromTable (rbTableName r) rAlias)
            (p : ps) ->
              let combinedCond = foldl (BinOpExpr "AND") p ps
               in InnerJoin curFrom (FromTable (rbTableName r) rAlias) combinedCond
       in (nextFrom, otherPreds, rAlias : joinedAliases)

-- | Render an abstract ANSI SQL FROM clause item into formatted SQL Text.
renderFromItem :: FromItem -> Text
renderFromItem = \case
  FromTable t alias -> "\"" <> t <> "\" AS \"" <> alias <> "\""
  InnerJoin left right onCond ->
    renderFromItem left
      <> " INNER JOIN "
      <> renderFromItem right
      <> " ON "
      <> renderExpr onCond
  LeftJoin left right onCond ->
    renderFromItem left
      <> " LEFT JOIN "
      <> renderFromItem right
      <> " ON "
      <> renderExpr onCond
  LeftJoinLateral left subquerySql latAlias _ ->
    renderFromItem left
      <> " LEFT JOIN LATERAL ("
      <> subquerySql
      <> ") AS \""
      <> latAlias
      <> "\" ON TRUE"
  CrossJoin left right ->
    renderFromItem left
      <> " CROSS JOIN "
      <> renderFromItem right
