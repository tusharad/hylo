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
import Hylo.Core.Query.AST (Projectable (..), renderExpr)
import Hylo.Core.Query.Monad
  ( Query (..)
  , QueryState (..)
  , initialQueryState
  )
import Hylo.PG.Compiler.JoinFlattener
  ( flattenJoins
  , renderFromItem
  )

-- | Render a Query into polymorphic string/text SQL representation.
renderSQL :: (Projectable a, IsString s) => Query db a -> s
renderSQL q = fromString (T.unpack (renderSQLText q))

-- | Render a Query into strict Text SQL representation.
renderSQLText :: Projectable a => Query db a -> Text
renderSQLText q =
  let (res, st) = runQuery q initialQueryState
      projections = projectColumns res
      selectClause =
        "SELECT "
          <> (if null projections then "1" else T.intercalate ", " projections)
      (mFrom, wherePreds) = flattenJoins (qsRelations st) (qsPredicates st)
      fromClause = case mFrom of
        Nothing -> ""
        Just item -> " FROM " <> renderFromItem item
      whereClause = case wherePreds of
        [] -> ""
        (p : ps) ->
          let combined = foldl (BinOpExpr "AND") p ps
           in " WHERE " <> renderExpr combined
   in selectClause <> fromClause <> whereClause
