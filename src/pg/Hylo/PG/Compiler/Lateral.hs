{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.Compiler.Lateral
Description : Correlated Subquery Lowering (LATERAL N+1 Eradication) via jsonb_agg
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.Lateral
  ( SupportsLateralSubqueries (..)
  , NestedList (..)
  , many
  ) where

import Control.DeepSeq (NFData (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Backend.Capability (SupportsLateralSubqueries (..))
import Hylo.Core.Expr (QExpr (..))
import Hylo.Core.Query.AST
  ( FromItem (..)
  , LateralBinding (..)
  , Projectable (..)
  , RelationBinding (..)
  , renderExpr
  )
import Hylo.Core.Query.Monad
  ( Query (..)
  , QueryState (..)
  , initialQueryState
  )
import Hylo.PG.Compiler.JoinFlattener
  ( flattenJoins
  , renderFromItem
  )

-- | Relational container representing a 1-to-many correlated nested graph slice.
data NestedList a = NestedList
  { nestedListAlias :: !Text
  , nestedListValue :: !a
  }
  deriving (Eq, Show, Functor)

instance NFData a => NFData (NestedList a) where
  rnf (NestedList a v) = rnf a `seq` rnf v

instance Projectable (NestedList a) where
  projectColumns nl = ["\"" <> nestedListAlias nl <> "\".*"]

{- | Lowers an inner query computation into an ANSI SQL-92 @LEFT JOIN LATERAL@
subquery aggregating results via PostgreSQL's @COALESCE(jsonb_agg(to_jsonb(...)), '[]'::jsonb)@.
-}
many ::
  forall db a.
  SupportsLateralSubqueries db =>
  Query db a ->
  Query db (NestedList a)
many childQuery = lateralSubqueriesDict (Proxy :: Proxy db) `seq` Query $ \s ->
  let childInit = initialQueryState {qsNextScope = qsNextScope s, qsNextLateral = qsNextLateral s}
      (childRes, childSt) = runQuery childQuery childInit
      targetAlias = case qsRelations childSt of
        [] -> "sub_0"
        (r : _) -> rbAlias r
      aggExpr = "COALESCE(jsonb_agg(to_jsonb(\"" <> targetAlias <> "\".*)), '[]'::jsonb)"
      (mChildFrom, childWherePreds) = flattenJoins (qsRelations childSt) (qsPredicates childSt)
      mChildFromWithLaterals = case mChildFrom of
        Nothing -> case qsLaterals childSt of
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
              (qsLaterals childSt)
      childFromSql = case mChildFromWithLaterals of
        Nothing -> ""
        Just fItem -> " FROM " <> renderFromItem fItem
      childWhereSql = case childWherePreds of
        [] -> ""
        (p : ps) ->
          let combined = foldl (BinOpExpr "AND") p ps
           in " WHERE " <> renderExpr combined
      subquerySql = "SELECT " <> aggExpr <> childFromSql <> childWhereSql
      latAlias = "lat_" <> T.pack (show (qsNextLateral childSt))
      latBinding = LateralBinding latAlias subquerySql
      s' =
        s
          { qsNextScope = qsNextScope childSt
          , qsNextLateral = qsNextLateral childSt + 1
          , qsLaterals = qsLaterals s ++ [latBinding]
          }
   in (NestedList latAlias childRes, s')
