{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Hylo.Core.Query.Monad
Description : Operational Query monad, monotonic relation binder, and filter constraints
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.Query.Monad
  ( QueryState (..)
  , initialQueryState
  , Query (..)
  , FromRelation (..)
  , eachWith
  , where_
  , having
  , limit
  , offset
  , leftJoin
  , OrderBy (..)
  , OrderSpec (..)
  , OrderDirection (..)
  , NullsOrder (..)
  , asc
  , desc
  , nullsFirst
  , nullsLast
  , renderOrderSpec
  , unOrderSpec
  ) where

import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Hylo.Core.Context (Ctx (..))
import Hylo.Core.Expr (QExpr, ToQExpr (..))
import Hylo.Core.Nullable (NullableTable (..))
import Hylo.Core.Table (Table (..), TableSchema (..))
import Hylo.SQL.AST
  ( QueryState (..)
  , RelationBinding (..)
  , ScopeId (..)
  , formatScope
  , initialQueryState
  , mkRelationBinding
  , renderExpr
  )

-- | Pure operational Query monad parameterised by database backend marker.
newtype Query db a = Query
  { runQuery :: QueryState -> (a, QueryState)
  }

instance NFData a => NFData (Query db a) where
  rnf q = rnf (runQuery q initialQueryState)

instance Functor (Query db) where
  fmap f (Query h) = Query $ \s ->
    let (a, s') = h s
     in (f a, s')

instance Applicative (Query db) where
  pure a = Query (a,)
  Query hf <*> Query ha = Query $ \s ->
    let (f, s1) = hf s
        (a, s2) = ha s1
     in (f a, s2)

instance Monad (Query db) where
  Query h >>= f = Query $ \s ->
    let (a, s1) = h s
        Query h2 = f a
     in h2 s1

-- | Class for relational sources (tables, CTEs) that can be bound into the Query monad.
class FromRelation rel record | rel -> record where
  each :: rel -> Query db record

instance Table table => FromRelation (TableSchema table) (table 'Expr) where
  each = eachWith toTableExpr

-- | Bind a database table using an explicit expression synthesis function.
eachWith :: (Text -> table 'Meta -> table 'Expr) -> TableSchema table -> Query db (table 'Expr)
eachWith mkExpr schema = Query $ \s ->
  let scope = ScopeId (qsNextScope s)
      tName = tableName schema
      alias = formatScope tName scope
      exprRecord = mkExpr alias (tableMeta schema)
      binding = mkRelationBinding tName scope alias
      s' =
        s
          { qsNextScope = qsNextScope s + 1
          , qsRelations = qsRelations s ++ [binding]
          }
   in (exprRecord, s')

-- | Bind a database table via LEFT JOIN with a typed ON condition.
leftJoin ::
  Table table =>
  TableSchema table ->
  (NullableTable (table 'Expr) -> QExpr Bool) ->
  Query db (NullableTable (table 'Expr))
leftJoin schema onCondFn = Query $ \s ->
  let scope = ScopeId (qsNextScope s)
      tName = tableName schema
      alias = formatScope tName scope
      exprRecord = toTableExpr alias (tableMeta schema)
      nullableRec = NullableTable exprRecord
      onCond = onCondFn nullableRec
      binding = RelationBinding tName scope alias (Just onCond)
      s' =
        s
          { qsNextScope = qsNextScope s + 1
          , qsRelations = qsRelations s ++ [binding]
          }
   in (nullableRec, s')

-- | Append a filter constraint to the active Query context.
where_ :: QExpr Bool -> Query db ()
where_ cond = Query $ \s ->
  let s' = s {qsPredicates = qsPredicates s ++ [cond]}
   in ((), s')

-- | Append a HAVING constraint to the active Query context.
having :: QExpr Bool -> Query db ()
having cond = Query $ \s ->
  let s' = s {qsHaving = qsHaving s ++ [cond]}
   in ((), s')

-- | Set a maximum row limit for the query.
limit :: Int -> Query db ()
limit n = Query $ \s ->
  let s' = s {qsLimit = Just n}
   in ((), s')

-- | Set a row offset for the query.
offset :: Int -> Query db ()
offset n = Query $ \s ->
  let s' = s {qsOffset = Just n}
   in ((), s')

-- | Sorting direction for ORDER BY clauses.
data OrderDirection = Asc | Desc
  deriving (Eq, Show, Generic)

instance NFData OrderDirection

-- | NULL placement semantics for ORDER BY clauses.
data NullsOrder = NullsDefault | NullsFirst | NullsLast
  deriving (Eq, Show, Generic)

instance NFData NullsOrder

-- | Order specification with direction and null placement semantics.
data OrderSpec = OrderSpec
  { orderExpr :: !Text
  , orderDirection :: !OrderDirection
  , orderNulls :: !NullsOrder
  }
  deriving (Eq, Show, Generic)

instance NFData OrderSpec where
  rnf (OrderSpec e d n) = rnf e `seq` rnf d `seq` rnf n

-- | Render an OrderSpec to standard SQL text.
renderOrderSpec :: OrderSpec -> Text
renderOrderSpec (OrderSpec expr dir nulls) =
  let dirText = case dir of
        Asc -> "ASC"
        Desc -> "DESC"
      nullsText = case nulls of
        NullsDefault -> ""
        NullsFirst -> " NULLS FIRST"
        NullsLast -> " NULLS LAST"
   in expr <> " " <> dirText <> nullsText

-- | Backward-compatible alias for rendering an order specification to text.
unOrderSpec :: OrderSpec -> Text
unOrderSpec = renderOrderSpec

-- | Ascending ordering specification.
asc :: ToQExpr e a => e -> OrderSpec
asc e = OrderSpec (renderExpr (toQExpr e)) Asc NullsDefault

-- | Descending ordering specification.
desc :: ToQExpr e a => e -> OrderSpec
desc e = OrderSpec (renderExpr (toQExpr e)) Desc NullsDefault

-- | Specify NULLS FIRST placement for an ordering specification.
nullsFirst :: OrderSpec -> OrderSpec
nullsFirst spec = spec {orderNulls = NullsFirst}

-- | Specify NULLS LAST placement for an ordering specification.
nullsLast :: OrderSpec -> OrderSpec
nullsLast spec = spec {orderNulls = NullsLast}

-- | Class overloading 'orderBy' across window specifications and monadic Query blocks.
class OrderBy target where
  orderBy :: [OrderSpec] -> target

instance (a ~ ()) => OrderBy (Query db a) where
  orderBy specs = Query $ \s ->
    let s' = s {qsOrderBy = qsOrderBy s ++ map renderOrderSpec specs}
     in ((), s')
