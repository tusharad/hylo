{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

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
  ) where

import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import Hylo.Core.Context (Ctx (..))
import Hylo.Core.Expr (QExpr)
import Hylo.Core.Query.AST
  ( CTEBinding (..)
  , LateralBinding (..)
  , RelationBinding (..)
  , ScopeId (..)
  , formatScope
  )
import Hylo.Core.Table (Table (..), TableSchema (..))

-- | Internal state tracked during Query monadic composition.
data QueryState = QueryState
  { qsNextScope :: !Int
  , qsNextLateral :: !Int
  , qsRelations :: ![RelationBinding]
  , qsPredicates :: ![QExpr Bool]
  , qsLaterals :: ![LateralBinding]
  , qsGroupBy :: ![Text]
  , qsHaving :: ![QExpr Bool]
  , qsOrderBy :: ![Text]
  , qsCTEs :: ![CTEBinding]
  }
  deriving (Eq, Show)

instance NFData QueryState where
  rnf (QueryState n l r p lat gb hv ob ctes) =
    rnf n
      `seq` rnf l
      `seq` rnf r
      `seq` rnf p
      `seq` rnf lat
      `seq` rnf gb
      `seq` rnf hv
      `seq` rnf ob
      `seq` rnf ctes

-- | Default initial state for a new Query computation.
initialQueryState :: QueryState
initialQueryState = QueryState 0 0 [] [] [] [] [] [] []

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
      binding = RelationBinding tName scope alias
      s' =
        s
          { qsNextScope = qsNextScope s + 1
          , qsRelations = qsRelations s ++ [binding]
          }
   in (exprRecord, s')

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
