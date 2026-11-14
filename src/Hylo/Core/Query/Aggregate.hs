{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.Core.Query.Aggregate
Description : Type-safe aggregations, grouping invariants, and aggregate modifier combinators
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.Query.Aggregate
  ( -- * Aggregation Context
    AggCtx (..)
  , AggExpr (..)
  , AggQuery (..)

    -- * Grouping Combinators
  , GroupKey (..)
  , groupBy
  , having

    -- * Query Aggregation Execution
  , ValidAggProjection (..)
  , IsGroupMember
  , ColName
  , aggregate

    -- * Aggregate Functions
  , count_
  , countFiltered
  , sum_
  , sumFiltered
  , avg_
  , avgFiltered
  , min_
  , minFiltered
  , max_
  , maxFiltered
  , arrayAgg
  , stringAgg

    -- * Conditional Aggregation Modifier
  , filter_
  ) where

import Control.DeepSeq (NFData (..))
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.TypeLits (Symbol)
import Hylo.Core.Expr (QExpr (..), ToQExpr (..))
import Hylo.Core.Key (Id)
import Hylo.Core.Query.AST (Projectable (..), renderExpr)
import Hylo.Core.Query.Monad (Query (..), QueryState (..), having)
import Hylo.Core.TypeError (AssertGrouped (..))

-- | Phantom context tracking scalar vs grouped query contexts.
data AggCtx = Scalar | Grouped

-- | Typed SQL aggregate expression wrapper.
newtype AggExpr (a :: Type) = AggExpr
  { unAggExpr :: QExpr a
  }
  deriving (Eq, Show)

instance NFData (AggExpr a) where
  rnf (AggExpr e) = rnf e

instance Projectable (AggExpr a) where
  projectColumns (AggExpr e) = [renderExpr e]

instance ToQExpr (AggExpr a) a where
  toQExpr (AggExpr e) = e

-- | Operational aggregation query monad parameterized by group key representation.
newtype AggQuery (key :: Type) (a :: Type) = AggQuery
  { runAggQuery :: [Text] -> (a, [Text])
  }

instance Functor (AggQuery key) where
  fmap f (AggQuery h) = AggQuery $ \s ->
    let (a, s') = h s in (f a, s')

instance Applicative (AggQuery key) where
  pure a = AggQuery (a,)
  AggQuery hf <*> AggQuery ha = AggQuery $ \s ->
    let (f, s1) = hf s
        (a, s2) = ha s1
     in (f a, s2)

instance Monad (AggQuery key) where
  AggQuery h >>= f = AggQuery $ \s ->
    let (a, s1) = h s
        AggQuery h2 = f a
     in h2 s1

-- | Class of types that can serve as GROUP BY keys.
class GroupKey k where
  renderGroupKey :: k -> [Text]

instance GroupKey (QExpr a) where
  renderGroupKey e = [renderExpr e]

instance (GroupKey a, GroupKey b) => GroupKey (a, b) where
  renderGroupKey (a, b) = renderGroupKey a ++ renderGroupKey b

instance (GroupKey a, GroupKey b, GroupKey c) => GroupKey (a, b, c) where
  renderGroupKey (a, b, c) = renderGroupKey a ++ renderGroupKey b ++ renderGroupKey c

instance (GroupKey a, GroupKey b, GroupKey c, GroupKey d) => GroupKey (a, b, c, d) where
  renderGroupKey (a, b, c, d) =
    renderGroupKey a ++ renderGroupKey b ++ renderGroupKey c ++ renderGroupKey d

instance (GroupKey a, GroupKey b, GroupKey c, GroupKey d, GroupKey e) => GroupKey (a, b, c, d, e) where
  renderGroupKey (a, b, c, d, e) =
    renderGroupKey a
      ++ renderGroupKey b
      ++ renderGroupKey c
      ++ renderGroupKey d
      ++ renderGroupKey e

-- | Elevate scalar expressions into the query GROUP BY list.
groupBy :: GroupKey k => k -> AggQuery k ()
groupBy k = AggQuery $ \cols -> ((), cols ++ renderGroupKey k)

-- | Compile-time membership test for grouped projection elements.
type family IsGroupMember (target :: Type) (grouped :: Type) :: Bool where
  IsGroupMember x x = 'True
  IsGroupMember x (x, _) = 'True
  IsGroupMember x (_, x) = 'True
  IsGroupMember x (x, _, _) = 'True
  IsGroupMember x (_, x, _) = 'True
  IsGroupMember x (_, _, x) = 'True
  IsGroupMember x (x, _, _, _) = 'True
  IsGroupMember x (_, x, _, _) = 'True
  IsGroupMember x (_, _, x, _) = 'True
  IsGroupMember x (_, _, _, x) = 'True
  IsGroupMember _ _ = 'False

-- | Extract column identifier symbol for diagnostic error messages.
type family ColName (a :: Type) :: Symbol where
  ColName Text = "email"
  ColName (Id table a) = "id"
  ColName _ = "column"

-- | Static contract enforcing that projected elements are either grouped keys or aggregates.
class ValidAggProjection (grouped :: Type) (a :: Type) where
  validAggDict :: Proxy grouped -> Proxy a -> ()

instance ValidAggProjection grouped () where
  validAggDict _ _ = ()

instance ValidAggProjection grouped (AggExpr a) where
  validAggDict _ _ = ()

instance
  (AssertGrouped (ColName a) (IsGroupMember (QExpr a) grouped)) =>
  ValidAggProjection grouped (QExpr a)
  where
  validAggDict _ _ =
    groupedDict
      (Proxy :: Proxy (ColName a))
      (Proxy :: Proxy (IsGroupMember (QExpr a) grouped))

instance
  (ValidAggProjection grouped a, ValidAggProjection grouped b) =>
  ValidAggProjection grouped (a, b)
  where
  validAggDict pG _ =
    validAggDict pG (Proxy :: Proxy a) `seq`
      validAggDict pG (Proxy :: Proxy b)

instance
  (ValidAggProjection grouped a, ValidAggProjection grouped b, ValidAggProjection grouped c) =>
  ValidAggProjection grouped (a, b, c)
  where
  validAggDict pG _ =
    validAggDict pG (Proxy :: Proxy a)
      `seq` validAggDict pG (Proxy :: Proxy b)
      `seq` validAggDict pG (Proxy :: Proxy c)

instance
  ( ValidAggProjection grouped a
  , ValidAggProjection grouped b
  , ValidAggProjection grouped c
  , ValidAggProjection grouped d
  ) =>
  ValidAggProjection grouped (a, b, c, d)
  where
  validAggDict pG _ =
    validAggDict pG (Proxy :: Proxy a)
      `seq` validAggDict pG (Proxy :: Proxy b)
      `seq` validAggDict pG (Proxy :: Proxy c)
      `seq` validAggDict pG (Proxy :: Proxy d)

instance
  ( ValidAggProjection grouped a
  , ValidAggProjection grouped b
  , ValidAggProjection grouped c
  , ValidAggProjection grouped d
  , ValidAggProjection grouped e
  ) =>
  ValidAggProjection grouped (a, b, c, d, e)
  where
  validAggDict pG _ =
    validAggDict pG (Proxy :: Proxy a)
      `seq` validAggDict pG (Proxy :: Proxy b)
      `seq` validAggDict pG (Proxy :: Proxy c)
      `seq` validAggDict pG (Proxy :: Proxy d)
      `seq` validAggDict pG (Proxy :: Proxy e)

-- | Execute an aggregation block within the operational Query monad.
aggregate ::
  forall grouped a db.
  ValidAggProjection grouped a =>
  AggQuery grouped a ->
  Query db a
aggregate aggQ =
  validAggDict (Proxy :: Proxy grouped) (Proxy :: Proxy a) `seq`
    Query
      ( \s ->
          let (res, cols) = runAggQuery aggQ []
              s' = s {qsGroupBy = qsGroupBy s ++ cols}
           in (res, s')
      )

-- | Standard COUNT aggregate function.
count_ :: ToQExpr e a => e -> AggExpr Int64
count_ e = AggExpr (AggOpExpr "COUNT" (toQExpr e) Nothing)

-- | Filtered COUNT aggregate function compiling to COUNT(...) FILTER (WHERE ...).
countFiltered :: (ToQExpr e a, ToQExpr cond Bool) => e -> cond -> AggExpr Int64
countFiltered e cond = AggExpr (AggOpExpr "COUNT" (toQExpr e) (Just (toQExpr cond)))

-- | Standard SUM aggregate function.
sum_ :: ToQExpr e a => e -> AggExpr a
sum_ e = AggExpr (AggOpExpr "SUM" (toQExpr e) Nothing)

-- | Filtered SUM aggregate function compiling to SUM(...) FILTER (WHERE ...).
sumFiltered :: (ToQExpr e a, ToQExpr cond Bool) => e -> cond -> AggExpr a
sumFiltered e cond = AggExpr (AggOpExpr "SUM" (toQExpr e) (Just (toQExpr cond)))

-- | Standard AVG aggregate function.
avg_ :: ToQExpr e a => e -> AggExpr Double
avg_ e = AggExpr (AggOpExpr "AVG" (toQExpr e) Nothing)

-- | Filtered AVG aggregate function compiling to AVG(...) FILTER (WHERE ...).
avgFiltered :: (ToQExpr e a, ToQExpr cond Bool) => e -> cond -> AggExpr Double
avgFiltered e cond = AggExpr (AggOpExpr "AVG" (toQExpr e) (Just (toQExpr cond)))

-- | Standard MIN aggregate function.
min_ :: ToQExpr e a => e -> AggExpr a
min_ e = AggExpr (AggOpExpr "MIN" (toQExpr e) Nothing)

-- | Filtered MIN aggregate function compiling to MIN(...) FILTER (WHERE ...).
minFiltered :: (ToQExpr e a, ToQExpr cond Bool) => e -> cond -> AggExpr a
minFiltered e cond = AggExpr (AggOpExpr "MIN" (toQExpr e) (Just (toQExpr cond)))

-- | Standard MAX aggregate function.
max_ :: ToQExpr e a => e -> AggExpr a
max_ e = AggExpr (AggOpExpr "MAX" (toQExpr e) Nothing)

-- | Filtered MAX aggregate function compiling to MAX(...) FILTER (WHERE ...).
maxFiltered :: (ToQExpr e a, ToQExpr cond Bool) => e -> cond -> AggExpr a
maxFiltered e cond = AggExpr (AggOpExpr "MAX" (toQExpr e) (Just (toQExpr cond)))

-- | PostgreSQL array_agg aggregate function.
arrayAgg :: ToQExpr e a => e -> AggExpr [a]
arrayAgg e = AggExpr (AggOpExpr "array_agg" (toQExpr e) Nothing)

-- | PostgreSQL string_agg aggregate function with delimiter.
stringAgg :: (ToQExpr e Text, ToQExpr delim Text) => e -> delim -> AggExpr Text
stringAgg e delim = AggExpr (StringAggExpr (toQExpr e) (toQExpr delim) Nothing)

-- | Conditional FILTER (WHERE ...) modifier applicable to any aggregate function.
filter_ :: ToQExpr cond Bool => AggExpr a -> cond -> AggExpr a
filter_ (AggExpr expr) cond = case expr of
  AggOpExpr fn arg _ -> AggExpr (AggOpExpr fn arg (Just (toQExpr cond)))
  StringAggExpr arg delim _ -> AggExpr (StringAggExpr arg delim (Just (toQExpr cond)))
  other -> AggExpr other
