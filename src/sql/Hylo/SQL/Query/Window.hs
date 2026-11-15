{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Hylo.SQL.Query.Window
Description : First-class window specification, partitioning, framing algebra, and analytical functions
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.SQL.Query.Window
  ( -- * Window Specification & Combinators
    WindowDef (..)
  , window
  , partitionBy
  , OrderBy (..)
  , PartitionKey (..)

    -- * Order Specifications
  , OrderSpec (..)
  , asc
  , desc
  , ascNullsFirst
  , ascNullsLast
  , descNullsFirst
  , descNullsLast

    -- * Frame Boundaries & Framing Algebra
  , FrameBound (..)
  , unboundedPreceding
  , preceding
  , currentRow
  , following
  , unboundedFollowing
  , rowsBetween
  , rangeBetween
  , groupsBetween

    -- * Window Execution & Target
  , WindowExpr (..)
  , WindowFn (..)
  , ToWindowTarget (..)
  , over

    -- * Analytical & Ranking Functions
  , rowNumber
  , rank
  , denseRank
  , cumeDist
  , percentRank
  , ntile
  , lag
  , lead
  , firstValue
  , lastValue
  , nthValue

    -- * Combinator Pipeline Re-export
  , (&)
  ) where

import Control.DeepSeq (NFData (..))
import Data.Function ((&))
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core.Expr (QExpr (..), ToQExpr (..))
import Hylo.Core.Query.AST (Projectable (..), renderExpr)
import Hylo.Core.Query.Monad (Query (..), QueryState (..))
import Hylo.SQL.Query.Aggregate (AggExpr (..))

-- | Order specification with direction and null placement semantics.
newtype OrderSpec = OrderSpec
  { unOrderSpec :: Text
  }
  deriving (Eq, Show)

instance NFData OrderSpec where
  rnf (OrderSpec t) = rnf t

-- | Ascending ordering specification.
asc :: ToQExpr e a => e -> OrderSpec
asc e = OrderSpec (renderExpr (toQExpr e) <> " ASC")

-- | Descending ordering specification.
desc :: ToQExpr e a => e -> OrderSpec
desc e = OrderSpec (renderExpr (toQExpr e) <> " DESC")

-- | Ascending ordering specification with NULLS FIRST.
ascNullsFirst :: ToQExpr e a => e -> OrderSpec
ascNullsFirst e = OrderSpec (renderExpr (toQExpr e) <> " ASC NULLS FIRST")

-- | Ascending ordering specification with NULLS LAST.
ascNullsLast :: ToQExpr e a => e -> OrderSpec
ascNullsLast e = OrderSpec (renderExpr (toQExpr e) <> " ASC NULLS LAST")

-- | Descending ordering specification with NULLS FIRST.
descNullsFirst :: ToQExpr e a => e -> OrderSpec
descNullsFirst e = OrderSpec (renderExpr (toQExpr e) <> " DESC NULLS FIRST")

-- | Descending ordering specification with NULLS LAST.
descNullsLast :: ToQExpr e a => e -> OrderSpec
descNullsLast e = OrderSpec (renderExpr (toQExpr e) <> " DESC NULLS LAST")

-- | Frame boundary specification for window framing clauses.
data FrameBound
  = UnboundedPreceding
  | Preceding !Int
  | CurrentRow
  | Following !Int
  | UnboundedFollowing
  deriving (Eq, Show)

instance NFData FrameBound where
  rnf = \case
    UnboundedPreceding -> ()
    Preceding n -> rnf n
    CurrentRow -> ()
    Following n -> rnf n
    UnboundedFollowing -> ()

-- | Render a frame bound into standard SQL text syntax.
renderFrameBound :: FrameBound -> Text
renderFrameBound = \case
  UnboundedPreceding -> "UNBOUNDED PRECEDING"
  Preceding n -> T.pack (show n) <> " PRECEDING"
  CurrentRow -> "CURRENT ROW"
  Following n -> T.pack (show n) <> " FOLLOWING"
  UnboundedFollowing -> "UNBOUNDED FOLLOWING"

-- | Frame bound representing UNBOUNDED PRECEDING.
unboundedPreceding :: FrameBound
unboundedPreceding = UnboundedPreceding

-- | Frame bound representing @n@ PRECEDING rows/range units.
preceding :: Int -> FrameBound
preceding = Preceding

-- | Frame bound representing CURRENT ROW.
currentRow :: FrameBound
currentRow = CurrentRow

-- | Frame bound representing @n@ FOLLOWING rows/range units.
following :: Int -> FrameBound
following = Following

-- | Frame bound representing UNBOUNDED FOLLOWING.
unboundedFollowing :: FrameBound
unboundedFollowing = UnboundedFollowing

-- | AST representation of a window definition with partitions, ordering, and framing.
data WindowDef = WindowDef
  { wdPartitions :: ![Text]
  , wdOrders :: ![Text]
  , wdFrame :: !(Maybe Text)
  }
  deriving (Eq, Show)

instance NFData WindowDef where
  rnf (WindowDef p o f) = rnf p `seq` rnf o `seq` rnf f

-- | Construct a blank, unpartitioned window definition.
window :: WindowDef
window = WindowDef [] [] Nothing

-- | Types that can serve as partition keys within a window specification.
class PartitionKey k where
  renderPartitionKey :: k -> [Text]

instance PartitionKey (QExpr a) where
  renderPartitionKey e = [renderExpr e]

instance (PartitionKey a, PartitionKey b) => PartitionKey (a, b) where
  renderPartitionKey (a, b) = renderPartitionKey a ++ renderPartitionKey b

instance (PartitionKey a, PartitionKey b, PartitionKey c) => PartitionKey (a, b, c) where
  renderPartitionKey (a, b, c) =
    renderPartitionKey a ++ renderPartitionKey b ++ renderPartitionKey c

-- | Add PARTITION BY expressions to a window specification.
partitionBy :: PartitionKey k => k -> WindowDef -> WindowDef
partitionBy k w = w {wdPartitions = wdPartitions w ++ renderPartitionKey k}

-- | Class overloading 'orderBy' across window specifications and monadic Query blocks.
class OrderBy target where
  orderBy :: [OrderSpec] -> target

instance OrderBy (WindowDef -> WindowDef) where
  orderBy specs w = w {wdOrders = wdOrders w ++ map unOrderSpec specs}

instance (a ~ ()) => OrderBy (Query db a) where
  orderBy specs = Query $ \s ->
    let s' = s {qsOrderBy = qsOrderBy s ++ map unOrderSpec specs}
     in ((), s')

-- | Add a ROWS BETWEEN framing specification to a window definition.
rowsBetween :: FrameBound -> FrameBound -> WindowDef -> WindowDef
rowsBetween lower upper w =
  w {wdFrame = Just ("ROWS BETWEEN " <> renderFrameBound lower <> " AND " <> renderFrameBound upper)}

-- | Add a RANGE BETWEEN framing specification to a window definition.
rangeBetween :: FrameBound -> FrameBound -> WindowDef -> WindowDef
rangeBetween lower upper w =
  w {wdFrame = Just ("RANGE BETWEEN " <> renderFrameBound lower <> " AND " <> renderFrameBound upper)}

-- | Add a GROUPS BETWEEN framing specification to a window definition.
groupsBetween :: FrameBound -> FrameBound -> WindowDef -> WindowDef
groupsBetween lower upper w =
  w
    { wdFrame = Just ("GROUPS BETWEEN " <> renderFrameBound lower <> " AND " <> renderFrameBound upper)
    }

-- | Render a window definition into SQL OVER (...) body text.
renderWindowDef :: WindowDef -> Text
renderWindowDef (WindowDef parts orders mFrame) =
  let partsClause = case parts of
        [] -> Nothing
        ps -> Just ("PARTITION BY " <> T.intercalate ", " ps)
      orderClause = case orders of
        [] -> Nothing
        os -> Just ("ORDER BY " <> T.intercalate ", " os)
      clauses = catMaybes [partsClause, orderClause, mFrame]
   in T.intercalate " " clauses

-- | Dedicated analytical window function call representation.
newtype WindowFn a = WindowFn
  { unWindowFn :: Text
  }
  deriving (Eq, Show)

instance NFData (WindowFn a) where
  rnf (WindowFn t) = rnf t

-- | Expression evaluated over a window specification in projection contexts.
newtype WindowExpr a = WindowExpr
  { unWindowExpr :: Text
  }
  deriving (Eq, Show)

instance NFData (WindowExpr a) where
  rnf (WindowExpr t) = rnf t

instance Projectable (WindowExpr a) where
  projectColumns (WindowExpr t) = [t]

-- | Class of expressions that can be projected over a window (aggregates and analytical functions).
class ToWindowTarget fn a | fn -> a where
  renderWindowTarget :: fn -> Text

instance ToWindowTarget (AggExpr a) a where
  renderWindowTarget (AggExpr e) = renderExpr e

instance ToWindowTarget (WindowFn a) a where
  renderWindowTarget (WindowFn t) = t

-- | Apply an aggregate or analytical function over a window specification.
over :: ToWindowTarget fn a => fn -> WindowDef -> WindowExpr a
over target win =
  let targetSql = renderWindowTarget target
      winSql = renderWindowDef win
      overClause =
        if T.null winSql
          then targetSql <> " OVER ()"
          else targetSql <> " OVER (" <> winSql <> ")"
   in WindowExpr overClause

-- | Standard ROW_NUMBER() analytical window function.
rowNumber :: WindowFn Int64
rowNumber = WindowFn "ROW_NUMBER()"

-- | Standard RANK() analytical window function.
rank :: WindowFn Int64
rank = WindowFn "RANK()"

-- | Standard DENSE_RANK() analytical window function.
denseRank :: WindowFn Int64
denseRank = WindowFn "DENSE_RANK()"

-- | Standard CUME_DIST() analytical window function.
cumeDist :: WindowFn Double
cumeDist = WindowFn "CUME_DIST()"

-- | Standard PERCENT_RANK() analytical window function.
percentRank :: WindowFn Double
percentRank = WindowFn "PERCENT_RANK()"

-- | Standard NTILE(n) analytical window function.
ntile :: Int -> WindowFn Int64
ntile n = WindowFn ("NTILE(" <> T.pack (show n) <> ")")

-- | Standard LAG(e, n) analytical window function.
lag :: ToQExpr e a => Int -> e -> WindowFn a
lag n e = WindowFn ("LAG(" <> renderExpr (toQExpr e) <> ", " <> T.pack (show n) <> ")")

-- | Standard LEAD(e, n) analytical window function.
lead :: ToQExpr e a => Int -> e -> WindowFn a
lead n e = WindowFn ("LEAD(" <> renderExpr (toQExpr e) <> ", " <> T.pack (show n) <> ")")

-- | Standard FIRST_VALUE(e) analytical window function.
firstValue :: ToQExpr e a => e -> WindowFn a
firstValue e = WindowFn ("FIRST_VALUE(" <> renderExpr (toQExpr e) <> ")")

-- | Standard LAST_VALUE(e) analytical window function.
lastValue :: ToQExpr e a => e -> WindowFn a
lastValue e = WindowFn ("LAST_VALUE(" <> renderExpr (toQExpr e) <> ")")

-- | Standard NTH_VALUE(e, n) analytical window function.
nthValue :: ToQExpr e a => e -> Int -> WindowFn a
nthValue e n = WindowFn ("NTH_VALUE(" <> renderExpr (toQExpr e) <> ", " <> T.pack (show n) <> ")")
