{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Hylo.SQL.Query.Recursive
Description : Capability-guarded recursive CTE graph traversals, least fixed-point evaluation, and cycle prevention
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.SQL.Query.Recursive
  ( -- * Capability-Guarded CTE Combinators
    with
  , withMaterialized
  , withRecursive
  , withRecursiveCycle

    -- * Re-exports from CTE core
  , module Hylo.SQL.Query.CTE
  ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Hylo.Backend.Capability
  ( SupportsCTE (..)
  , SupportsRecursiveCTE (..)
  )
import Hylo.Core.Query.Monad (Query (..))
import Hylo.SQL.Query.CTE hiding
  ( with
  , withMaterialized
  , withRecursive
  , withRecursiveCycle
  )
import qualified Hylo.SQL.Query.CTE as CTE

-- | Capability-guarded CTE combinator.
with ::
  forall db ret rec final.
  (SupportsCTE db, CTERow ret rec) =>
  Text ->
  Query db ret ->
  (CTERef rec -> Query db final) ->
  Query db final
with name q f =
  cteDict (Proxy @db) `seq` CTE.with name q f

-- | Capability-guarded CTE combinator with materialization fence.
withMaterialized ::
  forall db ret rec final.
  (SupportsCTE db, CTERow ret rec) =>
  Materialization ->
  Text ->
  Query db ret ->
  (CTERef rec -> Query db final) ->
  Query db final
withMaterialized mat name q f =
  cteDict (Proxy @db) `seq` CTE.withMaterialized mat name q f

-- | Capability-guarded recursive CTE combinator.
withRecursive ::
  forall db ret rec final.
  (SupportsRecursiveCTE db, CTERow ret rec) =>
  Text ->
  (CTERef rec -> Query db ret) ->
  (CTERef rec -> Query db ret) ->
  (CTERef rec -> Query db final) ->
  Query db final
withRecursive name baseCase recCase mainQuery =
  recursiveCTEDict (Proxy @db) `seq`
    CTE.withRecursive name baseCase recCase mainQuery

-- | Capability-guarded recursive CTE combinator with cycle detection clause.
withRecursiveCycle ::
  forall db ret rec final.
  (SupportsRecursiveCTE db, CTERow ret rec) =>
  Text ->
  Maybe CycleClause ->
  (CTERef rec -> Query db ret) ->
  (CTERef rec -> Query db ret) ->
  (CTERef rec -> Query db final) ->
  Query db final
withRecursiveCycle name mCycle baseCase recCase mainQuery =
  recursiveCTEDict (Proxy @db) `seq`
    CTE.withRecursiveCycle name mCycle baseCase recCase mainQuery
