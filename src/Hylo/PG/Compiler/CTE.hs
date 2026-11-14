{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.PG.Compiler.CTE
Description : PostgreSQL Common Table Expression (CTE) and recursive graph SQL compiler
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.CTE
  ( -- * Backend Capabilities
    SupportsCTE (..)
  , SupportsRecursiveCTEs (..)
  , DynamoDB

    -- * SQL Rendering
  , renderCTEs
  , renderCTEBinding

    -- * Capability-Guarded Combinators
  , with
  , withMaterialized
  , withRecursive
  , withRecursiveCycle

    -- * Re-exports from CTE Core
  , module Hylo.Core.Query.CTE
  ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core.Query (Postgres, Query)
import Hylo.Core.Query.CTE hiding
  ( with
  , withMaterialized
  , withRecursive
  , withRecursiveCycle
  )
import qualified Hylo.Core.Query.CTE as CTE
import Hylo.Core.TypeError (AssertBackendCapability (..))

-- | Capability typeclass certifying backend support for ANSI Common Table Expressions.
class SupportsCTE db where
  cteDict :: Proxy db -> ()

instance SupportsCTE Postgres where
  cteDict _ = ()

-- | Capability typeclass certifying backend support for ANSI Recursive Common Table Expressions.
class SupportsRecursiveCTEs db where
  recursiveCTEsDict :: Proxy db -> ()

instance SupportsRecursiveCTEs Postgres where
  recursiveCTEsDict _ = ()

-- | Phantom type tag for DynamoDB used in static capability rejection tests.
data DynamoDB

instance (AssertBackendCapability "Recursive CTEs" "DynamoDB" 'False) => SupportsRecursiveCTEs DynamoDB where
  recursiveCTEsDict _ =
    backendCapabilityDict (Proxy @"Recursive CTEs") (Proxy @"DynamoDB") (Proxy @'False)

instance (AssertBackendCapability "CTEs" "DynamoDB" 'False) => SupportsCTE DynamoDB where
  cteDict _ =
    backendCapabilityDict (Proxy @"CTEs") (Proxy @"DynamoDB") (Proxy @'False)

-- | Render a list of CTE bindings into SQL WITH / WITH RECURSIVE prefix clauses.
renderCTEs :: [CTEBinding] -> Text
renderCTEs = \case
  [] -> ""
  ctes ->
    let isRecursive = any isRec ctes
        prefix = if isRecursive then "WITH RECURSIVE " else "WITH "
        renderedBindings = T.intercalate ",\n" (map renderCTEBinding ctes)
     in prefix <> renderedBindings <> " "
  where
    isRec (CTEBinding _ (RecursiveCTE {})) = True
    isRec _ = False

-- | Render an individual CTE definition.
renderCTEBinding :: CTEBinding -> Text
renderCTEBinding (CTEBinding _ (NonRecursiveCTE mat name sql)) =
  let matStr = case mat of
        DefaultMaterialization -> ""
        Materialized -> "MATERIALIZED "
        NotMaterialized -> "NOT MATERIALIZED "
   in "\"" <> name <> "\" AS " <> matStr <> "(\n  " <> sql <> "\n)"
renderCTEBinding (CTEBinding _ (RecursiveCTE name baseSql recSql mCycle)) =
  let cycleStr = case mCycle of
        Nothing -> ""
        Just (CycleClause c s u) ->
          " CYCLE \"" <> c <> "\" SET \"" <> s <> "\" USING \"" <> u <> "\""
   in "\""
        <> name
        <> "\" AS (\n  "
        <> baseSql
        <> "\n  UNION ALL\n  "
        <> recSql
        <> "\n)"
        <> cycleStr

-- | Capability-guarded CTE combinator.
with ::
  forall db ret rec final.
  (SupportsCTE db, CTERow ret rec) =>
  Text ->
  Query db ret ->
  (CTERef rec -> Query db final) ->
  Query db final
with name q f = cteDict (Proxy @db) `seq` CTE.with name q f

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
  (SupportsRecursiveCTEs db, CTERow ret rec) =>
  Text ->
  (CTERef rec -> Query db ret) ->
  (CTERef rec -> Query db ret) ->
  (CTERef rec -> Query db final) ->
  Query db final
withRecursive name baseCase recCase mainQuery =
  recursiveCTEsDict (Proxy @db) `seq`
    CTE.withRecursive name baseCase recCase mainQuery

-- | Capability-guarded recursive CTE combinator with cycle detection clause.
withRecursiveCycle ::
  forall db ret rec final.
  (SupportsRecursiveCTEs db, CTERow ret rec) =>
  Text ->
  Maybe CycleClause ->
  (CTERef rec -> Query db ret) ->
  (CTERef rec -> Query db ret) ->
  (CTERef rec -> Query db final) ->
  Query db final
withRecursiveCycle name mCycle baseCase recCase mainQuery =
  recursiveCTEsDict (Proxy @db) `seq`
    CTE.withRecursiveCycle name mCycle baseCase recCase mainQuery
