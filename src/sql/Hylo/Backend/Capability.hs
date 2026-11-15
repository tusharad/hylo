{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.Backend.Capability
Description : Polyglot multi-backend capability matrix and static feature gating
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Fine-grained single-parameter typeclasses gating advanced database capabilities
(relational joins, lateral subqueries, window functions, transactions, vector search,
batch lookup, and recursive CTEs) across PostgreSQL, SQLite, MongoDB, and DynamoDB.
Unsupported feature usage fails statically at compile time with '[Hylo Error E008]'.
-}
module Hylo.Backend.Capability
  ( -- * Database Backend Markers
    Postgres
  , SQLite
  , MongoDB
  , DynamoDB

    -- * Capability Typeclasses
  , SupportsRelationalJoins (..)
  , SupportsLateralSubqueries (..)
  , SupportsWindowFunctions (..)
  , SupportsTransactions (..)
  , SupportsVectorSearch (..)
  , SupportsBatchLookup (..)
  , SupportsRecursiveCTE (..)
  , SupportsCTE (..)
  , SupportsRecursiveCTEs
  , recursiveCTEsDict

    -- * Custom Diagnostic Codes
  , AssertBackendCapability (..)
  , CheckBackendCapability

    -- * Cross-Backend Dialect Renderers
  , renderPostgresSQL
  , renderSQLiteSQL

    -- * DataLoader Batching Abstraction
  , DataLoaderBatch (..)
  , runBatchLoader
  , viaBatchLoader

    -- * Capability-Guarded CTE Combinators
  , with
  , withMaterialized
  , withRecursive
  , withRecursiveCycle
  ) where

import Control.DeepSeq (NFData (..))
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Core.Expr (QExpr (..))
import Hylo.Core.Query.AST
  ( CycleClause (..)
  , DynamoDB
  , FromItem (..)
  , LateralBinding (..)
  , Materialization (..)
  , MongoDB
  , Postgres
  , Projectable (..)
  , SQLite
  , renderExpr
  )
import Hylo.Core.Query.CTE (CTERef (..), CTERow (..), renderCTEs)
import qualified Hylo.Core.Query.CTE as CTE
import Hylo.Core.Query.Monad (Query (..), QueryState (..), initialQueryState)
import Hylo.Core.TypeError (AssertBackendCapability (..), CheckBackendCapability)
import Hylo.SQL.Compiler.JoinFlattener (flattenJoins, renderFromItem)

--------------------------------------------------------------------------------
-- 1. Capability Typeclasses
--------------------------------------------------------------------------------

-- | Capability typeclass certifying backend support for ANSI relational JOIN operations.
class SupportsRelationalJoins db where
  relationalJoinsDict :: Proxy db -> ()

-- | Capability typeclass certifying backend support for correlated LATERAL subqueries.
class SupportsLateralSubqueries db where
  lateralSubqueriesDict :: Proxy db -> ()

-- | Capability typeclass certifying backend support for ANSI window functions and framing.
class SupportsWindowFunctions db where
  windowFunctionsDict :: Proxy db -> ()

-- | Capability typeclass certifying backend support for ACID transaction boundaries.
class SupportsTransactions db where
  transactionsDict :: Proxy db -> ()

-- | Capability typeclass certifying backend support for vector similarity search.
class SupportsVectorSearch db where
  vectorSearchDict :: Proxy db -> ()

-- | Capability typeclass certifying backend support for bulk batch lookup (BatchGetItem / $in).
class SupportsBatchLookup db where
  batchLookupDict :: Proxy db -> ()

-- | Capability typeclass certifying backend support for ANSI Common Table Expressions.
class SupportsCTE db where
  cteDict :: Proxy db -> ()

-- | Capability typeclass certifying backend support for recursive Common Table Expressions.
class SupportsRecursiveCTE db where
  recursiveCTEDict :: Proxy db -> ()

-- | Backwards-compatibility type synonym for pluralized capability name.
type SupportsRecursiveCTEs = SupportsRecursiveCTE

-- | Backwards-compatibility dictionary method for pluralized capability name.
recursiveCTEsDict :: forall db. SupportsRecursiveCTE db => Proxy db -> ()
recursiveCTEsDict = recursiveCTEDict

--------------------------------------------------------------------------------
-- 2. Capability Instances: PostgreSQL
--------------------------------------------------------------------------------

instance SupportsRelationalJoins Postgres where
  relationalJoinsDict _ = ()

instance SupportsLateralSubqueries Postgres where
  lateralSubqueriesDict _ = ()

instance SupportsWindowFunctions Postgres where
  windowFunctionsDict _ = ()

instance SupportsTransactions Postgres where
  transactionsDict _ = ()

instance SupportsVectorSearch Postgres where
  vectorSearchDict _ = ()

instance SupportsBatchLookup Postgres where
  batchLookupDict _ = ()

instance SupportsCTE Postgres where
  cteDict _ = ()

instance SupportsRecursiveCTE Postgres where
  recursiveCTEDict _ = ()

--------------------------------------------------------------------------------
-- 3. Capability Instances: SQLite
--------------------------------------------------------------------------------

instance SupportsRelationalJoins SQLite where
  relationalJoinsDict _ = ()

instance SupportsLateralSubqueries SQLite where
  lateralSubqueriesDict _ = ()

instance SupportsWindowFunctions SQLite where
  windowFunctionsDict _ = ()

instance SupportsTransactions SQLite where
  transactionsDict _ = ()

instance (AssertBackendCapability "Vector Search" "SQLite" 'False) => SupportsVectorSearch SQLite where
  vectorSearchDict _ =
    backendCapabilityDict (Proxy @"Vector Search") (Proxy @"SQLite") (Proxy @'False)

instance SupportsBatchLookup SQLite where
  batchLookupDict _ = ()

instance SupportsCTE SQLite where
  cteDict _ = ()

instance SupportsRecursiveCTE SQLite where
  recursiveCTEDict _ = ()

--------------------------------------------------------------------------------
-- 4. Capability Instances: MongoDB
--------------------------------------------------------------------------------

instance (AssertBackendCapability "Relational Joins" "MongoDB" 'False) => SupportsRelationalJoins MongoDB where
  relationalJoinsDict _ =
    backendCapabilityDict (Proxy @"Relational Joins") (Proxy @"MongoDB") (Proxy @'False)

instance (AssertBackendCapability "Lateral Subqueries" "MongoDB" 'False) => SupportsLateralSubqueries MongoDB where
  lateralSubqueriesDict _ =
    backendCapabilityDict (Proxy @"Lateral Subqueries") (Proxy @"MongoDB") (Proxy @'False)

instance (AssertBackendCapability "Window Functions" "MongoDB" 'False) => SupportsWindowFunctions MongoDB where
  windowFunctionsDict _ =
    backendCapabilityDict (Proxy @"Window Functions") (Proxy @"MongoDB") (Proxy @'False)

instance SupportsTransactions MongoDB where
  transactionsDict _ = ()

instance SupportsVectorSearch MongoDB where
  vectorSearchDict _ = ()

instance SupportsBatchLookup MongoDB where
  batchLookupDict _ = ()

instance (AssertBackendCapability "CTEs" "MongoDB" 'False) => SupportsCTE MongoDB where
  cteDict _ =
    backendCapabilityDict (Proxy @"CTEs") (Proxy @"MongoDB") (Proxy @'False)

instance (AssertBackendCapability "Recursive CTEs" "MongoDB" 'False) => SupportsRecursiveCTE MongoDB where
  recursiveCTEDict _ =
    backendCapabilityDict (Proxy @"Recursive CTEs") (Proxy @"MongoDB") (Proxy @'False)

--------------------------------------------------------------------------------
-- 5. Capability Instances: DynamoDB
--------------------------------------------------------------------------------

instance (AssertBackendCapability "Relational Joins" "DynamoDB" 'False) => SupportsRelationalJoins DynamoDB where
  relationalJoinsDict _ =
    backendCapabilityDict (Proxy @"Relational Joins") (Proxy @"DynamoDB") (Proxy @'False)

instance
  (AssertBackendCapability "Lateral Subqueries" "DynamoDB" 'False) =>
  SupportsLateralSubqueries DynamoDB
  where
  lateralSubqueriesDict _ =
    backendCapabilityDict (Proxy @"Lateral Subqueries") (Proxy @"DynamoDB") (Proxy @'False)

instance (AssertBackendCapability "Window Functions" "DynamoDB" 'False) => SupportsWindowFunctions DynamoDB where
  windowFunctionsDict _ =
    backendCapabilityDict (Proxy @"Window Functions") (Proxy @"DynamoDB") (Proxy @'False)

instance SupportsTransactions DynamoDB where
  transactionsDict _ = ()

instance (AssertBackendCapability "Vector Search" "DynamoDB" 'False) => SupportsVectorSearch DynamoDB where
  vectorSearchDict _ =
    backendCapabilityDict (Proxy @"Vector Search") (Proxy @"DynamoDB") (Proxy @'False)

instance SupportsBatchLookup DynamoDB where
  batchLookupDict _ = ()

instance (AssertBackendCapability "CTEs" "DynamoDB" 'False) => SupportsCTE DynamoDB where
  cteDict _ =
    backendCapabilityDict (Proxy @"CTEs") (Proxy @"DynamoDB") (Proxy @'False)

instance (AssertBackendCapability "Recursive CTEs" "DynamoDB" 'False) => SupportsRecursiveCTE DynamoDB where
  recursiveCTEDict _ =
    backendCapabilityDict (Proxy @"Recursive CTEs") (Proxy @"DynamoDB") (Proxy @'False)

--------------------------------------------------------------------------------
-- 6. Cross-Backend Dialect Renderers
--------------------------------------------------------------------------------

-- | Generic ANSI SQL query renderer.
renderStandardSQLText :: Projectable a => Query db a -> Text
renderStandardSQLText q =
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
                  limitClause = case qsLimit st of
                    Nothing -> ""
                    Just n -> " LIMIT " <> T.pack (show n)
                  offsetClause = case qsOffset st of
                    Nothing -> ""
                    Just n -> " OFFSET " <> T.pack (show n)
               in cteClause
                    <> selectClause
                    <> fromClause
                    <> whereClause
                    <> groupByClause
                    <> havingClause
                    <> orderByClause
                    <> limitClause
                    <> offsetClause

-- | Render a Query into strict Text SQL representation for PostgreSQL.
renderPostgresSQL :: Projectable a => Query Postgres a -> Text
renderPostgresSQL = renderStandardSQLText

-- | Render a Query into strict Text SQL representation for SQLite.
renderSQLiteSQL :: Projectable a => Query SQLite a -> Text
renderSQLiteSQL = renderStandardSQLText

--------------------------------------------------------------------------------
-- 7. DataLoader Batching Abstraction (Law 2)
--------------------------------------------------------------------------------

-- | DataLoader batch execution tracking structure.
data DataLoaderBatch parent child = DataLoaderBatch
  { batchParentCount :: !Int
  , batchChildCount :: !Int
  , batchQueryCount :: !Int
  , batchResults :: ![(parent, [child])]
  }
  deriving (Show, Eq, Generic)

instance (NFData parent, NFData child) => NFData (DataLoaderBatch parent child)

-- | Execute batch loader over parent items and batched child lookup with guaranteed 2 queries.
runBatchLoader ::
  (Ord key) =>
  [parent] ->
  (parent -> key) ->
  ([key] -> [child]) ->
  (child -> key) ->
  DataLoaderBatch parent child
runBatchLoader parents getParentKey fetchChildrenBatch getChildKey =
  let parentKeys = nub (map getParentKey parents)
      children = if null parentKeys then [] else fetchChildrenBatch parentKeys
      childMap = Map.fromListWith (++) [(getChildKey c, [c]) | c <- children]
      stitched =
        [ (p, reverse (Map.findWithDefault [] (getParentKey p) childMap))
        | p <- parents
        ]
   in DataLoaderBatch
        { batchParentCount = length parents
        , batchChildCount = length children
        , batchQueryCount = 2
        , batchResults = stitched
        }

-- | Capability-guarded DataLoader batching combinator.
viaBatchLoader ::
  forall db parent child key.
  (SupportsBatchLookup db, Ord key) =>
  [parent] ->
  (parent -> key) ->
  ([key] -> [child]) ->
  (child -> key) ->
  DataLoaderBatch parent child
viaBatchLoader parents getParentKey fetchChildren getChildKey =
  batchLookupDict (Proxy @db) `seq`
    runBatchLoader parents getParentKey fetchChildren getChildKey

--------------------------------------------------------------------------------
-- 8. Capability-Guarded CTE Combinators
--------------------------------------------------------------------------------

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
