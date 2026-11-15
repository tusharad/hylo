{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
(relational joins, lateral subqueries, returning clauses, key-value ops, window functions,
transactions, vector search, batch lookup, and recursive CTEs) across PostgreSQL, MySQL,
SQLite, and Redis.
Unsupported feature usage fails statically at compile time with '[Hylo Error E008]'.
-}
module Hylo.Backend.Capability
  ( -- * Database Backend Markers & Engine Versions
    PGVersion (..)
  , Postgres
  , MySQL
  , SQLite
  , Redis

    -- * Capability Typeclasses
  , SupportsRelationalJoins (..)
  , SupportsLateralSubqueries (..)
  , SupportsReturningClause (..)
  , SupportsKeyValueOps (..)
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
  , CheckPGMinVersion
  , AssertPGMinVersion (..)

    -- * PostgreSQL Version-Gated Features (E016)
  , uniqueNullsNotDistinct
  , uniqueNullsNotDistinctConstraint
  , mergeInto
  , mergeReturning
  , returningWithOldNew
  , uuidv7
  , uuidv7Tx
  , pg15UniqueNullsNotDistinctDict
  , pg15MergeIntoDict
  , pg17MergeReturningDict
  , pg18ReturningWithOldNewDict
  , pg18UuidV7Dict

    -- * DataLoader Batching Abstraction
  , DataLoaderBatch (..)
  , runBatchLoader
  , viaBatchLoader
  ) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Hylo.Core.Changeset (Changeset (..), ConstraintDeclaration (..))
import Hylo.Core.Expr (QExpr (..))
import Hylo.Core.Transaction (Tx (..), recordMutation)
import Hylo.Core.TypeError
  ( AssertBackendCapability (..)
  , AssertPGMinVersion (..)
  , AssertWritable (..)
  , CheckBackendCapability
  , CheckPGMinVersion
  , PGVersion (..)
  , TxMode (..)
  , writableDict
  )

--------------------------------------------------------------------------------
-- 0. Database Backend Markers & Engine Versions
--------------------------------------------------------------------------------

-- | Target backend tag for PostgreSQL, optionally parameterized by engine version.
data Postgres (v :: PGVersion)

-- | Target backend tag for MySQL relational engine.
data MySQL

-- | Target backend tag for SQLite embedded relational engine.
data SQLite

-- | Target backend tag for Redis in-memory key-value engine.
data Redis

--------------------------------------------------------------------------------
-- 1. Capability Typeclasses
--------------------------------------------------------------------------------

-- | Capability typeclass certifying backend support for ANSI relational JOIN operations.
class SupportsRelationalJoins db where
  relationalJoinsDict :: Proxy db -> ()

-- | Capability typeclass certifying backend support for correlated LATERAL subqueries.
class SupportsLateralSubqueries db where
  lateralSubqueriesDict :: Proxy db -> ()

-- | Capability typeclass certifying backend support for RETURNING clauses on mutation statements.
class SupportsReturningClause db where
  returningClauseDict :: Proxy db -> ()

-- | Capability typeclass certifying backend support for native Key-Value operations.
class SupportsKeyValueOps db where
  keyValueOpsDict :: Proxy db -> ()

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
recursiveCTEsDict :: forall db. (SupportsRecursiveCTE db) => Proxy db -> ()
recursiveCTEsDict = recursiveCTEDict

--------------------------------------------------------------------------------
-- 2. Capability Instances: PostgreSQL
--------------------------------------------------------------------------------

instance SupportsRelationalJoins (Postgres v) where
  relationalJoinsDict _ = ()

instance SupportsLateralSubqueries (Postgres v) where
  lateralSubqueriesDict _ = ()

instance SupportsReturningClause (Postgres v) where
  returningClauseDict _ = ()

instance
  (AssertBackendCapability "Key-Value Operations" "PostgreSQL" 'False) =>
  SupportsKeyValueOps (Postgres v)
  where
  keyValueOpsDict _ =
    backendCapabilityDict (Proxy @"Key-Value Operations") (Proxy @"PostgreSQL") (Proxy @'False)

instance SupportsWindowFunctions (Postgres v) where
  windowFunctionsDict _ = ()

instance SupportsTransactions (Postgres v) where
  transactionsDict _ = ()

instance SupportsVectorSearch (Postgres v) where
  vectorSearchDict _ = ()

instance SupportsBatchLookup (Postgres v) where
  batchLookupDict _ = ()

instance SupportsCTE (Postgres v) where
  cteDict _ = ()

instance SupportsRecursiveCTE (Postgres v) where
  recursiveCTEDict _ = ()

instance SupportsRelationalJoins Postgres where
  relationalJoinsDict _ = ()

instance SupportsLateralSubqueries Postgres where
  lateralSubqueriesDict _ = ()

instance SupportsReturningClause Postgres where
  returningClauseDict _ = ()

instance
  (AssertBackendCapability "Key-Value Operations" "PostgreSQL" 'False) =>
  SupportsKeyValueOps Postgres
  where
  keyValueOpsDict _ =
    backendCapabilityDict (Proxy @"Key-Value Operations") (Proxy @"PostgreSQL") (Proxy @'False)

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
-- 3. Capability Instances: MySQL
--------------------------------------------------------------------------------

instance SupportsRelationalJoins MySQL where
  relationalJoinsDict _ = ()

instance SupportsLateralSubqueries MySQL where
  lateralSubqueriesDict _ = ()

instance
  (AssertBackendCapability "Returning Clause" "MySQL" 'False) =>
  SupportsReturningClause MySQL
  where
  returningClauseDict _ =
    backendCapabilityDict (Proxy @"Returning Clause") (Proxy @"MySQL") (Proxy @'False)

instance
  (AssertBackendCapability "Key-Value Operations" "MySQL" 'False) =>
  SupportsKeyValueOps MySQL
  where
  keyValueOpsDict _ =
    backendCapabilityDict (Proxy @"Key-Value Operations") (Proxy @"MySQL") (Proxy @'False)

instance SupportsWindowFunctions MySQL where
  windowFunctionsDict _ = ()

instance SupportsTransactions MySQL where
  transactionsDict _ = ()

instance SupportsBatchLookup MySQL where
  batchLookupDict _ = ()

instance SupportsCTE MySQL where
  cteDict _ = ()

instance SupportsRecursiveCTE MySQL where
  recursiveCTEDict _ = ()

--------------------------------------------------------------------------------
-- 4. Capability Instances: SQLite
--------------------------------------------------------------------------------

instance SupportsRelationalJoins SQLite where
  relationalJoinsDict _ = ()

instance SupportsLateralSubqueries SQLite where
  lateralSubqueriesDict _ = ()

instance SupportsReturningClause SQLite where
  returningClauseDict _ = ()

instance
  (AssertBackendCapability "Key-Value Operations" "SQLite" 'False) =>
  SupportsKeyValueOps SQLite
  where
  keyValueOpsDict _ =
    backendCapabilityDict (Proxy @"Key-Value Operations") (Proxy @"SQLite") (Proxy @'False)

instance SupportsWindowFunctions SQLite where
  windowFunctionsDict _ = ()

instance SupportsTransactions SQLite where
  transactionsDict _ = ()

instance
  (AssertBackendCapability "Vector Search" "SQLite" 'False) =>
  SupportsVectorSearch SQLite
  where
  vectorSearchDict _ =
    backendCapabilityDict (Proxy @"Vector Search") (Proxy @"SQLite") (Proxy @'False)

instance SupportsBatchLookup SQLite where
  batchLookupDict _ = ()

instance SupportsCTE SQLite where
  cteDict _ = ()

instance SupportsRecursiveCTE SQLite where
  recursiveCTEDict _ = ()

--------------------------------------------------------------------------------
-- 5. Capability Instances: Redis
--------------------------------------------------------------------------------

instance SupportsKeyValueOps Redis where
  keyValueOpsDict _ = ()

instance
  (AssertBackendCapability "Relational Joins" "Redis" 'False) =>
  SupportsRelationalJoins Redis
  where
  relationalJoinsDict _ =
    backendCapabilityDict (Proxy @"Relational Joins") (Proxy @"Redis") (Proxy @'False)

instance
  (AssertBackendCapability "Lateral Subqueries" "Redis" 'False) =>
  SupportsLateralSubqueries Redis
  where
  lateralSubqueriesDict _ =
    backendCapabilityDict (Proxy @"Lateral Subqueries") (Proxy @"Redis") (Proxy @'False)

instance
  (AssertBackendCapability "Returning Clause" "Redis" 'False) =>
  SupportsReturningClause Redis
  where
  returningClauseDict _ =
    backendCapabilityDict (Proxy @"Returning Clause") (Proxy @"Redis") (Proxy @'False)

instance
  (AssertBackendCapability "Window Functions" "Redis" 'False) =>
  SupportsWindowFunctions Redis
  where
  windowFunctionsDict _ =
    backendCapabilityDict (Proxy @"Window Functions") (Proxy @"Redis") (Proxy @'False)

instance
  (AssertBackendCapability "CTEs" "Redis" 'False) =>
  SupportsCTE Redis
  where
  cteDict _ =
    backendCapabilityDict (Proxy @"CTEs") (Proxy @"Redis") (Proxy @'False)

instance
  (AssertBackendCapability "Recursive CTEs" "Redis" 'False) =>
  SupportsRecursiveCTE Redis
  where
  recursiveCTEDict _ =
    backendCapabilityDict (Proxy @"Recursive CTEs") (Proxy @"Redis") (Proxy @'False)

--------------------------------------------------------------------------------
-- 6. DataLoader Batching Abstraction
--------------------------------------------------------------------------------

-- | DataLoader batch execution tracking structure.
data DataLoaderBatch parent child = DataLoaderBatch
  { batchParentCount :: !Int
  , batchChildCount :: !Int
  , batchQueryCount :: !Int
  , batchResults :: ![(parent, [child])]
  }
  deriving (Show, Eq, Generic)

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
-- 7. PostgreSQL Version-Gated Features ([Hylo Error E016])
--------------------------------------------------------------------------------

-- | UNIQUE NULLS NOT DISTINCT constraint (requires PostgreSQL 15+).
uniqueNullsNotDistinct ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG15 "UNIQUE NULLS NOT DISTINCT") =>
  Text ->
  Text
uniqueNullsNotDistinct col =
  pgMinVersionDict (Proxy @v) (Proxy @'PG15) (Proxy @"UNIQUE NULLS NOT DISTINCT") `seq`
    ("UNIQUE NULLS NOT DISTINCT (" <> col <> ")")

-- | Declare a UNIQUE NULLS NOT DISTINCT constraint in a Changeset (requires PostgreSQL 15+).
uniqueNullsNotDistinctConstraint ::
  forall (v :: PGVersion) table op.
  (AssertPGMinVersion v 'PG15 "UNIQUE NULLS NOT DISTINCT") =>
  Text ->
  Changeset table op ->
  Changeset table op
uniqueNullsNotDistinctConstraint col cs =
  pgMinVersionDict (Proxy @v) (Proxy @'PG15) (Proxy @"UNIQUE NULLS NOT DISTINCT") `seq`
    let decl = UniqueConstraint col Nothing "must be unique (nulls not distinct)"
     in cs {changesetConstraints = changesetConstraints cs ++ [decl]}

-- | MERGE INTO statement (requires PostgreSQL 15+).
mergeInto ::
  forall (v :: PGVersion) (mode :: TxMode).
  (AssertWritable mode, AssertPGMinVersion v 'PG15 "MERGE INTO") =>
  Text ->
  Tx (Postgres v) mode ()
mergeInto rawSql =
  writableDict (Proxy :: Proxy mode)
    `seq` pgMinVersionDict (Proxy @v) (Proxy @'PG15) (Proxy @"MERGE INTO")
    `seq` recordMutation ("MERGE INTO " <> rawSql)

-- | MERGE ... RETURNING statement (requires PostgreSQL 17+).
mergeReturning ::
  forall (v :: PGVersion) (mode :: TxMode).
  (AssertWritable mode, AssertPGMinVersion v 'PG17 "MERGE ... RETURNING") =>
  Text ->
  Tx (Postgres v) mode ()
mergeReturning rawSql =
  writableDict (Proxy :: Proxy mode)
    `seq` pgMinVersionDict (Proxy @v) (Proxy @'PG17) (Proxy @"MERGE ... RETURNING")
    `seq` recordMutation ("MERGE ... RETURNING " <> rawSql)

-- | RETURNING WITH (OLD AS old, NEW AS new) clause (requires PostgreSQL 18+).
returningWithOldNew ::
  forall (v :: PGVersion) (mode :: TxMode).
  (AssertWritable mode, AssertPGMinVersion v 'PG18 "RETURNING WITH (OLD AS old, NEW AS new)") =>
  Text ->
  Tx (Postgres v) mode ()
returningWithOldNew rawSql =
  writableDict (Proxy :: Proxy mode)
    `seq` pgMinVersionDict (Proxy @v) (Proxy @'PG18) (Proxy @"RETURNING WITH (OLD AS old, NEW AS new)")
    `seq` recordMutation ("RETURNING WITH (OLD AS old, NEW AS new) " <> rawSql)

-- | Server-side uuidv7() SQL expression (requires PostgreSQL 18+).
uuidv7 ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG18 "server-side uuidv7()") =>
  QExpr UUID
uuidv7 =
  pgMinVersionDict (Proxy @v) (Proxy @'PG18) (Proxy @"server-side uuidv7()") `seq`
    LitExpr "uuidv7()"

-- | Server-side uuidv7() within a Tx computation (requires PostgreSQL 18+).
uuidv7Tx ::
  forall (v :: PGVersion) (mode :: TxMode).
  (AssertPGMinVersion v 'PG18 "server-side uuidv7()") =>
  Tx (Postgres v) mode UUID
uuidv7Tx =
  pgMinVersionDict (Proxy @v) (Proxy @'PG18) (Proxy @"server-side uuidv7()") `seq`
    TxAction (Right (errorWithoutStackTrace "uuidv7: evaluated offline"),)

-- | Capability dictionary witness for PG15 UNIQUE NULLS NOT DISTINCT.
pg15UniqueNullsNotDistinctDict ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG15 "UNIQUE NULLS NOT DISTINCT") =>
  Proxy (v :: PGVersion) ->
  ()
pg15UniqueNullsNotDistinctDict p =
  pgMinVersionDict p (Proxy @'PG15) (Proxy @"UNIQUE NULLS NOT DISTINCT")

-- | Capability dictionary witness for PG15 MERGE INTO.
pg15MergeIntoDict ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG15 "MERGE INTO") =>
  Proxy (v :: PGVersion) ->
  ()
pg15MergeIntoDict p =
  pgMinVersionDict p (Proxy @'PG15) (Proxy @"MERGE INTO")

-- | Capability dictionary witness for PG17 MERGE ... RETURNING.
pg17MergeReturningDict ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG17 "MERGE ... RETURNING") =>
  Proxy (v :: PGVersion) ->
  ()
pg17MergeReturningDict p =
  pgMinVersionDict p (Proxy @'PG17) (Proxy @"MERGE ... RETURNING")

-- | Capability dictionary witness for PG18 RETURNING WITH.
pg18ReturningWithOldNewDict ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG18 "RETURNING WITH (OLD AS old, NEW AS new)") =>
  Proxy (v :: PGVersion) ->
  ()
pg18ReturningWithOldNewDict p =
  pgMinVersionDict p (Proxy @'PG18) (Proxy @"RETURNING WITH (OLD AS old, NEW AS new)")

-- | Capability dictionary witness for PG18 uuidv7().
pg18UuidV7Dict ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG18 "server-side uuidv7()") =>
  Proxy (v :: PGVersion) ->
  ()
pg18UuidV7Dict p =
  pgMinVersionDict p (Proxy @'PG18) (Proxy @"server-side uuidv7()")
