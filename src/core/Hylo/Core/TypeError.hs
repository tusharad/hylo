{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.Core.TypeError
Description : Closed Type Family Diagnostic Engine & Custom TypeErrors (E001–E015)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

This module provides closed assertion type families that short-circuit GHC's type
checker to emit clear, standardized compiler diagnostics conforming to the Hylo
Diagnostic Standard:

@
[Hylo Error E00X] <Title>
  Context: <Violating Types>
  Hint:    <Concrete Copy-Pasteable Code Remediation>
@
-}
module Hylo.Core.TypeError
  ( -- * E001, E002, E003: Relational Comparisons
    AssertComparable (..)
  , CheckComparable

    -- * E004: Ungrouped Column Selection
  , AssertGrouped (..)
  , CheckGrouped

    -- * E005: Mutation in ReadOnly Transactions
  , TxMode (..)
  , AssertWritable (..)
  , CheckWritable

    -- * E006: Forbidden IO in Transactions
  , AssertNoIO (..)
  , CheckNoIO

    -- * E007: Vector Model / Dimension Compatibility
  , AssertMatchingVectors (..)
  , CheckMatchingVectors

    -- * E008: Backend Capability Mismatch
  , AssertBackendCapability (..)
  , CheckBackendCapability

    -- * E009: Stale Object Mutation / Optimistic Lock
  , AssertHasVersion (..)
  , CheckHasVersion

    -- * E010: Unsafe Raw String Interpolation
  , AssertParameterized (..)
  , CheckParameterized

    -- * E011: Missing Primary Key in Table Definition
  , AssertHasPrimaryKey (..)
  , CheckHasPrimaryKey

    -- * E012: Invalid Foreign Key Reference
  , AssertValidForeignKey (..)
  , CheckValidForeignKey

    -- * E013: Circular Graph Traversal
  , AssertAcyclic (..)
  , CheckAcyclic

    -- * E014: Illegal Pagination on Correlated Graph Subquery
  , AssertPaginationValid (..)
  , CheckPaginationValid

    -- * E015: Unchecked Destructive Schema Migration
  , AssertSafeMigration (..)
  , CheckSafeMigration

    -- * E016: PostgreSQL Version Mismatch
  , PGVersion (..)
  , CheckPGMinVersion
  , AssertPGMinVersion (..)
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import GHC.TypeLits
  ( ErrorMessage (..)
  , Nat
  , Symbol
  , TypeError
  )
import Hylo.Core.Key (Id)

--------------------------------------------------------------------------------
-- E001, E002, E003: Relational Comparisons
--------------------------------------------------------------------------------

-- | Type family evaluating relational comparability and emitting E001–E003 errors.
type family CheckComparable (a :: Type) (b :: Type) :: Bool where
  -- Case 1: Identical types (including identical phantom Id Table k) -> OK
  CheckComparable a a = 'True
  -- Case 2: Cross-table key mismatch -> E001
  CheckComparable (Id t1 k1) (Id t2 k2) =
    TypeError
      ( 'Text "[Hylo Error E001] Cross-Table Foreign Key Mismatch"
          ':$$: 'Text "  Context: Attempted to compare primary/foreign key of table '"
          ':<>: 'ShowType t1
          ':<>: 'Text "' with table '"
          ':<>: 'ShowType t2
          ':<>: 'Text "'"
          ':$$: 'Text "  Hint:    Foreign key comparisons must target the matching table entity."
      )
  -- Case 3: Left is Maybe a, Right is raw a -> E002
  CheckComparable (Maybe a) a =
    TypeError
      ( 'Text "[Hylo Error E002] Nullability Comparison Mismatch"
          ':$$: 'Text "  Context: Comparing 'QExpr (Maybe "
          ':<>: 'ShowType a
          ':<>: 'Text ")' with 'QExpr "
          ':<>: 'ShowType a
          ':<>: 'Text "' without explicit null handling."
          ':$$: 'Text
                  "  Hint:    Wrap the non-null expression with 'just' (e.g. 'just rawExpr') or unwrap the nullable expression with 'coalesce'."
      )
  -- Case 4: Left is raw a, Right is Maybe a -> E002
  CheckComparable a (Maybe a) =
    TypeError
      ( 'Text "[Hylo Error E002] Nullability Comparison Mismatch"
          ':$$: 'Text "  Context: Comparing 'QExpr "
          ':<>: 'ShowType a
          ':<>: 'Text "' with 'QExpr (Maybe "
          ':<>: 'ShowType a
          ':<>: 'Text ")' without explicit null handling."
          ':$$: 'Text
                  "  Hint:    Wrap the non-null expression with 'just' (e.g. 'just rawExpr') or unwrap the nullable expression with 'coalesce'."
      )
  -- Case 5: Incompatible column types -> E003
  CheckComparable a b =
    TypeError
      ( 'Text "[Hylo Error E003] Incompatible Column Types in Relational Operator"
          ':$$: 'Text "  Context: Attempted to compare column of type '"
          ':<>: 'ShowType a
          ':<>: 'Text "' with column of type '"
          ':<>: 'ShowType b
          ':<>: 'Text "'"
          ':$$: 'Text
                  "  Hint:    Ensure both sides of the comparison have compatible SQL types, or apply an explicit cast."
      )

-- | Class asserting that two expressions are relationally comparable.
class AssertComparable a b where
  comparableDict :: Proxy a -> Proxy b -> ()

instance (CheckComparable a b ~ 'True) => AssertComparable a b where
  comparableDict _ _ = ()

--------------------------------------------------------------------------------
-- E004: Ungrouped Column Selection
--------------------------------------------------------------------------------

-- | Type family ensuring selected columns appear in GROUP BY list.
type family CheckGrouped (col :: Symbol) (isGrouped :: Bool) :: Bool where
  CheckGrouped _ 'True = 'True
  CheckGrouped col 'False =
    TypeError
      ( 'Text "[Hylo Error E004] Ungrouped Column Selection in Aggregate Query"
          ':$$: 'Text "  Context: Selected column '"
          ':<>: 'Text col
          ':<>: 'Text "' is not present in the GROUP BY clause."
          ':$$: 'Text "  Hint:    Add '"
          ':<>: 'Text col
          ':<>: 'Text
                  "' to the 'groupBy' list or wrap it in an aggregate function ('count', 'sum', 'avg', 'min', 'max')."
      )

class AssertGrouped (col :: Symbol) (isGrouped :: Bool) where
  groupedDict :: Proxy col -> Proxy isGrouped -> ()

instance (CheckGrouped col isGrouped ~ 'True) => AssertGrouped col isGrouped where
  groupedDict _ _ = ()

--------------------------------------------------------------------------------
-- E005: Mutation in ReadOnly Transactions
--------------------------------------------------------------------------------

-- | Transaction permission modes.
data TxMode
  = ReadOnly
  | ReadWrite
  deriving (Eq, Show)

-- | Type family ensuring transaction mode permits write operations.
type family CheckWritable (mode :: k) :: Bool where
  CheckWritable "ReadWrite" = 'True
  CheckWritable 'ReadWrite = 'True
  CheckWritable "ReadOnly" =
    TypeError
      ( 'Text "[Hylo Error E005] Mutation in ReadOnly Transaction"
          ':$$: 'Text
                  "  Context: Attempted to invoke mutation operator ('insert' / 'update' / 'delete') inside a 'ReadOnly' transaction."
          ':$$: 'Text "  Hint:    Switch transaction mode to 'ReadWrite' (e.g. 'transaction ReadWrite $ do ...')."
      )
  CheckWritable 'ReadOnly =
    TypeError
      ( 'Text "[Hylo Error E005] Mutation in ReadOnly Transaction"
          ':$$: 'Text
                  "  Context: Attempted to invoke mutation operator ('insert' / 'update' / 'delete') inside a 'ReadOnly' transaction."
          ':$$: 'Text "  Hint:    Switch transaction mode to 'ReadWrite' (e.g. 'transaction ReadWrite $ do ...')."
      )
  CheckWritable other =
    TypeError
      ( 'Text "[Hylo Error E005] Unknown Transaction Mode"
          ':$$: 'Text "  Context: Mode '"
          ':<>: 'ShowType other
          ':<>: 'Text "' is not recognized as writable."
          ':$$: 'Text "  Hint:    Transactions must be explicitly parameterized by 'ReadOnly' or 'ReadWrite'."
      )

class AssertWritable mode where
  writableDict :: Proxy mode -> ()

instance (CheckWritable mode ~ 'True) => AssertWritable mode where
  writableDict _ = ()

--------------------------------------------------------------------------------
-- E006: Forbidden IO in Transactions
--------------------------------------------------------------------------------

-- | Type family ensuring pure transaction monads do not execute raw IO.
type family CheckNoIO (hasIO :: Bool) :: Bool where
  CheckNoIO 'False = 'True
  CheckNoIO 'True =
    TypeError
      ( 'Text "[Hylo Error E006] Forbidden IO Operation in Database Transaction"
          ':$$: 'Text "  Context: Attempted to execute arbitrary 'IO' inside an atomic transaction."
          ':$$: 'Text
                  "  Hint:    Transactions must remain pure and reproducible for automatic retry (40001). Move external network/IO calls outside the transaction or use 'onCommit'."
      )

class AssertNoIO (hasIO :: Bool) where
  noIODict :: Proxy hasIO -> ()

instance (CheckNoIO hasIO ~ 'True) => AssertNoIO hasIO where
  noIODict _ = ()

--------------------------------------------------------------------------------
-- E007: Vector Model / Dimension Compatibility
--------------------------------------------------------------------------------

-- | Type family ensuring vector embeddings share identical model and dimension.
type family CheckMatchingVectors (m1 :: Symbol) (d1 :: Nat) (m2 :: Symbol) (d2 :: Nat) :: Bool where
  CheckMatchingVectors m d m d = 'True
  CheckMatchingVectors m1 d1 m2 d2 =
    TypeError
      ( 'Text "[Hylo Error E007] Vector Model or Dimension Incompatibility"
          ':$$: 'Text "  Context: Comparing embedding of model '"
          ':<>: 'Text m1
          ':<>: 'Text "' (dim: "
          ':<>: 'ShowType d1
          ':<>: 'Text ") with model '"
          ':<>: 'Text m2
          ':<>: 'Text "' (dim: "
          ':<>: 'ShowType d2
          ':<>: 'Text ")."
          ':$$: 'Text
                  "  Hint:    Vector metrics are only valid between embeddings produced by the identical model and dimensionality."
      )

class AssertMatchingVectors (m1 :: Symbol) (d1 :: Nat) (m2 :: Symbol) (d2 :: Nat) where
  matchingVectorsDict :: Proxy m1 -> Proxy d1 -> Proxy m2 -> Proxy d2 -> ()

instance (CheckMatchingVectors m1 d1 m2 d2 ~ 'True) => AssertMatchingVectors m1 d1 m2 d2 where
  matchingVectorsDict _ _ _ _ = ()

--------------------------------------------------------------------------------
-- E008: Backend Capability Mismatch
--------------------------------------------------------------------------------

-- | Type family validating that the database backend supports the requested feature.
type family CheckBackendCapability (cap :: Symbol) (backend :: Symbol) (supported :: Bool) :: Bool where
  CheckBackendCapability _ _ 'True = 'True
  CheckBackendCapability cap backend 'False =
    TypeError
      ( 'Text "[Hylo Error E008] Backend Capability Mismatch"
          ':$$: 'Text "  Context: Feature '"
          ':<>: 'Text cap
          ':<>: 'Text "' is not supported by database backend '"
          ':<>: 'Text backend
          ':<>: 'Text "'."
          ':$$: 'Text
                  "  Hint:    Restrict query features to capabilities supported by active backend, or switch to a capable SQL backend."
      )

class AssertBackendCapability (cap :: Symbol) (backend :: Symbol) (supported :: Bool) where
  backendCapabilityDict :: Proxy cap -> Proxy backend -> Proxy supported -> ()

instance
  (CheckBackendCapability cap backend supported ~ 'True) =>
  AssertBackendCapability cap backend supported
  where
  backendCapabilityDict _ _ _ = ()

--------------------------------------------------------------------------------
-- E009: Stale Object Mutation / Optimistic Lock
--------------------------------------------------------------------------------

-- | Type family ensuring version column exists for optimistic concurrency.
type family CheckHasVersion (table :: Symbol) (hasVersion :: Bool) :: Bool where
  CheckHasVersion _ 'True = 'True
  CheckHasVersion table 'False =
    TypeError
      ( 'Text "[Hylo Error E009] Stale Object Mutation / Missing Optimistic Lock"
          ':$$: 'Text "  Context: Table '"
          ':<>: 'Text table
          ':<>: 'Text "' does not define an optimistic lock version column."
          ':$$: 'Text
                  "  Hint:    Define an 'optimisticLockColumn' in TableSchema or supply an explicit version check predicate."
      )

class AssertHasVersion (table :: Symbol) (hasVersion :: Bool) where
  hasVersionDict :: Proxy table -> Proxy hasVersion -> ()

instance (CheckHasVersion table hasVersion ~ 'True) => AssertHasVersion table hasVersion where
  hasVersionDict _ _ = ()

--------------------------------------------------------------------------------
-- E010: Unsafe Raw String Interpolation
--------------------------------------------------------------------------------

-- | Type family forbidding unparameterized raw SQL string interpolation.
type family CheckParameterized (isSafe :: Bool) :: Bool where
  CheckParameterized 'True = 'True
  CheckParameterized 'False =
    TypeError
      ( 'Text "[Hylo Error E010] Unsafe Raw SQL String Interpolation"
          ':$$: 'Text
                  "  Context: Direct un-parameterized raw SQL string interpolation is forbidden in query construction."
          ':$$: 'Text
                  "  Hint:    Use parameterized query combinators (e.g. 'lit', 'where_') to prevent SQL injection vulnerabilities."
      )

class AssertParameterized (isSafe :: Bool) where
  parameterizedDict :: Proxy isSafe -> ()

instance (CheckParameterized isSafe ~ 'True) => AssertParameterized isSafe where
  parameterizedDict _ = ()

--------------------------------------------------------------------------------
-- E011: Missing Primary Key in Table Definition
--------------------------------------------------------------------------------

-- | Type family ensuring table definition includes a primary key.
type family CheckHasPrimaryKey (table :: Symbol) (hasPk :: Bool) :: Bool where
  CheckHasPrimaryKey _ 'True = 'True
  CheckHasPrimaryKey table 'False =
    TypeError
      ( 'Text "[Hylo Error E011] Missing Primary Key in Table Definition"
          ':$$: 'Text "  Context: Table '"
          ':<>: 'Text table
          ':<>: 'Text "' was declared without a primary key specification."
          ':$$: 'Text "  Hint:    Designate at least one column as a 'primaryKey' in your TableSchema definition."
      )

class AssertHasPrimaryKey (table :: Symbol) (hasPk :: Bool) where
  hasPrimaryKeyDict :: Proxy table -> Proxy hasPk -> ()

instance (CheckHasPrimaryKey table hasPk ~ 'True) => AssertHasPrimaryKey table hasPk where
  hasPrimaryKeyDict _ _ = ()

--------------------------------------------------------------------------------
-- E012: Invalid Foreign Key Reference
--------------------------------------------------------------------------------

-- | Type family ensuring foreign keys target existing primary or unique keys.
type family
  CheckValidForeignKey
    (fkCol :: Symbol)
    (targetTable :: Symbol)
    (targetCol :: Symbol)
    (isValid :: Bool) ::
    Bool
  where
  CheckValidForeignKey _ _ _ 'True = 'True
  CheckValidForeignKey fkCol targetTable targetCol 'False =
    TypeError
      ( 'Text "[Hylo Error E012] Invalid Foreign Key Reference"
          ':$$: 'Text "  Context: Column '"
          ':<>: 'Text fkCol
          ':<>: 'Text "' attempts to reference '"
          ':<>: 'Text targetTable
          ':<>: 'Text "."
          ':<>: 'Text targetCol
          ':<>: 'Text "', which is not a unique or primary key."
          ':$$: 'Text
                  "  Hint:    Foreign keys must target a unique constraint or primary key on the referenced table entity."
      )

class
  AssertValidForeignKey
    (fkCol :: Symbol)
    (targetTable :: Symbol)
    (targetCol :: Symbol)
    (isValid :: Bool)
  where
  validForeignKeyDict :: Proxy fkCol -> Proxy targetTable -> Proxy targetCol -> Proxy isValid -> ()

instance
  (CheckValidForeignKey fkCol targetTable targetCol isValid ~ 'True) =>
  AssertValidForeignKey fkCol targetTable targetCol isValid
  where
  validForeignKeyDict _ _ _ _ = ()

--------------------------------------------------------------------------------
-- E013: Circular Graph Traversal
--------------------------------------------------------------------------------

-- | Type family detecting cycles in graph relationship traversals.
type family CheckAcyclic (edge :: Symbol) (isCycle :: Bool) :: Bool where
  CheckAcyclic _ 'False = 'True
  CheckAcyclic edge 'True =
    TypeError
      ( 'Text "[Hylo Error E013] Circular Graph Traversal Without Cycle Breaker"
          ':$$: 'Text "  Context: Relationship graph traversal detected a circular dependency along edge '"
          ':<>: 'Text edge
          ':<>: 'Text "'."
          ':$$: 'Text "  Hint:    Break the cycle using an explicit recursion depth limit or a visited set filter."
      )

class AssertAcyclic (edge :: Symbol) (isCycle :: Bool) where
  acyclicDict :: Proxy edge -> Proxy isCycle -> ()

instance (CheckAcyclic edge isCycle ~ 'True) => AssertAcyclic edge isCycle where
  acyclicDict _ _ = ()

--------------------------------------------------------------------------------
-- E014: Illegal Pagination on Correlated Graph Subquery
--------------------------------------------------------------------------------

-- | Type family forbidding unordered limit/offset inside correlated subqueries.
type family CheckPaginationValid (isValid :: Bool) :: Bool where
  CheckPaginationValid 'True = 'True
  CheckPaginationValid 'False =
    TypeError
      ( 'Text "[Hylo Error E014] Illegal Pagination on Correlated Graph Subquery"
          ':$$: 'Text
                  "  Context: Applied 'limit' or 'offset' directly inside a correlated lateral relationship without explicit ordering."
          ':$$: 'Text
                  "  Hint:    Provide an unambiguous 'orderBy' clause with a unique tie-breaker before applying 'limit' or 'offset'."
      )

class AssertPaginationValid (isValid :: Bool) where
  paginationValidDict :: Proxy isValid -> ()

instance (CheckPaginationValid isValid ~ 'True) => AssertPaginationValid isValid where
  paginationValidDict _ = ()

--------------------------------------------------------------------------------
-- E015: Unchecked Destructive Schema Migration
--------------------------------------------------------------------------------

-- | Type family ensuring destructive migrations are explicitly acknowledged.
type family CheckSafeMigration (isDestructive :: Bool) (forceAllowed :: Bool) :: Bool where
  CheckSafeMigration 'False _ = 'True
  CheckSafeMigration 'True 'True = 'True
  CheckSafeMigration 'True 'False =
    TypeError
      ( 'Text "[Hylo Error E015] Unchecked Destructive Schema Migration"
          ':$$: 'Text
                  "  Context: Migration plan contains destructive column/table drop operations without explicit confirmation."
          ':$$: 'Text
                  "  Hint:    Pass '--allow-destructive-data-loss' or set 'allowDestructiveChanges = True' in migration runner configuration."
      )

class AssertSafeMigration (isDestructive :: Bool) (forceAllowed :: Bool) where
  safeMigrationDict :: Proxy isDestructive -> Proxy forceAllowed -> ()

instance
  (CheckSafeMigration isDestructive forceAllowed ~ 'True) =>
  AssertSafeMigration isDestructive forceAllowed
  where
  safeMigrationDict _ _ = ()

--------------------------------------------------------------------------------
-- E016: PostgreSQL Version Mismatch
--------------------------------------------------------------------------------

-- | PostgreSQL engine major versions.
data PGVersion
  = PG14
  | PG15
  | PG16
  | PG17
  | PG18
  deriving (Eq, Show, Enum, Bounded, Ord, Generic)

-- | Closed type family validating that the target PostgreSQL version meets the feature requirement.
type family CheckPGMinVersion (actual :: PGVersion) (required :: PGVersion) (feature :: Symbol) :: Bool where
  -- Required PG14: supported on all PG versions
  CheckPGMinVersion 'PG14 'PG14 _ = 'True
  CheckPGMinVersion 'PG15 'PG14 _ = 'True
  CheckPGMinVersion 'PG16 'PG14 _ = 'True
  CheckPGMinVersion 'PG17 'PG14 _ = 'True
  CheckPGMinVersion 'PG18 'PG14 _ = 'True
  -- Required PG15: supported on PG15, PG16, PG17, PG18
  CheckPGMinVersion 'PG15 'PG15 _ = 'True
  CheckPGMinVersion 'PG16 'PG15 _ = 'True
  CheckPGMinVersion 'PG17 'PG15 _ = 'True
  CheckPGMinVersion 'PG18 'PG15 _ = 'True
  -- Required PG16: supported on PG16, PG17, PG18
  CheckPGMinVersion 'PG16 'PG16 _ = 'True
  CheckPGMinVersion 'PG17 'PG16 _ = 'True
  CheckPGMinVersion 'PG18 'PG16 _ = 'True
  -- Required PG17: supported on PG17, PG18
  CheckPGMinVersion 'PG17 'PG17 _ = 'True
  CheckPGMinVersion 'PG18 'PG17 _ = 'True
  -- Required PG18: supported on PG18 only
  CheckPGMinVersion 'PG18 'PG18 _ = 'True
  -- Version mismatch: emit [Hylo Error E016]
  CheckPGMinVersion actual required feature =
    TypeError
      ( 'Text "[Hylo Error E016] PostgreSQL Version Mismatch"
          ':$$: 'Text "  Context: Feature '"
          ':<>: 'Text feature
          ':<>: 'Text "' requires PostgreSQL "
          ':<>: 'ShowType required
          ':<>: 'Text " or newer, but active backend is configured for PostgreSQL "
          ':<>: 'ShowType actual
          ':<>: 'Text "."
          ':$$: 'Text "  Hint:    Upgrade your target PostgreSQL backend version to 'Postgres '"
          ':<>: 'ShowType required
          ':<>: 'Text "' or newer (e.g. 'Postgres '"
          ':<>: 'ShowType required
          ':<>: 'Text "') to use this feature."
      )

-- | Capability dictionary witness for PostgreSQL minimum version check.
class AssertPGMinVersion (actual :: PGVersion) (required :: PGVersion) (feature :: Symbol) where
  pgMinVersionDict :: Proxy actual -> Proxy required -> Proxy feature -> ()

instance (CheckPGMinVersion actual required feature ~ 'True) => AssertPGMinVersion actual required feature where
  pgMinVersionDict _ _ _ = ()
