{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.Core.Query
Description : Query, Transaction (Tx), and Changeset abstractions
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Transactional mutation operations (insert, update, delete) statically guarded
against execution in 'ReadOnly' mode via closed TypeError assertion families.
-}
module Hylo.Core.Query
  ( -- * Backend Markers
    Postgres

    -- * Operational Query Monad & AST
  , Query (..)
  , QueryState (..)
  , initialQueryState
  , FromRelation (..)
  , eachWith
  , where_
  , limit
  , offset
  , ScopeId (..)
  , formatScope
  , generateScopes
  , RelationBinding (..)
  , FromItem (..)
  , LockStrength (..)
  , LockWait (..)
  , LockClause (..)
  , renderLockClause
  , Projectable (..)
  , renderExpr
  , exprAliases

    -- * Expressions
  , module Hylo.Core.Expr

    -- * Aggregation & Grouping
  , module Hylo.Core.Query.Aggregate

    -- * Window Functions & Framing
  , module Hylo.Core.Query.Window

    -- * Common Table Expressions & Recursive Graphs
  , module Hylo.Core.Query.CTE
  , module Hylo.Core.Query.Recursive

    -- * Keyset Pagination
  , module Hylo.Core.Query.Pagination

    -- * Transaction Monad & Modes
  , TxMode (..)
  , Tx (..)

    -- * Changeset Abstraction
  , Changeset (..)
  , emptyChangeset

    -- * Statically Guarded Mutations
  , insert
  , update
  , delete
  ) where

import Control.DeepSeq (NFData (..))
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Hylo.Core.Context (Ctx (..))
import Hylo.Core.Expr
import Hylo.Core.Key (Id)
import Hylo.Core.Query.AST
  ( FromItem (..)
  , LockClause (..)
  , LockStrength (..)
  , LockWait (..)
  , Projectable (..)
  , RelationBinding (..)
  , ScopeId (..)
  , exprAliases
  , formatScope
  , renderExpr
  , renderLockClause
  )
import Hylo.Core.Query.Aggregate
import Hylo.Core.Query.CTE
import Hylo.Core.Query.Monad
  ( FromRelation (..)
  , Query (..)
  , QueryState (..)
  , eachWith
  , initialQueryState
  , limit
  , offset
  , runQuery
  , where_
  )
import Hylo.Core.Query.Pagination
import Hylo.Core.Query.Recursive
import Hylo.Core.Query.Window
import Hylo.Core.Table (TableSchema)
import Hylo.Core.TypeError (AssertWritable (..), TxMode (..))
import Hylo.PG.Compiler.JoinFlattener (generateScopes)

-- | Phantom type identifying the PostgreSQL database engine.
data Postgres

-- | Atomic transaction computation parameterized by database and mode.
data Tx db (mode :: TxMode) a
  = TxReturn a
  | TxEmpty
  deriving (Eq, Show, Functor)

instance Applicative (Tx db mode) where
  pure = TxReturn
  TxReturn f <*> TxReturn a = TxReturn (f a)
  _ <*> _ = TxEmpty

instance Monad (Tx db mode) where
  TxReturn a >>= f = f a
  TxEmpty >>= _ = TxEmpty

instance NFData a => NFData (Tx db mode a) where
  rnf (TxReturn a) = rnf a
  rnf TxEmpty = ()

-- | Changeset descriptor for table records.
newtype Changeset (table :: Ctx -> Type) = Changeset
  { changesetPatch :: table 'Patch
  }

-- | Construct an empty changeset.
emptyChangeset :: table 'Patch -> Changeset table
emptyChangeset = Changeset

-- | Statically guarded insert operation. Fails with [Hylo Error E005] in ReadOnly mode.
insert ::
  forall table db (mode :: TxMode).
  (AssertWritable mode) =>
  TableSchema table ->
  Changeset table ->
  Tx db mode (table 'Result)
insert _ _ = writableDict (Proxy :: Proxy mode) `seq` TxEmpty

-- | Statically guarded update operation. Fails with [Hylo Error E005] in ReadOnly mode.
update ::
  forall table db (mode :: TxMode).
  (AssertWritable mode) =>
  TableSchema table ->
  Changeset table ->
  Tx db mode (table 'Result)
update _ _ = writableDict (Proxy :: Proxy mode) `seq` TxEmpty

-- | Statically guarded delete operation. Fails with [Hylo Error E005] in ReadOnly mode.
delete ::
  forall table db (mode :: TxMode).
  (AssertWritable mode) =>
  TableSchema table ->
  Id table Int64 ->
  Tx db mode ()
delete _ _ = writableDict (Proxy :: Proxy mode) `seq` TxEmpty
