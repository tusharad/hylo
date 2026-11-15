{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ( -- * Backend Markers & Engine Versions
    PGVersion (..)
  , Postgres
  , SQLite
  , MySQL
  , Redis

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
  , module Hylo.SQL.Query.Aggregate

    -- * Window Functions & Framing
  , module Hylo.SQL.Query.Window

    -- * Common Table Expressions & Recursive Graphs
  , module Hylo.SQL.Query.Recursive

    -- * Keyset Pagination
  , module Hylo.Core.Query.Pagination

    -- * Transaction Monad & Modes
  , module Hylo.Core.Transaction

    -- * Changeset Abstraction
  , module Hylo.Core.Changeset

    -- * Statically Guarded Mutations
  , select
  , insert
  , update
  , delete

    -- * Cross-Backend Dialect Renderers
  , compileStandardSQL
  , compileStandardQuery
  , renderPostgresSQL
  , renderSQLiteSQL
  , renderStandardSQLText
  , previewPostgresSQL
  , previewSQLiteSQL
  , previewStandardSQL
  )
where

import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import Data.Text (Text)
import Hylo.Backend.Capability
  ( MySQL
  , PGVersion (..)
  , Postgres
  , Redis
  , SQLite
  )
import Hylo.Core.Changeset
  ( Changeset (..)
  , OpKind (..)
  , emptyChangeset
  , withUniqueKey
  )
import Hylo.Core.Context (Ctx (..))
import Hylo.Core.Expr
import Hylo.Core.Key (Id)
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
import Hylo.Core.Table (TableSchema (..))
import Hylo.Core.Transaction
import Hylo.Core.TypeError (AssertWritable (..))
import Hylo.SQL.AST
  ( FromItem (..)
  , LockClause (..)
  , LockStrength (..)
  , LockWait (..)
  , Projectable (..)
  , RelationBinding (..)
  , ScopeId (..)
  , exprAliases
  , formatScope
  , generateScopes
  , renderExpr
  , renderLockClause
  )
import Hylo.SQL.Builder
  ( BoundParam (..)
  , CompiledQuery (..)
  , Dialect (..)
  , compileQuery
  , previewQuery
  )
import Hylo.SQL.Query.Aggregate
import Hylo.SQL.Query.Recursive
import Hylo.SQL.Query.Window

-- | Monadic query execution combinator within transactions.
select :: forall k (db :: k) a (mode :: TxMode). Query db a -> Tx db mode a
select q = TxAction $ \st ->
  let (res, _qState) = runQuery q initialQueryState
   in (Right res, st)

-- | Statically guarded insert operation. Fails with [Hylo Error E005] in ReadOnly mode.
insert ::
  forall k (db :: k) table (mode :: TxMode) op.
  (AssertWritable mode) =>
  TableSchema table ->
  Changeset table op ->
  Tx db mode (table 'Result)
insert schema cs =
  writableDict (Proxy :: Proxy mode) `seq`
    TxAction $ \st ->
      let tName = tableName schema
          key = changesetKey cs
       in if key /= "" && elem (tName, key) (txStore st)
            then (Left (TxConstraintViolation tName "unique_constraint" key), st)
            else
              let st' =
                    st
                      { txStore = if key /= "" then (tName, key) : txStore st else txStore st
                      , txOperations = txOperations st ++ [OpMutate ("INSERT INTO " <> tName)]
                      }
               in (Right (errorWithoutStackTrace "insert: simulated entity hydration"), st')

-- | Statically guarded update operation. Fails with [Hylo Error E005] in ReadOnly mode.
update ::
  forall k (db :: k) table (mode :: TxMode) op.
  (AssertWritable mode) =>
  TableSchema table ->
  Changeset table op ->
  Tx db mode (table 'Result)
update schema _ =
  writableDict (Proxy :: Proxy mode) `seq`
    TxAction $ \st ->
      let st' = st {txOperations = txOperations st ++ [OpMutate ("UPDATE " <> tableName schema)]}
       in (Right (errorWithoutStackTrace "update: simulated entity hydration"), st')

-- | Statically guarded delete operation. Fails with [Hylo Error E005] in ReadOnly mode.
delete ::
  forall k (db :: k) table (mode :: TxMode).
  (AssertWritable mode) =>
  TableSchema table ->
  Id table Int64 ->
  Tx db mode ()
delete schema _ =
  writableDict (Proxy :: Proxy mode) `seq`
    TxAction $ \st ->
      let st' = st {txOperations = txOperations st ++ [OpMutate ("DELETE FROM " <> tableName schema)]}
       in (Right (), st')

--------------------------------------------------------------------------------
-- Cross-Backend Dialect Compilers & Renderers
--------------------------------------------------------------------------------

-- | Compile a generic Query into a CompiledQuery with bound parameters.
compileStandardQuery :: (Projectable a) => Dialect -> Query db a -> CompiledQuery
compileStandardQuery = compileQuery

-- | Compile a generic Query into SQL text and strict bound parameters sequence.
compileStandardSQL :: (Projectable a) => Query db a -> (Text, Seq BoundParam)
compileStandardSQL q =
  let cq = compileQuery PostgresDialect q
   in (compiledSQL cq, compiledParams cq)

-- | Generic ANSI SQL query renderer.
renderStandardSQLText :: (Projectable a) => Query db a -> Text
renderStandardSQLText q = compiledSQL (compileQuery PostgresDialect q)

-- | Render a Query into strict Text SQL representation for PostgreSQL.
renderPostgresSQL :: (Projectable a) => Query (Postgres v) a -> Text
renderPostgresSQL q = compiledSQL (compileQuery PostgresDialect q)

-- | Render a Query into strict Text SQL representation for SQLite.
renderSQLiteSQL :: (Projectable a) => Query SQLite a -> Text
renderSQLiteSQL q = compiledSQL (compileQuery SQLiteDialect q)

-- | Generate a debug/preview SQL string by interpolating bound parameters.
previewStandardSQL :: (Projectable a) => Query db a -> Text
previewStandardSQL q = previewQuery (compileQuery PostgresDialect q)

-- | Generate a debug/preview SQL string for PostgreSQL.
previewPostgresSQL :: (Projectable a) => Query (Postgres v) a -> Text
previewPostgresSQL q = previewQuery (compileQuery PostgresDialect q)

-- | Generate a debug/preview SQL string for SQLite.
previewSQLiteSQL :: (Projectable a) => Query SQLite a -> Text
previewSQLiteSQL q = previewQuery (compileQuery SQLiteDialect q)
