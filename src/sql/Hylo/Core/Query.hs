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
  , renderPostgresSQL
  , renderSQLiteSQL
  , renderStandardSQLText
  )
where

import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
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
  , LateralBinding (..)
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
import Hylo.SQL.Compiler.JoinFlattener (flattenJoins, renderFromItem)
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
-- Cross-Backend Dialect Renderers
--------------------------------------------------------------------------------

-- | Generic ANSI SQL query renderer.
renderStandardSQLText :: (Projectable a) => Query db a -> Text
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
renderPostgresSQL :: (Projectable a) => Query (Postgres v) a -> Text
renderPostgresSQL = renderStandardSQLText

-- | Render a Query into strict Text SQL representation for SQLite.
renderSQLiteSQL :: (Projectable a) => Query SQLite a -> Text
renderSQLiteSQL = renderStandardSQLText
