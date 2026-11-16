{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
  , leftJoin
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
  , NullableTable (..)
  , LiftMay
  , liftNullableExpr
  , unNullableTable

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

    -- * Parameterized Mutation SQL Generation
  , findPrimaryKeyCol
  , findOptimisticLockCol
  , renderInsertChangesetSQL
  , renderUpdateChangesetSQL

    -- * Statically Guarded Mutations
  , insert
  , update
  , delete
  , deleteById
  , deleteWhere

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

import Data.Dynamic (fromDynamic)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
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
import Hylo.Core.Context (ColDef (..), Ctx (..))
import Hylo.Core.Expr
import Hylo.Core.Key (Id)
import Hylo.Core.Nullable
  ( LiftMay
  , NullableTable (..)
  , liftNullableExpr
  , unNullableTable
  )
import Hylo.Core.Query.Monad
  ( FromRelation (..)
  , Query (..)
  , QueryState (..)
  , eachWith
  , initialQueryState
  , leftJoin
  , limit
  , offset
  , runQuery
  , where_
  )
import Hylo.Core.Query.Pagination
import Hylo.Core.Table
  ( PatchVal (..)
  , Table (..)
  , TableSchema (..)
  , ToFieldValue (..)
  )
import Hylo.Core.Transaction
import Hylo.Core.TypeError (AssertWritable (..), writableDict)
import Hylo.Redis
  ( getDefaultRedisClient
  , redisDel
  , tableCacheKey
  )
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
  ( CompiledQuery (..)
  , Dialect (..)
  , compileExpr
  , compileQuery
  , inferParamType
  , previewQuery
  , quoteIdentifier
  )
import Hylo.SQL.Query.Aggregate
import Hylo.SQL.Query.Recursive
import Hylo.SQL.Query.Window

--------------------------------------------------------------------------------
-- Parameterized Mutation SQL Generation
--------------------------------------------------------------------------------

-- | Discover the primary key column name for a schema. Defaults to "id" if not found.
findPrimaryKeyCol :: (Table table) => TableSchema table -> Text
findPrimaryKeyCol schema =
  case [colName c | c <- tableColDefs (tableMeta schema), colIsPrimaryKey c] of
    (pk : _) -> pk
    [] -> "id"

-- | Discover the optimistic lock column name for a schema, if any.
findOptimisticLockCol :: (Table table) => TableSchema table -> Maybe Text
findOptimisticLockCol schema =
  case [colName c | c <- tableColDefs (tableMeta schema), colIsOptimisticLock c] of
    (c : _) -> Just c
    [] -> Nothing

{- | Render a parameterized UPDATE SQL statement setting only modified fields.

Omits 'Unchanged' fields entirely. Binds parameters for 'SetTo' values and emits
'DEFAULT' for 'SetDefault'. Appends the primary key predicate and ends with 'RETURNING *'.
Uses table metadata (colName) without GHC.Generics.
-}
renderUpdateChangesetSQL ::
  forall table op.
  (Table table) =>
  TableSchema table ->
  Changeset table op ->
  (Text, [BoundParam])
renderUpdateChangesetSQL schema cs =
  let tbl = quoteIdentifier PostgresDialect (tableName schema)
      pkCol = findPrimaryKeyCol schema
      optLockInfo = case changesetOptimisticLock cs of
        Just (c, v) ->
          let cName = if c /= "" then c else fromMaybe "version" (findOptimisticLockCol schema)
           in Just (cName, v)
        Nothing -> Nothing
      entries = patchEntries (tableMeta schema) (changesetPatch cs)
      -- Exclude primary key and optimistic lock version column from SET clause
      setEntries =
        filter
          ( \(c, _) ->
              colName c /= pkCol && Just (colName c) /= fmap fst optLockInfo
          )
          entries
      pkVal =
        if changesetKey cs /= "" && changesetKey cs /= tableName schema
          then changesetKey cs
          else case [v | (c, ValSetTo v) <- entries, colName c == pkCol] of
            (v : _) -> v
            [] -> "1"

      -- Fold SET entries with monotonically increasing parameter index
      buildSetClauses [] idx = ([], [], idx)
      buildSetClauses ((colDef, val) : rest) idx =
        let qCol = quoteIdentifier PostgresDialect (colName colDef)
         in case val of
              ValSetTo valText ->
                let clause = qCol <> " = $" <> T.pack (show idx)
                    param = BoundParam idx valText (inferParamType valText)
                    (restClauses, restParams, finalIdx) = buildSetClauses rest (idx + 1)
                 in (clause : restClauses, param : restParams, finalIdx)
              ValSetDefault ->
                let clause = qCol <> " = DEFAULT"
                    (restClauses, restParams, finalIdx) = buildSetClauses rest idx
                 in (clause : restClauses, restParams, finalIdx)

      (initialSetClauses, setParams, nextIdx) = buildSetClauses setEntries 1
      setClauses = case optLockInfo of
        Just (verCol, _) ->
          let qVerCol = quoteIdentifier PostgresDialect verCol
           in initialSetClauses ++ [qVerCol <> " = " <> qVerCol <> " + 1"]
        Nothing -> initialSetClauses

      qPkCol = quoteIdentifier PostgresDialect pkCol
      baseWhere = qPkCol <> " = $" <> T.pack (show nextIdx)
      pkParam = BoundParam nextIdx pkVal "integer"

      (whereClause, allParams) = case optLockInfo of
        Just (verCol, expectedVer) ->
          let qVerCol = quoteIdentifier PostgresDialect verCol
              verIdx = nextIdx + 1
              verWhere = qVerCol <> " = $" <> T.pack (show verIdx)
              verParam = BoundParam verIdx (toFieldValue expectedVer) "bigint"
           in (" WHERE " <> baseWhere <> " AND " <> verWhere, setParams ++ [pkParam, verParam])
        Nothing ->
          (" WHERE " <> baseWhere, setParams ++ [pkParam])

      sql = case setClauses of
        [] ->
          "UPDATE " <> tbl <> " SET " <> qPkCol <> " = " <> qPkCol <> whereClause <> " RETURNING *"
        _ ->
          "UPDATE " <> tbl <> " SET " <> T.intercalate ", " setClauses <> whereClause <> " RETURNING *"
   in (sql, allParams)

{- | Render a parameterized INSERT SQL statement for non-Unchanged fields.

Binds parameters for 'SetTo' values and emits 'DEFAULT' for 'SetDefault'.
Ends with 'RETURNING *'. Uses table metadata (colName) without GHC.Generics.
-}
renderInsertChangesetSQL ::
  forall table op.
  (Table table) =>
  TableSchema table ->
  Changeset table op ->
  (Text, [BoundParam])
renderInsertChangesetSQL schema cs =
  let tbl = quoteIdentifier PostgresDialect (tableName schema)
      entries = patchEntries (tableMeta schema) (changesetPatch cs)

      buildInsertClauses [] _ = ([], [], [])
      buildInsertClauses ((colDef, val) : rest) idx =
        let qCol = quoteIdentifier PostgresDialect (colName colDef)
         in case val of
              ValSetTo valText ->
                let valExpr = "$" <> T.pack (show idx)
                    param = BoundParam idx valText (inferParamType valText)
                    (restCols, restVals, restParams) = buildInsertClauses rest (idx + 1)
                 in (qCol : restCols, valExpr : restVals, param : restParams)
              ValSetDefault ->
                let valExpr = "DEFAULT"
                    (restCols, restVals, restParams) = buildInsertClauses rest idx
                 in (qCol : restCols, valExpr : restVals, restParams)

      (cols, vals, params) = buildInsertClauses entries 1

      sql = case cols of
        [] -> "INSERT INTO " <> tbl <> " DEFAULT VALUES RETURNING *"
        _ ->
          "INSERT INTO "
            <> tbl
            <> " ("
            <> T.intercalate ", " cols
            <> ") VALUES ("
            <> T.intercalate ", " vals
            <> ") RETURNING *"
   in (sql, params)

--------------------------------------------------------------------------------
-- Statically Guarded Mutations
--------------------------------------------------------------------------------

-- | Statically guarded insert operation. Fails with [Hylo Error E005] in ReadOnly mode.
insert ::
  forall k (db :: k) table (mode :: TxMode) op.
  (AssertWritable mode, Table table) =>
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
              let (sql, params) = renderInsertChangesetSQL schema cs
                  st' =
                    st
                      { txStore = if key /= "" then (tName, key) : txStore st else txStore st
                      , txOperations = txOperations st ++ [OpMutate (sql, params)]
                      }
               in (Right (errorWithoutStackTrace "insert: simulated entity hydration"), st')

-- | Attempt to extract a primary key literal value from a boolean predicate expression.
extractKeyFromPredicate :: Text -> QExpr Bool -> Maybe Text
extractKeyFromPredicate pkCol = \case
  BinOpExpr "=" (ColExpr _ c) (LitExpr val) | c == pkCol -> Just val
  BinOpExpr "=" (ColExpr _ c) (LitTypedExpr val _) | c == pkCol -> Just val
  BinOpExpr "=" (LitExpr val) (ColExpr _ c) | c == pkCol -> Just val
  BinOpExpr "=" (LitTypedExpr val _) (ColExpr _ c) | c == pkCol -> Just val
  _ -> Nothing

-- | Statically guarded update operation. Fails with [Hylo Error E005] in ReadOnly mode.
update ::
  forall k (db :: k) table (mode :: TxMode) op.
  (AssertWritable mode, Table table) =>
  TableSchema table ->
  Changeset table op ->
  Tx db mode (table 'Result)
update schema cs =
  writableDict (Proxy :: Proxy mode) `seq`
    TxAction $ \st ->
      let (sql, params) = renderUpdateChangesetSQL schema cs
          key = changesetKey cs
          tName = tableName schema
          pkCol = findPrimaryKeyCol schema
          entries = patchEntries (tableMeta schema) (changesetPatch cs)
          pkVal =
            if key /= "" && key /= tName
              then key
              else case [v | (c, ValSetTo v) <- entries, colName c == pkCol] of
                (v : _) -> v
                [] -> if key /= "" then key else "1"
          mTxClient = lookup "redis_client" (txCustomExtensions st) >>= fromDynamic
          cacheHooks = case tableCachePolicy schema of
            Just policy ->
              let cacheKey = tableCacheKey policy schema pkVal
                  hook = case mTxClient of
                    Just client -> redisDel client cacheKey
                    Nothing -> do
                      mDefault <- getDefaultRedisClient
                      case mDefault of
                        Just client -> redisDel client cacheKey
                        Nothing -> pure ()
               in [hook]
            Nothing -> []
          st' =
            st
              { txOperations = txOperations st ++ [OpMutate (sql, params)]
              , txCommitHooks = txCommitHooks st ++ cacheHooks
              }
       in (Right (errorWithoutStackTrace "update: simulated entity hydration"), st')

-- | Statically guarded delete operation. Fails with [Hylo Error E005] in ReadOnly mode.
delete ::
  forall k (db :: k) table (mode :: TxMode).
  (AssertWritable mode, Table table) =>
  TableSchema table ->
  Id table Int64 ->
  Tx db mode ()
delete = deleteById

-- | Statically guarded delete operation by key with custom representation.
deleteById ::
  forall k (db :: k) table (mode :: TxMode) a.
  (AssertWritable mode, Table table, ToFieldValue (Id table a)) =>
  TableSchema table ->
  Id table a ->
  Tx db mode ()
deleteById schema pkId =
  writableDict (Proxy :: Proxy mode) `seq`
    TxAction $ \st ->
      let tName = quoteIdentifier PostgresDialect (tableName schema)
          pkVal = toFieldValue pkId
          (pkCol, pkType) = case [(colName c, colSqlType c) | c <- tableColDefs (tableMeta schema), colIsPrimaryKey c] of
            ((c, t) : _) ->
              ( quoteIdentifier PostgresDialect c
              , if T.null t then inferParamType pkVal else t
              )
            [] -> (quoteIdentifier PostgresDialect "id", inferParamType pkVal)
          sql = "DELETE FROM " <> tName <> " WHERE " <> pkCol <> " = $1"
          params = [BoundParam 1 pkVal pkType]
          mTxClient = lookup "redis_client" (txCustomExtensions st) >>= fromDynamic
          cacheHooks = case tableCachePolicy schema of
            Just policy ->
              let key = tableCacheKey policy schema pkVal
                  hook = case mTxClient of
                    Just client -> redisDel client key
                    Nothing -> do
                      mDefault <- getDefaultRedisClient
                      case mDefault of
                        Just client -> redisDel client key
                        Nothing -> pure ()
               in [hook]
            Nothing -> []
          st' =
            st
              { txOperations = txOperations st ++ [OpMutate (sql, params)]
              , txCommitHooks = txCommitHooks st ++ cacheHooks
              }
       in (Right (), st')

-- | Statically guarded delete operation with a predicate expression.
deleteWhere ::
  forall k (db :: k) table (mode :: TxMode).
  (AssertWritable mode, Table table) =>
  TableSchema table ->
  (table 'Expr -> QExpr Bool) ->
  Tx db mode ()
deleteWhere schema predicate =
  writableDict (Proxy :: Proxy mode) `seq`
    TxAction $ \st ->
      let tName = quoteIdentifier PostgresDialect (tableName schema)
          exprTable = toTableExpr "" (tableMeta schema)
          predExpr = predicate exprTable
          (whereSql, paramsSeq) = compileExpr PostgresDialect predExpr
          params = toList paramsSeq
          sql = "DELETE FROM " <> tName <> (if T.null whereSql then "" else " WHERE " <> whereSql)
          pkCol = findPrimaryKeyCol schema
          mExtractedKey = extractKeyFromPredicate pkCol predExpr
          mTxClient = lookup "redis_client" (txCustomExtensions st) >>= fromDynamic
          cacheHooks = case (tableCachePolicy schema, mExtractedKey) of
            (Just policy, Just k) ->
              let key = tableCacheKey policy schema k
                  hook = case mTxClient of
                    Just client -> redisDel client key
                    Nothing -> do
                      mDefault <- getDefaultRedisClient
                      case mDefault of
                        Just client -> redisDel client key
                        Nothing -> pure ()
               in [hook]
            _ -> []
          st' =
            st
              { txOperations = txOperations st ++ [OpMutate (sql, params)]
              , txCommitHooks = txCommitHooks st ++ cacheHooks
              }
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
