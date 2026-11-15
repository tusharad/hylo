{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.SQL.Builder
Description : Parameterized multi-dialect SQL builder with strict parameter accumulation
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Compiles Hylo Query ASTs into parameterized SQL across PostgreSQL, MySQL, and SQLite dialects.
Emits dialect-specific bind parameter placeholders ($1, ?, ?1) via a strict 'Seq BoundParam'
accumulator, quotes identifiers according to dialect conventions ("col" vs `col`), and maintains
a monotonic scope counter (t0, t1) for deterministic, collision-free table aliasing and self-joins.
-}
module Hylo.SQL.Builder
  ( -- * Dialects & Backend Markers
    Dialect (..)
  , IsDialect (..)

    -- * Bound Parameters
  , BoundParam (..)
  , mkBoundParam
  , cleanParamValue
  , inferParamType

    -- * Builder State & Monad
  , SqlBuilderState (..)
  , initialBuilderState
  , SqlBuilderM
  , runSqlBuilderM
  , runSqlBuilderWithState
  , evalSqlBuilderM
  , execSqlBuilderM

    -- * Scope Management & Table Aliasing
  , freshScopeAlias
  , allocateScopeAlias
  , resolveRelationAlias

    -- * Identifiers & Placeholders
  , quoteIdentifier
  , quoteScopedCol
  , bindPlaceholder

    -- * Expressions & AST Compilation
  , buildExpr
  , compileExpr
  , buildFromItem
  , compileFromItem

    -- * Query Compilation
  , CompiledQuery (..)
  , compileQuery
  , compileQueryAST
  , previewQuery
  , previewQueryForDialect
  , formatParamValue
  , interpolateMarker
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (forM)
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import qualified Data.Text.Lazy.Builder as B
import GHC.Generics (Generic)
import Hylo.Backend.Capability
  ( MySQL
  , Postgres
  , SQLite
  )
import Hylo.Core.Expr (QExpr (..))
import Hylo.Core.Query.Monad (Query (..), QueryState (..), initialQueryState)
import Hylo.Core.Transaction (BoundParam (..), mkBoundParam)
import Hylo.SQL.AST
  ( CTEBinding (..)
  , CTEType (..)
  , CycleClause (..)
  , FromItem (..)
  , LateralBinding (..)
  , Materialization (..)
  , Projectable (..)
  , RelationBinding (..)
  , renderLockClause
  )
import Hylo.SQL.Compiler.JoinFlattener (flattenJoins)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- 1. Dialects & Backend Markers
--------------------------------------------------------------------------------

-- | Target database SQL dialects supported by the query compiler.
data Dialect
  = PostgresDialect
  | MySQLDialect
  | SQLiteDialect
  deriving (Eq, Show, Enum, Bounded, Generic)

instance NFData Dialect

-- | Typeclass associating backend phantom types with their corresponding SQL dialect.
class IsDialect db where
  dialectOf :: Proxy db -> Dialect

instance IsDialect (Postgres v) where
  dialectOf _ = PostgresDialect

instance IsDialect Postgres where
  dialectOf _ = PostgresDialect

instance IsDialect MySQL where
  dialectOf _ = MySQLDialect

instance IsDialect SQLite where
  dialectOf _ = SQLiteDialect

--------------------------------------------------------------------------------
-- 2. Bound Parameters
--------------------------------------------------------------------------------

-- | Strip surrounding quotes introduced by 'Show' on string literals.
cleanParamValue :: Text -> Text
cleanParamValue t = case T.uncons t of
  Just ('"', rest) -> case T.unsnoc rest of
    Just (middle, '"') -> middle
    _ -> t
  _ -> t

-- | Infer a coarse SQL logical type for a literal string value.
inferParamType :: Text -> Text
inferParamType raw
  | raw == "True" || raw == "False" = "boolean"
  | not (T.null raw) && T.all isDigit raw = "integer"
  | not (T.null raw) && T.isInfixOf "." raw && T.all (\c -> isDigit c || c == '.') raw = "numeric"
  | otherwise = "text"

--------------------------------------------------------------------------------
-- 3. Identifiers & Placeholders
--------------------------------------------------------------------------------

-- | Quote a table or column identifier according to dialect rules.
quoteIdentifier :: Dialect -> Text -> Text
quoteIdentifier d name
  | name == "*" = "*"
  | otherwise = case d of
      MySQLDialect -> "`" <> name <> "`"
      _ -> "\"" <> name <> "\""

-- | Quote a qualified relation and column identifier according to dialect rules.
quoteScopedCol :: Dialect -> Text -> Text -> Text
quoteScopedCol d rel col
  | T.null rel = quoteIdentifier d col
  | rel == "EXCLUDED" = case d of
      MySQLDialect -> "EXCLUDED.`" <> col <> "`"
      _ -> "EXCLUDED.\"" <> col <> "\""
  | otherwise = quoteIdentifier d rel <> "." <> quoteIdentifier d col

-- | Generate a dialect-specific bind parameter placeholder for a 1-based index.
bindPlaceholder :: Dialect -> Int -> Text
bindPlaceholder d idx = case d of
  PostgresDialect -> "$" <> T.pack (show idx)
  MySQLDialect -> "?"
  SQLiteDialect -> "?" <> T.pack (show idx)

--------------------------------------------------------------------------------
-- 4. Builder State & Monad
--------------------------------------------------------------------------------

-- | State accumulator tracking SQL emission, bind parameters, and table scope aliases.
data SqlBuilderState = SqlBuilderState
  { sbsDialect :: !Dialect
  , sbsParamCount :: !Int
  , sbsParams :: !(Seq BoundParam)
  , sbsScopeCount :: !Int
  , sbsAliasMap :: !(Map Text Text)
  }
  deriving (Eq, Show, Generic)

instance NFData SqlBuilderState where
  rnf (SqlBuilderState d pc ps sc am) =
    rnf d `seq` rnf pc `seq` rnf (toList ps) `seq` rnf sc `seq` rnf am

-- | Initial builder state for a given dialect.
initialBuilderState :: Dialect -> SqlBuilderState
initialBuilderState d =
  SqlBuilderState
    { sbsDialect = d
    , sbsParamCount = 0
    , sbsParams = Seq.empty
    , sbsScopeCount = 0
    , sbsAliasMap = Map.empty
    }

-- | State monad for SQL query compilation.
type SqlBuilderM = State SqlBuilderState

-- | Run a builder computation starting with a fresh state for a dialect.
runSqlBuilderM :: Dialect -> SqlBuilderM a -> (a, SqlBuilderState)
runSqlBuilderM d act = runState act (initialBuilderState d)

-- | Run a builder computation with an explicitly provided initial builder state.
runSqlBuilderWithState :: SqlBuilderState -> SqlBuilderM a -> (a, SqlBuilderState)
runSqlBuilderWithState st act = runState act st

-- | Evaluate a builder computation, discarding the final state.
evalSqlBuilderM :: Dialect -> SqlBuilderM a -> a
evalSqlBuilderM d act = fst (runSqlBuilderM d act)

-- | Execute a builder computation, returning the accumulated state.
execSqlBuilderM :: Dialect -> SqlBuilderM a -> SqlBuilderState
execSqlBuilderM d act = snd (runSqlBuilderM d act)

--------------------------------------------------------------------------------
-- 5. Scope Management & Table Aliasing (t0, t1, ...)
--------------------------------------------------------------------------------

-- | Generate an anonymous fresh table scope alias (e.g. t0, t1, t2).
freshScopeAlias :: SqlBuilderM Text
freshScopeAlias = do
  st <- get
  let idx = sbsScopeCount st
      alias = "t" <> T.pack (show idx)
  put st {sbsScopeCount = idx + 1}
  pure alias

-- | Allocate or retrieve a deterministic alias (t0, t1...) mapped to an input relation identifier.
allocateScopeAlias :: Text -> SqlBuilderM Text
allocateScopeAlias orig = do
  st <- get
  case Map.lookup orig (sbsAliasMap st) of
    Just mapped -> pure mapped
    Nothing -> do
      let idx = sbsScopeCount st
          newAlias = "t" <> T.pack (show idx)
      put
        st
          { sbsScopeCount = idx + 1
          , sbsAliasMap = Map.insert orig newAlias (sbsAliasMap st)
          }
      pure newAlias

-- | Resolve an alias from the active scope mapping, falling back to original if unmapped.
resolveRelationAlias :: Text -> SqlBuilderM Text
resolveRelationAlias orig = do
  st <- get
  case Map.lookup orig (sbsAliasMap st) of
    Just mapped -> pure mapped
    Nothing -> pure orig

--------------------------------------------------------------------------------
-- 6. Expression Compilation
--------------------------------------------------------------------------------

-- | Build a typed QExpr into SQL Text Builder, accumulating bind parameters.
buildExpr :: Dialect -> QExpr a -> SqlBuilderM Builder
buildExpr d = \case
  LitExpr t -> case T.toLower t of
    "true" -> pure "TRUE"
    "false" -> pure "FALSE"
    "null" -> pure "NULL"
    _ -> do
      st <- get
      let nextIdx = sbsParamCount st + 1
          val = cleanParamValue t
          pType = inferParamType val
          param = BoundParam nextIdx val pType
          ph = bindPlaceholder d nextIdx
      put
        st
          { sbsParamCount = nextIdx
          , sbsParams = sbsParams st |> param
          }
      pure (B.fromText ph)
  ColExpr rel col -> do
    mappedRel <- resolveRelationAlias rel
    pure (B.fromText (quoteScopedCol d mappedRel col))
  BinOpExpr op l r -> do
    case (op, r) of
      ("=", LitExpr t) | T.toLower t == "null" -> do
        lB <- buildExpr d l
        pure ("(" <> lB <> " IS NULL)")
      _ -> do
        lB <- buildExpr d l
        rB <- buildExpr d r
        pure ("(" <> lB <> " " <> B.fromText op <> " " <> rB <> ")")
  UnaryOpExpr op e -> do
    eB <- buildExpr d e
    if op == "IS NULL" || op == "IS NOT NULL"
      then pure ("(" <> eB <> " " <> B.fromText op <> ")")
      else pure ("(" <> B.fromText op <> " " <> eB <> ")")
  LiftNullableExpr e -> buildExpr d e
  CoalesceExpr m dflt -> do
    mB <- buildExpr d m
    dB <- buildExpr d dflt
    pure ("COALESCE(" <> mB <> ", " <> dB <> ")")
  AggOpExpr fn arg mFilter -> do
    argB <- buildExpr d arg
    let base = B.fromText fn <> "(" <> argB <> ")"
    case mFilter of
      Nothing -> pure base
      Just cond -> do
        condB <- buildExpr d cond
        pure (base <> " FILTER (WHERE " <> condB <> ")")
  StringAggExpr arg delim mFilter -> do
    argB <- buildExpr d arg
    delimB <- buildExpr d delim
    let base = "string_agg(" <> argB <> ", " <> delimB <> ")"
    case mFilter of
      Nothing -> pure base
      Just cond -> do
        condB <- buildExpr d cond
        pure (base <> " FILTER (WHERE " <> condB <> ")")
  RowCmpExpr op cols _ vals _ -> do
    st <- get
    let rewrittenCols = map (rewriteProjection d (sbsAliasMap st)) cols
        lhs = "(" <> T.intercalate ", " rewrittenCols <> ")"
    phs <- forM vals $ \v -> do
      stCur <- get
      let nextIdx = sbsParamCount stCur + 1
          val = cleanParamValue v
          pType = inferParamType val
          param = BoundParam nextIdx val pType
          ph = bindPlaceholder d nextIdx
      put
        stCur
          { sbsParamCount = nextIdx
          , sbsParams = sbsParams stCur |> param
          }
      pure ph
    let rhs = "(" <> T.intercalate ", " phs <> ")"
    pure (B.fromText ("(" <> lhs <> " " <> op <> " " <> rhs <> ")"))
  InListExpr isIn e items -> do
    eB <- buildExpr d e
    let op = if isIn then "IN" else "NOT IN"
    if null items
      then pure (if isIn then "(1 = 0)" else "(1 = 1)")
      else do
        phs <- forM items $ \item -> do
          stCur <- get
          let nextIdx = sbsParamCount stCur + 1
              val = cleanParamValue item
              pType = inferParamType val
              param = BoundParam nextIdx val pType
              ph = bindPlaceholder d nextIdx
          put
            stCur
              { sbsParamCount = nextIdx
              , sbsParams = sbsParams stCur |> param
              }
          pure ph
        let rhs = "(" <> T.intercalate ", " phs <> ")"
        pure ("(" <> eB <> " " <> B.fromText op <> " " <> B.fromText rhs <> ")")

-- | Purely compile a standalone QExpr into SQL Text and accumulated parameters.
compileExpr :: Dialect -> QExpr a -> (Text, Seq BoundParam)
compileExpr d expr =
  let (b, st) = runSqlBuilderM d (buildExpr d expr)
   in (toStrict (toLazyText b), sbsParams st)

--------------------------------------------------------------------------------
-- 7. FromItem Compilation (ANSI Join Tree & Scopes)
--------------------------------------------------------------------------------

-- | Compile an ANSI SQL FROM tree item, registering deterministic t0, t1 aliases.
buildFromItem :: Dialect -> FromItem -> SqlBuilderM Builder
buildFromItem d = \case
  FromTable t origAlias -> do
    mappedAlias <- allocateScopeAlias origAlias
    let tQuoted = quoteIdentifier d t
        aQuoted = quoteIdentifier d mappedAlias
    pure (B.fromText (tQuoted <> " AS " <> aQuoted))
  InnerJoin left right onCond -> do
    lB <- buildFromItem d left
    rB <- buildFromItem d right
    condB <- buildExpr d onCond
    pure (lB <> " INNER JOIN " <> rB <> " ON " <> condB)
  LeftJoin left right onCond -> do
    lB <- buildFromItem d left
    rB <- buildFromItem d right
    condB <- buildExpr d onCond
    pure (lB <> " LEFT JOIN " <> rB <> " ON " <> condB)
  LeftJoinLateral left subquerySql latAlias mAST -> do
    lB <- buildFromItem d left
    mappedLat <- allocateScopeAlias latAlias
    let aQuoted = quoteIdentifier d mappedLat
    subqueryB <- case mAST of
      Just (aggExpr, childSt) -> buildLateralSubquery d aggExpr childSt
      Nothing -> pure (B.fromText subquerySql)
    pure
      ( lB
          <> " LEFT JOIN LATERAL ("
          <> subqueryB
          <> ") AS "
          <> B.fromText aQuoted
          <> " ON TRUE"
      )
  CrossJoin left right -> do
    lB <- buildFromItem d left
    rB <- buildFromItem d right
    pure (lB <> " CROSS JOIN " <> rB)

-- | Purely compile a standalone FromItem into SQL Text.
compileFromItem :: Dialect -> FromItem -> Text
compileFromItem d item =
  let (b, _) = runSqlBuilderM d (buildFromItem d item)
   in toStrict (toLazyText b)

--------------------------------------------------------------------------------
-- 8. Query Compilation
--------------------------------------------------------------------------------

-- | Complete compiled query bundle pairing SQL text with strict bound parameters.
data CompiledQuery = CompiledQuery
  { compiledSQL :: !Text
  , compiledParams :: !(Seq BoundParam)
  }
  deriving (Eq, Show, Generic)

instance NFData CompiledQuery where
  rnf (CompiledQuery s p) = rnf s `seq` rnf (toList p)

-- | Compile a monadic Query computation into dialect-specific SQL with bound parameters.
compileQuery :: forall db a. (Projectable a) => Dialect -> Query db a -> CompiledQuery
compileQuery d q =
  let (res, st) = runQuery q initialQueryState
      rawProjections = projectColumns res
   in compileQueryAST d rawProjections st

-- | Compile a Query AST state and projection expressions into a CompiledQuery.
compileQueryAST :: Dialect -> [Text] -> QueryState -> CompiledQuery
compileQueryAST d rawProjections st =
  case qsUpsert st of
    Just (upsertSql, params) ->
      CompiledQuery
        { compiledSQL = upsertSql
        , compiledParams = Seq.fromList params
        }
    Nothing ->
      case qsCustomQuery st of
        Just (customSql, params) ->
          CompiledQuery
            { compiledSQL = customSql
            , compiledParams = Seq.fromList params
            }
        Nothing ->
          let act = buildQueryParts d rawProjections st
              (sqlBuilder, finalSt) = runSqlBuilderM d act
           in CompiledQuery
                { compiledSQL = toStrict (toLazyText sqlBuilder)
                , compiledParams = sbsParams finalSt
                }

-- | Internal builder composing all SQL clauses with dialect quoting and scope mapping.
buildQueryParts :: Dialect -> [Text] -> QueryState -> SqlBuilderM Builder
buildQueryParts d rawProjections st = do
  -- 0. CTE clause (compiled inside SqlBuilderM, sharing parameters!)
  cteClause <- buildCTEs d (qsCTEs st)

  -- 1. Flatten joins and build FROM clause (this assigns t0, t1... in sbsAliasMap)
  let (mFrom, wherePreds) = flattenJoins (qsRelations st) (qsPredicates st)

  mFromB <- case mFrom of
    Nothing -> case qsLaterals st of
      [] -> pure Nothing
      (l : ls) -> do
        let base = FromTable (lbAlias l) (lbAlias l)
            item = foldl (\acc (LateralBinding a sql mast) -> LeftJoinLateral acc sql a mast) base ls
        Just <$> buildFromItem d item
    Just baseItem -> do
      let withLats =
            foldl (\acc (LateralBinding a sql mast) -> LeftJoinLateral acc sql a mast) baseItem (qsLaterals st)
      Just <$> buildFromItem d withLats

  -- 2. Build Projections (rewriting column aliases to t0, t1... and applying dialect quoting)
  builderState <- get
  let projTexts = map (rewriteProjection d (sbsAliasMap builderState)) rawProjections
      selectClause =
        "SELECT " <> B.fromText (if null projTexts then "1" else T.intercalate ", " projTexts)

  let fromClause = case mFromB of
        Nothing -> ""
        Just fb -> " FROM " <> fb

  -- 3. Build WHERE clause
  whereClause <- case wherePreds of
    [] -> pure ""
    ps -> do
      condBuilders <- mapM (buildExpr d) ps
      let combined = case condBuilders of
            [] -> ""
            [c] -> c
            (c : cs) -> foldl (\acc next -> acc <> " AND " <> next) c cs
      pure (" WHERE " <> combined)

  -- 4. GROUP BY clause
  let groupByClause = case qsGroupBy st of
        [] -> ""
        cols ->
          let rewrittenCols = map (rewriteProjection d (sbsAliasMap builderState)) cols
           in " GROUP BY " <> B.fromText (T.intercalate ", " rewrittenCols)

  -- 5. HAVING clause
  havingClause <- case qsHaving st of
    [] -> pure ""
    hs -> do
      hBuilders <- mapM (buildExpr d) hs
      let combined = case hBuilders of
            [] -> ""
            [h] -> h
            (h : cs) -> foldl (\acc next -> acc <> " AND " <> next) h cs
      pure (" HAVING " <> combined)

  -- 6. ORDER BY clause
  let orderByClause = case qsOrderBy st of
        [] -> ""
        orders ->
          let rewrittenOrders = map (rewriteProjection d (sbsAliasMap builderState)) orders
           in " ORDER BY " <> B.fromText (T.intercalate ", " rewrittenOrders)

  -- 7. LIMIT & OFFSET clauses
  let limitOffsetClause = case d of
        MySQLDialect -> case (qsLimit st, qsOffset st) of
          (Nothing, Nothing) -> ""
          (Just lim, Nothing) -> " LIMIT " <> B.fromText (T.pack (show lim))
          (Nothing, Just off) -> " LIMIT 18446744073709551615 OFFSET " <> B.fromText (T.pack (show off))
          (Just lim, Just off) -> " LIMIT " <> B.fromText (T.pack (show lim)) <> " OFFSET " <> B.fromText (T.pack (show off))
        _ ->
          let limitClause = case qsLimit st of
                Nothing -> ""
                Just n -> " LIMIT " <> B.fromText (T.pack (show n))
              offsetClause = case qsOffset st of
                Nothing -> ""
                Just n -> " OFFSET " <> B.fromText (T.pack (show n))
           in limitClause <> offsetClause

  -- 8. Lock clause
  let lockClause = case qsLock st of
        Nothing -> ""
        Just lk -> " " <> B.fromText (renderLockClause lk)

  pure
    ( cteClause
        <> selectClause
        <> fromClause
        <> whereClause
        <> groupByClause
        <> havingClause
        <> orderByClause
        <> limitOffsetClause
        <> lockClause
    )

-- | Build a correlated LATERAL subquery inside SqlBuilderM, sharing parameters and scopes.
buildLateralSubquery :: Dialect -> Text -> QueryState -> SqlBuilderM Builder
buildLateralSubquery d aggExpr childSt = do
  builderSt <- get
  let childRelAliases = [rbAlias r | r <- qsRelations childSt]
      childAliasMap = foldr (\a m -> Map.insert a a m) (sbsAliasMap builderSt) childRelAliases
  put builderSt {sbsAliasMap = childAliasMap}

  let (mFrom, wherePreds) = flattenJoins (qsRelations childSt) (qsPredicates childSt)

  mFromB <- case mFrom of
    Nothing -> case qsLaterals childSt of
      [] -> pure Nothing
      (l : ls) -> do
        let base = FromTable (lbAlias l) (lbAlias l)
            item = foldl (\acc (LateralBinding a sql mast) -> LeftJoinLateral acc sql a mast) base ls
        Just <$> buildFromItem d item
    Just baseItem -> do
      let withLats =
            foldl
              (\acc (LateralBinding a sql mast) -> LeftJoinLateral acc sql a mast)
              baseItem
              (qsLaterals childSt)
      Just <$> buildFromItem d withLats

  whereClause <- case wherePreds of
    [] -> pure ""
    ps -> do
      condBuilders <- mapM (buildExpr d) ps
      let combined = case condBuilders of
            [] -> ""
            [c] -> c
            (c : cs) -> foldl (\acc next -> acc <> " AND " <> next) c cs
      pure (" WHERE " <> combined)

  let fromClause = case mFromB of
        Nothing -> ""
        Just fb -> " FROM " <> fb

  finalSt <- get
  put finalSt {sbsAliasMap = sbsAliasMap builderSt}

  pure ("SELECT " <> B.fromText aggExpr <> fromClause <> whereClause)

-- | Compile a list of CTE bindings within SqlBuilderM, sharing parameter and scope counters.
buildCTEs :: Dialect -> [CTEBinding] -> SqlBuilderM Builder
buildCTEs _ [] = pure ""
buildCTEs d ctes = do
  let isRecursive = any isRec ctes
      prefix = if isRecursive then "WITH RECURSIVE " else "WITH "
  rendered <- forM ctes (buildCTEBinding d)
  let joined = case rendered of
        [] -> ""
        [b] -> b
        (b : bs) -> foldl (\acc next -> acc <> ",\n" <> next) b bs
  pure (B.fromText prefix <> joined <> " ")
  where
    isRec (CTEBinding _ def) = case def of
      RecursiveCTE {} -> True
      _ -> False

-- | Compile an individual CTE definition inside SqlBuilderM.
buildCTEBinding :: Dialect -> CTEBinding -> SqlBuilderM Builder
buildCTEBinding d (CTEBinding _ def) = case def of
  NonRecursiveCTE mat name fallbackSql mAST -> do
    let matStr = case mat of
          DefaultMaterialization -> ""
          Materialized -> "MATERIALIZED "
          NotMaterialized -> "NOT MATERIALIZED "
    bodyB <- case mAST of
      Just (projs, cteSt) -> buildCTEQueryParts d projs cteSt
      Nothing -> pure (B.fromText fallbackSql)
    pure ("\"" <> B.fromText name <> "\" AS " <> B.fromText matStr <> "(\n  " <> bodyB <> "\n)")
  RecursiveCTE name baseFallbackSql recFallbackSql mCycle mAST -> do
    let cycleStr = case mCycle of
          Nothing -> ""
          Just (CycleClause c s u) ->
            " CYCLE \"" <> c <> "\" SET \"" <> s <> "\" USING \"" <> u <> "\""
    (baseB, recB) <- case mAST of
      Just ((baseProjs, baseSt), (recProjs, recSt)) -> do
        bB <- buildCTEQueryParts d baseProjs baseSt
        rB <- buildCTEQueryParts d recProjs recSt
        pure (bB, rB)
      Nothing ->
        pure (B.fromText baseFallbackSql, B.fromText recFallbackSql)
    pure
      ( "\""
          <> B.fromText name
          <> "\" AS (\n  "
          <> baseB
          <> "\n  UNION ALL\n  "
          <> recB
          <> "\n)"
          <> B.fromText cycleStr
      )

-- | Compile inner query parts of a CTE definition inside SqlBuilderM.
buildCTEQueryParts :: Dialect -> [Text] -> QueryState -> SqlBuilderM Builder
buildCTEQueryParts d rawProjections st = do
  builderSt <- get
  let relAliases = [rbAlias r | r <- qsRelations st]
      cteAliasMap = foldr (\a m -> Map.insert a a m) (sbsAliasMap builderSt) relAliases
  put builderSt {sbsAliasMap = cteAliasMap}

  let (mFrom, wherePreds) = flattenJoins (qsRelations st) (qsPredicates st)

  mFromB <- case mFrom of
    Nothing -> case qsLaterals st of
      [] -> pure Nothing
      (l : ls) -> do
        let base = FromTable (lbAlias l) (lbAlias l)
            item = foldl (\acc (LateralBinding a sql mast) -> LeftJoinLateral acc sql a mast) base ls
        Just <$> buildFromItem d item
    Just baseItem -> do
      let withLats =
            foldl (\acc (LateralBinding a sql mast) -> LeftJoinLateral acc sql a mast) baseItem (qsLaterals st)
      Just <$> buildFromItem d withLats

  bSt <- get
  let projTexts = map (rewriteProjection d (sbsAliasMap bSt)) rawProjections
      selectClause =
        "SELECT " <> B.fromText (if null projTexts then "1" else T.intercalate ", " projTexts)

  let fromClause = case mFromB of
        Nothing -> ""
        Just fb -> " FROM " <> fb

  whereClause <- case wherePreds of
    [] -> pure ""
    ps -> do
      condBuilders <- mapM (buildExpr d) ps
      let combined = case condBuilders of
            [] -> ""
            [c] -> c
            (c : cs) -> foldl (\acc next -> acc <> " AND " <> next) c cs
      pure (" WHERE " <> combined)

  let groupByClause = case qsGroupBy st of
        [] -> ""
        cols ->
          let rewrittenCols = map (rewriteProjection d (sbsAliasMap bSt)) cols
           in " GROUP BY " <> B.fromText (T.intercalate ", " rewrittenCols)

  havingClause <- case qsHaving st of
    [] -> pure ""
    hs -> do
      hBuilders <- mapM (buildExpr d) hs
      let combined = case hBuilders of
            [] -> ""
            [h] -> h
            (h : cs) -> foldl (\acc next -> acc <> " AND " <> next) h cs
      pure (" HAVING " <> combined)

  let orderByClause = case qsOrderBy st of
        [] -> ""
        orders ->
          let rewrittenOrders = map (rewriteProjection d (sbsAliasMap bSt)) orders
           in " ORDER BY " <> B.fromText (T.intercalate ", " rewrittenOrders)

  finalSt <- get
  put finalSt {sbsAliasMap = sbsAliasMap builderSt}

  pure
    ( selectClause
        <> fromClause
        <> whereClause
        <> groupByClause
        <> havingClause
        <> orderByClause
    )

-- | Generate a debug/preview SQL string by interpolating bound parameters into the compiled query.
previewQuery :: CompiledQuery -> Text
previewQuery (CompiledQuery sql params) =
  let paramList = toList params
   in case paramList of
        [] -> sql
        _ ->
          if T.isInfixOf "$1" sql
            then interpolateMarker '$' sql paramList
            else
              if T.isInfixOf "?1" sql
                then interpolateMarker '?' sql paramList
                else replaceQMarks sql (map (\(BoundParam _ val pType) -> formatParamValue val pType) paramList)
  where
    replaceQMarks text [] = text
    replaceQMarks text (val : vs) = case T.breakOn "?" text of
      (before, after)
        | T.null after -> before
        | otherwise -> before <> val <> replaceQMarks (T.drop 1 after) vs

-- | Single-pass tokenization substituting marker digits ($1..$N or ?1..?N) with bound parameters.
interpolateMarker :: Char -> Text -> [BoundParam] -> Text
interpolateMarker marker sql params = go sql
  where
    paramMap = Map.fromList [(paramIndex p, formatParamValue (paramValue p) (paramType p)) | p <- params]
    go t = case T.break (== marker) t of
      (before, rest)
        | T.null rest -> before
        | otherwise ->
            let afterMarker = T.drop 1 rest
                (digits, remaining) = T.span isDigit afterMarker
             in if T.null digits
                  then before <> T.singleton marker <> go afterMarker
                  else case readMaybe (T.unpack digits) of
                    Just idx
                      | Just formatted <- Map.lookup idx paramMap ->
                          before <> formatted <> go remaining
                    _ -> before <> T.singleton marker <> digits <> go remaining

-- | Generate a preview SQL string explicitly specifying the dialect.
previewQueryForDialect :: Dialect -> CompiledQuery -> Text
previewQueryForDialect MySQLDialect (CompiledQuery sql params) =
  replaceQMarks sql (map (\(BoundParam _ val pType) -> formatParamValue val pType) (toList params))
  where
    replaceQMarks text [] = text
    replaceQMarks text (val : vs) = case T.breakOn "?" text of
      (before, after)
        | T.null after -> before
        | otherwise -> before <> val <> replaceQMarks (T.drop 1 after) vs
previewQueryForDialect _ cq = previewQuery cq

-- | Format a bound parameter value for preview SQL interpolation.
formatParamValue :: Text -> Text -> Text
formatParamValue val pType
  | pType == "integer" || pType == "numeric" || pType == "boolean" = val
  | otherwise = "'" <> T.replace "'" "''" val <> "'"

-- | Rewrite a projection column reference with dialect identifier quoting and alias substitution.
rewriteProjection :: Dialect -> Map Text Text -> Text -> Text
rewriteProjection d aliasMap raw =
  -- First substitute any relation aliases mapped in aliasMap
  let substituted = foldl (\acc (orig, mapped) -> replaceWord orig mapped acc) raw (Map.toList aliasMap)
   in applyDialectQuotes d substituted
  where
    replaceWord orig mapped text =
      T.replace ("\"" <> orig <> "\".") ("\"" <> mapped <> "\".") $
        T.replace (orig <> ".") (mapped <> ".") text

-- | Adapt double-quoted identifiers to backticks if dialect is MySQL.
applyDialectQuotes :: Dialect -> Text -> Text
applyDialectQuotes d text = case d of
  MySQLDialect -> T.replace "\"" "`" text
  _ -> text
