{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.PG.Compiler.SQL
Description : High-level SQL code generator from operational Query AST
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.SQL
  ( compilePostgresQuery
  , compilePostgresSQL
  , renderSQL
  , renderSQLText
  , renderPostgresSQL
  , previewPostgresSQL
  , explainQuery
  , previewExplainSQL
  ) where

import Data.Sequence (Seq)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Backend.Capability (Postgres)
import Hylo.Core.Query.Monad (Query (..))
import Hylo.SQL.AST (Projectable (..))
import Hylo.SQL.Builder
  ( BoundParam (..)
  , CompiledQuery (..)
  , Dialect (PostgresDialect)
  , compileQuery
  , previewQuery
  )

-- | Compile a PostgreSQL query into a CompiledQuery with bound parameters.
compilePostgresQuery :: Projectable a => Query (Postgres v) a -> CompiledQuery
compilePostgresQuery = compileQuery PostgresDialect

-- | Compile a PostgreSQL query returning SQL text and strict bound parameters sequence.
compilePostgresSQL :: Projectable a => Query (Postgres v) a -> (Text, Seq BoundParam)
compilePostgresSQL q =
  let cq = compilePostgresQuery q
   in (compiledSQL cq, compiledParams cq)

-- | Render a Query into strict Text SQL representation for PostgreSQL.
renderPostgresSQL :: Projectable a => Query (Postgres v) a -> Text
renderPostgresSQL = compiledSQL . compilePostgresQuery

-- | Render a Query into strict Text SQL representation.
renderSQLText :: Projectable a => Query (Postgres v) a -> Text
renderSQLText = renderPostgresSQL

-- | Render a Query into polymorphic string/text SQL representation.
renderSQL :: (Projectable a, IsString s) => Query (Postgres v) a -> s
renderSQL q = fromString (T.unpack (renderPostgresSQL q))

-- | Generate a debug/preview SQL string by interpolating bound parameters.
previewPostgresSQL :: Projectable a => Query (Postgres v) a -> Text
previewPostgresSQL = previewQuery . compilePostgresQuery

-- | Compile a PostgreSQL query wrapped with EXPLAIN (FORMAT JSON) or EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON).
explainQuery :: Projectable a => Bool -> Query (Postgres v) a -> CompiledQuery
explainQuery analyze q =
  let cq = compilePostgresQuery q
      prefix =
        if analyze
          then "EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON) "
          else "EXPLAIN (FORMAT JSON) "
   in cq {compiledSQL = prefix <> compiledSQL cq}

-- | Generate a debug/preview EXPLAIN SQL string with interpolated bound parameters.
previewExplainSQL :: Projectable a => Bool -> Query (Postgres v) a -> Text
previewExplainSQL analyze q = previewQuery (explainQuery analyze q)
