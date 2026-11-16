{- |
Module      : Hylo.SQLite.Compiler
Description : High-level SQL code generator for SQLite dialect from operational Query AST
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.SQLite.Compiler
  ( compileSQLiteQuery
  , compileSQLiteSQL
  , renderSQLiteSQL
  , renderSQLiteSQLText
  , previewSQLiteSQL
  , many
  , NestedList (..)
  ) where

import Data.Sequence (Seq)
import Data.Text (Text)
import Hylo.Backend.Capability (SQLite)
import Hylo.Core.Query.Monad (Query (..))
import Hylo.SQL.AST (Projectable (..))
import Hylo.SQL.Builder
  ( BoundParam (..)
  , CompiledQuery (..)
  , Dialect (SQLiteDialect)
  , compileQuery
  , previewQuery
  )
import Hylo.SQL.Compiler.Lateral (NestedList (..), many)

-- | Compile a SQLite query into a CompiledQuery with bound parameters.
compileSQLiteQuery :: Projectable a => Query SQLite a -> CompiledQuery
compileSQLiteQuery = compileQuery SQLiteDialect

-- | Compile a SQLite query returning SQL text and strict bound parameters sequence.
compileSQLiteSQL :: Projectable a => Query SQLite a -> (Text, Seq BoundParam)
compileSQLiteSQL q =
  let cq = compileSQLiteQuery q
   in (compiledSQL cq, compiledParams cq)

-- | Render a Query into strict Text SQL representation for SQLite.
renderSQLiteSQL :: Projectable a => Query SQLite a -> Text
renderSQLiteSQL = compiledSQL . compileSQLiteQuery

-- | Render a Query into strict Text SQL representation for SQLite.
renderSQLiteSQLText :: Projectable a => Query SQLite a -> Text
renderSQLiteSQLText = renderSQLiteSQL

-- | Generate a debug/preview SQL string by interpolating bound parameters.
previewSQLiteSQL :: Projectable a => Query SQLite a -> Text
previewSQLiteSQL = previewQuery . compileSQLiteQuery
