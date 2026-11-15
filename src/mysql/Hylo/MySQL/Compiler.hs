{- |
Module      : Hylo.MySQL.Compiler
Description : High-level SQL code generator for MySQL dialect from operational Query AST
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.MySQL.Compiler
  ( compileMySQLQuery
  , compileMySQLSQL
  , renderMySQLSQL
  , renderMySQLSQLText
  , previewMySQLSQL
  ) where

import Data.Sequence (Seq)
import Data.Text (Text)
import Hylo.Backend.Capability (MySQL)
import Hylo.Core.Query.Monad (Query (..))
import Hylo.SQL.AST (Projectable (..))
import Hylo.SQL.Builder
  ( BoundParam (..)
  , CompiledQuery (..)
  , Dialect (MySQLDialect)
  , compileQuery
  , previewQueryForDialect
  )

-- | Compile a MySQL query into a CompiledQuery with bound parameters.
compileMySQLQuery :: Projectable a => Query MySQL a -> CompiledQuery
compileMySQLQuery = compileQuery MySQLDialect

-- | Compile a MySQL query returning SQL text and strict bound parameters sequence.
compileMySQLSQL :: Projectable a => Query MySQL a -> (Text, Seq BoundParam)
compileMySQLSQL q =
  let cq = compileMySQLQuery q
   in (compiledSQL cq, compiledParams cq)

-- | Render a Query into strict Text SQL representation for MySQL.
renderMySQLSQL :: Projectable a => Query MySQL a -> Text
renderMySQLSQL = compiledSQL . compileMySQLQuery

-- | Render a Query into strict Text SQL representation for MySQL.
renderMySQLSQLText :: Projectable a => Query MySQL a -> Text
renderMySQLSQLText = renderMySQLSQL

-- | Generate a debug/preview SQL string by interpolating bound parameters.
previewMySQLSQL :: Projectable a => Query MySQL a -> Text
previewMySQLSQL = previewQueryForDialect MySQLDialect . compileMySQLQuery
