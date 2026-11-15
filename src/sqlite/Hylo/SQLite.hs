{- |
Module      : Hylo.SQLite
Description : SQLite backend adapter and dialect compiler
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.SQLite
  ( -- * Backend Marker
    SQLite

    -- * SQLite Dialect Compiler
  , renderSQLiteSQL
  , renderSQLiteSQLText
  ) where

import Hylo.Core.Query.AST (SQLite)
import Hylo.SQLite.Compiler (renderSQLiteSQL, renderSQLiteSQLText)
