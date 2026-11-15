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
  , compileSQLiteQuery
  , compileSQLiteSQL
  , renderSQLiteSQL
  , renderSQLiteSQLText
  , previewSQLiteSQL
  ) where

import Hylo.Backend.Capability (SQLite)
import Hylo.SQLite.Compiler
  ( compileSQLiteQuery
  , compileSQLiteSQL
  , previewSQLiteSQL
  , renderSQLiteSQL
  , renderSQLiteSQLText
  )
