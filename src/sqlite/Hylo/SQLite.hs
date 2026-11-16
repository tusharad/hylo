{- |
Module      : Hylo.SQLite
Description : SQLite backend adapter, dialect compiler, and C-API execution bridge
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.SQLite
  ( -- * Backend Marker
    SQLite

    -- * SQLite Dialect Compiler
  , module Hylo.SQLite.Compiler

    -- * Connection Pool & Execution Bridge
  , module Hylo.SQLite.Connection
  ) where

import Hylo.Backend.Capability (SQLite)
import Hylo.SQLite.Compiler
import Hylo.SQLite.Connection
