{- |
Module      : Hylo.MySQL
Description : MySQL backend adapter and dialect compiler
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.MySQL
  ( -- * Backend Marker
    MySQL

    -- * MySQL Dialect Compiler
  , compileMySQLQuery
  , compileMySQLSQL
  , renderMySQLSQL
  , renderMySQLSQLText
  , previewMySQLSQL
  ) where

import Hylo.Backend.Capability (MySQL)
import Hylo.MySQL.Compiler
  ( compileMySQLQuery
  , compileMySQLSQL
  , previewMySQLSQL
  , renderMySQLSQL
  , renderMySQLSQLText
  )
