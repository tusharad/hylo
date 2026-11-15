{- |
Module      : Hylo.PG.Compiler.JoinFlattener
Description : Re-export of shared ANSI SQL Join Flattener for PostgreSQL compiler
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.JoinFlattener
  ( generateScopes
  , flattenJoins
  , renderFromItem
  ) where

import Hylo.SQL.Compiler.JoinFlattener
  ( flattenJoins
  , generateScopes
  , renderFromItem
  )
