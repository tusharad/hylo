{- |
Module      : Hylo.PG.Compiler.Window
Description : PostgreSQL-specific window function and framing compiler
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.Window
  ( SupportsWindowFunctions (..)
  , compileWindowExpr
  ) where

import Data.Text (Text)
import Hylo.Backend.Capability (SupportsWindowFunctions (..))
import Hylo.SQL.Query.Window (WindowExpr (..))

-- | Compile a typed window expression into PostgreSQL SQL text.
compileWindowExpr :: WindowExpr a -> Text
compileWindowExpr (WindowExpr t) = t
