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

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Hylo.Core.Query (Postgres)
import Hylo.Core.Query.Window (WindowExpr (..))

-- | Capability typeclass certifying backend support for ANSI SQL window functions.
class SupportsWindowFunctions db where
  windowFunctionsDict :: Proxy db -> ()

instance SupportsWindowFunctions Postgres where
  windowFunctionsDict _ = ()

-- | Compile a typed window expression into PostgreSQL SQL text.
compileWindowExpr :: WindowExpr a -> Text
compileWindowExpr (WindowExpr t) = t
