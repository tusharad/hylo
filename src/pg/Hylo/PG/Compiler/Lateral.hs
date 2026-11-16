{- |
Module      : Hylo.PG.Compiler.Lateral
Description : Correlated Subquery Lowering (LATERAL N+1 Eradication) via jsonb_agg
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.Lateral
  ( module Hylo.SQL.Compiler.Lateral
  ) where

import Hylo.SQL.Compiler.Lateral
