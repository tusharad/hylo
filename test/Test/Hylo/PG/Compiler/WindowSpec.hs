{- |
Module      : Test.Hylo.PG.Compiler.WindowSpec
Description : Mirror module for Epic 7 formal specification test suite
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Test.Hylo.PG.Compiler.WindowSpec
  ( spec
  ) where

import qualified Hylo.PG.Compiler.WindowSpec as W
import Test.Hspec

spec :: Spec
spec = W.spec
