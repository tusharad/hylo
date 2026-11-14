{- |
Module      : Test.Hylo.PG.Compiler.CTESpec
Description : Mirror module for Epic 8 formal specification test suite
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Test.Hylo.PG.Compiler.CTESpec
  ( spec
  ) where

import qualified Hylo.PG.Compiler.CTESpec as C
import Test.Hspec

spec :: Spec
spec = C.spec
