{- |
Module      : Test.Hylo.PG.Compiler.AggregateSpec
Description : Mirror module for Epic 6 formal specification test suite
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Test.Hylo.PG.Compiler.AggregateSpec
  ( spec
  ) where

import qualified Hylo.PG.Compiler.AggregateSpec as A
import Test.Hspec

spec :: Spec
spec = A.spec
