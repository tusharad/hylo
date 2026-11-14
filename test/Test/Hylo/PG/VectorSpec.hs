{- |
Module      : Test.Hylo.PG.VectorSpec
Description : Test Oracle adapter for Epic 15: Type-Level pgvector Support
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Test.Hylo.PG.VectorSpec (spec) where

import qualified Hylo.PG.VectorSpec as Actual
import Test.Hspec

spec :: Spec
spec = Actual.spec
