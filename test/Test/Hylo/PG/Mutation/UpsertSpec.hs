{- |
Module      : Test.Hylo.PG.Mutation.UpsertSpec
Description : Test runner alias matching formal specification path for Epic 9
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Test.Hylo.PG.Mutation.UpsertSpec
  ( spec
  ) where

import qualified Hylo.PG.Mutation.UpsertSpec as UpsertSpec
import Test.Hspec

spec :: Spec
spec = UpsertSpec.spec
