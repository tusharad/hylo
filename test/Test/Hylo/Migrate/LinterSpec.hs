{- |
Module      : Test.Hylo.Migrate.LinterSpec
Description : Test Oracle adapter for Epic 19: Zero-Downtime Migration Safety Linter & Shadow Database Verifier
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Test.Hylo.Migrate.LinterSpec (spec) where

import qualified Hylo.Migrate.LinterSpec as Actual
import Test.Hspec

spec :: Spec
spec = Actual.spec
