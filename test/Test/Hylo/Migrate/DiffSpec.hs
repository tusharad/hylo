{- |
Module      : Test.Hylo.Migrate.DiffSpec
Description : Test Oracle adapter for Epic 18: Live Catalog Introspection, Reversible DDL Synthesis & Legacy Reverse Codegen
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Test.Hylo.Migrate.DiffSpec (spec) where

import qualified Hylo.Migrate.DiffSpec as Actual
import Test.Hspec

spec :: Spec
spec = Actual.spec
