{- |
Module      : Test.Hylo.PG.Mutation.BulkSpec
Description : Test Oracle adapter for Epic 14: Bulk UNNEST Ingestion & Constraint Translation
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Test.Hylo.PG.Mutation.BulkSpec (spec) where

import qualified Hylo.PG.Mutation.BulkSpec as Actual
import Test.Hspec

spec :: Spec
spec = Actual.spec
