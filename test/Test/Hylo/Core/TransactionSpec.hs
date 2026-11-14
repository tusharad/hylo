{- |
Module      : Test.Hylo.Core.TransactionSpec
Description : Test Oracle adapter for Epic 11: Lexically Isolated Tx Monad & Savepoints
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Test.Hylo.Core.TransactionSpec (spec) where

import qualified Hylo.Core.TransactionSpec as Actual
import Test.Hspec

spec :: Spec
spec = Actual.spec
