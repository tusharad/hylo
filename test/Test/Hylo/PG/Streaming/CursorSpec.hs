{- |
Module      : Test.Hylo.PG.Streaming.CursorSpec
Description : Test Oracle adapter for Epic 10: Constant-Memory Binary Cursor Streaming & Keyset Pagination
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Test.Hylo.PG.Streaming.CursorSpec (spec) where

import qualified Hylo.PG.Streaming.CursorSpec as Actual
import Test.Hspec

spec :: Spec
spec = Actual.spec
