{- |
Module      : Main
Description : Test suite entry point for Hylo
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Main (main) where

import qualified Hylo.Core.CodecSpec as CodecSpec
import qualified Hylo.Core.TableSpec as TableSpec
import qualified Hylo.Core.TypeErrorSpec as TypeErrorSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  TableSpec.spec
  CodecSpec.spec
  TypeErrorSpec.spec
