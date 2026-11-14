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
import qualified Hylo.PG.Compiler.AggregateSpec as AggregateSpec
import qualified Hylo.PG.Compiler.CTESpec as CTESpec
import qualified Hylo.PG.Compiler.JoinFlattenerSpec as JoinFlattenerSpec
import qualified Hylo.PG.Compiler.LateralSpec as LateralSpec
import qualified Hylo.PG.Compiler.WindowSpec as WindowSpec
import qualified Hylo.PG.Mutation.UpsertSpec as UpsertSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  TableSpec.spec
  CodecSpec.spec
  TypeErrorSpec.spec
  JoinFlattenerSpec.spec
  LateralSpec.spec
  AggregateSpec.spec
  WindowSpec.spec
  CTESpec.spec
  UpsertSpec.spec
