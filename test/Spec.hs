{- |
Module      : Main
Description : Test suite entry point for Hylo
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Main (main) where

import qualified Hylo.AI.QueryToolSpec as QueryToolSpec
import qualified Hylo.Backend.CapabilitySpec as CapabilitySpec
import qualified Hylo.Core.CodecSpec as CodecSpec
import qualified Hylo.Core.TableSpec as TableSpec
import qualified Hylo.Core.TransactionSpec as TransactionSpec
import qualified Hylo.Core.TypeErrorSpec as TypeErrorSpec
import qualified Hylo.Migrate.DiffSpec as DiffSpec
import qualified Hylo.Migrate.LinterSpec as LinterSpec
import qualified Hylo.PG.Compiler.AggregateSpec as AggregateSpec
import qualified Hylo.PG.Compiler.CTESpec as CTESpec
import qualified Hylo.PG.Compiler.JoinFlattenerSpec as JoinFlattenerSpec
import qualified Hylo.PG.Compiler.LateralSpec as LateralSpec
import qualified Hylo.PG.Compiler.WindowSpec as WindowSpec
import qualified Hylo.PG.Mutation.BulkSpec as BulkSpec
import qualified Hylo.PG.Mutation.UpsertSpec as UpsertSpec
import qualified Hylo.PG.RealPostgresSpec as RealPostgresSpec
import qualified Hylo.PG.Search.HybridSpec as HybridSpec
import qualified Hylo.PG.Streaming.CursorSpec as CursorSpec
import qualified Hylo.PG.Transaction.RetrySpec as RetrySpec
import qualified Hylo.PG.VectorSpec as VectorSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  TableSpec.spec
  TransactionSpec.spec
  CodecSpec.spec
  TypeErrorSpec.spec
  JoinFlattenerSpec.spec
  LateralSpec.spec
  AggregateSpec.spec
  WindowSpec.spec
  CTESpec.spec
  UpsertSpec.spec
  CursorSpec.spec
  RetrySpec.spec
  BulkSpec.spec
  VectorSpec.spec
  HybridSpec.spec
  QueryToolSpec.spec
  DiffSpec.spec
  LinterSpec.spec
  CapabilitySpec.spec
  RealPostgresSpec.spec
