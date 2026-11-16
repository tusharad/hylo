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
import qualified Hylo.Core.ChangesetSpec as ChangesetSpec
import qualified Hylo.Core.CodecSpec as CodecSpec
import qualified Hylo.Core.TableSpec as TableSpec
import qualified Hylo.Core.TransactionSpec as TransactionSpec
import qualified Hylo.Core.TypeErrorSpec as TypeErrorSpec
import qualified Hylo.Migrate.DiffSpec as DiffSpec
import qualified Hylo.Migrate.LinterSpec as LinterSpec
import qualified Hylo.Migrate.LiveRunnerSpec as LiveRunnerSpec
import qualified Hylo.PG.AlgebraSpec as AlgebraSpec
import qualified Hylo.PG.Cache.RedisSpec as RedisCacheSpec
import qualified Hylo.PG.Compiler.AggregateSpec as AggregateSpec
import qualified Hylo.PG.Compiler.CTESpec as CTESpec
import qualified Hylo.PG.Compiler.JoinFlattenerSpec as JoinFlattenerSpec
import qualified Hylo.PG.Compiler.LateralSpec as LateralSpec
import qualified Hylo.PG.Compiler.WindowSpec as WindowSpec
import qualified Hylo.PG.ConnectionSpec as ConnectionSpec
import qualified Hylo.PG.JSONBSpec as JSONBSpec
import qualified Hylo.PG.Mutation.BulkSpec as BulkSpec
import qualified Hylo.PG.Mutation.ChangesetSpec as PGChangesetSpec
import qualified Hylo.PG.Mutation.UpsertSpec as UpsertSpec
import qualified Hylo.PG.RangeSpec as RangeSpec
import qualified Hylo.PG.RealPostgresSpec as RealPostgresSpec
import qualified Hylo.PG.Search.HybridSpec as HybridSpec
import qualified Hylo.PG.SelectableSpec as SelectableSpec
import qualified Hylo.PG.Streaming.CursorSpec as CursorSpec
import qualified Hylo.PG.Transaction.RetrySpec as RetrySpec
import qualified Hylo.PG.VectorSpec as VectorSpec
import qualified Hylo.SQL.BuilderSpec as BuilderSpec
import qualified Hylo.SQLiteSpec as SQLiteSpec
import qualified Hylo.Test.Properties as PropertiesSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  ChangesetSpec.spec
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
  PGChangesetSpec.spec
  VectorSpec.spec
  HybridSpec.spec
  JSONBSpec.spec
  RangeSpec.spec
  AlgebraSpec.spec
  QueryToolSpec.spec
  DiffSpec.spec
  LinterSpec.spec
  LiveRunnerSpec.spec
  CapabilitySpec.spec
  BuilderSpec.spec
  ConnectionSpec.spec
  SelectableSpec.spec
  RedisCacheSpec.spec
  SQLiteSpec.spec
  PropertiesSpec.spec
  RealPostgresSpec.spec
