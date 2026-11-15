{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted function" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant pure" #-}

{- |
Module      : Hylo.PG.Transaction.RetrySpec
Description : Test suite for Epic 12: Automatic Conflict Retry Engine & Replica Routing
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Transaction.RetrySpec (spec) where

import Control.Concurrent.STM
  ( atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  )
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.Text (Text)
import Hylo.Core
import Hylo.Core.Query
  ( Postgres
  , Projectable (..)
  , each
  , insert
  , renderExpr
  , select
  )
import Hylo.PG.Connection (Pool (..), pool)
import Hylo.PG.Pool.Routing
  ( NodeDestination (..)
  , defaultClusterPool
  , routeNode
  , routeTransactionMode
  , runClusterTx
  , runReadOnlyTx
  )
import Hylo.PG.Transaction.Retry
  ( IsolationLevel (..)
  , RetryPolicy (..)
  , defaultRetryPolicy
  , isRetryableError
  , renderBeginTransaction
  , renderIsolationLevel
  , runTxWithRetry
  , simulateDeadlockOnce
  , simulateDeadlocks
  , simulateSerializationFailureOnce
  , simulateSerializationFailures
  )
import Test.Hspec
import Test.QuickCheck

default (Text, Int64, Double)

-- | Test Table: Account balance entity for serialization and retry testing.
data AccountT (ctx :: Ctx) = Account
  { id :: !(Col ctx (Id AccountT Int64))
  , balance :: !(Col ctx Int64)
  }

instance Table AccountT where
  toTableExpr alias m =
    Account
      { id = colSpecExpr alias m.id
      , balance = colSpecExpr alias m.balance
      }
  tableExprColumns a =
    [ renderExpr a.id
    , renderExpr a.balance
    ]

instance Projectable (AccountT 'Expr) where
  projectColumns = tableExprColumns

accountTable :: TableSchema AccountT
accountTable =
  tableSchema "accounts" $
    Account
      { id = primaryKey "id" "bigint"
      , balance = column "balance" "bigint"
      }

-- | Helper for fast test retry policy with minimal backoff delay.
fastRetryPolicy :: Int -> RetryPolicy
fastRetryPolicy retries =
  RetryPolicy
    { maxRetries = retries
    , baseBackoffMicros = 0
    , maxBackoffMicros = 0
    , backoffFactor = 1.0
    }

spec :: Spec
spec = describe "Epic 12: Automatic Retry Engine & Commit Hooks" $ do
  describe "12.4 Executable Oracle Specification" $ do
    it "purges and re-accumulates onCommit hooks across serialization retries" $ do
      hookCounter <- newTVarIO (0 :: Int)
      let txWithHook = do
            onCommit (atomically $ modifyTVar' hookCounter (+ 1))
            simulateSerializationFailureOnce
            pure ()
      _ <- runTxWithRetry (fastRetryPolicy 5) pool Serializable txWithHook
      finalCount <- readTVarIO hookCounter
      finalCount `shouldBe` 1

  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 1: Retry Purity Equivalence" $
      property $ \(Positive (n :: Int)) (Positive (initialBal :: Int)) ->
        let k = (n `mod` 4) + 1
            bal = fromIntegral initialBal :: Int64
            cs =
              withUniqueKey "acc_1" $
                emptyChangeset
                  Account
                    { id = SetTo (toId 1)
                    , balance = SetTo bal
                    }
            isolatedTx :: Tx Postgres 'ReadWrite (AccountT 'Result)
            isolatedTx = insert accountTable cs

            retryTx :: Tx Postgres 'ReadWrite (AccountT 'Result)
            retryTx = do
              simulateSerializationFailures k
              insert accountTable cs

            resIsolated = runTxPure isolatedTx
            (resRetry, finalSt) = executeTx retryTx (initialTxState {txAttempt = k})
         in isRight resIsolated ==>
              (isRight resRetry && txStore finalSt == [("accounts", "acc_1")])

    it "Law 2: Exactly-Once Commit Hook Execution (Success on Commit)" $
      property $ \(Positive (n :: Int)) ->
        let k = (n `mod` 4) + 1
         in ioProperty $ do
              commitCounter <- newTVarIO (0 :: Int)
              rollbackCounter <- newTVarIO (0 :: Int)
              let tx = do
                    onCommit (atomically $ modifyTVar' commitCounter (+ 1))
                    onRollback (atomically $ modifyTVar' rollbackCounter (+ 1))
                    simulateSerializationFailures k
                    pure ()
              res <- runTxWithRetry (fastRetryPolicy 5) pool Serializable tx
              commits <- readTVarIO commitCounter
              rollbacks <- readTVarIO rollbackCounter
              pure (isRight res .&&. commits === 1 .&&. rollbacks === 0)

    it "Law 2: Exactly-Once Commit Hook Execution (Zero on Abort)" $
      property $ \(Positive (n :: Int)) ->
        let excessFailures = n + 5
         in ioProperty $ do
              commitCounter <- newTVarIO (0 :: Int)
              rollbackCounter <- newTVarIO (0 :: Int)
              let tx = do
                    onCommit (atomically $ modifyTVar' commitCounter (+ 1))
                    onRollback (atomically $ modifyTVar' rollbackCounter (+ 1))
                    simulateSerializationFailures excessFailures
                    pure ()
              res <- runTxWithRetry (fastRetryPolicy 3) pool Serializable tx
              commits <- readTVarIO commitCounter
              rollbacks <- readTVarIO rollbackCounter
              pure (isLeft res .&&. commits === 0 .&&. rollbacks === 1)

    it "Law 3: Replica Dispatch Safety (ReadWrite strictly to Primary)" $
      property $ \(Positive (replicas :: Int)) ->
        let dest = routeTransactionMode ReadWrite (replicas `mod` 10)
         in dest === TargetPrimary

    it "Law 3: Replica Dispatch Safety (ReadOnly to Replica or Primary)" $
      property $ \(NonNegative (replicas :: Int)) ->
        let repCount = replicas `mod` 10
            dest = routeTransactionMode ReadOnly repCount
         in if repCount == 0
              then dest === TargetPrimary
              else dest === TargetReplica 0

  describe "Comprehensive Automatic Conflict Retry & Routing Combinators" $ do
    it "transparently resolves transient deadlock detection (40P01)" $ do
      counter <- newTVarIO (0 :: Int)
      let tx = do
            onCommit (atomically $ modifyTVar' counter (+ 1))
            simulateDeadlockOnce
            pure ("ok" :: Text)
      res <- runTxWithRetry (fastRetryPolicy 3) pool Serializable tx
      res `shouldBe` Right "ok"
      runs <- readTVarIO counter
      runs `shouldBe` 1

    it "immediately aborts without retrying on permanent constraint violations" $ do
      counter <- newTVarIO (0 :: Int)
      let tx = do
            onCommit (atomically $ modifyTVar' counter (+ 1))
            txFail (TxConstraintViolation "accounts" "unique_idx" "acc_1")
      res <- runTxWithRetry (fastRetryPolicy 5) pool Serializable tx
      case res of
        Left (TxConstraintViolation "accounts" "unique_idx" "acc_1") -> True `shouldBe` True
        _ -> expectationFailure "Expected constraint violation"
      runs <- readTVarIO counter
      runs `shouldBe` 0

    it "purges commit hooks registered inside a rolled back savepoint" $ do
      hookCounter <- newTVarIO (0 :: Int)
      let tx :: Tx Postgres 'ReadWrite ()
          tx = do
            onCommit (atomically $ modifyTVar' hookCounter (+ 1))
            _ <- withSavepoint $ do
              onCommit (atomically $ modifyTVar' hookCounter (+ 100))
              txFail (TxUserError "subtransaction failed")
            pure ()
      res <- runTxWithRetry (fastRetryPolicy 2) pool Serializable tx
      res `shouldSatisfy` isRight
      finalCount <- readTVarIO hookCounter
      finalCount `shouldBe` 1

    it "renders standard PostgreSQL BEGIN TRANSACTION statements" $ do
      renderIsolationLevel Serializable `shouldBe` "SERIALIZABLE"
      renderBeginTransaction ReadCommitted ReadWrite
        `shouldBe` "BEGIN TRANSACTION ISOLATION LEVEL READ COMMITTED READ WRITE"
      renderBeginTransaction RepeatableRead ReadOnly
        `shouldBe` "BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY"
      renderBeginTransaction Serializable ReadWrite
        `shouldBe` "BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE READ WRITE"

    it "identifies retryable vs non-retryable transaction errors" $ do
      isRetryableError (TxSerializationFailure "err") `shouldBe` True
      isRetryableError (TxDeadlockDetected "err") `shouldBe` True
      isRetryableError (TxUserError "err") `shouldBe` False
      isRetryableError (TxConstraintViolation "t" "c" "k") `shouldBe` False
      maxRetries defaultRetryPolicy `shouldBe` 5

    it "resolves multiple consecutive deadlocks up to retry limit" $ do
      let tx :: Tx Postgres 'ReadWrite ()
          tx = simulateDeadlocks 2
      res <- runTxWithRetry (fastRetryPolicy 3) pool Serializable tx
      res `shouldBe` Right ()

    it "routes cluster pool read-only transactions to read replicas" $ do
      let cluster = defaultClusterPool
          (dest, selectedPool) = routeNode ReadOnly cluster
      dest `shouldBe` TargetReplica 0
      poolConnectionString selectedPool `shouldBe` "postgresql://localhost:5433/hylo_replica_1"

    it "routes cluster pool read-write transactions to primary" $ do
      let cluster = defaultClusterPool
          (dest, selectedPool) = routeNode ReadWrite cluster
      dest `shouldBe` TargetPrimary
      poolConnectionString selectedPool `shouldBe` "postgresql://localhost:5432/hylo_test"
      res <- runClusterTx cluster ReadWrite (pure () :: Tx Postgres 'ReadWrite ())
      res `shouldBe` Right ()

    it "executes read-only query transactions on cluster replicas via runReadOnlyTx" $ do
      let cluster = defaultClusterPool
          validTx :: Tx Postgres 'ReadOnly (AccountT 'Expr)
          validTx = select (each accountTable)
      res <- runReadOnlyTx cluster validTx
      isRight res `shouldBe` True
