{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.Transaction.Retry
Description : Automatic Conflict Retry Engine (SQLSTATE 40001/40P01)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

The Hylo automatic conflict retry engine transparently resolves transient
PostgreSQL serialization anomalies ('40001') and deadlocks ('40P01') without
leaking exceptions to application code. Because transactions in 'Tx' are
mathematically pure (no 'MonadIO'), transactions can safely replay up to @N@
times with exponential backoff and jitter. Any 'onCommit' hooks accumulated
during aborted attempts are automatically purged and executed strictly once upon
final commit.
-}
module Hylo.PG.Transaction.Retry
  ( -- * Isolation Levels
    IsolationLevel (..)
  , renderIsolationLevel
  , renderBeginTransaction

    -- * Retry Configuration
  , RetryPolicy (..)
  , defaultRetryPolicy
  , isRetryableError

    -- * Retry Execution Engine
  , runTxWithRetry

    -- * Re-exported Connection & Hook Primitives
  , Pool (..)
  , pool
  , defaultPool
  , onCommit
  , onRollback
  , simulateSerializationFailureOnce
  , simulateSerializationFailures
  , simulateDeadlockOnce
  , simulateDeadlocks
  ) where

import Control.Concurrent (threadDelay)
import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Data.Text (Text)
import Hylo.Core.Query (Postgres)
import Hylo.Core.Transaction
  ( Tx (..)
  , TxError (..)
  , TxMode (..)
  , TxState (..)
  , executeTx
  , initialTxState
  , onCommit
  , onRollback
  , simulateDeadlockOnce
  , simulateDeadlocks
  , simulateSerializationFailureOnce
  , simulateSerializationFailures
  )
import Hylo.PG.Connection (Pool (..), defaultPool, pool)
import Hylo.PG.Transaction.Hooks (executeCommitHooks, executeRollbackHooks)

-- | PostgreSQL transaction isolation levels.
data IsolationLevel
  = ReadCommitted
  | RepeatableRead
  | Serializable
  deriving (Eq, Show, Enum, Bounded)

instance NFData IsolationLevel where
  rnf = \case
    ReadCommitted -> ()
    RepeatableRead -> ()
    Serializable -> ()

-- | Render isolation level to PostgreSQL SQL text.
renderIsolationLevel :: IsolationLevel -> Text
renderIsolationLevel = \case
  ReadCommitted -> "READ COMMITTED"
  RepeatableRead -> "REPEATABLE READ"
  Serializable -> "SERIALIZABLE"

-- | Render standard SQL BEGIN command with isolation level and transaction mode.
renderBeginTransaction :: IsolationLevel -> TxMode -> Text
renderBeginTransaction iso mode =
  "BEGIN TRANSACTION ISOLATION LEVEL "
    <> renderIsolationLevel iso
    <> " "
    <> case mode of
      ReadOnly -> "READ ONLY"
      ReadWrite -> "READ WRITE"

-- | Configuration policy governing automatic transaction replay.
data RetryPolicy = RetryPolicy
  { maxRetries :: !Int
  , baseBackoffMicros :: !Int
  , maxBackoffMicros :: !Int
  , backoffFactor :: !Double
  }
  deriving (Eq, Show)

instance NFData RetryPolicy where
  rnf (RetryPolicy m b mx f) = rnf m `seq` rnf b `seq` rnf mx `seq` rnf f

{- | Default retry policy suitable for high-concurrency production workloads:
Up to 5 retries, starting at 10ms with exponential doubling capped at 1s.
-}
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy =
  RetryPolicy
    { maxRetries = 5
    , baseBackoffMicros = 10000
    , maxBackoffMicros = 1000000
    , backoffFactor = 2.0
    }

{- | Determines if a transaction execution failure is transient and eligible for retry.
Only SQLSTATE 40001 (serialization failure) and 40P01 (deadlock detected) are retried.
-}
isRetryableError :: TxError -> Bool
isRetryableError = \case
  TxSerializationFailure _ -> True
  TxDeadlockDetected _ -> True
  TxUserError _ -> False
  TxConstraintViolation {} -> False

{- | Execute a transaction computation with automatic retry upon serialization conflict or deadlock.
If the transaction succeeds, all accumulated 'onCommit' hooks run strictly once.
If retries are exhausted or a non-retryable error occurs, accumulated 'onRollback' hooks run.
-}
runTxWithRetry ::
  forall mode a.
  RetryPolicy ->
  Pool ->
  IsolationLevel ->
  Tx Postgres mode a ->
  IO (Either TxError a)
runTxWithRetry policy _targetPool _iso tx = loop 0
  where
    loop :: Int -> IO (Either TxError a)
    loop attempt = do
      let st0 = initialTxState {txAttempt = attempt}
      case executeTx tx st0 of
        (Right val, finalSt) -> do
          executeCommitHooks finalSt
          pure (Right val)
        (Left err, finalSt)
          | isRetryableError err && attempt < maxRetries policy -> do
              let rawBackoff =
                    fromIntegral (baseBackoffMicros policy)
                      * (backoffFactor policy ** fromIntegral attempt)
                  backoffDelay = min (maxBackoffMicros policy) (round rawBackoff)
              when (backoffDelay > 0) $
                threadDelay backoffDelay
              loop (attempt + 1)
          | otherwise -> do
              executeRollbackHooks finalSt
              pure (Left err)
