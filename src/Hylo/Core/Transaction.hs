{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.Core.Transaction
Description : Lexically isolated Tx monad, phantom transaction modes, and savepoints
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

The 'Tx' monad provides an effect-pure, lexically isolated environment for transactional
database operations. It strictly disallows 'MonadIO' / 'liftIO' at compile time via closed
type family assertion [Hylo Error E006], ensuring mathematical purity for safe automatic
retry engines and preventing transaction-window connection starvation.
-}
module Hylo.Core.Transaction
  ( -- * Transaction Monad & Phantom Modes
    Tx (..)
  , TxMode (..)
  , TxError (..)
  , TxOp (..)
  , TxState (..)
  , initialTxState

    -- * Transaction Combinators
  , select
  , txFail
  , withSavepoint
  , executeTx
  , runTxPure

    -- * Deferrable Execution Hooks
  , onCommit
  , onRollback

    -- * Simulation Helpers for Fault-Tolerance Testing
  , simulateSerializationFailureOnce
  , simulateSerializationFailures
  , simulateDeadlockOnce
  , simulateDeadlocks

    -- * Low-Level Mutation & Query Recording
  , recordMutation
  , recordQuery
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core.Query.Monad (Query, initialQueryState, runQuery)
import Hylo.Core.TypeError (AssertNoIO (..), TxMode (..))

-- | Transaction execution error descriptors.
data TxError
  = TxUserError !Text
  | TxConstraintViolation !Text !Text !Text
  | TxSerializationFailure !Text
  | TxDeadlockDetected !Text
  deriving (Eq, Show)

instance NFData TxError where
  rnf (TxUserError t) = rnf t
  rnf (TxConstraintViolation t c d) = rnf t `seq` rnf c `seq` rnf d
  rnf (TxSerializationFailure t) = rnf t
  rnf (TxDeadlockDetected t) = rnf t

-- | Operation descriptors recorded during transaction execution.
data TxOp
  = OpQuery !Text
  | OpMutate !Text
  | OpSavepoint !Int !Text
  | OpReleaseSavepoint !Int !Text
  | OpRollbackSavepoint !Int !Text
  deriving (Eq, Show)

instance NFData TxOp where
  rnf = \case
    OpQuery t -> rnf t
    OpMutate t -> rnf t
    OpSavepoint i t -> rnf i `seq` rnf t
    OpReleaseSavepoint i t -> rnf i `seq` rnf t
    OpRollbackSavepoint i t -> rnf i `seq` rnf t

-- | Internal state tracked during transaction evaluation.
data TxState = TxState
  { txSavepointSeq :: !Int
  , txOperations :: ![TxOp]
  , txStore :: ![(Text, Text)]
  , txAttempt :: !Int
  , txCommitHooks :: ![IO ()]
  , txRollbackHooks :: ![IO ()]
  }

instance Eq TxState where
  TxState s1 o1 st1 a1 _ _ == TxState s2 o2 st2 a2 _ _ =
    s1 == s2 && o1 == o2 && st1 == st2 && a1 == a2

instance Show TxState where
  showsPrec d (TxState s o st a ch rh) =
    showParen (d > 10) $
      showString "TxState { txSavepointSeq = "
        . shows s
        . showString ", txOperations = "
        . shows o
        . showString ", txStore = "
        . shows st
        . showString ", txAttempt = "
        . shows a
        . showString ", txCommitHooks = <"
        . shows (length ch)
        . showString " hooks>"
        . showString ", txRollbackHooks = <"
        . shows (length rh)
        . showString " hooks> }"

instance NFData TxState where
  rnf (TxState s ops store a ch rh) =
    rnf s `seq` rnf ops `seq` rnf store `seq` rnf a `seq` length ch `seq` length rh `seq` ()

-- | Initial state for a new transaction computation.
initialTxState :: TxState
initialTxState = TxState 0 [] [] 0 [] []

-- | Pure operational Transaction monad parameterized by backend marker and phantom mode.
data Tx db (mode :: TxMode) a where
  TxReturn :: a -> Tx db mode a
  TxEmpty :: Tx db mode a
  TxFail :: !TxError -> Tx db mode a
  TxAction :: !(TxState -> (Either TxError a, TxState)) -> Tx db mode a

instance Functor (Tx db mode) where
  fmap f = \case
    TxReturn a -> TxReturn (f a)
    TxEmpty -> TxEmpty
    TxFail err -> TxFail err
    TxAction g -> TxAction $ \st -> case g st of
      (Left err, st') -> (Left err, st')
      (Right a, st') -> (Right (f a), st')

instance Applicative (Tx db mode) where
  pure = TxReturn
  TxReturn f <*> x = fmap f x
  TxEmpty <*> _ = TxEmpty
  TxFail err <*> _ = TxFail err
  TxAction g <*> x = TxAction $ \st -> case g st of
    (Left err, st1) -> (Left err, st1)
    (Right f, st1) -> case x of
      TxReturn a -> (Right (f a), st1)
      TxEmpty -> (Left (TxUserError "TxEmpty"), st1)
      TxFail err -> (Left err, st1)
      TxAction h -> case h st1 of
        (Left err, st2) -> (Left err, st2)
        (Right a, st2) -> (Right (f a), st2)

instance Monad (Tx db mode) where
  TxReturn a >>= f = f a
  TxEmpty >>= _ = TxEmpty
  TxFail err >>= _ = TxFail err
  TxAction g >>= f = TxAction $ \st -> case g st of
    (Left err, st') -> (Left err, st')
    (Right a, st') -> case f a of
      TxReturn b -> (Right b, st')
      TxEmpty -> (Left (TxUserError "TxEmpty"), st')
      TxFail err -> (Left err, st')
      TxAction h -> h st'

instance NFData a => NFData (Tx db mode a) where
  rnf (TxReturn a) = rnf a
  rnf TxEmpty = ()
  rnf (TxFail err) = rnf err
  rnf (TxAction _) = ()

-- | Statically forbids 'liftIO' inside transaction blocks, raising [Hylo Error E006].
instance AssertNoIO 'True => MonadIO (Tx db mode) where
  liftIO _ = noIODict (Proxy :: Proxy 'True) `seq` errorWithoutStackTrace "[Hylo Error E006] unreachable"

-- | Execute a query within the transaction context.
select :: Query db a -> Tx db mode a
select q = TxAction $ \st ->
  let (res, _) = runQuery q initialQueryState
   in (Right res, st)

-- | Explicitly fail a transaction with a descriptive 'TxError'.
txFail :: TxError -> Tx db mode a
txFail = TxFail

{- | Execute a sub-transaction within an isolated PostgreSQL savepoint.
If the sub-transaction fails, intermediate state changes are reverted and
the savepoint is rolled back ('ROLLBACK TO SAVEPOINT svp_N'), returning 'Left err'
without aborting the parent transaction.
-}
withSavepoint :: Tx db mode a -> Tx db mode (Either TxError a)
withSavepoint subTx = TxAction $ \st ->
  let token = txSavepointSeq st
      spName = "svp_" <> T.pack (show token)
      stBefore =
        st
          { txSavepointSeq = token + 1
          , txOperations = txOperations st ++ [OpSavepoint token spName]
          }
      storeSnapshot = txStore st
      commitHooksSnapshot = txCommitHooks st
      rollbackHooksSnapshot = txRollbackHooks st
   in case executeTx subTx stBefore of
        (Left err, stAfterFail) ->
          let stRolledBack =
                stAfterFail
                  { txStore = storeSnapshot
                  , txCommitHooks = commitHooksSnapshot
                  , txRollbackHooks = rollbackHooksSnapshot
                  , txOperations = txOperations stAfterFail ++ [OpRollbackSavepoint token spName]
                  }
           in (Right (Left err), stRolledBack)
        (Right val, stAfterSuccess) ->
          let stReleased =
                stAfterSuccess
                  { txOperations = txOperations stAfterSuccess ++ [OpReleaseSavepoint token spName]
                  }
           in (Right (Right val), stReleased)

-- | Execute a 'Tx' action against an explicit initial state.
executeTx :: Tx db mode a -> TxState -> (Either TxError a, TxState)
executeTx tx st = case tx of
  TxReturn a -> (Right a, st)
  TxEmpty -> (Left (TxUserError "TxEmpty"), st)
  TxFail err -> (Left err, st)
  TxAction f -> f st

-- | Run a transaction purely using the default initial state.
runTxPure :: Tx db mode a -> Either TxError a
runTxPure tx = fst (executeTx tx initialTxState)

-- | Queue an IO action to be executed strictly once after the outermost transaction commits.
onCommit :: IO () -> Tx db mode ()
onCommit hook = TxAction $ \st ->
  (Right (), st {txCommitHooks = txCommitHooks st ++ [hook]})

-- | Queue an IO action to be executed if the transaction ultimately aborts / rolls back.
onRollback :: IO () -> Tx db mode ()
onRollback hook = TxAction $ \st ->
  (Right (), st {txRollbackHooks = txRollbackHooks st ++ [hook]})

-- | Simulate a transient PostgreSQL serialization failure ('40001') on the first attempt only.
simulateSerializationFailureOnce :: Tx db mode ()
simulateSerializationFailureOnce = simulateSerializationFailures 1

-- | Simulate transient PostgreSQL serialization failures ('40001') for the first @N@ attempts.
simulateSerializationFailures :: Int -> Tx db mode ()
simulateSerializationFailures n = TxAction $ \st ->
  if txAttempt st < n
    then
      ( Left
          (TxSerializationFailure "40001: could not serialize access due to concurrent update (simulated)")
      , st
      )
    else (Right (), st)

-- | Simulate a transient PostgreSQL deadlock detection ('40P01') on the first attempt only.
simulateDeadlockOnce :: Tx db mode ()
simulateDeadlockOnce = simulateDeadlocks 1

-- | Simulate transient PostgreSQL deadlock detections ('40P01') for the first @N@ attempts.
simulateDeadlocks :: Int -> Tx db mode ()
simulateDeadlocks n = TxAction $ \st ->
  if txAttempt st < n
    then (Left (TxDeadlockDetected "40P01: deadlock detected (simulated)"), st)
    else (Right (), st)

-- | Record a mutation operation within the transaction state.
recordMutation :: Text -> Tx db mode ()
recordMutation descSql = TxAction $ \st ->
  let st' = st {txOperations = txOperations st ++ [OpMutate descSql]}
   in (Right (), st')

-- | Record a raw query operation within the transaction state.
recordQuery :: Text -> Tx db mode ()
recordQuery descSql = TxAction $ \st ->
  let st' = st {txOperations = txOperations st ++ [OpQuery descSql]}
   in (Right (), st')
