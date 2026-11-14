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
  }
  deriving (Eq, Show)

instance NFData TxState where
  rnf (TxState s ops store) = rnf s `seq` rnf ops `seq` rnf store

-- | Initial state for a new transaction computation.
initialTxState :: TxState
initialTxState = TxState 0 [] []

-- | Pure operational Transaction monad parameterized by backend marker and phantom mode.
data Tx db (mode :: TxMode) a where
  TxReturn :: a -> Tx db mode a
  TxEmpty :: Tx db mode a
  TxFail :: !TxError -> Tx db mode a
  TxAction :: !(TxState -> Either TxError (a, TxState)) -> Tx db mode a

instance Functor (Tx db mode) where
  fmap f = \case
    TxReturn a -> TxReturn (f a)
    TxEmpty -> TxEmpty
    TxFail err -> TxFail err
    TxAction g -> TxAction $ \st -> case g st of
      Left err -> Left err
      Right (a, st') -> Right (f a, st')

instance Applicative (Tx db mode) where
  pure = TxReturn
  TxReturn f <*> x = fmap f x
  TxEmpty <*> _ = TxEmpty
  TxFail err <*> _ = TxFail err
  TxAction g <*> x = TxAction $ \st -> case g st of
    Left err -> Left err
    Right (f, st1) -> case x of
      TxReturn a -> Right (f a, st1)
      TxEmpty -> Left (TxUserError "TxEmpty")
      TxFail err -> Left err
      TxAction h -> case h st1 of
        Left err -> Left err
        Right (a, st2) -> Right (f a, st2)

instance Monad (Tx db mode) where
  TxReturn a >>= f = f a
  TxEmpty >>= _ = TxEmpty
  TxFail err >>= _ = TxFail err
  TxAction g >>= f = TxAction $ \st -> case g st of
    Left err -> Left err
    Right (a, st') -> case f a of
      TxReturn b -> Right (b, st')
      TxEmpty -> Left (TxUserError "TxEmpty")
      TxFail err -> Left err
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
   in Right (res, st)

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
   in case executeTx subTx stBefore of
        (Left err, stAfterFail) ->
          let stRolledBack =
                stAfterFail
                  { txStore = storeSnapshot
                  , txOperations = txOperations stAfterFail ++ [OpRollbackSavepoint token spName]
                  }
           in Right (Left err, stRolledBack)
        (Right val, stAfterSuccess) ->
          let stReleased =
                stAfterSuccess
                  { txOperations = txOperations stAfterSuccess ++ [OpReleaseSavepoint token spName]
                  }
           in Right (Right val, stReleased)

-- | Execute a 'Tx' action against an explicit initial state.
executeTx :: Tx db mode a -> TxState -> (Either TxError a, TxState)
executeTx tx st = case tx of
  TxReturn a -> (Right a, st)
  TxEmpty -> (Left (TxUserError "TxEmpty"), st)
  TxFail err -> (Left err, st)
  TxAction f -> case f st of
    Left err -> (Left err, st)
    Right (a, st') -> (Right a, st')

-- | Run a transaction purely using the default initial state.
runTxPure :: Tx db mode a -> Either TxError a
runTxPure tx = fst (executeTx tx initialTxState)

-- | Record a mutation operation within the transaction state.
recordMutation :: Text -> Tx db mode ()
recordMutation descSql = TxAction $ \st ->
  let st' = st {txOperations = txOperations st ++ [OpMutate descSql]}
   in Right ((), st')

-- | Record a raw query operation within the transaction state.
recordQuery :: Text -> Tx db mode ()
recordQuery descSql = TxAction $ \st ->
  let st' = st {txOperations = txOperations st ++ [OpQuery descSql]}
   in Right ((), st')
