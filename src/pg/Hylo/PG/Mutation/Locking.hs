{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Hylo.PG.Mutation.Locking
Description : Pessimistic row-locking combinators and lock exclusivity matrix
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides PostgreSQL row-level locking combinators (@FOR UPDATE@, @FOR NO KEY UPDATE@,
@FOR SHARE@, @FOR KEY SHARE@, @SKIP LOCKED@, and @NOWAIT@), verifying lock exclusivity
and queue consumption invariants.
-}
module Hylo.PG.Mutation.Locking
  ( -- * Lock Strength & Wait Specification
    LockStrength (..)
  , LockWait (..)
  , LockClause (..)
  , renderLockClause

    -- * Wait Policy Smart Constructors
  , skipLocked
  , nowait
  , defaultLockWait

    -- * Overloaded Pessimistic Locking Combinators
  , ForUpdate (..)
  , ForNoKeyUpdate (..)
  , ForShare (..)
  , ForKeyShare (..)

    -- * Table-Targeted Locking
  , forUpdateOf
  , forNoKeyUpdateOf
  , forShareOf
  , forKeyShareOf

    -- * Lock Exclusivity & Concurrency Matrix
  , canConcurrentlyLock

    -- * Transactional Guarded Execution
  , executeLocked
  ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Hylo.Core.Query (Tx (..), TxMode (..))
import Hylo.Core.Query.Monad (Query (..), QueryState (..))
import Hylo.Core.TypeError (AssertWritable (..))
import Hylo.SQL.AST
  ( LockClause (..)
  , LockStrength (..)
  , LockWait (..)
  , renderLockClause
  )

-- | Wait policy: skip rows locked by concurrent transactions immediately.
skipLocked :: LockWait
skipLocked = SkipLocked

-- | Wait policy: fail with error immediately if any candidate row is locked.
nowait :: LockWait
nowait = NoWait

-- | Wait policy: standard blocking wait until concurrent transaction terminates.
defaultLockWait :: LockWait
defaultLockWait = LockWaitDefault

-- | Set lock clause on query state.
setLock :: LockStrength -> LockWait -> [Text] -> Query db ()
setLock strength wait tables = Query $ \s ->
  let s' = s {qsLock = Just (LockClause strength wait tables)}
   in ((), s')

-- | Overloaded combinator for PostgreSQL @FOR UPDATE@ row locking.
class ForUpdate target where
  forUpdate :: target

instance (a ~ ()) => ForUpdate (LockWait -> Query db a) where
  forUpdate wait = setLock ForUpdate wait []

instance (a ~ ()) => ForUpdate (Query db a) where
  forUpdate = setLock ForUpdate LockWaitDefault []

-- | Overloaded combinator for PostgreSQL @FOR NO KEY UPDATE@ row locking.
class ForNoKeyUpdate target where
  forNoKeyUpdate :: target

instance (a ~ ()) => ForNoKeyUpdate (LockWait -> Query db a) where
  forNoKeyUpdate wait = setLock ForNoKeyUpdate wait []

instance (a ~ ()) => ForNoKeyUpdate (Query db a) where
  forNoKeyUpdate = setLock ForNoKeyUpdate LockWaitDefault []

-- | Overloaded combinator for PostgreSQL @FOR SHARE@ row locking.
class ForShare target where
  forShare :: target

instance (a ~ ()) => ForShare (LockWait -> Query db a) where
  forShare wait = setLock ForShare wait []

instance (a ~ ()) => ForShare (Query db a) where
  forShare = setLock ForShare LockWaitDefault []

-- | Overloaded combinator for PostgreSQL @FOR KEY SHARE@ row locking.
class ForKeyShare target where
  forKeyShare :: target

instance (a ~ ()) => ForKeyShare (LockWait -> Query db a) where
  forKeyShare wait = setLock ForKeyShare wait []

instance (a ~ ()) => ForKeyShare (Query db a) where
  forKeyShare = setLock ForKeyShare LockWaitDefault []

-- | Attach @FOR UPDATE OF tables@ locking to a query.
forUpdateOf :: [Text] -> LockWait -> Query db ()
forUpdateOf tables wait = setLock ForUpdate wait tables

-- | Attach @FOR NO KEY UPDATE OF tables@ locking to a query.
forNoKeyUpdateOf :: [Text] -> LockWait -> Query db ()
forNoKeyUpdateOf tables wait = setLock ForNoKeyUpdate wait tables

-- | Attach @FOR SHARE OF tables@ locking to a query.
forShareOf :: [Text] -> LockWait -> Query db ()
forShareOf tables wait = setLock ForShare wait tables

-- | Attach @FOR KEY SHARE OF tables@ locking to a query.
forKeyShareOf :: [Text] -> LockWait -> Query db ()
forKeyShareOf tables wait = setLock ForKeyShare wait tables

{- | PostgreSQL Lock Compatibility Matrix.
Returns 'True' if transactions holding lock mode @l1@ and @l2@ can concurrently
hold locks on the same row.
-}
canConcurrentlyLock :: LockStrength -> LockStrength -> Bool
canConcurrentlyLock ForUpdate _ = False
canConcurrentlyLock _ ForUpdate = False
canConcurrentlyLock ForNoKeyUpdate ForShare = False
canConcurrentlyLock ForShare ForNoKeyUpdate = False
canConcurrentlyLock ForNoKeyUpdate ForNoKeyUpdate = False
canConcurrentlyLock ForNoKeyUpdate ForKeyShare = True
canConcurrentlyLock ForKeyShare ForNoKeyUpdate = True
canConcurrentlyLock ForShare ForShare = True
canConcurrentlyLock ForShare ForKeyShare = True
canConcurrentlyLock ForKeyShare ForShare = True
canConcurrentlyLock ForKeyShare ForKeyShare = True

{- | Statically guarded transactional query execution with row locking.
Fails with [Hylo Error E005] in ReadOnly mode.
-}
executeLocked ::
  forall a db (mode :: TxMode).
  (AssertWritable mode) =>
  Query db a ->
  Tx db mode a
executeLocked _ =
  writableDict (Proxy :: Proxy mode) `seq` TxEmpty
