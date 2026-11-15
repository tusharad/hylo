{- |
Module      : Hylo.PG.Transaction.Hooks
Description : Deferrable onCommit and onRollback execution hooks
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides deferrable side-effect hooks that accumulate within transactions
without violating effect purity. Registered 'onCommit' actions execute strictly
once following successful completion of the outer transaction commit, and are
purged without execution if transactions abort or retry due to serialization
conflicts (SQLSTATE 40001) or deadlocks (SQLSTATE 40P01).
-}
module Hylo.PG.Transaction.Hooks
  ( -- * Hook Registration Combinators
    onCommit
  , onRollback

    -- * Hook Execution Engines
  , executeCommitHooks
  , executeRollbackHooks
  ) where

import Hylo.Core.Transaction
  ( TxState (..)
  , onCommit
  , onRollback
  )

-- | Sequentially execute all accumulated commit hooks in order of registration.
executeCommitHooks :: TxState -> IO ()
executeCommitHooks st = sequence_ (txCommitHooks st)

-- | Sequentially execute all accumulated rollback hooks in order of registration.
executeRollbackHooks :: TxState -> IO ()
executeRollbackHooks st = sequence_ (txRollbackHooks st)
