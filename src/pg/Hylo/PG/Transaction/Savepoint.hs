{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.PG.Transaction.Savepoint
Description : PostgreSQL savepoint descriptors and SQL generation
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Savepoints allow partial rollbacks within active PostgreSQL transactions,
enabling nested transactions and fault-tolerant sub-computations.
-}
module Hylo.PG.Transaction.Savepoint
  ( -- * Savepoint Descriptor
    Savepoint (..)
  , allocateSavepoint
  , renderSavepoint
  , renderReleaseSavepoint
  , renderRollbackSavepoint

    -- * Savepoint Execution
  , withSavepoint
  ) where

import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core.Transaction (withSavepoint)

-- | Named savepoint token descriptor.
data Savepoint = Savepoint
  { spToken :: !Int
  , spName :: !Text
  }
  deriving (Eq, Show)

instance NFData Savepoint where
  rnf (Savepoint t n) = rnf t `seq` rnf n

-- | Allocate a monotonically incremented savepoint descriptor.
allocateSavepoint :: Int -> (Savepoint, Int)
allocateSavepoint seqNum =
  let name = "svp_" <> T.pack (show seqNum)
   in (Savepoint seqNum name, seqNum + 1)

-- | Render SQL statement to establish a savepoint.
renderSavepoint :: Savepoint -> Text
renderSavepoint sp = "SAVEPOINT \"" <> spName sp <> "\""

-- | Render SQL statement to release an established savepoint.
renderReleaseSavepoint :: Savepoint -> Text
renderReleaseSavepoint sp = "RELEASE SAVEPOINT \"" <> spName sp <> "\""

-- | Render SQL statement to roll back to an established savepoint.
renderRollbackSavepoint :: Savepoint -> Text
renderRollbackSavepoint sp = "ROLLBACK TO SAVEPOINT \"" <> spName sp <> "\""
