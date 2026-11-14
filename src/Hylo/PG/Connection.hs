{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.Connection
Description : Connection pool descriptor and transaction runner
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Connection
  ( Pool (..)
  , createPool
  , defaultPool
  , pool
  , runTx
  ) where

import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import Hylo.Core.Query (Postgres)
import Hylo.Core.Transaction
  ( Tx (..)
  , TxError (..)
  , TxMode (..)
  , executeTx
  , initialTxState
  )

-- | PostgreSQL connection pool descriptor.
data Pool = Pool
  { poolConnectionString :: !Text
  , poolMaxConnections :: !Int
  }
  deriving (Eq, Show)

instance NFData Pool where
  rnf (Pool c m) = rnf c `seq` rnf m

-- | Create a new connection pool with a connection string.
createPool :: Text -> IO Pool
createPool connStr = pure $ Pool connStr 10

-- | Default connection pool for local execution and test environments.
defaultPool :: Pool
defaultPool = Pool "postgresql://localhost:5432/hylo_test" 10

-- | Top-level default pool reference.
pool :: Pool
pool = defaultPool

-- | Execute a transaction computation against the connection pool.
runTx ::
  Pool ->
  TxMode ->
  Tx Postgres mode a ->
  IO (Either TxError a)
runTx _ _ tx = pure $ fst (executeTx tx initialTxState)
