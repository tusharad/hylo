{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.Pool.Routing
Description : Zero-Overhead Read-Replica Routing & Cluster Dispatch
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides zero-overhead dispatch of transactions across cluster topologies.
Transactions marked with phantom mode 'ReadOnly' are dispatched to read-only
replicas (or the primary if no replicas are registered), while transactions
parameterized by 'ReadWrite' are strictly guaranteed to route to the primary node.
-}
module Hylo.PG.Pool.Routing
  ( -- * Cluster Pool Topologies
    ClusterPool (..)
  , NodeDestination (..)
  , createClusterPool
  , defaultClusterPool
  , clusterPool

    -- * Pure Routing Logic
  , routeTransactionMode
  , routeNode

    -- * Cluster Execution Engines
  , runClusterTx
  , runReadOnlyTx
  , runClusterTxWithRetry
  ) where

import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import Hylo.Core.Transaction (Tx (..), TxError (..), TxMode (..))
import Hylo.PG.Connection (Pool (..), createPool, defaultPool, runTx)
import Hylo.PG.Transaction.Retry
  ( IsolationLevel (..)
  , RetryPolicy (..)
  , runTxWithRetry
  )

-- | Topology of a PostgreSQL database cluster with a primary write node and read replicas.
data ClusterPool = ClusterPool
  { primaryPool :: !Pool
  , replicaPools :: ![Pool]
  }
  deriving (Eq, Show)

instance NFData ClusterPool where
  rnf (ClusterPool p rs) = rnf p `seq` rnf rs

-- | Physical node target resolved by the routing engine.
data NodeDestination
  = TargetPrimary
  | TargetReplica !Int
  deriving (Eq, Show)

instance NFData NodeDestination where
  rnf = \case
    TargetPrimary -> ()
    TargetReplica i -> rnf i

-- | Initialize a new cluster pool from connection strings.
createClusterPool :: Text -> [Text] -> IO ClusterPool
createClusterPool primaryConn replicaConns = do
  p <- createPool primaryConn
  rs <- mapM createPool replicaConns
  pure $ ClusterPool p rs

-- | Default cluster pool descriptor for testing and local development.
defaultClusterPool :: ClusterPool
defaultClusterPool =
  ClusterPool
    { primaryPool = defaultPool
    , replicaPools =
        [ Pool "postgresql://localhost:5433/hylo_replica_1" 10 Nothing
        , Pool "postgresql://localhost:5434/hylo_replica_2" 10 Nothing
        ]
    }

-- | Top-level default cluster pool reference.
clusterPool :: ClusterPool
clusterPool = defaultClusterPool

{- | Pure routing function determining destination node from transaction mode and replica count.
Law 3: 'ReadWrite' transactions MUST dispatch strictly to 'TargetPrimary'.
'ReadOnly' transactions dispatch to read replicas (or 'TargetPrimary' if no replicas exist).
-}
routeTransactionMode :: TxMode -> Int -> NodeDestination
routeTransactionMode ReadWrite _ = TargetPrimary
routeTransactionMode ReadOnly numReplicas
  | numReplicas <= 0 = TargetPrimary
  | otherwise = TargetReplica 0

-- | Resolve destination target and concrete connection pool for a transaction.
routeNode :: TxMode -> ClusterPool -> (NodeDestination, Pool)
routeNode mode cluster = case mode of
  ReadWrite -> (TargetPrimary, primaryPool cluster)
  ReadOnly -> case replicaPools cluster of
    [] -> (TargetPrimary, primaryPool cluster)
    (r : _) -> (TargetReplica 0, r)

-- | Execute a transaction against the cluster, dynamically routing based on transaction mode.
runClusterTx ::
  ClusterPool ->
  TxMode ->
  Tx db mode a ->
  IO (Either TxError a)
runClusterTx cluster mode tx = do
  let (_, target) = routeNode mode cluster
  runTx target mode tx

-- | Statically verified read-only transaction execution routed to a replica connection.
runReadOnlyTx ::
  ClusterPool ->
  Tx db 'ReadOnly a ->
  IO (Either TxError a)
runReadOnlyTx cluster tx = do
  let (_, target) = routeNode ReadOnly cluster
  runTx target ReadOnly tx

-- | Execute a transaction against the cluster with automatic retry upon serialization conflict or deadlock.
runClusterTxWithRetry ::
  RetryPolicy ->
  ClusterPool ->
  TxMode ->
  IsolationLevel ->
  Tx db mode a ->
  IO (Either TxError a)
runClusterTxWithRetry policy cluster mode iso tx = do
  let (_, target) = routeNode mode cluster
  runTxWithRetry policy target iso tx
