{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Hylo.NoSQL.Dynamo
Description : AWS DynamoDB document adapter, node schemas, and batch execution engine
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.NoSQL.Dynamo
  ( -- * DynamoDB Backend Marker
    DynamoDB

    -- * Node Graph Entity (HKD)
  , NodeT (..)
  , nodeId
  , nodeLabel
  , nodeTable

    -- * DynamoDB Batch Execution Engine (Law 2)
  , DynamoBatchRequest (..)
  , executeDynamoBatchGet
  , batchLoadDynamoRelations
  ) where

import Control.DeepSeq (NFData (..))
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hylo.Backend.Capability
  ( DataLoaderBatch (..)
  , DynamoDB
  , viaBatchLoader
  )
import Hylo.Core
  ( Col
  , Ctx (..)
  , Id (..)
  , Projectable (..)
  , Table (..)
  , TableSchema
  , colExpr
  , colSpecExpr
  , column
  , primaryKey
  , renderExpr
  , tableSchema
  )
import Hylo.Core.Query.CTE (CTERow (..))

--------------------------------------------------------------------------------
-- 1. HKD Node Record
--------------------------------------------------------------------------------

-- | Domain node entity representing hierarchical nodes or items.
data NodeT (ctx :: Ctx) = Node
  { id :: !(Col ctx (Id NodeT Int64))
  , label :: !(Col ctx Text)
  }
  deriving (Generic)

-- | Safe dot-accessor helper for node identifier.
nodeId :: NodeT ctx -> Col ctx (Id NodeT Int64)
nodeId n = n.id

-- | Safe dot-accessor helper for node text label.
nodeLabel :: NodeT ctx -> Col ctx Text
nodeLabel n = n.label

deriving instance Show (NodeT 'Result)
deriving instance Eq (NodeT 'Result)

instance Table NodeT where
  toTableExpr alias meta =
    Node
      { id = colSpecExpr alias meta.id
      , label = colSpecExpr alias meta.label
      }
  tableExprColumns expr =
    [renderExpr expr.id, renderExpr expr.label]

-- | Static schema definition for the nodes collection.
nodeTable :: TableSchema NodeT
nodeTable =
  tableSchema "nodes" $
    Node
      { id = primaryKey "id" "bigint"
      , label = column "label" "text"
      }

instance Projectable (NodeT 'Expr) where
  projectColumns n = [renderExpr n.id, renderExpr n.label]

instance NFData (NodeT 'Expr) where
  rnf (Node i l) = rnf i `seq` rnf l

instance CTERow (NodeT 'Expr) (NodeT 'Expr) where
  defaultCTEColumns _ = ["id", "label"]
  mkCTERow cols alias =
    let c0 = case cols of
          (x : _) -> x
          [] -> "id"
        c1 = case drop 1 cols of
          (x : _) -> x
          [] -> "label"
     in Node
          { id = colExpr alias c0
          , label = colExpr alias c1
          }

--------------------------------------------------------------------------------
-- 2. DynamoDB Batch Execution Engine (Law 2)
--------------------------------------------------------------------------------

-- | In-flight DynamoDB BatchGetItem request specification.
data DynamoBatchRequest = DynamoBatchRequest
  { tableName :: !Text
  , requestKeys :: ![Int64]
  }
  deriving (Show, Eq, Generic)

instance NFData DynamoBatchRequest

-- | Simulate executing a secondary batch lookup via DynamoDB BatchGetItem API.
executeDynamoBatchGet ::
  Text ->
  [Int64] ->
  (Int64 -> item) ->
  [item]
executeDynamoBatchGet _tName ids itemFactory =
  map itemFactory ids

-- | High-level 2-query batch loader execution for DynamoDB relationships.
batchLoadDynamoRelations ::
  (Ord key) =>
  [parent] ->
  (parent -> key) ->
  ([key] -> [child]) ->
  (child -> key) ->
  DataLoaderBatch parent child
batchLoadDynamoRelations =
  viaBatchLoader @DynamoDB
