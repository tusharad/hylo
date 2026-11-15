{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{-# HLINT ignore "Avoid restricted function" #-}

{- |
Module      : Hylo.Backend.CapabilitySpec
Description : Formal specification and executable test oracle for Epic 20 (Polyglot Multi-Backend Capability Matrix)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Backend.CapabilitySpec
  ( spec
  ) where

import Control.DeepSeq (NFData (..))
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Backend.Capability
import Hylo.Core
  ( Col
  , Ctx (..)
  , Id (..)
  , Table (..)
  , TableSchema
  , colSpecExpr
  , column
  , foreignKey
  , lit
  , primaryKey
  , tableSchema
  , (==.)
  , (>.)
  )
import Hylo.Core.Query
  ( AggExpr
  , Query
  , aggregate
  , asc
  , each
  , groupBy
  , having
  , limit
  , offset
  , orderBy
  , renderExpr
  , sum_
  , where_
  )
import Hylo.Test.ShouldNotTypecheck (shouldNotTypecheckWith)
import Test.Hspec
import Test.QuickCheck

default (Text, Int64, Double)

--------------------------------------------------------------------------------
-- Test HKD Entities & Schemas
--------------------------------------------------------------------------------

data UserT (ctx :: Ctx) = User
  { id :: !(Col ctx (Id UserT Int64))
  , orgId :: !(Col ctx (Id OrgT Int64))
  , email :: !(Col ctx Text)
  , active :: !(Col ctx Bool)
  , totalSpend :: !(Col ctx Int64)
  }
  deriving (Generic)

data OrgT (ctx :: Ctx) = Org
  { id :: !(Col ctx (Id OrgT Int64))
  , name :: !(Col ctx Text)
  }
  deriving (Generic)

instance Table UserT where
  toTableExpr alias meta =
    User
      { id = colSpecExpr alias meta.id
      , orgId = colSpecExpr alias meta.orgId
      , email = colSpecExpr alias meta.email
      , active = colSpecExpr alias meta.active
      , totalSpend = colSpecExpr alias meta.totalSpend
      }
  tableExprColumns expr =
    [ renderExpr expr.id
    , renderExpr expr.orgId
    , renderExpr expr.email
    , renderExpr expr.active
    , renderExpr expr.totalSpend
    ]

instance Table OrgT where
  toTableExpr alias meta =
    Org
      { id = colSpecExpr alias meta.id
      , name = colSpecExpr alias meta.name
      }
  tableExprColumns expr =
    [renderExpr expr.id, renderExpr expr.name]

userTable :: TableSchema UserT
userTable =
  tableSchema "users" $
    User
      { id = primaryKey "id" "bigint"
      , orgId = foreignKey "org_id" "bigint"
      , email = column "email" "text"
      , active = column "active" "boolean"
      , totalSpend = column "total_spend" "bigint"
      }

orgTable :: TableSchema OrgT
orgTable =
  tableSchema "orgs" $
    Org
      { id = primaryKey "id" "bigint"
      , name = column "name" "text"
      }

-- Parent and child entities for DataLoader batching tests
data ParentItem = ParentItem
  { parentId :: !Int64
  , parentName :: !Text
  }
  deriving (Show, Eq, Generic)

instance NFData ParentItem

data ChildItem = ChildItem
  { childId :: !Int64
  , childParentId :: !Int64
  , childLabel :: !Text
  }
  deriving (Show, Eq, Generic)

instance NFData ChildItem

--------------------------------------------------------------------------------
-- Test Suite
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Epic 20: Polyglot Multi-Backend Capability Matrix & NoSQL Adapters" $ do
  describe "20.4 Executable Oracle Specification" $ do
    it "compiles shared relational query identically across PostgreSQL and SQLite" $ do
      let q = do
            u <- each userTable
            where_ (u.active ==. lit True)
            pure u.email
          pgSQL = renderPostgresSQL q
          sqliteSQL = renderSQLiteSQL q
      pgSQL `shouldBe` sqliteSQL

    it "statically rejects recursive CTEs on DynamoDB with [Hylo Error E008]" $ do
      shouldNotTypecheckWith
        "[Hylo Error E008]"
        ( let invalidDynamoQuery =
                withRecursive "nodes" undefined undefined undefined :: Query DynamoDB (Col 'Expr Int64)
           in invalidDynamoQuery
        )

  describe "Formal Law 1: Cross-Backend Semantic Equivalence" $ do
    it "renders identical SQL for multi-predicate WHERE conjunctions across PostgreSQL and SQLite" $ do
      let q = do
            u <- each userTable
            where_ (u.active ==. lit True)
            where_ (u.totalSpend >. lit 100)
            pure (u.id, u.email)
          pgSQL = renderPostgresSQL q
          sqliteSQL = renderSQLiteSQL q
      pgSQL `shouldBe` sqliteSQL

    it "renders identical SQL for relational JOIN queries across PostgreSQL and SQLite" $ do
      let q = do
            u <- each userTable
            o <- each orgTable
            where_ (u.orgId ==. o.id)
            pure (u.email, o.name)
          pgSQL = renderPostgresSQL q
          sqliteSQL = renderSQLiteSQL q
      pgSQL `shouldBe` sqliteSQL

    it "renders identical SQL for GROUP BY and HAVING queries across PostgreSQL and SQLite" $ do
      let q :: forall db. Query db (Col 'Expr (Id OrgT Int64), AggExpr Int64)
          q = do
            u <- each userTable
            (orgId, totalSpend) <- aggregate $ do
              groupBy u.orgId
              let spendSum = sum_ u.totalSpend
              pure (u.orgId, spendSum)
            having (totalSpend >. lit 500)
            pure (orgId, totalSpend)
          pgSQL = renderPostgresSQL q
          sqliteSQL = renderSQLiteSQL q
      pgSQL `shouldBe` sqliteSQL

    it "renders identical SQL for ORDER BY, LIMIT, and OFFSET across PostgreSQL and SQLite" $ do
      let q :: forall db. Query db (Col 'Expr (Id UserT Int64), Col 'Expr Text)
          q = do
            u <- each userTable
            orderBy [asc u.email]
            limit 25
            offset 50
            pure (u.id, u.email)
          pgSQL = renderPostgresSQL q
          sqliteSQL = renderSQLiteSQL q
      pgSQL `shouldBe` sqliteSQL

  describe "Formal Law 2: DataLoader Batching Complexity" $ do
    it "executes exactly 2 queries regardless of parent item count (QueryCount == 2)" $ do
      property $ \(Positive (n :: Int)) ->
        let count = n `mod` 50
            parents = [ParentItem (fromIntegral i) ("Parent " <> T.pack (show i)) | i <- [1 .. count]]
            mockFetchChildren keys =
              [ ChildItem (k * 10 + c) k ("Child " <> T.pack (show (k * 10 + c)))
              | k <- keys
              , c <- [1 .. 3]
              ]
            batch = viaBatchLoader @DynamoDB parents (.parentId) mockFetchChildren (.childParentId)
         in batch.batchQueryCount == 2 && batch.batchParentCount == length parents

    it "executes exactly 2 queries on empty parent input pool" $ do
      let parents = [] :: [ParentItem]
          mockFetchChildren (_ :: [Int64]) = []
          batch = viaBatchLoader @DynamoDB parents (.parentId) mockFetchChildren (.childParentId)
      batch.batchQueryCount `shouldBe` 2
      batch.batchParentCount `shouldBe` 0
      batch.batchChildCount `shouldBe` 0

    it "correctly stitches child collections to their respective parent keys in O(N + M) time" $ do
      let parents = [ParentItem 1 "Org 1", ParentItem 2 "Org 2", ParentItem 3 "Org 3"]
          allChildren =
            [ ChildItem 101 1 "User 1-A"
            , ChildItem 102 1 "User 1-B"
            , ChildItem 201 2 "User 2-A"
            ]
          fetchChildren keys = filter (\c -> c.childParentId `elem` keys) allChildren
          batch = viaBatchLoader @DynamoDB parents (.parentId) fetchChildren (.childParentId)
      length batch.batchResults `shouldBe` 3
      case lookup (ParentItem 1 "Org 1") [(p, cs) | (p, cs) <- batch.batchResults] of
        Just cs -> map (.childId) cs `shouldBe` [101, 102]
        Nothing -> expectationFailure "Parent 1 missing"
      case lookup (ParentItem 2 "Org 2") [(p, cs) | (p, cs) <- batch.batchResults] of
        Just cs -> map (.childId) cs `shouldBe` [201]
        Nothing -> expectationFailure "Parent 2 missing"
      case lookup (ParentItem 3 "Org 3") [(p, cs) | (p, cs) <- batch.batchResults] of
        Just cs -> map (.childId) cs `shouldBe` []
        Nothing -> expectationFailure "Parent 3 missing"

  describe "Compile-Time Capability Rejection ([Hylo Error E008])" $ do
    it "statically rejects recursive CTEs on MongoDB with [Hylo Error E008]" $ do
      shouldNotTypecheckWith
        "[Hylo Error E008]"
        ( let invalidQuery =
                withRecursive "nodes" undefined undefined undefined :: Query MongoDB (Col 'Expr Int64)
           in invalidQuery
        )

    it "statically rejects window functions on DynamoDB with [Hylo Error E008]" $ do
      shouldNotTypecheckWith
        "[Hylo Error E008]"
        (windowFunctionsDict (Proxy @DynamoDB))

    it "statically rejects window functions on MongoDB with [Hylo Error E008]" $ do
      shouldNotTypecheckWith
        "[Hylo Error E008]"
        (windowFunctionsDict (Proxy @MongoDB))

    it "statically rejects relational joins on DynamoDB with [Hylo Error E008]" $ do
      shouldNotTypecheckWith
        "[Hylo Error E008]"
        (relationalJoinsDict (Proxy @DynamoDB))

    it "statically rejects relational joins on MongoDB with [Hylo Error E008]" $ do
      shouldNotTypecheckWith
        "[Hylo Error E008]"
        (relationalJoinsDict (Proxy @MongoDB))

    it "statically rejects lateral subqueries on DynamoDB with [Hylo Error E008]" $ do
      shouldNotTypecheckWith
        "[Hylo Error E008]"
        (lateralSubqueriesDict (Proxy @DynamoDB))

    it "statically rejects lateral subqueries on MongoDB with [Hylo Error E008]" $ do
      shouldNotTypecheckWith
        "[Hylo Error E008]"
        (lateralSubqueriesDict (Proxy @MongoDB))

    it "statically rejects vector search on SQLite with [Hylo Error E008]" $ do
      shouldNotTypecheckWith
        "[Hylo Error E008]"
        (vectorSearchDict (Proxy @SQLite))

    it "statically rejects vector search on DynamoDB with [Hylo Error E008]" $ do
      shouldNotTypecheckWith
        "[Hylo Error E008]"
        (vectorSearchDict (Proxy @DynamoDB))

  describe "SQLite Full Relational Capability Verification" $ do
    it "confirms SQLite provides relational joins, window functions, and recursive CTEs" $ do
      relationalJoinsDict (Proxy @SQLite) `shouldBe` ()
      lateralSubqueriesDict (Proxy @SQLite) `shouldBe` ()
      windowFunctionsDict (Proxy @SQLite) `shouldBe` ()
      recursiveCTEDict (Proxy @SQLite) `shouldBe` ()
      transactionsDict (Proxy @SQLite) `shouldBe` ()
      batchLookupDict (Proxy @SQLite) `shouldBe` ()
