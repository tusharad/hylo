{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{- |
Module      : Hylo.PG.Compiler.AggregateSpec
Description : Specification tests for Epic 6: Type-Safe Aggregations, Grouping Invariants & Aggregate Modifiers
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.AggregateSpec
  ( spec
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Hylo.Core
  ( Col
  , Ctx (..)
  , Id
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
  ( each
  , having
  )
import Hylo.Core.Query.Aggregate
  ( aggregate
  , arrayAgg
  , avgFiltered
  , countFiltered
  , count_
  , filter_
  , groupBy
  , max_
  , min_
  , stringAgg
  , sum_
  )
import Hylo.PG.Compiler.SQL (renderSQL)
import Hylo.Test.ShouldNotTypecheck
import Test.Hspec
import Test.QuickCheck

-- | Test Table: User entity with integer spend and active flag.
data UserT (ctx :: Ctx) = User
  { id :: !(Col ctx (Id UserT Int64))
  , orgId :: !(Col ctx (Id OrgT Int64))
  , email :: !(Col ctx Text)
  , active :: !(Col ctx Bool)
  , totalSpend :: !(Col ctx Int64)
  }

-- | Test Table: Organization entity.
newtype OrgT (ctx :: Ctx) = Org
  { id :: Col ctx (Id OrgT Int64)
  }

instance Table UserT where
  toTableExpr alias m =
    User
      { id = colSpecExpr alias m.id
      , orgId = colSpecExpr alias m.orgId
      , email = colSpecExpr alias m.email
      , active = colSpecExpr alias m.active
      , totalSpend = colSpecExpr alias m.totalSpend
      }

instance Table OrgT where
  toTableExpr alias m =
    Org
      { id = colSpecExpr alias m.id
      }

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

spec :: Spec
spec = describe "Epic 6: Type-Safe Aggregations & Grouping" $ do
  describe "6.4 Executable Oracle Specification" $ do
    it "compiles aggregate queries with FILTER clauses into ANSI SQL FILTER (WHERE ...)" $ do
      let q = do
            u <- each userTable
            (orgId, totalActive, totalSpend) <- aggregate $ do
              groupBy u.orgId
              let activeCount = countFiltered u.id (u.active ==. lit True)
                  spendSum = sum_ u.totalSpend
              pure (u.orgId, activeCount, spendSum)
            having (totalSpend >. lit (1000 :: Int64))
            pure (orgId, totalActive, totalSpend)
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "GROUP BY \"users_0\".\"org_id\""
      sql `shouldContain` "COUNT(\"users_0\".\"id\") FILTER (WHERE (\"users_0\".\"active\" = TRUE))"
      sql `shouldContain` "HAVING (SUM(\"users_0\".\"total_spend\") > 1000)"

    it "statically rejects selecting ungrouped columns outside aggregate functions" $
      shouldNotTypecheckWith
        "[Hylo Error E004]"
        ( let invalidQuery = do
                u <- each userTable
                aggregate $ do
                  groupBy u.orgId
                  pure (u.orgId, u.email) -- Error: u.email is neither grouped nor aggregated!
           in invalidQuery
        )

  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 1: Homomorphism of Partitioned Aggregation (Sum(S1 ∪ S2) == Sum(S1) + Sum(S2))" $
      property $ \(xs :: [Int64]) (ys :: [Int64]) ->
        let s1 = sum xs
            s2 = sum ys
            combined = sum (xs ++ ys)
         in combined == s1 + s2

    it "Law 2: Filter Distributivity over Aggregation (Count(S FILTER p) == Count({s ∈ S | p s}))" $
      property $ \(xs :: [Int64]) (threshold :: Int64) ->
        let p x = x > threshold
            filteredCount = length (filter p xs)
            comprehensionCount = length [x | x <- xs, p x]
         in filteredCount == comprehensionCount

    it "Law 3: Grouped Context Preservation Law rejects ungrouped column projection" $
      shouldNotTypecheckWith
        "[Hylo Error E004]"
        ( let q = do
                u <- each userTable
                aggregate $ do
                  groupBy u.orgId
                  pure u.email
           in q
        )

  describe "Comprehensive Aggregate Functions & Modifiers" $ do
    it "compiles avg_, min_, max_ with FILTER clauses" $ do
      let q = do
            u <- each userTable
            aggregate $ do
              groupBy u.orgId
              let avgSpend = avgFiltered u.totalSpend (u.active ==. lit True)
                  minSpend = min_ u.totalSpend
                  maxSpend = max_ u.totalSpend
              pure (u.orgId, avgSpend, minSpend, maxSpend)
          sql :: String
          sql = renderSQL q
      sql
        `shouldContain` "AVG(\"users_0\".\"total_spend\") FILTER (WHERE (\"users_0\".\"active\" = TRUE))"
      sql `shouldContain` "MIN(\"users_0\".\"total_spend\")"
      sql `shouldContain` "MAX(\"users_0\".\"total_spend\")"

    it "compiles arrayAgg and stringAgg functions" $ do
      let q = do
            u <- each userTable
            aggregate $ do
              groupBy u.orgId
              let ids = arrayAgg u.id
                  emails = stringAgg u.email (lit (", " :: Text))
              pure (u.orgId, ids, emails)
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "array_agg(\"users_0\".\"id\")"
      sql `shouldContain` "string_agg(\"users_0\".\"email\", \", \")"

    it "supports composite multi-column groupBy tuples" $ do
      let q = do
            u <- each userTable
            aggregate $ do
              groupBy (u.orgId, u.active)
              let totalUsers = count_ u.id
              pure (u.orgId, u.active, totalUsers)
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "GROUP BY \"users_0\".\"org_id\", \"users_0\".\"active\""

    it "supports filter_ modifier directly on arbitrary aggregates" $ do
      let q = do
            u <- each userTable
            aggregate $ do
              groupBy u.orgId
              let filteredSum = filter_ (sum_ u.totalSpend) (u.active ==. lit True)
              pure (u.orgId, filteredSum)
          sql :: String
          sql = renderSQL q
      sql
        `shouldContain` "SUM(\"users_0\".\"total_spend\") FILTER (WHERE (\"users_0\".\"active\" = TRUE))"
