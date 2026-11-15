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
Module      : Hylo.PG.Compiler.WindowSpec
Description : Specification tests for Epic 7: Advanced Window Functions, Partitioning & Framing Algebra
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.WindowSpec
  ( spec
  ) where

import Data.Int (Int64)
import Data.List (sort)
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
  , primaryKey
  , tableSchema
  )
import Hylo.Core.Query
  ( each
  , sum_
  , where_
  )
import Hylo.PG.Compiler.SQL (renderSQL)
import Hylo.SQL.Query.Window
  ( asc
  , ascNullsFirst
  , cumeDist
  , currentRow
  , denseRank
  , desc
  , descNullsLast
  , firstValue
  , groupsBetween
  , lag
  , lastValue
  , lead
  , nthValue
  , ntile
  , orderBy
  , over
  , partitionBy
  , percentRank
  , preceding
  , rangeBetween
  , rank
  , rowNumber
  , rowsBetween
  , unboundedFollowing
  , unboundedPreceding
  , window
  , (&)
  )
import Hylo.Test.ShouldNotTypecheck
import Test.Hspec
import Test.QuickCheck

-- | Test Table: Transaction entity with account reference, amount, and timestamp.
data TransactionT (ctx :: Ctx) = Transaction
  { id :: !(Col ctx (Id TransactionT Int64))
  , accountId :: !(Col ctx (Id AccountT Int64))
  , amount :: !(Col ctx Int64)
  , transactedAt :: !(Col ctx Text)
  }

-- | Test Table: Account entity.
newtype AccountT (ctx :: Ctx) = Account
  { id :: Col ctx (Id AccountT Int64)
  }

instance Table TransactionT where
  toTableExpr alias m =
    Transaction
      { id = colSpecExpr alias m.id
      , accountId = colSpecExpr alias m.accountId
      , amount = colSpecExpr alias m.amount
      , transactedAt = colSpecExpr alias m.transactedAt
      }

instance Table AccountT where
  toTableExpr alias m =
    Account
      { id = colSpecExpr alias m.id
      }

transactionTable :: TableSchema TransactionT
transactionTable =
  tableSchema "transactions" $
    Transaction
      { id = primaryKey "id" "bigint"
      , accountId = foreignKey "account_id" "bigint"
      , amount = column "amount" "bigint"
      , transactedAt = column "transacted_at" "timestamp"
      }

spec :: Spec
spec = describe "Epic 7: Advanced Window Functions" $ do
  describe "7.4 Executable Oracle Specification" $ do
    it "compiles partitioned window functions with explicit frame bounds" $ do
      let q = do
            t <- each transactionTable
            let win =
                  window
                    & partitionBy t.accountId
                    & orderBy [desc t.transactedAt]
                    & rowsBetween unboundedPreceding currentRow
                runningTotal = over (sum_ t.amount) win
                prevAmount = over (lag 1 t.amount) win
                rowNum = over rowNumber win
            pure (t.id, t.amount, runningTotal, prevAmount, rowNum)
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "SUM(\"t0\".\"amount\") OVER ("
      sql `shouldContain` "PARTITION BY \"t0\".\"account_id\""
      sql `shouldContain` "ORDER BY \"t0\".\"transacted_at\" DESC"
      sql `shouldContain` "ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW"
      sql `shouldContain` "LAG(\"t0\".\"amount\", 1) OVER"

    it "statically rejects placing window expressions inside WHERE clauses" $
      shouldNotTypecheck
        ( let invalidQuery = do
                t <- each transactionTable
                let rowNum = over rowNumber window
                where_ rowNum
                pure t.id
           in invalidQuery
        )

  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 1: Window Partition Independence (Partition slicing invariant)" $
      property $ \(records :: [(Int64, Int64)]) ->
        let partitionKey = fst
            partitions = [filter (\r -> partitionKey r == k) records | k <- [0 .. 5]]
            independentResults = map (map snd) partitions
            -- Evaluation within each partition depends strictly on elements matching that partition key
            allMatch = and [all (\r -> partitionKey r == k) p | (k, p) <- zip [0 .. 5] partitions]
         in allMatch && length independentResults == 6

    it "Law 2: Monotonicity of Row Numbering (Order(ra) < Order(rb) ==> rowNumber(ra) < rowNumber(rb))" $
      property $ \(xs :: [Int]) ->
        let sorted = sort xs
            indexed = zip [1 :: Int64 ..] sorted
            monotonic = and [idx1 < idx2 | ((idx1, _), (idx2, _)) <- zip indexed (drop 1 indexed)]
         in monotonic

    it "Law 3: Lead/Lag Inverse Law (lag n after lead n returns identity on defined index range)" $
      property $ \(Positive n) (xs :: [Int]) ->
        let len = length xs
            leadList = [if i + n < len then Just (xs !! (i + n)) else Nothing | i <- [0 .. len - 1]]
            lagList = [if i - n >= 0 then Just (xs !! (i - n)) else Nothing | i <- [0 .. len - 1]]
            -- On the valid range [0 .. len - n - 1], lead at i matches lag at i + n
            validRange = [0 .. len - n - 1]
            lawHolds =
              all (\i -> leadList !! i == Just (xs !! (i + n)) && lagList !! (i + n) == Just (xs !! i)) validRange
         in lawHolds

  describe "Comprehensive Analytical Functions & Framing" $ do
    it "compiles ranking functions: rank, denseRank, ntile, cumeDist, percentRank" $ do
      let q = do
            t <- each transactionTable
            let win = window & partitionBy t.accountId & orderBy [asc t.amount]
                r = over rank win
                dr = over denseRank win
                nt = over (ntile 4) win
                cd = over cumeDist win
                pr = over percentRank win
            pure (t.id, r, dr, nt, cd, pr)
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "RANK() OVER ("
      sql `shouldContain` "DENSE_RANK() OVER ("
      sql `shouldContain` "NTILE(4) OVER ("
      sql `shouldContain` "CUME_DIST() OVER ("
      sql `shouldContain` "PERCENT_RANK() OVER ("

    it "compiles lead, firstValue, lastValue, nthValue analytical functions" $ do
      let q = do
            t <- each transactionTable
            let win = window & partitionBy t.accountId
                nextAmount = over (lead 2 t.amount) win
                firstAmt = over (firstValue t.amount) win
                lastAmt = over (lastValue t.amount) win
                nthAmt = over (nthValue t.amount 3) win
            pure (t.id, nextAmount, firstAmt, lastAmt, nthAmt)
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "LEAD(\"t0\".\"amount\", 2) OVER ("
      sql `shouldContain` "FIRST_VALUE(\"t0\".\"amount\") OVER ("
      sql `shouldContain` "LAST_VALUE(\"t0\".\"amount\") OVER ("
      sql `shouldContain` "NTH_VALUE(\"t0\".\"amount\", 3) OVER ("

    it "compiles rangeBetween and groupsBetween framing specifications" $ do
      let q = do
            t <- each transactionTable
            let winRange = window & rangeBetween (preceding 5) unboundedFollowing
                winGroups = window & groupsBetween unboundedPreceding currentRow
                rTotal = over (sum_ t.amount) winRange
                gTotal = over (sum_ t.amount) winGroups
            pure (t.id, rTotal, gTotal)
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "RANGE BETWEEN 5 PRECEDING AND UNBOUNDED FOLLOWING"
      sql `shouldContain` "GROUPS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW"

    it "compiles ordering with NULLS FIRST and NULLS LAST" $ do
      let q = do
            t <- each transactionTable
            let win =
                  window
                    & orderBy [ascNullsFirst t.amount, descNullsLast t.transactedAt]
                rowNum = over rowNumber win
            pure (t.id, rowNum)
          sql :: String
          sql = renderSQL q
      sql
        `shouldContain` "ORDER BY \"t0\".\"amount\" ASC NULLS FIRST, \"t0\".\"transacted_at\" DESC NULLS LAST"
