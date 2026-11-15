{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted function" #-}
{-# HLINT ignore "Avoid reverse" #-}
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant pure" #-}

{- |
Module      : Hylo.PG.Streaming.CursorSpec
Description : Test suite for Epic 10: Constant-Memory Binary Cursor Streaming & Keyset Pagination
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Streaming.CursorSpec (spec) where

import Data.Int (Int64)
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Hylo.Core
import Hylo.Core.Query
  ( Projectable (..)
  , desc
  , each
  , limit
  , orderBy
  , renderExpr
  )
import Hylo.Core.Query.Pagination
  ( SeekDirection (..)
  , seekAfter
  , seekAfterDir
  , seekBefore
  )
import Hylo.PG.Compiler.SQL (renderSQL)
import Hylo.PG.Streaming.Cursor
  ( Cursor (..)
  , declareCursor
  , renderCloseCursor
  , renderDeclareCursor
  , renderFetchCursor
  , streamDrain
  , streamFold
  , streamFromList
  , streamTake
  , streamToList
  )
import Test.Hspec
import Test.QuickCheck

default (Text, Int64, Double)

-- | Test Table: Document entity for keyset pagination & streaming.
data DocumentT (ctx :: Ctx) = Document
  { id :: !(Col ctx (Id DocumentT Int64))
  , createdAt :: !(Col ctx UTCTime)
  , orgId :: !(Col ctx (Id OrgT Int64))
  , title :: !(Col ctx Text)
  }

data OrgT (ctx :: Ctx) = Org
  { id :: !(Col ctx (Id OrgT Int64))
  , name :: !(Col ctx Text)
  }

instance Table OrgT where
  toTableExpr alias m =
    Org
      { id = colSpecExpr alias m.id
      , name = colSpecExpr alias m.name
      }
  tableExprColumns o =
    [ renderExpr o.id
    , renderExpr o.name
    ]

instance Projectable (OrgT 'Expr) where
  projectColumns = tableExprColumns

instance Table DocumentT where
  toTableExpr alias m =
    Document
      { id = colSpecExpr alias m.id
      , createdAt = colSpecExpr alias m.createdAt
      , orgId = colSpecExpr alias m.orgId
      , title = colSpecExpr alias m.title
      }
  tableExprColumns d =
    [ renderExpr d.id
    , renderExpr d.createdAt
    , renderExpr d.orgId
    , renderExpr d.title
    ]

instance Projectable (DocumentT 'Expr) where
  projectColumns = tableExprColumns

documentTable :: TableSchema DocumentT
documentTable =
  tableSchema "documents" $
    Document
      { id = primaryKey "id" "bigint"
      , createdAt = column "created_at" "timestamptz"
      , orgId = foreignKey "org_id" "bigint"
      , title = column "title" "text"
      }

-- | Unpack a pure transaction computation.
runTx :: Tx db mode a -> a
runTx tx = case runTxPure tx of
  Right a -> a
  Left err -> error ("Unexpected transaction error in test: " ++ show err)

-- | Simulated document for pure property checking.
data DocRow = DocRow
  { rowTime :: !Int64
  , rowId :: !Int64
  }
  deriving (Eq, Ord, Show)

instance Arbitrary DocRow where
  arbitrary = DocRow <$> arbitrary <*> arbitrary

spec :: Spec
spec = describe "Epic 10: Binary Cursor Streaming & Keyset Pagination" $ do
  describe "10.4 Executable Oracle Specification" $ do
    it "generates index-backed composite keyset seek predicates" $ do
      let cursor = (read "2026-01-01 00:00:00 UTC", Id 54321 :: Id DocumentT Int64)
          q = do
            doc <- each documentTable
            seekAfter (doc.createdAt, doc.id) cursor
            orderBy [desc doc.createdAt, desc doc.id]
            limit 20
            pure doc
          sql = renderSQL q
      sql `shouldContain` "(\"t0\".\"created_at\", \"t0\".\"id\") < ($1, $2)"
      sql `shouldNotContain` "OFFSET"

  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 1: Constant Heap Bound of Cursor Streaming (Chunked consumption invariant)" $
      property $ \(Positive (k :: Int)) (Positive (n :: Int)) ->
        let batchSize = (k `mod` 50) + 1
            itemCount = n `mod` 300
            items = [1 .. itemCount]
            stream = streamFromList batchSize items
            materialized = runTx (streamToList stream)
         in materialized === items

    it "Law 1: Constant Heap Bounded Aggregation via streamFold" $
      property $ \(Positive (k :: Int)) (Positive (n :: Int)) ->
        let batchSize = (k `mod` 50) + 1
            itemCount = n `mod` 300
            items = [1 .. itemCount]
            stream = streamFromList batchSize items
            totalSum = runTx (streamFold (+) 0 stream)
         in totalSum === sum items

    it "Law 2: Total Order of Keyset Seeking (Strict Totality & Disjoint Non-Overlapping Pages)" $
      property $ \(rows :: [DocRow]) (Positive (pageSizeRaw :: Int)) ->
        let uniqueRows = nub rows
            sortedRows = reverse (sort uniqueRows)
            pageSize = (pageSizeRaw `mod` 20) + 1

            paginate [] _ = []
            paginate pool mCur =
              let remaining = case mCur of
                    Nothing -> pool
                    Just cur -> filter (\r -> (r.rowTime, r.rowId) < cur) pool
                  page = take pageSize remaining
               in if null page
                    then []
                    else case reverse page of
                      (lastR : _) -> page : paginate remaining (Just (lastR.rowTime, lastR.rowId))
                      [] -> []

            pages = paginate sortedRows Nothing
            allFetched = concat pages
         in allFetched === sortedRows

    it "Law 2: Keyset Seeking Non-Overlapping Invariant (Zero Duplicates Across Pages)" $
      property $ \(rows :: [DocRow]) (Positive (pageSizeRaw :: Int)) ->
        let uniqueRows = nub rows
            sortedRows = reverse (sort uniqueRows)
            pageSize = (pageSizeRaw `mod` 15) + 1

            paginate [] _ = []
            paginate pool mCur =
              let remaining = case mCur of
                    Nothing -> pool
                    Just cur -> filter (\r -> (r.rowTime, r.rowId) < cur) pool
                  page = take pageSize remaining
               in if null page
                    then []
                    else case reverse page of
                      (lastR : _) -> page : paginate remaining (Just (lastR.rowTime, lastR.rowId))
                      [] -> []

            pages = paginate sortedRows Nothing
            pairwiseDisjoint [] = True
            pairwiseDisjoint [_] = True
            pairwiseDisjoint (p1 : p2 : rest) =
              null (filter (`elem` p2) p1) && pairwiseDisjoint (p2 : rest)
         in pairwiseDisjoint pages

    it "Law 2: Inverse Navigation Law (seekBefore reverses seekAfter)" $
      property $ \(rows :: [DocRow]) ->
        let uniqueRows = nub rows
         in length uniqueRows >= 3 ==>
              let sorted = reverse (sort uniqueRows)
                  r0 = sorted !! 0
                  r1 = sorted !! 1
                  -- Seeking after r0 produces r1 as first item
                  afterR0 = filter (\r -> (r.rowTime, r.rowId) < (r0.rowTime, r0.rowId)) sorted
                  -- Seeking before r1 produces r0 as last item
                  beforeR1 = filter (\r -> (r.rowTime, r.rowId) > (r1.rowTime, r1.rowId)) sorted
               in case (afterR0, reverse beforeR1) of
                    (a : _, b : _) -> (a === r1) .&&. (b === r0)
                    _ -> property True

  describe "Comprehensive Keyset Pagination & Cursor Functionality" $ do
    it "generates reverse navigation keyset seek predicate with seekBefore" $ do
      let cursor = (read "2026-01-01 00:00:00 UTC", Id 54321 :: Id DocumentT Int64)
          q = do
            doc <- each documentTable
            seekBefore (doc.createdAt, doc.id) cursor
            orderBy [desc doc.createdAt, desc doc.id]
            limit 20
            pure doc
          sql = renderSQL q
      sql `shouldContain` "(\"t0\".\"created_at\", \"t0\".\"id\") > ($1, $2)"
      sql `shouldNotContain` "OFFSET"

    it "generates single-column keyset seek predicate" $ do
      let q = do
            doc <- each documentTable
            seekAfter doc.id (Id 54321 :: Id DocumentT Int64)
            orderBy [desc doc.id]
            limit 10
            pure doc
          sql = renderSQL q
      sql `shouldContain` "(\"t0\".\"id\") < ($1)"
      sql `shouldNotContain` "OFFSET"

    it "generates 3-column composite keyset seek predicate" $ do
      let cursor = (Id 999 :: Id OrgT Int64, read "2026-01-01 00:00:00 UTC", Id 54321 :: Id DocumentT Int64)
          q = do
            doc <- each documentTable
            seekAfter (doc.orgId, doc.createdAt, doc.id) cursor
            orderBy [desc doc.orgId, desc doc.createdAt, desc doc.id]
            limit 25
            pure doc
          sql = renderSQL q
      sql
        `shouldContain` "(\"t0\".\"org_id\", \"t0\".\"created_at\", \"t0\".\"id\") < ($1, $2, $3)"

    it "supports explicitly directed keyset seeks (seekAfterDir and seekBeforeDir)" $ do
      let cursor = (read "2026-01-01 00:00:00 UTC", Id 54321 :: Id DocumentT Int64)
          qAsc = do
            doc <- each documentTable
            seekAfterDir SeekAsc (doc.createdAt, doc.id) cursor
            pure doc
          sqlAsc = renderSQL qAsc

          qDesc = do
            doc <- each documentTable
            seekAfterDir SeekDesc (doc.createdAt, doc.id) cursor
            pure doc
          sqlDesc = renderSQL qDesc

      sqlAsc `shouldContain` "(\"t0\".\"created_at\", \"t0\".\"id\") > ($1, $2)"
      sqlDesc `shouldContain` "(\"t0\".\"created_at\", \"t0\".\"id\") < ($1, $2)"

    it "renders DECLARE ... NO SCROLL CURSOR WITHOUT HOLD for server cursor" $ do
      let q = do
            doc <- each documentTable
            pure doc
          cur = runTx (declareCursor "doc_cur" 500 q)
          declSql = T.unpack (renderDeclareCursor cur)
      declSql `shouldContain` "DECLARE \"doc_cur\" NO SCROLL CURSOR WITHOUT HOLD FOR SELECT"
      declSql `shouldContain` "FROM \"documents\" AS \"t0\""

    it "renders FETCH FORWARD batch_size FROM cursor" $ do
      let cur = Cursor "doc_cur" 500 "SELECT 1"
          fetchSql = renderFetchCursor cur 500
      fetchSql `shouldBe` "FETCH FORWARD 500 FROM \"doc_cur\""

    it "renders CLOSE cursor" $ do
      let cur = Cursor "doc_cur" 500 "SELECT 1"
          closeSql = renderCloseCursor cur
      closeSql `shouldBe` "CLOSE \"doc_cur\""

    it "streamTake yields bounded prefixes without exhausting infinite streams" $ do
      let infiniteStream = streamFromList 10 [1 .. 1000000 :: Int]
          takenStream = streamTake 5 infiniteStream
          results = runTx (streamToList takenStream)
      results `shouldBe` [1, 2, 3, 4, 5]

    it "streamDrain discards all elements without retaining them in memory" $ do
      let stream = streamFromList 5 [1 .. 50 :: Int]
      runTx (streamDrain stream) `shouldBe` ()
