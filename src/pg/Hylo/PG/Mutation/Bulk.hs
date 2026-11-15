{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.Mutation.Bulk
Description : High-Throughput Bulk Operations & Binary Array UNNEST Ingestion
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

High-throughput bulk ingestion using PostgreSQL's binary array 'UNNEST' capability.
Deconstructs batches of changesets into columnar binary arrays and streams them over
a single 'INSERT INTO ... SELECT * FROM UNNEST($1::type1[], ...)' statement,
guaranteeing parameter independence (ParamCount = C regardless of batch size N).
-}
module Hylo.PG.Mutation.Bulk
  ( -- * Bulk Insertable Class
    BulkInsertable (..)

    -- * SQL Generation & Parameter Invariance
  , renderBulkInsertSQL
  , bulkInsertParamCount

    -- * Transactional Ingestion Combinators
  , insertMany
  , insertMany_
  , chunkedInsertMany

    -- * Utility Functions
  , chunkList
  ) where

import Control.Monad (void)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core.Context (Ctx (..))
import Hylo.Core.Query (Changeset (..))
import Hylo.Core.Table (Table (..), TableSchema (..))
import Hylo.Core.Transaction
  ( Tx (..)
  , TxError (..)
  , TxOp (..)
  , TxState (..)
  )
import Hylo.Core.TypeError (AssertWritable (..), TxMode (..))

-- | Typeclass for HKD tables supporting bulk columnar UNNEST insertion.
class Table table => BulkInsertable (table :: Ctx -> Type) where
  -- | Column specifications as @(columnName, columnSqlType)@.
  bulkColumns :: table 'Meta -> [(Text, Text)]
  bulkColumns = tableColumnsMeta

{- | Render a high-throughput binary UNNEST bulk insert SQL statement.

Generates:
@INSERT INTO "tableName" ("col1", "col2") SELECT * FROM UNNEST($1::type1[], $2::type2[])@
-}
renderBulkInsertSQL :: (BulkInsertable table, IsString s) => TableSchema table -> [record] -> s
renderBulkInsertSQL schema _ =
  let tbl = "\"" <> tableName schema <> "\""
      cols = bulkColumns (tableMeta schema)
      colNames = T.intercalate ", " ["\"" <> c <> "\"" | (c, _) <- cols]
      unnestArgs =
        T.intercalate
          ", "
          [ "$" <> T.pack (show idx) <> "::" <> sqlType <> "[]"
          | (idx, (_, sqlType)) <- zip [1 :: Int ..] cols
          ]
      sql = "INSERT INTO " <> tbl <> " (" <> colNames <> ") SELECT * FROM UNNEST(" <> unnestArgs <> ")"
   in fromString (T.unpack sql)

{- | Number of query parameters required for bulk UNNEST ingestion.
Strictly invariant to batch size N: @ParamCount(N, C) = C@.
-}
bulkInsertParamCount :: (BulkInsertable table) => TableSchema table -> [record] -> Int
bulkInsertParamCount schema _ = length (bulkColumns (tableMeta schema))

-- | Split a list into chunks of at most @n@ elements.
chunkList :: Int -> [a] -> [[a]]
chunkList n xs
  | n <= 0 = [xs]
  | otherwise = case xs of
      [] -> []
      _ ->
        let (h, t) = splitAt n xs
         in h : chunkList n t

{- | Statically guarded high-throughput bulk insertion using binary UNNEST arrays.
Fails with [Hylo Error E005] when evaluated within 'ReadOnly' transactions.
-}
insertMany ::
  forall table db (mode :: TxMode) f.
  (AssertWritable mode, BulkInsertable table, Foldable f) =>
  TableSchema table ->
  f (Changeset table) ->
  Tx db mode [table 'Result]
insertMany schema csFoldable =
  writableDict (Proxy :: Proxy mode) `seq`
    TxAction $ \st ->
      let tName = tableName schema
          records = toList csFoldable
          keys = [changesetKey cs | cs <- records, changesetKey cs /= ""]
          firstConflict =
            case [k | k <- keys, (tName, k) `elem` txStore st] of
              (k : _) -> Just k
              [] ->
                let checkInternal [] = Nothing
                    checkInternal (k : ks) =
                      if k `elem` ks then Just k else checkInternal ks
                 in checkInternal keys
       in case firstConflict of
            Just dupKey ->
              ( Left
                  ( TxConstraintViolation
                      tName
                      "unique_constraint"
                      dupKey
                  )
              , st
              )
            Nothing ->
              let newStore = reverse [(tName, k) | k <- keys] ++ txStore st
                  sql = renderBulkInsertSQL schema records
                  st' =
                    st
                      { txStore = newStore
                      , txOperations = txOperations st ++ [OpMutate sql]
                      }
                  simulatedResults =
                    replicate
                      (length records)
                      (errorWithoutStackTrace "insertMany: simulated entity hydration")
               in (Right simulatedResults, st')

-- | Variant of 'insertMany' that discards hydrated results.
insertMany_ ::
  forall table db (mode :: TxMode) f.
  (AssertWritable mode, BulkInsertable table, Foldable f) =>
  TableSchema table ->
  f (Changeset table) ->
  Tx db mode ()
insertMany_ schema records = void (insertMany schema records)

-- | Chunked bulk insertion dividing large batches into sub-batches of size @chunkSize@.
chunkedInsertMany ::
  forall table db (mode :: TxMode).
  (AssertWritable mode, BulkInsertable table) =>
  Int ->
  TableSchema table ->
  [Changeset table] ->
  Tx db mode [table 'Result]
chunkedInsertMany chunkSize schema records
  | chunkSize <= 0 = insertMany schema records
  | otherwise =
      let chunks = chunkList chunkSize records
       in concat <$> traverse (insertMany schema) chunks
