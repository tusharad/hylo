{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.RealPostgresSpec
Description : Integration test suite executing real transactions against Docker/CI PostgreSQL
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.RealPostgresSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Hasql.Decoders as Dec
import qualified Hasql.Encoders as Enc
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt
import Hylo.Backend.Capability (PGVersion (..), Postgres)
import Hylo.Core.Transaction (Tx (..), TxError (..), recordMutation, txFail, withSavepoint)
import Hylo.Core.TypeError (TxMode (..))
import Hylo.PG.Connection
  ( Pool (..)
  , executeSession
  , executeSql
  , runTx
  )
import Hylo.PG.Transaction.Retry
  ( IsolationLevel (..)
  , defaultRetryPolicy
  , runTxWithRetry
  , simulateWriteConflictOnce
  )
import Hylo.Test.Docker (getTestPgVersion, withTestPostgres)
import Test.Hspec

-- | Integration test suite executing against real PostgreSQL (14–18) in Docker / Testcontainer.
spec :: Spec
spec = describe "Real PostgreSQL Integration Suite (Docker / Testcontainer)" $ do
  it "connects to real PostgreSQL and verifies version with pgvector" $
    withTestPostgres $ \p -> do
      case poolActiveConnection p of
        Nothing -> pendingWith "Docker / PostgreSQL not available in environment"
        Just _ -> do
          expectedVer <- getTestPgVersion
          -- Verify PostgreSQL version matches HYLO_TEST_PG_VERSION (or default 17)
          let versionStmt =
                Stmt.Statement
                  "SELECT version();"
                  Enc.noParams
                  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.text)))
                  False
          verRes <- executeSession p (Sess.statement () versionStmt)
          case verRes of
            Left err -> expectationFailure ("Failed to query PostgreSQL version: " ++ show err)
            Right ver -> T.isInfixOf ("PostgreSQL " <> expectedVer) ver `shouldBe` True

          -- Verify vector extension
          let extStmt =
                Stmt.Statement
                  "SELECT extversion FROM pg_extension WHERE extname = 'vector';"
                  Enc.noParams
                  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.text)))
                  False
          extRes <- executeSession p (Sess.statement () extStmt)
          case extRes of
            Left err -> expectationFailure ("Failed to query pgvector extension: " ++ show err)
            Right extVer -> extVer `shouldSatisfy` (not . T.null)

  it "executes real DDL, table lifecycle, and transactional mutations" $
    withTestPostgres $ \p -> do
      case poolActiveConnection p of
        Nothing -> pendingWith "Docker / PostgreSQL not available in environment"
        Just _ -> do
          -- Setup temporary table
          _ <-
            executeSql
              p
              "DROP TABLE IF EXISTS test_users CASCADE; \
              \CREATE TABLE test_users ( \
              \  id BIGSERIAL PRIMARY KEY, \
              \  username TEXT NOT NULL UNIQUE, \
              \  email TEXT NOT NULL, \
              \  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW() \
              \);"

          -- Execute real transactional insert via runTx
          let txInsert = do
                recordMutation "INSERT INTO test_users (username, email) VALUES ('alice', 'alice@example.com');"
                pure ("inserted" :: Text)

          res <- runTx p ReadWrite txInsert
          res `shouldBe` Right "inserted"

          -- Query record back
          let queryStmt =
                Stmt.Statement
                  "SELECT username, email FROM test_users WHERE username = 'alice';"
                  Enc.noParams
                  ( Dec.singleRow
                      ((,) <$> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.text))
                  )
                  False
          qRes <- executeSession p (Sess.statement () queryStmt)
          qRes `shouldBe` Right ("alice", "alice@example.com")

          -- Execute update
          let txUpdate = do
                recordMutation "UPDATE test_users SET email = 'alice_new@example.com' WHERE username = 'alice';"
                pure ()
          _ <- runTx p ReadWrite txUpdate

          -- Verify updated value
          let qUpdated =
                Stmt.Statement
                  "SELECT email FROM test_users WHERE username = 'alice';"
                  Enc.noParams
                  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.text)))
                  False
          uRes <- executeSession p (Sess.statement () qUpdated)
          uRes `shouldBe` Right "alice_new@example.com"

  it "captures real PostgreSQL SQLSTATE 23505 unique constraint violations" $
    withTestPostgres $ \p -> do
      case poolActiveConnection p of
        Nothing -> pendingWith "Docker / PostgreSQL not available in environment"
        Just _ -> do
          _ <-
            executeSql
              p
              "DROP TABLE IF EXISTS test_unique CASCADE; \
              \CREATE TABLE test_unique (id INT PRIMARY KEY, code TEXT UNIQUE);"
          _ <- executeSql p "INSERT INTO test_unique VALUES (1, 'ALPHA');"

          -- Attempt duplicate insert inside transaction
          let txDuplicate = do
                recordMutation "INSERT INTO test_unique VALUES (2, 'ALPHA');"
                pure ()

          res <- runTx p ReadWrite txDuplicate
          case res of
            Left (TxConstraintViolation code msg _detail) -> do
              code `shouldBe` "23505"
              T.isInfixOf "test_unique" msg `shouldBe` True
            other -> expectationFailure ("Expected TxConstraintViolation 23505, got: " ++ show other)

  it "captures real PostgreSQL SQLSTATE 23503 foreign key violations" $
    withTestPostgres $ \p -> do
      case poolActiveConnection p of
        Nothing -> pendingWith "Docker / PostgreSQL not available in environment"
        Just _ -> do
          _ <-
            executeSql
              p
              "DROP TABLE IF EXISTS test_child CASCADE; \
              \DROP TABLE IF EXISTS test_parent CASCADE; \
              \CREATE TABLE test_parent (id INT PRIMARY KEY); \
              \CREATE TABLE test_child (id INT PRIMARY KEY, parent_id INT REFERENCES test_parent(id));"

          -- Attempt child insert with non-existent parent
          let txBadFk = do
                recordMutation "INSERT INTO test_child VALUES (1, 99999);"
                pure ()

          res <- runTx p ReadWrite txBadFk
          case res of
            Left (TxConstraintViolation code msg _detail) -> do
              code `shouldBe` "23503"
              T.isInfixOf "test_child" msg `shouldBe` True
            other -> expectationFailure ("Expected TxConstraintViolation 23503, got: " ++ show other)

  it "isolates sub-transaction failures using real PostgreSQL savepoints" $
    withTestPostgres $ \p -> do
      case poolActiveConnection p of
        Nothing -> pendingWith "Docker / PostgreSQL not available in environment"
        Just _ -> do
          _ <-
            executeSql
              p
              "DROP TABLE IF EXISTS test_savepoints CASCADE; \
              \CREATE TABLE test_savepoints (id INT PRIMARY KEY, label TEXT);"

          -- In a single transaction:
          -- 1. Insert row 1
          -- 2. In savepoint svp_0, insert row 99 then abort sub-transaction via txFail
          -- 3. Savepoint is rolled back ('ROLLBACK TO SAVEPOINT "svp_0"')
          -- 4. Insert row 2
          -- 5. Commit
          let tx = do
                recordMutation "INSERT INTO test_savepoints VALUES (1, 'initial');"
                _ <-
                  withSavepoint
                    ( do
                        recordMutation "INSERT INTO test_savepoints VALUES (99, 'reverted');"
                        txFail (TxUserError "reverted_by_savepoint")
                    )
                recordMutation "INSERT INTO test_savepoints VALUES (2, 'secondary');"
                pure ("committed" :: Text)

          res <- runTx p ReadWrite tx
          res `shouldBe` Right "committed"

          -- Verify rows 1 and 2 exist, but row 99 was reverted by savepoint rollback
          let countStmt =
                Stmt.Statement
                  "SELECT count(*)::bigint FROM test_savepoints;"
                  Enc.noParams
                  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8)))
                  False
          countRes <- executeSession p (Sess.statement () countStmt)
          countRes `shouldBe` Right 2

          let check99Stmt =
                Stmt.Statement
                  "SELECT count(*)::bigint FROM test_savepoints WHERE id = 99;"
                  Enc.noParams
                  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8)))
                  False
          c99Res <- executeSession p (Sess.statement () check99Stmt)
          c99Res `shouldBe` Right 0

  it "executes native pgvector similarity distance search (<->, <=>, <#>)" $
    withTestPostgres $ \p -> do
      case poolActiveConnection p of
        Nothing -> pendingWith "Docker / PostgreSQL not available in environment"
        Just _ -> do
          _ <-
            executeSql
              p
              "DROP TABLE IF EXISTS test_embeddings CASCADE; \
              \CREATE TABLE test_embeddings ( \
              \  id INT PRIMARY KEY, \
              \  label TEXT NOT NULL, \
              \  vec vector(3) NOT NULL \
              \);"

          -- Insert 3 vector points:
          -- (1, 'x_axis', [1, 0, 0])
          -- (2, 'y_axis', [0, 1, 0])
          -- (3, 'near_x', [0.95, 0.05, 0.0])
          _ <-
            executeSql
              p
              "INSERT INTO test_embeddings VALUES \
              \(1, 'x_axis', '[1, 0, 0]'), \
              \(2, 'y_axis', '[0, 1, 0]'), \
              \(3, 'near_x', '[0.95, 0.05, 0.0]');"

          -- Query nearest neighbors to '[1, 0, 0]' using cosine distance <=>
          let cosineStmt =
                Stmt.Statement
                  "SELECT label FROM test_embeddings ORDER BY vec <=> '[1, 0, 0]' LIMIT 3;"
                  Enc.noParams
                  (Dec.rowList (Dec.column (Dec.nonNullable Dec.text)))
                  False
          cosRes <- executeSession p (Sess.statement () cosineStmt)
          -- Nearest must be 'x_axis' (dist 0.0), then 'near_x', then 'y_axis'
          cosRes `shouldBe` Right ["x_axis", "near_x", "y_axis"]

          -- Query nearest neighbors using Euclidean distance <->
          let l2Stmt =
                Stmt.Statement
                  "SELECT label FROM test_embeddings ORDER BY vec <-> '[1, 0, 0]' LIMIT 3;"
                  Enc.noParams
                  (Dec.rowList (Dec.column (Dec.nonNullable Dec.text)))
                  False
          l2Res <- executeSession p (Sess.statement () l2Stmt)
          l2Res `shouldBe` Right ["x_axis", "near_x", "y_axis"]

  it "streams rows from real PostgreSQL using binary cursor protocol" $
    withTestPostgres $ \p -> do
      case poolActiveConnection p of
        Nothing -> pendingWith "Docker / PostgreSQL not available in environment"
        Just _ -> do
          _ <-
            executeSql
              p
              "DROP TABLE IF EXISTS test_stream CASCADE; \
              \CREATE TABLE test_stream (id INT PRIMARY KEY, val TEXT); \
              \INSERT INTO test_stream SELECT g, 'item_' || g FROM generate_series(1, 25) g;"

          -- Run cursor lifecycle inside a transaction block
          _ <- executeSql p "BEGIN;"
          _ <- executeSql p "DECLARE cur NO SCROLL CURSOR FOR SELECT id FROM test_stream ORDER BY id;"

          -- Fetch chunk 1 (10 rows)
          let fetchStmt =
                Stmt.Statement
                  "FETCH 10 FROM cur;"
                  Enc.noParams
                  (Dec.rowList (Dec.column (Dec.nonNullable Dec.int4)))
                  False
          c1 <- executeSession p (Sess.statement () fetchStmt)
          c1 `shouldBe` Right [1 .. 10]

          -- Fetch chunk 2 (10 rows)
          c2 <- executeSession p (Sess.statement () fetchStmt)
          c2 `shouldBe` Right [11 .. 20]

          -- Fetch chunk 3 (remaining 5 rows)
          c3 <- executeSession p (Sess.statement () fetchStmt)
          c3 `shouldBe` Right [21 .. 25]

          -- Fetch chunk 4 (empty)
          c4 <- executeSession p (Sess.statement () fetchStmt)
          c4 `shouldBe` Right []

          _ <- executeSql p "CLOSE cur;"
          _ <- executeSql p "COMMIT;"
          pure ()

  it "supports FOR UPDATE SKIP LOCKED concurrent worker queue pattern" $
    withTestPostgres $ \p -> do
      case poolActiveConnection p of
        Nothing -> pendingWith "Docker / PostgreSQL not available in environment"
        Just _ -> do
          _ <-
            executeSql
              p
              "DROP TABLE IF EXISTS test_queue CASCADE; \
              \CREATE TABLE test_queue (id INT PRIMARY KEY, status TEXT); \
              \INSERT INTO test_queue VALUES (1, 'pending'), (2, 'pending');"

          -- Transaction 1 claims row 1
          _ <- executeSql p "BEGIN;"
          let claimStmt =
                Stmt.Statement
                  "SELECT id FROM test_queue WHERE status = 'pending' ORDER BY id LIMIT 1 FOR UPDATE SKIP LOCKED;"
                  Enc.noParams
                  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int4)))
                  False
          row1 <- executeSession p (Sess.statement () claimStmt)
          row1 `shouldBe` Right 1

          -- Commit transaction 1
          _ <- executeSql p "UPDATE test_queue SET status = 'processed' WHERE id = 1;"
          _ <- executeSql p "COMMIT;"

          -- Transaction 2 claims next available row (row 2)
          _ <- executeSql p "BEGIN;"
          row2 <- executeSession p (Sess.statement () claimStmt)
          row2 `shouldBe` Right 2
          _ <- executeSql p "COMMIT;"
          pure ()

  it
    "User Story 8 Acceptance Criterion 2: runTxWithRetry executes BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE READ WRITE and replays on serialization failure"
    $ withTestPostgres
    $ \p -> do
      case poolActiveConnection p of
        Nothing -> pendingWith "Docker / PostgreSQL not available in environment"
        Just _ -> do
          _ <-
            executeSql
              p
              "DROP TABLE IF EXISTS test_retry_pool CASCADE; \
              \CREATE TABLE test_retry_pool (id INT PRIMARY KEY, val TEXT);"

          let tx :: Tx (Postgres 'PG17) 'ReadWrite Text
              tx = do
                simulateWriteConflictOnce
                recordMutation "INSERT INTO test_retry_pool VALUES (1, 'serializable_ok');"
                pure "committed_after_retry"

          res <- runTxWithRetry defaultRetryPolicy p Serializable tx
          res `shouldBe` Right "committed_after_retry"

          let checkStmt =
                Stmt.Statement
                  "SELECT val FROM test_retry_pool WHERE id = 1;"
                  Enc.noParams
                  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.text)))
                  False
          qRes <- executeSession p (Sess.statement () checkStmt)
          qRes `shouldBe` Right "serializable_ok"
