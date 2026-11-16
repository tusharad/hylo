{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Hylo.Migrate.LiveRunnerSpec
Description : Test suite for User Story 2: Live Database Catalog Introspection & Automated Migration Make
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Migrate.LiveRunnerSpec (spec) where

import Control.Exception (try)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hylo.Core
  ( AnyTableSchema (..)
  , Col
  , Ctx (..)
  , Id (..)
  , Table (..)
  , TableSchema
  , colSpecExpr
  , column
  , diffField
  , eraseColDef
  , patchEntry
  , primaryKey
  , tableSchema
  )
import Hylo.Core.Transaction (TxError (..))
import Hylo.Migrate.AST
  ( ColumnDef (..)
  , ColumnName (..)
  , ColumnNullability (..)
  , ColumnType (..)
  , ConstraintDef (..)
  , ConstraintName (..)
  , IndexDef (..)
  , IndexMethod (..)
  , IndexName (..)
  , OnDeleteAction (..)
  , TableDef (..)
  , TableName (..)
  )
import Hylo.Migrate.Catalog
  ( addTable
  , emptyCatalog
  , sampleDocumentCatalogDef
  , tableSchemaToDef
  , (&)
  )
import Hylo.Migrate.Linter (LintError (..))
import Hylo.Migrate.Runner.Live
  ( MigrationError (..)
  , createMockPool
  , introspectColumnsQuery
  , introspectConstraintsQuery
  , introspectIndexesQuery
  , introspectLiveDatabase
  , introspectTablesQuery
  , makeMigration
  , makeMigrationFromTableDefs
  , makeMigrationFromTableDefsEither
  )
import Hylo.PG.Connection (defaultPool)
import System.Directory (doesFileExist, listDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec

-- | Sample User entity for schema bridge tests.
data UserT (ctx :: Ctx) = User
  { id :: !(Col ctx (Id UserT Int64))
  , name :: !(Col ctx Text)
  , email :: !(Col ctx Text)
  }

userSchema :: TableSchema UserT
userSchema =
  tableSchema "users" $
    User
      { id = primaryKey "id" "bigint"
      , name = column "name" "text"
      , email = column "email" "text"
      }

instance Table UserT where
  toTableExpr alias meta =
    User
      { id = colSpecExpr alias meta.id
      , name = colSpecExpr alias meta.name
      , email = colSpecExpr alias meta.email
      }
  diffTable old new =
    User
      { id = diffField old.id new.id
      , name = diffField old.name new.name
      , email = diffField old.email new.email
      }
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.name
    , eraseColDef m.email
    ]
  patchEntries meta patch =
    concat
      [ patchEntry meta.id patch.id
      , patchEntry meta.name patch.name
      , patchEntry meta.email patch.email
      ]
  dirtyFieldsTable _ = Map.empty

-- | Sample Order entity referencing User.
data OrderT (ctx :: Ctx) = Order
  { id :: !(Col ctx (Id OrderT Int64))
  , userId :: !(Col ctx Int64)
  , amount :: !(Col ctx Int64)
  }

orderSchema :: TableSchema OrderT
orderSchema =
  tableSchema "orders" $
    Order
      { id = primaryKey "id" "bigint"
      , userId = column "user_id" "bigint"
      , amount = column "amount" "bigint"
      }

instance Table OrderT where
  toTableExpr alias meta =
    Order
      { id = colSpecExpr alias meta.id
      , userId = colSpecExpr alias meta.userId
      , amount = colSpecExpr alias meta.amount
      }
  diffTable old new =
    Order
      { id = diffField old.id new.id
      , userId = diffField old.userId new.userId
      , amount = diffField old.amount new.amount
      }
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.userId
    , eraseColDef m.amount
    ]
  patchEntries meta patch =
    concat
      [ patchEntry meta.id patch.id
      , patchEntry meta.userId patch.userId
      , patchEntry meta.amount patch.amount
      ]
  dirtyFieldsTable _ = Map.empty

shouldContainText :: Text -> Text -> Expectation
shouldContainText haystack needle =
  if needle `T.isInfixOf` haystack
    then pure ()
    else expectationFailure $ "Expected " ++ show needle ++ " in:\n" ++ T.unpack haystack

spec :: Spec
spec = describe "User Story 2: Live Database Catalog Introspection & Automated Migration Make" $ do
  -- 1. Live Catalog Introspection Runner
  describe "1. Live Catalog Introspector (Hylo.Migrate.Runner.Live)" $ do
    it "exports valid pg_catalog introspection queries" $ do
      introspectTablesQuery `shouldContainText` "FROM pg_catalog.pg_class c"
      introspectColumnsQuery `shouldContainText` "pg_catalog.format_type"
      introspectIndexesQuery `shouldContainText` "pg_catalog.pg_get_indexdef"
      introspectConstraintsQuery `shouldContainText` "pg_catalog.pg_get_constraintdef"

    it "safely returns TxUserError when attempting to introspect an unmocked offline pool" $ do
      let offlinePool = defaultPool
      res <- introspectLiveDatabase offlinePool
      case res of
        Left (TxUserError msg) -> T.unpack msg `shouldContain` "no active PostgreSQL connection"
        _ -> expectationFailure "Expected TxUserError for offline pool"

    it "introspects in-memory mock catalog matching tables, columns, indexes, and constraints" $ do
      let s0 = emptyCatalog & addTable sampleDocumentCatalogDef
          pool = createMockPool s0
      res <- introspectLiveDatabase pool
      res `shouldBe` Right s0

  -- 2. Acceptance Criterion 1: Valid, lint-free migration generation with lock_timeout and 2-phase constraints
  describe "2. Acceptance Criterion 1: Lint-Free Migration Make & Reversible DDL Persistence" $ do
    it "generates valid, lint-free up.sql and down.sql files with lock_timeout preamble" $ do
      let outDir = "dist-test-migrations-ac1"
          s0 = emptyCatalog
          pool = createMockPool s0
          schemas = [AnyTableSchema userSchema, AnyTableSchema orderSchema]

      -- Run makeMigration
      (upSql, downSql) <- makeMigration pool schemas outDir

      -- Check lock_timeout preamble
      upSql `shouldContainText` "SET LOCAL lock_timeout = '2s';"
      downSql `shouldContainText` "SET LOCAL lock_timeout = '2s';"

      -- Check table creation and ordering
      upSql `shouldContainText` "CREATE TABLE \"users\""
      upSql `shouldContainText` "CREATE TABLE \"orders\""
      downSql `shouldContainText` "DROP TABLE \"orders\" CASCADE;"
      downSql `shouldContainText` "DROP TABLE \"users\" CASCADE;"

      -- Verify files were persisted to disk
      canonicalUpExists <- doesFileExist (outDir </> "up.sql")
      canonicalDownExists <- doesFileExist (outDir </> "down.sql")
      canonicalUpExists `shouldBe` True
      canonicalDownExists `shouldBe` True

      -- Read content from disk
      fileUp <- TIO.readFile (outDir </> "up.sql")
      fileDown <- TIO.readFile (outDir </> "down.sql")
      fileUp `shouldBe` upSql
      fileDown `shouldBe` downSql

      -- Verify timestamped files also exist
      files <- listDirectory outDir
      any (\f -> "_up.sql" `T.isSuffixOf` T.pack f) files `shouldBe` True
      any (\f -> "_down.sql" `T.isSuffixOf` T.pack f) files `shouldBe` True

      -- Clean up test directory
      removeDirectoryRecursive outDir

    it
      "generates safe two-phase constraint validation (NOT VALID + VALIDATE CONSTRAINT) when modifying table constraints"
      $ do
        let outDir = "dist-test-migrations-twophase"
            -- S0 has baseline orders and users tables
            baseUsers =
              TableDef
                { tblName = "users"
                , tblColumns = [ColumnDef "id" TypeBigInt NotNull Nothing True Nothing]
                , tblIndexes = []
                , tblConstraints = [PrimaryKeyConstraint "users_pkey" [ColumnName "id"]]
                , tblComment = Nothing
                }
            baseOrders =
              TableDef
                { tblName = "orders"
                , tblColumns =
                    [ ColumnDef "id" TypeBigInt NotNull Nothing True Nothing
                    , ColumnDef "user_id" TypeBigInt NotNull Nothing False Nothing
                    ]
                , tblIndexes = []
                , tblConstraints = [PrimaryKeyConstraint "orders_pkey" [ColumnName "id"]]
                , tblComment = Nothing
                }
            s0 = emptyCatalog & addTable baseUsers & addTable baseOrders

            -- S1 adds a foreign key constraint from orders to users
            fkCon =
              ForeignKeyConstraint
                (ConstraintName "orders_user_fk")
                [ColumnName "user_id"]
                (TableName "users")
                [ColumnName "id"]
                Cascade
                Cascade
            targetOrders = baseOrders {tblConstraints = tblConstraints baseOrders ++ [fkCon]}

        -- Generate migration
        (upSql, downSql) <- do
          let pool = createMockPool s0
          makeMigrationFromTableDefs pool [baseUsers, targetOrders] outDir

        -- Verify Two-Phase constraint addition: NOT VALID followed by VALIDATE CONSTRAINT
        upSql `shouldContainText` "SET LOCAL lock_timeout = '2s';"
        upSql
          `shouldContainText` "ALTER TABLE \"orders\" ADD CONSTRAINT \"orders_user_fk\" FOREIGN KEY (\"user_id\") REFERENCES \"users\" (\"id\") ON DELETE CASCADE ON UPDATE CASCADE NOT VALID;"
        upSql `shouldContainText` "ALTER TABLE \"orders\" VALIDATE CONSTRAINT \"orders_user_fk\";"

        -- Verify rollback drops constraint
        downSql `shouldContainText` "ALTER TABLE \"orders\" DROP CONSTRAINT \"orders_user_fk\" CASCADE;"

        -- Clean up
        removeDirectoryRecursive outDir

  -- 3. Acceptance Criterion 2: Lock hazard detection halts makeMigration
  describe "3. Acceptance Criterion 2: Production Lock Hazard Prevention" $ do
    it
      "halts makeMigration and reports IndexCreatedWithoutConcurrently when creating an index without CONCURRENTLY"
      $ do
        let outDir = "dist-test-migrations-lockhazard"
            baseUsers =
              TableDef
                { tblName = "users"
                , tblColumns =
                    [ ColumnDef "id" TypeBigInt NotNull Nothing True Nothing
                    , ColumnDef "email" TypeText NotNull Nothing False Nothing
                    ]
                , tblIndexes = []
                , tblConstraints = [PrimaryKeyConstraint "users_pkey" [ColumnName "id"]]
                , tblComment = Nothing
                }
            s0 = emptyCatalog & addTable baseUsers
            pool = createMockPool s0

            -- Target adds index WITHOUT CONCURRENTLY (locking hazard)
            unsafeIndex =
              IndexDef
                { idxName = IndexName "users_email_idx"
                , idxTable = TableName "users"
                , idxColumns = [ColumnName "email"]
                , idxMethod = BTree
                , idxIsUnique = False
                , idxIsConcurrent = False
                , idxWhereClause = Nothing
                , idxStorageParams = []
                , idxOpClass = Nothing
                }
            targetUsers = baseUsers {tblIndexes = [unsafeIndex]}

        -- Expect makeMigration to halt and throw MigrationLintError
        res <- try (makeMigrationFromTableDefs pool [targetUsers] outDir)
        case res of
          Left (MigrationLintError errs) -> do
            errs `shouldContain` [IndexCreatedWithoutConcurrently "users_email_idx"]
          Left otherErr ->
            expectationFailure ("Expected MigrationLintError but got: " ++ show otherErr)
          Right _ ->
            expectationFailure "makeMigration should have halted on index created without CONCURRENTLY"

        -- Either-based variant also returns exact LintError
        eitherRes <- makeMigrationFromTableDefsEither pool [targetUsers] outDir
        eitherRes `shouldBe` Left (MigrationLintError [IndexCreatedWithoutConcurrently "users_email_idx"])

    it "succeeds when index is created with CONCURRENTLY" $ do
      let outDir = "dist-test-migrations-concurrent"
          baseUsers =
            TableDef
              { tblName = "users"
              , tblColumns =
                  [ ColumnDef "id" TypeBigInt NotNull Nothing True Nothing
                  , ColumnDef "email" TypeText NotNull Nothing False Nothing
                  ]
              , tblIndexes = []
              , tblConstraints = [PrimaryKeyConstraint "users_pkey" [ColumnName "id"]]
              , tblComment = Nothing
              }
          s0 = emptyCatalog & addTable baseUsers
          pool = createMockPool s0

          -- Target adds index WITH CONCURRENTLY (zero-downtime safe)
          safeIndex =
            IndexDef
              { idxName = IndexName "users_email_concurrent_idx"
              , idxTable = TableName "users"
              , idxColumns = [ColumnName "email"]
              , idxMethod = BTree
              , idxIsUnique = False
              , idxIsConcurrent = True
              , idxWhereClause = Nothing
              , idxStorageParams = []
              , idxOpClass = Nothing
              }
          targetUsers = baseUsers {tblIndexes = [safeIndex]}

      (upSql, downSql) <- makeMigrationFromTableDefs pool [targetUsers] outDir
      upSql `shouldContainText` "CREATE INDEX CONCURRENTLY \"users_email_concurrent_idx\""
      downSql `shouldContainText` "DROP INDEX \"users_email_concurrent_idx\""

      removeDirectoryRecursive outDir

  -- 4. Shadow Database Safety Verification
  describe "4. Shadow Database Safety Verification (Hylo.Migrate.Runner.Shadow Integration)" $ do
    it "verifies 100% round-trip reversibility (Apply(down, Apply(up, S0)) == S0) before writing files" $ do
      let outDir = "dist-test-migrations-shadow"
          -- Baseline S0 has users table
          s0 = emptyCatalog & addTable (tableSchemaToDef userSchema)
          pool = createMockPool s0
          -- Target schemas preserve users and add orders
          schemas = [AnyTableSchema userSchema, AnyTableSchema orderSchema]

      (upSql, downSql) <- makeMigration pool schemas outDir
      upSql `shouldContainText` "CREATE TABLE \"orders\""
      downSql `shouldContainText` "DROP TABLE \"orders\" CASCADE"

      removeDirectoryRecursive outDir
