{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.Migrate.LinterSpec
Description : Test suite for Epic 19: Zero-Downtime Migration Safety Linter & Shadow Database Verifier
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Migrate.LinterSpec (spec) where

import Hylo.Migrate.AST
import Hylo.Migrate.Catalog
import Hylo.Migrate.Diff
import Hylo.Migrate.Linter
import Hylo.Migrate.Runner.Shadow
import Test.Hspec

spec :: Spec
spec = describe "Epic 19: Zero-Downtime Migration Safety Linter" $ do
  -- 19.4 Executable Oracle Specification
  describe "19.4 Executable Oracle Specification" $ do
    it "rejects CREATE INDEX statements lacking CONCURRENTLY" $ do
      let dangerousMigration = "CREATE INDEX users_email_idx ON users (email);"
          lintResult = lintMigrationSQL dangerousMigration
      lintResult `shouldBe` Left [IndexCreatedWithoutConcurrently "users_email_idx"]

    it "enforces lock_timeout preamble on all production DDL scripts" $ do
      let scriptWithoutTimeout = "ALTER TABLE documents ADD COLUMN flag boolean;"
          lintResult = lintMigrationSQL scriptWithoutTimeout
      lintResult `shouldBe` Left [MissingLockTimeoutPreamble]

  -- Law 1: Concurrence Preservation Law
  describe "Law 1: Concurrence Preservation Law" $ do
    it "passes migration scripts declaring explicit lock_timeout within threshold" $ do
      let safeScript =
            "SET LOCAL lock_timeout = '2s';\n\
            \ALTER TABLE documents ADD COLUMN flag boolean;"
          lintResult = lintMigrationSQL safeScript
      lintResult `shouldBe` Right ()

    it "rejects lock_timeout declarations exceeding safety threshold" $ do
      let dangerousTimeout =
            "SET LOCAL lock_timeout = '30s';\n\
            \ALTER TABLE documents ADD COLUMN flag boolean;"
          lintResult = lintMigrationSQL dangerousTimeout
      lintResult `shouldBe` Left [LockTimeoutTooHigh 30]

    it "passes standalone concurrent index creation scripts without transaction lock" $ do
      let safeIndexScript = "CREATE INDEX CONCURRENTLY users_email_idx ON users (email);"
          lintResult = lintMigrationSQL safeIndexScript
      lintResult `shouldBe` Right ()

    it "rejects destructive table drop without explicit override" $ do
      let dropScript =
            "SET LOCAL lock_timeout = '2s';\n\
            \DROP TABLE legacy_users CASCADE;"
          lintResult = lintMigrationSQL dropScript
      lintResult `shouldBe` Left [DestructiveDropTable "legacy_users"]

    it "permits destructive operations when allowDestructive configuration is set" $ do
      let dropScript =
            "SET LOCAL lock_timeout = '2s';\n\
            \DROP TABLE legacy_users CASCADE;"
          customCfg = defaultLintConfig {cfgAllowDestructive = True}
          lintResult = lintMigrationSQLWith customCfg dropScript
      lintResult `shouldBe` Right ()

    it "rejects unsafe maintenance commands like VACUUM FULL" $ do
      let vacuumScript =
            "SET LOCAL lock_timeout = '2s';\n\
            \VACUUM FULL documents;"
          lintResult = lintMigrationSQL vacuumScript
      lintResult `shouldBe` Left [UnsafeMaintenanceCommand "VACUUM FULL"]

    it "flags unsafe column type changes triggering full table rewrites" $ do
      let alterScript =
            "SET LOCAL lock_timeout = '2s';\n\
            \ALTER TABLE users ALTER COLUMN age TYPE bigint;"
          lintResult = lintMigrationSQL alterScript
      lintResult `shouldBe` Left [UnsafeColumnTypeChange "users" "age"]

  -- Law 2: Two-Phase Constraint Safety
  describe "Law 2: Two-Phase Constraint Safety" $ do
    it "rejects single-step foreign key constraint addition without NOT VALID" $ do
      let singleStepFk =
            "SET LOCAL lock_timeout = '2s';\n\
            \ALTER TABLE orders ADD CONSTRAINT fk_user FOREIGN KEY (user_id) REFERENCES users (id);"
          lintResult = lintMigrationSQL singleStepFk
      lintResult `shouldBe` Left [UnsafeForeignKeyAdded "orders" "fk_user"]

    it "accepts two-phase constraint addition with NOT VALID followed by VALIDATE" $ do
      let twoPhaseScript =
            "SET LOCAL lock_timeout = '2s';\n\
            \ALTER TABLE orders ADD CONSTRAINT fk_user FOREIGN KEY (user_id) REFERENCES users (id) NOT VALID;\n\
            \ALTER TABLE orders VALIDATE CONSTRAINT fk_user;"
          lintResult = lintMigrationSQL twoPhaseScript
      lintResult `shouldBe` Right ()

    it "generates compliant two-phase constraint statements from AST combinator" $ do
      let fkCon =
            ForeignKeyConstraint
              (ConstraintName "orders_user_fk")
              [ColumnName "user_id"]
              (TableName "users")
              [ColumnName "id"]
              Cascade
              Cascade
          (step1Sql, step2Sql) = renderTwoPhaseConstraint (TableName "orders") fkCon
      step1Sql `shouldContain` "ADD CONSTRAINT \"orders_user_fk\" FOREIGN KEY"
      step1Sql `shouldContain` "NOT VALID"
      step2Sql `shouldContain` "VALIDATE CONSTRAINT \"orders_user_fk\""

  -- Shadow Database Verification Runner
  describe "Shadow Database Verification Runner" $ do
    it "verifies bidirectional idempotence (up -> down -> up) for valid catalog diffs" $ do
      let s0 = emptyCatalog
          s1 = emptyCatalog & addTable sampleDocumentCatalogDef
          diff = computeCatalogDiff s0 s1
          verification = verifyCatalogDiff s0 diff
      verification `shouldBe` ShadowSuccess

    it "detects and reports asymmetric rollback failures in shadow runner" $ do
      let s0 = emptyCatalog
          tbl = sampleDocumentCatalogDef
          upStmts = [CreateTableStmt tbl]
          -- Asymmetric down statements (dropping wrong table)
          downStmts = [DropTableStmt (TableName "non_existent_table") True]
          verification = verifyMigrationSequence s0 upStmts downStmts
      case verification of
        ShadowDownFailed _ -> pure ()
        other -> expectationFailure ("Expected ShadowDownFailed but got: " ++ show other)

    it "verifies full lifecycle over multi-step column additions and alterations" $ do
      let baseTable =
            TableDef
              { tblName = "metrics"
              , tblColumns = [ColumnDef "id" TypeBigInt NotNull Nothing True Nothing]
              , tblIndexes = []
              , tblConstraints = []
              , tblComment = Nothing
              }
          targetTable =
            baseTable
              { tblColumns =
                  [ ColumnDef "id" TypeBigInt NotNull Nothing True Nothing
                  , ColumnDef "score" TypeInteger Nullable Nothing False Nothing
                  ]
              }
          s0 = emptyCatalog & addTable baseTable
          s1 = emptyCatalog & addTable targetTable
          verification = verifyReversibilityAndIdempotence s0 s1
      verification `shouldBe` ShadowSuccess
