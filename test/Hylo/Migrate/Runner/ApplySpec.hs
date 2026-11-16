{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.Migrate.Runner.ApplySpec
Description : Specification tests for User Story 20: Transactional DDL Execution Runner
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Integration tests driving the backend-agnostic migration runner core over an
in-memory SQLite catalog (User Story 20 acceptance criterion), verifying the
happy path, compensation atomicity on failing DDL, idempotent re-runs, and
rollback of the last applied migration.
-}
module Hylo.Migrate.Runner.ApplySpec (spec) where

import Control.Exception (bracket, try)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime, utctDayTime)
import Hylo.Migrate.Linter (LintError (..))
import Hylo.Migrate.Runner.Apply
import Hylo.Migrate.Runner.Live (MigrationError (..))
import qualified Hylo.SQLite.Connection as SQLite
import System.Directory
  ( createDirectoryIfMissing
  , getTemporaryDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec =
  around withFreshMigrationsDir $
    describe "User Story 20: Transactional DDL Execution Runner" $
      describe "with an in-memory SQLite catalog" $ do
        let v1 = "20261101120000" :: T.Text
            v2 = "20261102120000" :: T.Text
            up1 = "CREATE TABLE pets (id INTEGER PRIMARY KEY, name TEXT);" :: T.Text
            down1 = "DROP TABLE pets;" :: T.Text
            upFile dir v = dir </> "migrations" </> T.unpack v <> "_up.sql"

        it "creates the migration history table idempotently" $ \_ -> do
          result <- withSqliteBackend $ \backend -> do
            first <- ensureMigrationsTableWith backend
            second <- ensureMigrationsTableWith backend
            pure (first >> second)
          result `shouldBe` Right ()

        it "Law (happy path): applies pending DDL and records exactly one history row" $ \dir -> do
          result <- withSqliteBackend $ \backend -> do
            _ <- ensureMigrationsTableWith backend
            applyRes <- applyMigrationWith backend False (upFile dir v1)
            applied <- listAppliedWith backend
            pure (applyRes, applied)
          case result of
            (Right (), Right [rec]) -> do
              mrVersion rec `shouldBe` v1
              mrChecksum rec `shouldBe` checksumOf up1
              mrUpSql rec `shouldBe` up1
              mrDownSql rec `shouldBe` down1
            other -> expectationFailure ("Unexpected result: " <> show other)

        it "Law (compensation atomicity): failing DDL rolls back the entire migration file" $ \dir -> do
          let badUp = "CREATE TABLE ghosts (id INTEGER PRIMARY KEY); THIS IS NOT VALID SQL;"
          TIO.writeFile (upFile dir v2) (badUp <> "\n")
          outcome <- withSqliteBackend $ \backend -> do
            _ <- ensureMigrationsTableWith backend
            _ <- applyMigrationWith backend False (upFile dir v1)
            failedRes <- applyMigrationWith backend False (upFile dir v2)
            applied <- listAppliedWith backend
            ghostProbe <- mbExecuteInTx backend "INSERT INTO ghosts (id) VALUES (1);"
            pure (failedRes, applied, ghostProbe)
          case outcome of
            (Left (MigrationExecutionError _), Right [rec], Left _) ->
              mrVersion rec `shouldBe` v1
            other ->
              expectationFailure
                ("Expected failed migration + untouched history + rolled-back schema, got: " <> show other)

        it "Law (idempotency): applied versions are excluded from pending scans" $ \dir -> do
          (versionsAfterOne, pendingAfterOne, versionsAfterAll, pendingAfterAll) <- withSqliteBackend $ \backend -> do
            _ <- ensureMigrationsTableWith backend
            _ <- applyMigrationWith backend False (upFile dir v1)
            applied1 <- listAppliedWith backend
            pending1 <- case applied1 of
              Right recs -> pendingFiles (dir </> "migrations") (map mrVersion recs)
              Left err -> do
                expectationFailure (show err)
                pure []
            mapM_ (applyMigrationWith backend False) pending1
            applied2 <- listAppliedWith backend
            pending2 <- case applied2 of
              Right recs -> pendingFiles (dir </> "migrations") (map mrVersion recs)
              Left err -> do
                expectationFailure (show err)
                pure []
            pure (map mrVersion <$> applied1, pending1, map mrVersion <$> applied2, pending2)
          versionsAfterOne `shouldBe` Right [v1]
          pendingAfterOne `shouldBe` [upFile dir v2]
          versionsAfterAll `shouldBe` Right [v1, v2]
          pendingAfterAll `shouldBe` []

        it "orders pending migrations by timestamp prefix" $ \dir -> do
          pendingList <- pendingFiles (dir </> "migrations") []
          pendingList `shouldBe` [upFile dir v1, upFile dir v2]

        it "rollback reverts the last migration and removes its record row" $ \dir -> do
          outcome <- withSqliteBackend $ \backend -> do
            _ <- ensureMigrationsTableWith backend
            _ <- applyMigrationWith backend False (upFile dir v1)
            _ <- applyMigrationWith backend False (upFile dir v2)
            rollbackRes <- rollbackLastMigrationWith backend
            applied <- listAppliedWith backend
            toysProbe <- mbExecuteInTx backend "INSERT INTO toys (id, label) VALUES (1, 'x');"
            pure (rollbackRes, fmap (map mrVersion) applied, toysProbe)
          case outcome of
            (Right (), Right [v], Left _) -> v `shouldBe` v1
            other -> expectationFailure ("Unexpected rollback outcome: " <> show other)

        it "rollback with empty history fails descriptively" $ \_ -> do
          result <- withSqliteBackend $ \backend -> do
            _ <- ensureMigrationsTableWith backend
            rollbackLastMigrationWith backend
          case result of
            Left (MigrationExecutionError msg) -> T.isInfixOf "No applied" msg `shouldBe` True
            Left err -> expectationFailure ("Expected MigrationExecutionError, got: " <> show err)
            Right () -> expectationFailure "Expected empty-history failure"

        it "pre-flight lints hazards and aborts without --force" $ \dir -> do
          let v3 = "20261103120000" :: T.Text
              hazardUp = "CREATE INDEX idx_pets_name ON pets (name);" :: T.Text
          TIO.writeFile (upFile dir v3) hazardUp
          TIO.writeFile (dir </> "migrations" </> T.unpack v3 <> "_down.sql") "DROP INDEX idx_pets_name;"
          outcome <- withSqliteBackend $ \backend -> do
            _ <- ensureMigrationsTableWith backend
            _ <- applyMigrationWith backend False (upFile dir v1)
            lintedRes <- applyMigrationWith backend False (upFile dir v3)
            forcedRes <- applyMigrationWith backend True (upFile dir v3)
            applied <- listAppliedWith backend
            pure (lintedRes, forcedRes, fmap (map mrVersion) applied)
          case outcome of
            (Left (MigrationLintError errs), Right (), Right vs) -> do
              errs `shouldBe` [IndexCreatedWithoutConcurrently "idx_pets_name"]
              vs `shouldBe` [v1, v3]
            other -> expectationFailure ("Unexpected lint outcome: " <> show other)

        it "checksumOf is deterministic and content-sensitive" $ \_ -> do
          checksumOf "abc" `shouldBe` checksumOf "abc"
          (checksumOf "abc" == checksumOf "abd") `shouldBe` False

        it "versionOfMigrationFile and downPathFor derive companion artifacts" $ \_ -> do
          versionOfMigrationFile ("dir" </> "20261101120000_up.sql") `shouldBe` ("20261101120000" :: T.Text)
          downPathFor ("migrations" </> "20261101120000_up.sql")
            `shouldBe` ("migrations" </> "20261101120000_down.sql")

        it "reports a descriptive error when the up file is missing" $ \dir -> do
          result <- withSqliteBackend $ \backend -> do
            _ <- ensureMigrationsTableWith backend
            applyMigrationWith backend False (dir </> "migrations" </> "99999999999999_up.sql")
          case result of
            Left (MigrationExecutionError msg) -> T.isInfixOf "not found" msg `shouldBe` True
            Left err -> expectationFailure ("Expected MigrationExecutionError, got: " <> show err)
            Right () -> expectationFailure "Expected missing-file failure"

-- | Bracketed in-memory SQLite backend with a fresh pool per usage.
withSqliteBackend :: (MigrationBackend -> IO a) -> IO a
withSqliteBackend action =
  bracket SQLite.createMemoryPool SQLite.closePool (action . sqliteBackend)

-- | Fresh migrations directory containing two timestamped fixtures, cleaned up after.
withFreshMigrationsDir :: (FilePath -> IO ()) -> IO ()
withFreshMigrationsDir action =
  bracket makeDir removeDir $ \dir -> do
    TIO.writeFile (dir </> "migrations" </> "20261101120000_up.sql") up1
    TIO.writeFile (dir </> "migrations" </> "20261101120000_down.sql") down1
    TIO.writeFile (dir </> "migrations" </> "20261102120000_up.sql") up2
    TIO.writeFile (dir </> "migrations" </> "20261102120000_down.sql") down2
    action dir
  where
    up1 = "CREATE TABLE pets (id INTEGER PRIMARY KEY, name TEXT);" :: T.Text
    down1 = "DROP TABLE pets;" :: T.Text
    up2 = "CREATE TABLE toys (id INTEGER PRIMARY KEY, label TEXT);" :: T.Text
    down2 = "DROP TABLE toys;" :: T.Text
    makeDir = do
      tmp <- getTemporaryDirectory
      t <- getCurrentTime
      let unique = tmp </> ("hylo-us20-" <> show (utctDayTime t))
      createDirectoryIfMissing True (unique </> "migrations")
      pure unique
    removeDir dir = do
      _ <- (try (removeDirectoryRecursive dir) :: IO (Either IOError ()))
      pure ()
