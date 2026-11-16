{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.Migrate.Runner.Apply
Description : Transactional DDL Execution Runner with Migration History (User Story 20)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Applies pending migration files inside a single database transaction with
automatic rollback on failure, records each applied migration in a
@_hylo_migrations@ history table, and supports rolling back the last applied
migration. The runner core is abstracted over a 'MigrationBackend' so the same
execution laws are verifiable against an in-memory SQLite catalog.
-}
module Hylo.Migrate.Runner.Apply
  ( -- * Migration History Records
    MigrationRecord (..)
  , migrationsTableName

    -- * Abstract Execution Backend
  , MigrationBackend (..)

    -- * Backend Instances
  , postgresBackend
  , sqliteBackend

    -- * Core Runner (Backend-Agnostic)
  , applyMigrationWith
  , rollbackLastMigrationWith
  , listAppliedWith
  , ensureMigrationsTableWith

    -- * PostgreSQL Convenience API (User Story 20 signatures)
  , ensureMigrationsTable
  , listApplied
  , listPending
  , applyMigration
  , rollbackLastMigration

    -- * Utilities
  , checksumOf
  , versionOfMigrationFile
  , downPathFor
  , pendingFiles
  , preflightLintConfig
  , readMigrationFile
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (bracket, throwIO)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time
  ( UTCTime
  , formatTime
  , getCurrentTime
  )
import Data.Time.Format (defaultTimeLocale)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Word (Word64)
import qualified Database.SQLite3 as S
import qualified Database.SQLite3.Direct as SD
import GHC.Generics (Generic)
import qualified Hasql.Decoders as Dec
import qualified Hasql.Encoders as Enc
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt
import Hylo.Core.Transaction (BoundParam (..), TxError (..))
import Hylo.Migrate.Linter
  ( LintConfig (..)
  , defaultLintConfig
  , lintMigrationSQLWith
  , splitStatements
  )
import Hylo.Migrate.Runner.Live (MigrationError (..))
import Hylo.PG.Connection
  ( Pool (..)
  , executeSession
  , mkPreparedStatement
  )
import qualified Hylo.SQLite.Connection as SQLite
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO.Error (tryIOError)

--------------------------------------------------------------------------------
-- Migration History Records
--------------------------------------------------------------------------------

-- | A recorded, successfully applied migration (User Story 20).
data MigrationRecord = MigrationRecord
  { mrVersion :: !Text
  , mrAppliedAt :: !UTCTime
  , mrUpSql :: !Text
  , mrDownSql :: !Text
  , mrChecksum :: !Text
  }
  deriving (Eq, Show, Generic)

instance NFData MigrationRecord where
  rnf (MigrationRecord v t up down c) = rnf v `seq` t `seq` rnf up `seq` rnf down `seq` rnf c

-- | Reserved name of the migration history table.
migrationsTableName :: Text
migrationsTableName = "_hylo_migrations"

--------------------------------------------------------------------------------
-- Abstract Execution Backend
--------------------------------------------------------------------------------

{- | Abstract transactional execution surface consumed by the backend-agnostic
runner core. PostgreSQL (production) and SQLite (in-memory test catalogs)
both provide instances.
-}
data MigrationBackend = MigrationBackend
  { mbEnsureHistoryTable :: IO (Either TxError ())
  , mbBeginTx :: IO (Either TxError ())
  , mbCommitTx :: IO (Either TxError ())
  , mbRollbackTx :: IO (Either TxError ())
  , mbExecuteInTx :: Text -> IO (Either TxError ())
  -- ^ Execute a single DDL statement inside the open transaction.
  , mbListApplied :: IO (Either TxError [MigrationRecord])
  , mbInsertRecord :: MigrationRecord -> IO (Either TxError ())
  , mbDeleteRecord :: Text -> IO (Either TxError ())
  }

--------------------------------------------------------------------------------
-- Core Runner (Backend-Agnostic)
--------------------------------------------------------------------------------

{- | Lint configuration for run-time pre-flight: enforces hazard checks
(CONCURRENTLY, destructive operations, two-phase constraints) while
tolerating migration files that omit the generation-time @lock_timeout@
preamble, which is attached by @hylo migrate make@ and is not portable
across backends.
-}
preflightLintConfig :: LintConfig
preflightLintConfig = defaultLintConfig {cfgRequireLockTimeout = False}

{- | Apply a single migration file transactionally (User Story 20 Law 2):
lints the up script (unless forced), opens one transaction, executes every
statement, inserts the history record, and commits. Any failure rolls back
the entire migration, leaving schema and history untouched.
-}
applyMigrationWith ::
  MigrationBackend ->
  -- | Skip the linter pre-flight (--force)
  Bool ->
  FilePath ->
  IO (Either MigrationError ())
applyMigrationWith backend force path = do
  eUp <- readMigrationFile path
  case eUp of
    Left missingErr -> pure (Left (MigrationExecutionError missingErr))
    Right upSql -> do
      let downPath = downPathFor path
      eDown <- readMigrationFile downPath
      case eDown of
        Left missingErr -> pure (Left (MigrationExecutionError missingErr))
        Right downSql
          | not force ->
              case lintMigrationSQLWith preflightLintConfig upSql of
                Left lintErrs -> pure (Left (MigrationLintError lintErrs))
                Right () -> runTransaction upSql downSql
          | otherwise -> runTransaction upSql downSql
  where
    runTransaction upSql downSql = do
      let version = versionOfMigrationFile path
      beginRes <- mbBeginTx backend
      case beginRes of
        Left err -> pure (Left (MigrationHistoryError (txErrorText err)))
        Right () -> runStatements version upSql downSql

    runStatements version upSql downSql = do
      stmtErrs <- executeAll (splitStatements upSql)
      case stmtErrs of
        Just err -> abortWith ("Statement failed in " <> version <> ": " <> err)
        Nothing -> do
          now <- getCurrentTime
          let rec =
                MigrationRecord
                  { mrVersion = version
                  , mrAppliedAt = now
                  , mrUpSql = upSql
                  , mrDownSql = downSql
                  , mrChecksum = checksumOf upSql
                  }
          insertRes <- mbInsertRecord backend rec
          case insertRes of
            Left err -> abortWith (txErrorText err)
            Right () -> do
              commitRes <- mbCommitTx backend
              case commitRes of
                Left err -> abortWith (txErrorText err)
                Right () -> pure (Right ())

    executeAll [] = pure Nothing
    executeAll (stmt : rest)
      | T.null (T.strip stmt) = executeAll rest
      | otherwise = do
          res <- mbExecuteInTx backend (T.strip stmt)
          case res of
            Left err -> pure (Just (txErrorText err))
            Right () -> executeAll rest

    abortWith err = do
      _ <- mbRollbackTx backend
      pure (Left (MigrationExecutionError err))

{- | Roll back the most recently applied migration by executing its recorded
@down_sql@ inside a transaction and deleting its history record.
-}
rollbackLastMigrationWith :: MigrationBackend -> IO (Either MigrationError ())
rollbackLastMigrationWith backend = do
  appliedRes <- listAppliedWith backend
  case appliedRes of
    Left err -> pure (Left err)
    Right [] -> pure (Left (MigrationExecutionError "No applied migrations to roll back"))
    Right recs -> case reverse recs of
      [] -> pure (Left (MigrationExecutionError "No applied migrations to roll back"))
      (rec : _) -> do
        beginRes <- mbBeginTx backend
        case beginRes of
          Left err -> pure (Left (MigrationHistoryError (txErrorText err)))
          Right () -> do
            stmtErrs <- executeAll (splitStatements (mrDownSql rec))
            case stmtErrs of
              Just err -> abortRollback ("Rollback of " <> mrVersion rec <> " failed: " <> err)
              Nothing -> do
                delRes <- mbDeleteRecord backend (mrVersion rec)
                case delRes of
                  Left err -> abortRollback (txErrorText err)
                  Right () -> do
                    commitRes <- mbCommitTx backend
                    case commitRes of
                      Left err -> abortRollback (txErrorText err)
                      Right () -> pure (Right ())
  where
    executeAll [] = pure Nothing
    executeAll (stmt : rest)
      | T.null (T.strip stmt) = executeAll rest
      | otherwise = do
          res <- mbExecuteInTx backend (T.strip stmt)
          case res of
            Left err -> pure (Just (txErrorText err))
            Right () -> executeAll rest
    abortRollback err = do
      _ <- mbRollbackTx backend
      pure (Left (MigrationExecutionError err))

-- | Read all applied migration records in application order.
listAppliedWith :: MigrationBackend -> IO (Either MigrationError [MigrationRecord])
listAppliedWith backend = do
  res <- mbListApplied backend
  pure (either (Left . MigrationHistoryError . txErrorText) Right res)

-- | Create the @_hylo_migrations@ history table if absent.
ensureMigrationsTableWith :: MigrationBackend -> IO (Either MigrationError ())
ensureMigrationsTableWith backend = do
  res <- mbEnsureHistoryTable backend
  pure (either (Left . MigrationHistoryError . txErrorText) Right res)

-- | Render a 'TxError' as a human-readable message.
txErrorText :: TxError -> Text
txErrorText (TxUserError t) = t
txErrorText (TxConstraintViolation st msg det) = "SQLSTATE " <> st <> ": " <> msg <> " (" <> det <> ")"
txErrorText (TxWriteConflict t) = t
txErrorText (TxDeadlockDetected t) = t

--------------------------------------------------------------------------------
-- PostgreSQL Convenience API (User Story 20 signatures)
--------------------------------------------------------------------------------

{- | Create the @_hylo_migrations@ history table if absent.
Throws 'MigrationError' on failure.
-}
ensureMigrationsTable :: Pool -> IO ()
ensureMigrationsTable p = do
  res <- ensureMigrationsTableWith (postgresBackend p)
  either throwIO (const (pure ())) res

{- | List all applied migrations in application order.
Throws 'MigrationError' on failure.
-}
listApplied :: Pool -> IO [MigrationRecord]
listApplied p = do
  res <- listAppliedWith (postgresBackend p)
  either throwIO pure res

{- | Scan a migration directory for pending @<timestamp>_up.sql@ files, in
timestamp order, excluding versions already recorded in the history table.
-}
listPending :: FilePath -> Pool -> IO [FilePath]
listPending dir p = do
  applied <- listApplied p
  pendingFiles dir (map mrVersion applied)

-- | Apply a single migration file transactionally against PostgreSQL.
applyMigration :: Pool -> FilePath -> IO (Either MigrationError ())
applyMigration p = applyMigrationWith (postgresBackend p) False

-- | Roll back the most recently applied migration against PostgreSQL.
rollbackLastMigration :: Pool -> IO (Either MigrationError ())
rollbackLastMigration p = rollbackLastMigrationWith (postgresBackend p)

-- | Pending migration files in timestamp order, excluding applied versions.
pendingFiles :: FilePath -> [Text] -> IO [FilePath]
pendingFiles dir appliedVersions = do
  entries <- listDirectory dir
  let ups = sort [f | f <- entries, "_up.sql" `isSuffixOf` f]
  pure [dir </> f | f <- ups, versionOfMigrationFile f `notElem` appliedVersions]

{- | Derive the migration version from an up-file name
(e.g. @\"20261101120000_up.sql\" -> \"20261101120000\"@).
-}
versionOfMigrationFile :: FilePath -> Text
versionOfMigrationFile f =
  let name = takeFileName f
   in T.dropEnd (length ("_up.sql" :: String)) (T.pack name)

-- | Matching down-script path for an up-script path.
downPathFor :: FilePath -> FilePath
downPathFor upPath =
  takeDirectory upPath </> T.unpack (T.replace "_up.sql" "_down.sql" (T.pack (takeFileName upPath)))

--------------------------------------------------------------------------------
-- Checksum
--------------------------------------------------------------------------------

-- | Deterministic FNV-1a 64-bit checksum of a migration script.
checksumOf :: Text -> Text
checksumOf = T.pack . show . fnv1a64 . TE.encodeUtf8

-- | FNV-1a 64-bit hash over raw bytes (dependency-free deterministic digest).
fnv1a64 :: BS.ByteString -> Word64
fnv1a64 = BS.foldl' step 0xcbf29ce484222325
  where
    step h b = (h `xor` fromIntegral b) * 0x100000001b3

--------------------------------------------------------------------------------
-- PostgreSQL Backend (Hasql)
--------------------------------------------------------------------------------

-- | PostgreSQL migration execution backend over a Hasql pool.
postgresBackend :: Pool -> MigrationBackend
postgresBackend p =
  MigrationBackend
    { mbEnsureHistoryTable = executeSession p (Sess.sql (TE.encodeUtf8 ensureTableSql))
    , mbBeginTx = executeSession p (Sess.sql "BEGIN;")
    , mbCommitTx = executeSession p (Sess.sql "COMMIT;")
    , mbRollbackTx = executeSession p (Sess.sql "ROLLBACK;")
    , mbExecuteInTx = executeSession p . Sess.sql . TE.encodeUtf8
    , mbListApplied = do
        let stmt =
              Stmt.Statement
                ( TE.encodeUtf8
                    ( "SELECT version, applied_at, checksum, up_sql, down_sql FROM "
                        <> migrationsTableName
                        <> " ORDER BY applied_at, version"
                    )
                )
                Enc.noParams
                (Dec.rowList recordDecoder)
                True
        executeSession p (Sess.statement () stmt)
    , mbInsertRecord = \rec ->
        let params =
              [ BoundParam 1 (mrVersion rec) "text"
              , BoundParam
                  2
                  (T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (mrAppliedAt rec)))
                  "timestamptz"
              , BoundParam 3 (mrChecksum rec) "text"
              , BoundParam 4 (mrUpSql rec) "text"
              , BoundParam 5 (mrDownSql rec) "text"
              ]
            stmt =
              mkPreparedStatement
                ( TE.encodeUtf8
                    ( "INSERT INTO "
                        <> migrationsTableName
                        <> " (version, applied_at, checksum, up_sql, down_sql) VALUES ($1, $2, $3, $4, $5)"
                    )
                )
                params
         in executeSession p (Sess.statement params stmt)
    , mbDeleteRecord = \version ->
        let params = [BoundParam 1 version "text"]
            stmt =
              mkPreparedStatement
                (TE.encodeUtf8 ("DELETE FROM " <> migrationsTableName <> " WHERE version = $1"))
                params
         in executeSession p (Sess.statement params stmt)
    }
  where
    ensureTableSql =
      "CREATE TABLE IF NOT EXISTS "
        <> migrationsTableName
        <> " (version TEXT PRIMARY KEY, applied_at TIMESTAMPTZ NOT NULL DEFAULT now(), checksum TEXT, up_sql TEXT, down_sql TEXT);"

-- | Hasql decoder for migration history rows.
recordDecoder :: Dec.Row MigrationRecord
recordDecoder =
  MigrationRecord
    <$> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.timestamptz)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text)

--------------------------------------------------------------------------------
-- SQLite Backend (In-Memory Test Catalogs)
--------------------------------------------------------------------------------

{- | SQLite migration execution backend over an in-memory or file-backed pool.
The history table stores @applied_at@ as ISO-8601 text.
-}
sqliteBackend :: SQLite.Pool -> MigrationBackend
sqliteBackend p =
  MigrationBackend
    { mbEnsureHistoryTable = SQLite.executeSql p ensureTableSql
    , mbBeginTx = SQLite.executeSql p "BEGIN;"
    , mbCommitTx = SQLite.executeSql p "COMMIT;"
    , mbRollbackTx = SQLite.executeSql p "ROLLBACK;"
    , mbExecuteInTx = SQLite.executeSql p
    , mbListApplied = do
        eRows <- sqliteQueryRows p selectSql
        pure (eRows >>= mapM decodeRecordRow)
    , mbInsertRecord = \rec ->
        SQLite.executeSql
          p
          ( "INSERT INTO "
              <> migrationsTableName
              <> " (version, applied_at, checksum, up_sql, down_sql) VALUES ('"
              <> sqliteEscape (mrVersion rec)
              <> "', '"
              <> sqliteEscape (T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (mrAppliedAt rec)))
              <> "', '"
              <> sqliteEscape (mrChecksum rec)
              <> "', '"
              <> sqliteEscape (mrUpSql rec)
              <> "', '"
              <> sqliteEscape (mrDownSql rec)
              <> "');"
          )
    , mbDeleteRecord = \version ->
        SQLite.executeSql
          p
          ("DELETE FROM " <> migrationsTableName <> " WHERE version = '" <> sqliteEscape version <> "';")
    }
  where
    ensureTableSql =
      "CREATE TABLE IF NOT EXISTS "
        <> migrationsTableName
        <> " (version TEXT PRIMARY KEY, applied_at TEXT NOT NULL, checksum TEXT, up_sql TEXT, down_sql TEXT);"
    selectSql =
      "SELECT version, applied_at, checksum, up_sql, down_sql FROM "
        <> migrationsTableName
        <> " ORDER BY applied_at, version;"

    sqliteEscape = T.replace "'" "''"

    decodeRecordRow :: [S.SQLData] -> Either TxError MigrationRecord
    decodeRecordRow [S.SQLText v, S.SQLText at, S.SQLText c, S.SQLText up, S.SQLText down] =
      case iso8601ParseM (T.unpack at) of
        Just t -> Right (MigrationRecord v t up down c)
        Nothing -> Left (TxUserError ("Unparsable applied_at in history row: " <> at))
    decodeRecordRow other =
      Left (TxUserError ("Malformed history row: " <> T.pack (show other)))

-- | Execute a raw SELECT against SQLite and return raw column values per row.
sqliteQueryRows :: SQLite.Pool -> Text -> IO (Either TxError [[S.SQLData]])
sqliteQueryRows p sqlText = SQLite.withPool p $ \db -> do
  res <-
    tryIOError $
      bracket (S.prepare db sqlText) S.finalize $ \stmt -> do
        let fetchRows acc = do
              stepRes <- S.step stmt
              case stepRes of
                SD.Done -> pure (Right (reverse acc))
                SD.Row -> do
                  cols <- S.columns stmt
                  fetchRows (cols : acc)
        fetchRows []
  case res of
    Left (err :: IOError) -> pure (Left (TxUserError (T.pack (show err))))
    Right val -> pure val

--------------------------------------------------------------------------------
-- File Reading Helpers
--------------------------------------------------------------------------------

-- | Read a text file, translating IO errors into a descriptive message.
readMigrationFile :: FilePath -> IO (Either Text Text)
readMigrationFile path = do
  exists <- doesFileExist path
  if not exists
    then pure (Left ("Migration file not found: " <> T.pack path))
    else do
      res <- tryIOError (TIO.readFile path)
      case res of
        Left (err :: IOError) -> pure (Left ("Failed to read " <> T.pack path <> ": " <> T.pack (show err)))
        Right content -> pure (Right content)
