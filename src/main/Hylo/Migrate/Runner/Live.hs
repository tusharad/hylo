{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.Migrate.Runner.Live
Description : Live Database Catalog Introspection Runner & Automated Migration Make
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Migrate.Runner.Live
  ( -- * Live Catalog Introspection
    introspectLiveDatabase
  , introspectTablesQuery
  , introspectColumnsQuery
  , introspectIndexesQuery
  , introspectConstraintsQuery

    -- * Automated Migration Generator
  , makeMigration
  , makeMigrationEither
  , makeMigrationWithConfig
  , makeMigrationWithConfigEither
  , makeMigrationWithCatalog
  , makeMigrationWithTimestamp
  , makeMigrationWithTimestampEither
  , makeMigrationFromTableDefs
  , makeMigrationFromTableDefsEither
  , makeMigrationFromCatalogsWithTimestampEither
  , makeMigrationFromCatalogsWithConfigTimestampEither

    -- * Migration Errors & Exceptions
  , MigrationError (..)

    -- * Offline / Mock Pool Support
  , withMockCatalog
  , createMockPool
  , clearMockCatalogs

    -- * File Persistence Helper
  , persistMigrationFiles
  ) where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception, throwIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import GHC.Generics (Generic)
import qualified Hasql.Decoders as Dec
import qualified Hasql.Encoders as Enc
import qualified Hasql.Session as Sess
import qualified Hasql.Statement as Stmt
import Hylo.Core.Table (AnyTableSchema (..))
import Hylo.Core.Transaction (TxError (..))
import Hylo.Migrate.AST (TableDef (..))
import Hylo.Migrate.Catalog
  ( Catalog (..)
  , anyTableSchemaToDef
  , schemasToCatalog
  )
import Hylo.Migrate.Catalog.PG
  ( PGColumnRow (..)
  , PGConstraintRow (..)
  , PGIndexRow (..)
  , PGTableRow (..)
  , buildCatalogFromRows
  , introspectColumnsQuery
  , introspectConstraintsQuery
  , introspectIndexesQuery
  , introspectTablesQuery
  )
import Hylo.Migrate.Diff (computeCatalogDiff, renderReversibleDDL)
import Hylo.Migrate.Linter
  ( LintConfig (..)
  , LintError (..)
  , attachLockTimeoutPreamble
  , defaultLintConfig
  , lintMigrationSQLWith
  )
import Hylo.Migrate.Runner.Shadow (ShadowVerificationResult (..), verifyCatalogDiff)
import Hylo.PG.Connection (Pool (..), executeSession)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

-- | Structured migration generation failure reasons.
data MigrationError
  = MigrationIntrospectError !TxError
  | MigrationShadowError !ShadowVerificationResult
  | MigrationLintError ![LintError]
  deriving (Eq, Show, Generic)

instance NFData MigrationError where
  rnf = \case
    MigrationIntrospectError e -> e `seq` ()
    MigrationShadowError r -> rnf r
    MigrationLintError l -> rnf l

instance Exception MigrationError

-- | Global registry for in-memory mock catalogs used in testing and offline introspection.
{-# NOINLINE mockCatalogRegistry #-}
mockCatalogRegistry :: IORef (Map Text Catalog)
mockCatalogRegistry = unsafePerformIO (newIORef Map.empty)

{- | Associate a mock 'Catalog' with a 'Pool' so 'introspectLiveDatabase' returns it
without needing a live PostgreSQL database connection.
-}
withMockCatalog :: Catalog -> Pool -> Pool
withMockCatalog cat p =
  let key =
        if T.null (poolConnectionString p)
          || poolConnectionString p == "postgresql://localhost:5432/hylo_test"
          then "mock://catalog_" <> T.pack (show (length (catalogTables cat)))
          else poolConnectionString p
   in unsafePerformIO $ do
        modifyIORef' mockCatalogRegistry (Map.insert key cat)
        pure (p {poolConnectionString = key, poolActiveConnection = Nothing})

-- | Construct a dedicated mock connection pool preloaded with an in-memory catalog.
createMockPool :: Catalog -> Pool
createMockPool cat = withMockCatalog cat (Pool "mock://in-memory" 10 Nothing)

-- | Clear all registered mock catalogs from memory.
clearMockCatalogs :: IO ()
clearMockCatalogs = writeIORef mockCatalogRegistry Map.empty

-- | Introspect a live PostgreSQL database via Hasql or retrieve a registered mock catalog.
introspectLiveDatabase :: Pool -> IO (Either TxError Catalog)
introspectLiveDatabase p =
  case poolActiveConnection p of
    Just _conn -> do
      let sess = do
            tbls <-
              Sess.statement
                ()
                (Stmt.Statement (TE.encodeUtf8 introspectTablesQuery) Enc.noParams (Dec.rowList tableRowDecoder) True)
            cols <-
              Sess.statement
                ()
                ( Stmt.Statement
                    (TE.encodeUtf8 introspectColumnsQuery)
                    Enc.noParams
                    (Dec.rowList columnRowDecoder)
                    True
                )
            idxs <-
              Sess.statement
                ()
                ( Stmt.Statement
                    (TE.encodeUtf8 introspectIndexesQuery)
                    Enc.noParams
                    (Dec.rowList indexRowDecoder)
                    True
                )
            cons <-
              Sess.statement
                ()
                ( Stmt.Statement
                    (TE.encodeUtf8 introspectConstraintsQuery)
                    Enc.noParams
                    (Dec.rowList constraintRowDecoder)
                    True
                )
            pure (buildCatalogFromRows tbls cols idxs cons)
      executeSession p sess
    Nothing -> do
      reg <- readIORef mockCatalogRegistry
      case Map.lookup (poolConnectionString p) reg of
        Just cat -> pure (Right cat)
        Nothing ->
          pure
            ( Left
                ( TxUserError
                    ( "introspectLiveDatabase: no active PostgreSQL connection in pool for "
                        <> poolConnectionString p
                    )
                )
            )

-- | Hasql decoder for 'PGTableRow'.
tableRowDecoder :: Dec.Row PGTableRow
tableRowDecoder =
  PGTableRow
    <$> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nullable Dec.text)

-- | Hasql decoder for 'PGColumnRow'.
columnRowDecoder :: Dec.Row PGColumnRow
columnRowDecoder =
  PGColumnRow
    <$> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.bool)
    <*> Dec.column (Dec.nullable Dec.text)
    <*> Dec.column (Dec.nullable Dec.text)

-- | Hasql decoder for 'PGIndexRow'.
indexRowDecoder :: Dec.Row PGIndexRow
indexRowDecoder =
  PGIndexRow
    <$> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.bool)
    <*> Dec.column (Dec.nonNullable Dec.bool)
    <*> Dec.column (Dec.nonNullable Dec.text)

-- | Hasql decoder for 'PGConstraintRow'.
constraintRowDecoder :: Dec.Row PGConstraintRow
constraintRowDecoder =
  PGConstraintRow
    <$> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable (Dec.custom (\_ bs -> Right (TE.decodeUtf8 bs))))
    <*> Dec.column (Dec.nonNullable Dec.text)

{- | Automated migration generator: introspects live database, diffs against application
schemas, verifies round-trip reversibility via shadow catalog, lints for production locking
hazards, persists timestamped migration files, and returns (up.sql, down.sql).

Throws 'MigrationError' on failure.
-}
makeMigration :: Pool -> [AnyTableSchema] -> FilePath -> IO (Text, Text)
makeMigration p schemas dir = do
  res <- makeMigrationEither p schemas dir
  case res of
    Left err -> throwIO err
    Right ok -> pure ok

-- | Either-based variant of 'makeMigration'.
makeMigrationEither ::
  Pool -> [AnyTableSchema] -> FilePath -> IO (Either MigrationError (Text, Text))
makeMigrationEither p schemas dir = do
  now <- getCurrentTime
  makeMigrationWithTimestampEither now p schemas dir

-- | Automated migration generator with an explicitly provided timestamp.
makeMigrationWithTimestamp :: UTCTime -> Pool -> [AnyTableSchema] -> FilePath -> IO (Text, Text)
makeMigrationWithTimestamp now p schemas dir = do
  res <- makeMigrationWithTimestampEither now p schemas dir
  case res of
    Left err -> throwIO err
    Right ok -> pure ok

-- | Either-based variant of 'makeMigrationWithTimestamp'.
makeMigrationWithTimestampEither ::
  UTCTime ->
  Pool ->
  [AnyTableSchema] ->
  FilePath ->
  IO (Either MigrationError (Text, Text))
makeMigrationWithTimestampEither now p schemas dir = do
  eS0 <- introspectLiveDatabase p
  case eS0 of
    Left txErr -> pure (Left (MigrationIntrospectError txErr))
    Right s0 -> do
      let targetDefs = map anyTableSchemaToDef schemas
          targetCat = schemasToCatalog targetDefs
      makeMigrationFromCatalogsWithTimestampEither now s0 targetCat dir

-- | Generate migration from an explicitly provided baseline catalog.
makeMigrationWithCatalog :: Catalog -> [AnyTableSchema] -> FilePath -> IO (Text, Text)
makeMigrationWithCatalog s0 schemas dir = do
  now <- getCurrentTime
  let targetDefs = map anyTableSchemaToDef schemas
      targetCat = schemasToCatalog targetDefs
  res <- makeMigrationFromCatalogsWithTimestampEither now s0 targetCat dir
  case res of
    Left err -> throwIO err
    Right ok -> pure ok

-- | Generate migration from target 'TableDef' definitions.
makeMigrationFromTableDefs :: Pool -> [TableDef] -> FilePath -> IO (Text, Text)
makeMigrationFromTableDefs p defs dir = do
  res <- makeMigrationFromTableDefsEither p defs dir
  case res of
    Left err -> throwIO err
    Right ok -> pure ok

-- | Either-based variant of 'makeMigrationFromTableDefs'.
makeMigrationFromTableDefsEither ::
  Pool -> [TableDef] -> FilePath -> IO (Either MigrationError (Text, Text))
makeMigrationFromTableDefsEither p defs dir = do
  now <- getCurrentTime
  eS0 <- introspectLiveDatabase p
  case eS0 of
    Left txErr -> pure (Left (MigrationIntrospectError txErr))
    Right s0 -> do
      let targetCat = schemasToCatalog defs
      makeMigrationFromCatalogsWithTimestampEither now s0 targetCat dir

-- | Core migration generation pipeline taking source and target catalogs directly.
makeMigrationFromCatalogsWithTimestampEither ::
  UTCTime ->
  Catalog ->
  Catalog ->
  FilePath ->
  IO (Either MigrationError (Text, Text))
makeMigrationFromCatalogsWithTimestampEither =
  makeMigrationFromCatalogsWithConfigTimestampEither defaultLintConfig

-- | Core migration generation pipeline with configurable lint settings.
makeMigrationFromCatalogsWithConfigTimestampEither ::
  LintConfig ->
  UTCTime ->
  Catalog ->
  Catalog ->
  FilePath ->
  IO (Either MigrationError (Text, Text))
makeMigrationFromCatalogsWithConfigTimestampEither cfg now s0 s1 dir = do
  let diff = computeCatalogDiff s0 s1
  case verifyCatalogDiff s0 diff of
    ShadowSuccess -> do
      let (rawUp, rawDown) = renderReversibleDDL diff
          upSql = attachLockTimeoutPreamble rawUp
          downSql = attachLockTimeoutPreamble rawDown
      case lintMigrationSQLWith cfg upSql of
        Left lintErrs -> pure (Left (MigrationLintError lintErrs))
        Right () -> do
          let ts = T.pack (formatTime defaultTimeLocale "%Y%m%d%H%M%S" now)
          _ <- persistMigrationFiles dir ts upSql downSql
          pure (Right (upSql, downSql))
    shadowFailure ->
      pure (Left (MigrationShadowError shadowFailure))

-- | Automated migration generator using a custom 'LintConfig'.
makeMigrationWithConfig ::
  LintConfig ->
  Pool ->
  [AnyTableSchema] ->
  FilePath ->
  IO (Text, Text)
makeMigrationWithConfig cfg p schemas dir = do
  res <- makeMigrationWithConfigEither cfg p schemas dir
  case res of
    Left err -> throwIO err
    Right ok -> pure ok

-- | Either-based variant of 'makeMigrationWithConfig'.
makeMigrationWithConfigEither ::
  LintConfig ->
  Pool ->
  [AnyTableSchema] ->
  FilePath ->
  IO (Either MigrationError (Text, Text))
makeMigrationWithConfigEither cfg p schemas dir = do
  now <- getCurrentTime
  eS0 <- introspectLiveDatabase p
  case eS0 of
    Left txErr -> pure (Left (MigrationIntrospectError txErr))
    Right s0 -> do
      let targetDefs = map anyTableSchemaToDef schemas
          targetCat = schemasToCatalog targetDefs
      makeMigrationFromCatalogsWithConfigTimestampEither cfg now s0 targetCat dir

-- | Persist synthesized up.sql and down.sql files to the migration directory.
persistMigrationFiles :: FilePath -> Text -> Text -> Text -> IO (FilePath, FilePath)
persistMigrationFiles dir timestamp upSql downSql = do
  createDirectoryIfMissing True dir
  let upFile = dir </> (T.unpack timestamp <> "_up.sql")
      downFile = dir </> (T.unpack timestamp <> "_down.sql")
      canonicalUp = dir </> "up.sql"
      canonicalDown = dir </> "down.sql"
  TIO.writeFile upFile upSql
  TIO.writeFile downFile downSql
  TIO.writeFile canonicalUp upSql
  TIO.writeFile canonicalDown downSql
  pure (upFile, downFile)
