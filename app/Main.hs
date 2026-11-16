{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Executable entry point for Hylo CLI (e.g. hylo migrate make, hylo ai-context)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Main (main) where

import Control.Exception (handle)
import Control.Monad (forM_)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (diffUTCTime, formatTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale)
import Hylo.AI.Context.Exporter
  ( AiContextConfig (..)
  , defaultAiContextConfig
  , estimateTokens
  , exportAgentsMarkdown
  , exportLlmsTxt
  )
import Hylo.Core
import Hylo.Migrate.Runner.Apply
  ( MigrationRecord (..)
  , applyMigrationWith
  , ensureMigrationsTable
  , listApplied
  , listPending
  , postgresBackend
  , rollbackLastMigration
  , versionOfMigrationFile
  )
import Hylo.Migrate.Runner.Live
  ( MigrationError (..)
  , makeMigration
  )
import Hylo.PG.Connection (createPool)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (</>))
import Text.Read (readMaybe)

-- | Demo table: user accounts with optimistic locking.
data DemoUserT (ctx :: Ctx) = DemoUser
  { duId :: !(Col ctx (Id DemoUserT Int64))
  , duEmail :: !(Col ctx Text)
  , duVersion :: !(Col ctx Int64)
  }

-- | Demo table: organizations.
data DemoOrgT (ctx :: Ctx) = DemoOrg
  { doId :: !(Col ctx (Id DemoOrgT Int64))
  , doName :: !(Col ctx Text)
  }

-- | Demo table: uploaded documents with a Redis read-through cache.
data DemoDocT (ctx :: Ctx) = DemoDoc
  { ddId :: !(Col ctx (Id DemoDocT Int64))
  , ddTitle :: !(Col ctx Text)
  , ddBody :: !(Col ctx (Maybe Text))
  }

demoUserMeta :: DemoUserT 'Meta
demoUserMeta =
  DemoUser
    { duId = primaryKey "id" "bigint"
    , duEmail = column "email" "text"
    , duVersion = optimisticLockColumn "version" "bigint"
    }

demoOrgMeta :: DemoOrgT 'Meta
demoOrgMeta =
  DemoOrg
    { doId = primaryKey "id" "bigint"
    , doName = column "name" "text"
    }

demoDocMeta :: DemoDocT 'Meta
demoDocMeta =
  DemoDoc
    { ddId = primaryKey "id" "bigint"
    , ddTitle = column "title" "text"
    , ddBody = nullableColumn "body" "text"
    }

demoUserSchema :: TableSchema DemoUserT
demoUserSchema = withTableDescription "User accounts" (tableSchema "users" demoUserMeta)

demoOrgSchema :: TableSchema DemoOrgT
demoOrgSchema = withTableDescription "Organizations" (tableSchema "orgs" demoOrgMeta)

describedDocSchema :: TableSchema DemoDocT
describedDocSchema = withTableDescription "Uploaded documents" (tableSchema "documents" demoDocMeta)

demoDocSchema :: TableSchema DemoDocT
demoDocSchema = withRedisCache (RedisCachePolicy "docs:" 120) describedDocSchema

-- | Built-in three-table demo registry rendered by @hylo ai-context@.
demoRegistry :: [AnyTableSchema]
demoRegistry =
  [ AnyTableSchema demoUserSchema
  , AnyTableSchema demoOrgSchema
  , AnyTableSchema demoDocSchema
  ]

instance Table DemoUserT where
  toTableExpr alias m =
    DemoUser
      { duId = colSpecExpr alias m.duId
      , duEmail = colSpecExpr alias m.duEmail
      , duVersion = colSpecExpr alias m.duVersion
      }
  tableColDefs m =
    [ eraseColDef m.duId
    , eraseColDef m.duEmail
    , eraseColDef m.duVersion
    ]

instance Table DemoOrgT where
  toTableExpr alias m =
    DemoOrg
      { doId = colSpecExpr alias m.doId
      , doName = colSpecExpr alias m.doName
      }
  tableColDefs m =
    [ eraseColDef m.doId
    , eraseColDef m.doName
    ]

instance Table DemoDocT where
  toTableExpr alias m =
    DemoDoc
      { ddId = colSpecExpr alias m.ddId
      , ddTitle = colSpecExpr alias m.ddTitle
      , ddBody = colSpecExpr alias m.ddBody
      }
  tableColDefs m =
    [ eraseColDef m.ddId
    , eraseColDef m.ddTitle
    , eraseColDef m.ddBody
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["migrate", "make"] -> runMake defaultConn "migrations"
    ["migrate", "make", "--dir", dir] -> runMake defaultConn dir
    ["migrate", "make", "--conn", connStr] -> runMake connStr "migrations"
    ["migrate", "make", "--conn", connStr, "--dir", dir] -> runMake connStr dir
    ["migrate", "make", "--dir", dir, "--conn", connStr] -> runMake connStr dir
    ("migrate" : sub : rest)
      | sub `elem` ["run", "rollback", "status"] ->
          case parseMigrateFlags rest of
            Right cfg -> runMigrateSubCommand sub cfg
            Left err -> do
              putStrLn err
              printUsage
    ("ai-context" : rest) ->
      case parseAiFlags rest of
        Right cfg -> runAiContext cfg
        Left err -> do
          putStrLn err
          printUsage
    _ -> printUsage
  where
    defaultConn = "postgresql://localhost:5432/hylo_test"
    runMake connStr dir = handle handleErr $ do
      p <- createPool (T.pack connStr)
      putStrLn $ "Introspecting live database at: " <> connStr
      (upSql, downSql) <- makeMigration p [] dir
      putStrLn $ "Successfully generated migration in directory: " <> dir
      putStrLn "--- up.sql ---"
      TIO.putStrLn upSql
      putStrLn "--- down.sql ---"
      TIO.putStrLn downSql

    handleErr (MigrationLintError errs) = do
      putStrLn $ "Migration halted due to locking hazards:\n" ++ show errs
      exitFailure
    handleErr (MigrationShadowError res) = do
      putStrLn $ "Migration halted: shadow verification failed:\n" ++ show res
      exitFailure
    handleErr (MigrationIntrospectError err) = do
      putStrLn $ "Migration halted: failed to introspect database:\n" ++ show err
      exitFailure
    handleErr (MigrationExecutionError err) = do
      putStrLn $ "Migration execution failed:\n" ++ T.unpack err
      exitFailure
    handleErr (MigrationHistoryError err) = do
      putStrLn $ "Migration history access failed:\n" ++ T.unpack err
      exitFailure

    printUsage = do
      putStrLn "Hylo — Type-Safe, High-Performance ORM for Haskell"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  hylo migrate make [--conn <connection-string>] [--dir <migrations-dir>]"
      putStrLn "  hylo migrate run [--conn <connection-string>] [--dir <migrations-dir>] [--force]"
      putStrLn "  hylo migrate rollback [--conn <connection-string>] [--dir <migrations-dir>]"
      putStrLn "  hylo migrate status [--conn <connection-string>] [--dir <migrations-dir>]"
      putStrLn "  hylo ai-context [--dir <output-dir>] [--tables <table1,table2,...>] [--budget <tokens>]"

-- | CLI flags shared by @migrate run@, @migrate rollback@, and @migrate status@.
data MigrateFlags = MigrateFlags
  { mfConn :: !String
  , mfDir :: !FilePath
  , mfForce :: !Bool
  }

-- | Default migration flags for @parseMigrateFlags@.
defaultMigrateFlags :: MigrateFlags
defaultMigrateFlags =
  MigrateFlags
    { mfConn = "postgresql://localhost:5432/hylo_test"
    , mfDir = "migrations"
    , mfForce = False
    }

-- | Parse @migrate run/rollback/status@ flags in any order into a 'MigrateFlags'.
parseMigrateFlags :: [String] -> Either String MigrateFlags
parseMigrateFlags = go defaultMigrateFlags
  where
    go cfg [] = Right cfg
    go cfg ("--conn" : c : rest) = go cfg {mfConn = c} rest
    go cfg ("--dir" : d : rest) = go cfg {mfDir = d} rest
    go cfg ("--force" : rest) = go cfg {mfForce = True} rest
    go _ (flag : _) = Left ("Unknown or incomplete migrate flag: " <> flag)

-- | Dispatch @migrate run@, @migrate rollback@, and @migrate status@.
runMigrateSubCommand :: String -> MigrateFlags -> IO ()
runMigrateSubCommand sub cfg = handle handleErrTop $ case sub of
  "run" -> runMigrateRun cfg
  "rollback" -> runMigrateRollback cfg
  "status" -> runMigrateStatus cfg
  _ -> pure ()
  where
    handleErrTop err = do
      putStrLn (migrationErrorMessage err)
      exitFailure

-- | Human-readable message for a 'MigrationError'.
migrationErrorMessage :: MigrationError -> String
migrationErrorMessage (MigrationLintError errs) = "Migration halted due to locking hazards:\n" ++ show errs
migrationErrorMessage (MigrationShadowError res) = "Migration halted: shadow verification failed:\n" ++ show res
migrationErrorMessage (MigrationIntrospectError err) = "Migration halted: failed to introspect database:\n" ++ show err
migrationErrorMessage (MigrationExecutionError err) = "Migration execution failed:\n" ++ T.unpack err
migrationErrorMessage (MigrationHistoryError err) = "Migration history access failed:\n" ++ T.unpack err

{- | Apply every pending migration in timestamp order, printing per-migration
status lines. Prints @Nothing to apply.@ and exits 0 when there are no
pending migrations.
-}
runMigrateRun :: MigrateFlags -> IO ()
runMigrateRun cfg = do
  p <- createPool (T.pack (mfConn cfg))
  ensureMigrationsTable p
  pending <- listPending (mfDir cfg) p
  if null pending
    then putStrLn "Nothing to apply."
    else forM_ pending $ \file -> do
      t0 <- getCurrentTime
      res <- applyMigrationWith (postgresBackend p) (mfForce cfg) file
      t1 <- getCurrentTime
      let ms = round (realToFrac (diffUTCTime t1 t0) * 1000 :: Double) :: Int64
      case res of
        Right () ->
          TIO.putStrLn
            ( "✓ "
                <> T.pack (takeFileName file)
                <> " applied ("
                <> T.pack (show ms)
                <> " ms)"
            )
        Left err -> do
          TIO.putStrLn
            ( "✗ "
                <> T.pack (takeFileName file)
                <> " failed: "
                <> T.pack (migrationErrorMessage err)
            )
          exitFailure

-- | Roll back the most recently applied migration and remove its record.
runMigrateRollback :: MigrateFlags -> IO ()
runMigrateRollback cfg = do
  p <- createPool (T.pack (mfConn cfg))
  res <- rollbackLastMigration p
  case res of
    Right () -> putStrLn "✓ rolled back last migration"
    Left err -> do
      putStrLn ("✗ rollback failed: " <> migrationErrorMessage err)
      exitFailure

-- | Print applied versus pending migration versions.
runMigrateStatus :: MigrateFlags -> IO ()
runMigrateStatus cfg = do
  p <- createPool (T.pack (mfConn cfg))
  ensureMigrationsTable p
  applied <- listApplied p
  pending <- listPending (mfDir cfg) p
  putStrLn "Applied migrations:"
  if null applied
    then putStrLn "  (none)"
    else forM_ applied $ \rec ->
      TIO.putStrLn
        ( "  "
            <> mrVersion rec
            <> "  "
            <> mrChecksum rec
            <> "  applied at "
            <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%QZ" (mrAppliedAt rec))
        )
  putStrLn "Pending migrations:"
  if null pending
    then putStrLn "  (none)"
    else forM_ pending $ \file ->
      TIO.putStrLn ("  " <> versionOfMigrationFile file <> "  " <> T.pack (takeFileName file))

-- | Parse @ai-context@ flags in any order into an 'AiContextConfig'.
parseAiFlags :: [String] -> Either String AiContextConfig
parseAiFlags = go defaultAiContextConfig
  where
    go cfg [] = Right cfg
    go cfg ("--dir" : dir : rest) = go cfg {acOutputDir = dir} rest
    go cfg ("--tables" : ts : rest) = go cfg {acIncludeTables = parseTableList ts} rest
    go cfg ("--budget" : b : rest) = case readMaybe b of
      Just n -> go cfg {acTokenBudget = n} rest
      Nothing -> Left ("Invalid --budget value: " <> b)
    go _ (flag : _) = Left ("Unknown or incomplete ai-context flag: " <> flag)

parseTableList :: String -> [T.Text]
parseTableList = filter (not . T.null) . T.splitOn "," . T.pack

{- | Render @AGENTS.md@ and @llms.txt@ for the demo registry into the
configured output directory and print a token summary.
-}
runAiContext :: AiContextConfig -> IO ()
runAiContext cfg = do
  createDirectoryIfMissing True (acOutputDir cfg)
  let agentsMd = exportAgentsMarkdown demoRegistry cfg
      llmsTxt = exportLlmsTxt demoRegistry cfg
  TIO.writeFile (acOutputDir cfg </> "AGENTS.md") agentsMd
  TIO.writeFile (acOutputDir cfg </> "llms.txt") llmsTxt
  putStrLn $
    "Generated AGENTS.md ("
      <> groupThousands (estimateTokens agentsMd)
      <> " tokens) and llms.txt ("
      <> groupThousands (estimateTokens llmsTxt)
      <> " tokens)"

-- | Group an integer with single spaces every three digits (e.g. @1 234@).
groupThousands :: Int -> String
groupThousands n = unwords (reverse (map reverse (chunk 3 (reverse (show n)))))
  where
    chunk k xs = case splitAt k xs of
      (p, []) -> [p]
      (p, rest) -> p : chunk k rest
