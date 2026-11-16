{- |
Module      : Main
Description : Executable entry point for Hylo CLI (e.g. hylo migrate make)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Main (main) where

import Control.Exception (handle)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hylo.Migrate.Runner.Live
  ( MigrationError (..)
  , makeMigration
  )
import Hylo.PG.Connection (createPool)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["migrate", "make"] -> runMake defaultConn "migrations"
    ["migrate", "make", "--dir", dir] -> runMake defaultConn dir
    ["migrate", "make", "--conn", connStr] -> runMake connStr "migrations"
    ["migrate", "make", "--conn", connStr, "--dir", dir] -> runMake connStr dir
    ["migrate", "make", "--dir", dir, "--conn", connStr] -> runMake connStr dir
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

    printUsage = do
      putStrLn "Hylo — Type-Safe, High-Performance ORM for Haskell"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  hylo migrate make [--conn <connection-string>] [--dir <migrations-dir>]"
