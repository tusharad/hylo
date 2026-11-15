{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.Test.Docker
Description : Testcontainer and Docker PostgreSQL runner for integration tests
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Test.Docker
  ( withTestPostgres
  , getTestPostgresPool
  , ensureVectorExtension
  , detectPostgresUrl
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Hasql.Connection as C
import qualified Hasql.Connection.Setting as S
import qualified Hasql.Connection.Setting.Connection as SC
import Hylo.PG.Connection
  ( Pool (..)
  , closePool
  , createPool
  , executeSql
  )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Candidate connection strings to check in priority order.
candidateUrls :: [Text]
candidateUrls =
  [ "postgresql://postgres:postgres@localhost:5432/hylo_test"
  , "postgresql://postgres:postgres@localhost:5434/postgres_air"
  , "postgresql://postgres:postgres@localhost:5432/postgres"
  ]

-- | Test whether a connection string can successfully establish a PostgreSQL connection.
canConnect :: Text -> IO Bool
canConnect url = do
  res <- C.acquire [S.connection (SC.string url)]
  case res of
    Right conn -> do
      C.release conn
      pure True
    Left _ -> pure False

-- | Start a temporary Docker container running PostgreSQL 17 with pgvector.
startDockerPostgres :: IO Bool
startDockerPostgres = do
  -- Check if docker is installed
  (exitCode, _, _) <- readProcessWithExitCode "which" ["docker"] ""
  case exitCode of
    ExitSuccess -> do
      -- Launch container in background if not already running
      _ <-
        readProcessWithExitCode
          "docker"
          [ "run"
          , "-d"
          , "--rm"
          , "--name"
          , "hylo-testcontainer"
          , "-p"
          , "5432:5432"
          , "-e"
          , "POSTGRES_PASSWORD=postgres"
          , "-e"
          , "POSTGRES_DB=hylo_test"
          , "pgvector/pgvector:pg17"
          ]
          ""
      -- Wait up to 10 seconds for container to become healthy
      waitForReady 20
    ExitFailure _ -> pure False
  where
    waitForReady (0 :: Int) = pure False
    waitForReady n = do
      threadDelay 500000 -- 500ms
      ok <- canConnect "postgresql://postgres:postgres@localhost:5432/hylo_test"
      if ok then pure True else waitForReady (n - 1)

-- | Detect an active PostgreSQL database URL from environment, active containers, or local Docker.
detectPostgresUrl :: IO (Maybe Text)
detectPostgresUrl = do
  mEnv <- lookupEnv "DATABASE_URL"
  case mEnv of
    Just url | not (null url) -> do
      let tUrl = T.pack url
      ok <- canConnect tUrl
      if ok then pure (Just tUrl) else checkCandidates
    _ -> checkCandidates
  where
    checkCandidates = checkList candidateUrls

    checkList [] = do
      -- No active PostgreSQL found on candidate ports; attempt starting docker container
      started <- startDockerPostgres
      if started
        then pure (Just "postgresql://postgres:postgres@localhost:5432/hylo_test")
        else pure Nothing
    checkList (u : us) = do
      ok <- canConnect u
      if ok then pure (Just u) else checkList us

-- | Acquire a connected Pool to real PostgreSQL, enabling vector extension if available.
getTestPostgresPool :: IO (Maybe Pool)
getTestPostgresPool = do
  mUrl <- detectPostgresUrl
  case mUrl of
    Nothing -> pure Nothing
    Just url -> do
      p <- createPool url
      case poolActiveConnection p of
        Just _ -> do
          _ <- ensureVectorExtension p
          pure (Just p)
        Nothing -> pure Nothing

-- | Ensure pgvector extension is created on the test database.
ensureVectorExtension :: Pool -> IO ()
ensureVectorExtension p = do
  _ <- executeSql p "CREATE EXTENSION IF NOT EXISTS vector;"
  pure ()

{- | Run a test block with real PostgreSQL. If PostgreSQL is not reachable,
passes an in-memory Pool fallback so pure tests still succeed without crashing.
-}
withTestPostgres :: (Pool -> IO a) -> IO a
withTestPostgres action = do
  mPool <- getTestPostgresPool
  let fallbackPool = Pool "postgresql://localhost:5432/hylo_test" 10 Nothing
  bracket
    (pure (fromMaybe fallbackPool mPool))
    closePool
    action
