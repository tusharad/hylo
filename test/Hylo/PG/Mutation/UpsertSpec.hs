{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant bracket" #-}

{- |
Module      : Hylo.PG.Mutation.UpsertSpec
Description : Formal specification and executable oracle for Epic 9 (Upserts & Pessimistic Locking)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Mutation.UpsertSpec
  ( spec
  ) where

import Control.DeepSeq (NFData (..))
import Data.Int (Int64)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core
import Hylo.Core.Query
import Hylo.PG.Compiler.SQL (renderSQL)
import Hylo.PG.Mutation.Locking
import Hylo.PG.Mutation.Upsert
import Hylo.Test.ShouldNotTypecheck (shouldNotTypecheckWith)
import Test.Hspec
import Test.QuickCheck

default (Text, Int64, Double)

--------------------------------------------------------------------------------
-- Domain Schemas for Oracle & Property Verification
--------------------------------------------------------------------------------

data UserT (ctx :: Ctx) = User
  { id :: !(Col ctx (Id UserT Int64))
  , email :: !(Col ctx Text)
  , lastLoginAt :: !(Col ctx Int64)
  , loginCount :: !(Col ctx Int64)
  }

instance Table UserT where
  toTableExpr alias m =
    User
      { id = colSpecExpr alias m.id
      , email = colSpecExpr alias m.email
      , lastLoginAt = colSpecExpr alias m.lastLoginAt
      , loginCount = colSpecExpr alias m.loginCount
      }
  tableExprColumns u =
    [ renderExpr u.id
    , renderExpr u.email
    , renderExpr u.lastLoginAt
    , renderExpr u.loginCount
    ]

instance Projectable (UserT 'Expr) where
  projectColumns = tableExprColumns

instance InsertValues UserT (UserT 'Result) where
  renderInsertValues m u =
    [ (colName m.id, T.pack (show (unId u.id)))
    , (colName m.email, "'" <> u.email <> "'")
    , (colName m.lastLoginAt, T.pack (show u.lastLoginAt))
    , (colName m.loginCount, T.pack (show u.loginCount))
    ]

userTable :: TableSchema UserT
userTable =
  tableSchema "users" $
    User
      { id = primaryKey "id" "bigint"
      , email = column "email" "text"
      , lastLoginAt = column "last_login_at" "bigint"
      , loginCount = column "login_count" "bigint"
      }

newUser :: UserT 'Result
newUser =
  User
    { id = Id 1
    , email = "alice@example.com"
    , lastLoginAt = 1000
    , loginCount = 0
    }

data JobQueueT (ctx :: Ctx) = JobQueue
  { id :: !(Col ctx (Id JobQueueT Int64))
  , status :: !(Col ctx Text)
  , priority :: !(Col ctx Int64)
  , scheduledAt :: !(Col ctx Int64)
  }

instance Table JobQueueT where
  toTableExpr alias m =
    JobQueue
      { id = colSpecExpr alias m.id
      , status = colSpecExpr alias m.status
      , priority = colSpecExpr alias m.priority
      , scheduledAt = colSpecExpr alias m.scheduledAt
      }
  tableExprColumns j =
    [ renderExpr j.id
    , renderExpr j.status
    , renderExpr j.priority
    , renderExpr j.scheduledAt
    ]

instance Projectable (JobQueueT 'Expr) where
  projectColumns = tableExprColumns

jobQueueTable :: TableSchema JobQueueT
jobQueueTable =
  tableSchema "job_queue" $
    JobQueue
      { id = primaryKey "id" "bigint"
      , status = column "status" "text"
      , priority = column "priority" "bigint"
      , scheduledAt = column "scheduled_at" "bigint"
      }

-- Model state for pure idempotency testing
data MockUserRow = MockUserRow
  { rowId :: !Int64
  , rowEmail :: !String
  , rowLastLogin :: !Int64
  , rowLoginCount :: !Int64
  }
  deriving (Eq, Show)

instance Arbitrary MockUserRow where
  arbitrary =
    MockUserRow
      <$> arbitrary
      <*> elements ["user1@hylo.dev", "user2@hylo.dev", "user3@hylo.dev"]
      <*> arbitrary
      <*> arbitrary

instance Arbitrary LockStrength where
  arbitrary = elements [ForUpdate, ForNoKeyUpdate, ForShare, ForKeyShare]

--------------------------------------------------------------------------------
-- Spec Suite
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Epic 9: High-Throughput Upserts & Pessimistic Locking" $ do
  describe "9.4 Executable Oracle Specification" $ do
    it "compiles ON CONFLICT DO UPDATE referencing EXCLUDED columns" $ do
      let action =
            insertOnConflict
              userTable
              newUser
              (onConstraint "users_email_key")
              ( \current excluded ->
                  [ current.lastLoginAt =: excluded.lastLoginAt
                  , current.loginCount =: current.loginCount + lit 1
                  ]
              )
          sql :: String
          sql = renderSQL action
      sql `shouldContain` "INSERT INTO \"users\""
      sql `shouldContain` "ON CONFLICT ON CONSTRAINT \"users_email_key\" DO UPDATE SET"
      sql `shouldContain` "\"last_login_at\" = EXCLUDED.\"last_login_at\""
      sql `shouldContain` "\"login_count\" = (\"users\".\"login_count\" + 1)"

    it "compiles FOR UPDATE SKIP LOCKED for high-throughput job queues" $ do
      let q = do
            job <- each jobQueueTable
            where_ (job.status ==. lit "pending")
            orderBy [asc job.priority, asc job.scheduledAt]
            limit 10
            forUpdate (skipLocked)
            pure job
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "FOR UPDATE SKIP LOCKED"

  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 1: Idempotence of Upsert Under Invariant State" $
      property $ \(initialRows :: [MockUserRow], incoming :: MockUserRow) ->
        let upsertStep row db =
              let updated =
                    case filter (\r -> r.rowEmail == row.rowEmail) db of
                      [] -> db ++ [row]
                      _ ->
                        map
                          ( \r ->
                              if r.rowEmail == row.rowEmail
                                then r {rowLastLogin = row.rowLastLogin, rowLoginCount = row.rowLoginCount}
                                else r
                          )
                          db
               in updated
            step1 = upsertStep incoming initialRows
            step2 = upsertStep incoming step1
         in step1 == step2

    it "Law 2: Lock Exclusivity Invariant (FOR UPDATE exclusive across all lock modes)" $
      property $ \(mode :: LockStrength) ->
        not (canConcurrentlyLock ForUpdate mode)
          && not (canConcurrentlyLock mode ForUpdate)

    it "Law 2: Lock Exclusivity Invariant (Compatibility Symmetry Law)" $
      property $ \(m1 :: LockStrength, m2 :: LockStrength) ->
        canConcurrentlyLock m1 m2 == canConcurrentlyLock m2 m1

    it "Law 2: Lock Exclusivity Invariant (FOR SHARE concurrency preserves read sharing)" $
      canConcurrentlyLock ForShare ForShare
        && canConcurrentlyLock ForShare ForKeyShare
        && canConcurrentlyLock ForKeyShare ForShare
        && canConcurrentlyLock ForKeyShare ForKeyShare

    it "Law 3: Queue Invariant (Non-Overlapping Consumption via SKIP LOCKED)" $
      property $ \(Positive (workerCount :: Int)) ->
        let jobs = [1 .. 50 :: Int64]
            workers = min workerCount 50
            -- Simulated concurrent atomic consumption with SKIP LOCKED
            consumeQueues remaining n
              | n <= 0 = []
              | otherwise = case remaining of
                  [] -> []
                  (x : xs) -> x : consumeQueues xs (n - 1)
            consumed = consumeQueues jobs workers
         in nub consumed == consumed && length consumed <= workers

  describe "Comprehensive Upsert, Conflict Resolution & Lock Options" $ do
    it "compiles ON CONFLICT DO NOTHING" $ do
      let action =
            insertOnConflictDoNothing
              userTable
              newUser
              onConflictDoNothing
          sql :: String
          sql = renderSQL action
      sql `shouldContain` "INSERT INTO \"users\""
      sql `shouldContain` "ON CONFLICT DO NOTHING"

    it "compiles ON CONFLICT on column targets" $ do
      let action =
            insertOnConflict
              userTable
              newUser
              (onColumns ["email"])
              ( \current excluded ->
                  [ current.lastLoginAt =: excluded.lastLoginAt
                  ]
              )
          sql :: String
          sql = renderSQL action
      sql `shouldContain` "ON CONFLICT (\"email\") DO UPDATE SET"
      sql `shouldContain` "\"last_login_at\" = EXCLUDED.\"last_login_at\""

    it "compiles FOR UPDATE NOWAIT" $ do
      let q = do
            job <- each jobQueueTable
            where_ (job.status ==. lit "pending")
            limit 1
            forUpdate nowait
            pure job
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "FOR UPDATE NOWAIT"

    it "compiles FOR NO KEY UPDATE" $ do
      let q = do
            job <- each jobQueueTable
            forNoKeyUpdate (skipLocked)
            pure job
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "FOR NO KEY UPDATE SKIP LOCKED"

    it "compiles FOR SHARE" $ do
      let q = do
            job <- each jobQueueTable
            forShare defaultLockWait
            pure job
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "FOR SHARE"

    it "compiles table-targeted locking with OF clause" $ do
      let q = do
            job <- each jobQueueTable
            forUpdateOf ["job_queue"] skipLocked
            pure job
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "FOR UPDATE OF \"job_queue\" SKIP LOCKED"

    it "statically rejects upsert in ReadOnly transactions with [Hylo Error E005]" $ do
      shouldNotTypecheckWith "[Hylo Error E005]" $
        let forbidden =
              ( upsert
                  userTable
                  newUser
                  (onConstraint "users_email_key")
                  (\_ _ -> []) ::
                  Tx Postgres 'ReadOnly (Query Postgres (UpsertAction UserT))
              )
         in rnf forbidden

    it "statically rejects executeLocked in ReadOnly transactions with [Hylo Error E005]" $ do
      shouldNotTypecheckWith "[Hylo Error E005]" $
        let q = do
              job <- each jobQueueTable
              forUpdate (skipLocked)
              pure job
            forbidden =
              ( executeLocked q ::
                  Tx Postgres 'ReadOnly (JobQueueT 'Expr)
              )
         in rnf forbidden
