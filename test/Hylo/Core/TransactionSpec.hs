{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{-# HLINT ignore "Avoid restricted function" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant pure" #-}

{- |
Module      : Hylo.Core.TransactionSpec
Description : Test suite for Epic 11: Lexically Isolated Tx Monad, Phantom Modes & Savepoints
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.TransactionSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Either (isRight)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core
import Hylo.Core.Query
  ( Postgres
  , Projectable (..)
  , each
  , insert
  , renderExpr
  , select
  )
import Hylo.PG.Connection (pool, runTx)
import Hylo.PG.Transaction.Savepoint
  ( allocateSavepoint
  , renderReleaseSavepoint
  , renderRollbackSavepoint
  , renderSavepoint
  )
import Hylo.Test.ShouldNotTypecheck (shouldNotTypecheckWith)
import Test.Hspec
import Test.QuickCheck

default (Text, Int64, Double)

-- | Test Table: User entity for transaction mode and savepoint testing.
data UserT (ctx :: Ctx) = User
  { id :: !(Col ctx (Id UserT Int64))
  , email :: !(Col ctx Text)
  }

instance Table UserT where
  toTableExpr alias m =
    User
      { id = colSpecExpr alias m.id
      , email = colSpecExpr alias m.email
      }
  tableExprColumns u =
    [ renderExpr u.id
    , renderExpr u.email
    ]

instance Projectable (UserT 'Expr) where
  projectColumns = tableExprColumns

userTable :: TableSchema UserT
userTable =
  tableSchema "users" $
    User
      { id = primaryKey "id" "bigint"
      , email = column "email" "text"
      }

-- | Construct a valid user changeset.
validUser :: Text -> Changeset UserT 'InsertOp
validUser name =
  withUniqueKey name $
    emptyChangeset
      User
        { id = SetTo (toId 1)
        , email = SetTo (name <> "@example.com")
        }

-- | Construct a duplicate user changeset conflicting on unique key.
duplicateUser :: Text -> Changeset UserT 'InsertOp
duplicateUser name =
  withUniqueKey name $
    emptyChangeset
      User
        { id = SetTo (toId 2)
        , email = SetTo (name <> "@example.com")
        }

spec :: Spec
spec = describe "Epic 11: Lexically Isolated Tx Monad & Savepoints" $ do
  describe "11.4 Executable Oracle Specification" $ do
    it "rejects liftIO inside Tx blocks at compile time" $
      shouldNotTypecheckWith
        "[Hylo Error E006]"
        ( let invalidTx = do
                _ <- select (each userTable)
                liftIO (putStrLn "Sending webhook...")
           in (invalidTx :: Tx Postgres 'ReadWrite ())
        )

    it "recovers from nested sub-transaction failure via withSavepoint" $ do
      let tx :: Tx Postgres 'ReadWrite (UserT 'Result)
          tx = do
            _ <- insert userTable (validUser "alice")
            _ <- withSavepoint $ do
              insert userTable (duplicateUser "alice")
            insert userTable (validUser "bob")
      result <- runTx pool ReadWrite tx
      result `shouldSatisfy` isRight

  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 1: Effect Purity of Transaction Blocks (Reject raw IO in ReadOnly)" $
      shouldNotTypecheckWith
        "[Hylo Error E006]"
        ( let invalidTx = do
                liftIO (putStrLn "Arbitrary IO in ReadOnly")
           in (invalidTx :: Tx Postgres 'ReadOnly ())
        )

    it "Law 2: Compile-Time Read-Only Immutability (Reject insert in ReadOnly)" $
      shouldNotTypecheckWith
        "[Hylo Error E005]"
        ( let invalidTx = insert userTable (validUser "alice")
           in (invalidTx :: Tx Postgres 'ReadOnly (UserT 'Result))
        )

    it "Law 2: Compile-Time Read-Only Immutability (Permit select in ReadOnly)" $ do
      let validReadOnlyTx :: Tx Postgres 'ReadOnly (UserT 'Expr)
          validReadOnlyTx = select (each userTable)
      res <- runTx pool ReadOnly validReadOnlyTx
      res `shouldSatisfy` isRight

    it "Law 3: Savepoint Rollback Isolation (mutate_1 >> fail) >> mutate_2 == mutate_2" $
      property $ \(Positive (n1 :: Int)) (Positive (n2 :: Int)) ->
        let k1 = "key_fail_" <> T.pack (show n1)
            k2 = "key_success_" <> T.pack (show n2)
            tx :: Tx Postgres 'ReadWrite (UserT 'Result)
            tx = do
              _ <- withSavepoint $ do
                _ <- insert userTable (validUser k1)
                txFail (TxUserError "Simulated failure inside savepoint")
              insert userTable (validUser k2)
            (_, finalState) = executeTx tx initialTxState
            storedKeys = map snd (txStore finalState)
         in (k1 `notElem` storedKeys) .&&. (k2 `elem` storedKeys)

    it "Law 3: Monotonic Savepoint Sequence Invariant" $
      property $ \(Positive (n :: Int)) ->
        let count = (n `mod` 10) + 1
            createSavepoints 0 = pure ()
            createSavepoints i = do
              _ <- withSavepoint (pure () :: Tx Postgres 'ReadWrite ())
              createSavepoints (i - 1)
            (_, finalState) = executeTx (createSavepoints count :: Tx Postgres 'ReadWrite ()) initialTxState
         in txSavepointSeq finalState === count

  describe "Comprehensive Savepoint & Transactional Combinators" $ do
    it "returns Right on successful savepoint computation and records RELEASE" $ do
      let tx :: Tx Postgres 'ReadWrite (Either TxError (UserT 'Result))
          tx = withSavepoint (insert userTable (validUser "charlie"))
          (res, st) = executeTx tx initialTxState
      res `shouldSatisfy` isRight
      any
        ( \case
            OpReleaseSavepoint 0 "svp_0" -> True
            _ -> False
        )
        (txOperations st)
        `shouldBe` True

    it "returns Left on failed savepoint computation and records ROLLBACK" $ do
      let tx :: Tx Postgres 'ReadWrite (Either TxError ())
          tx = withSavepoint (txFail (TxUserError "intentional failure"))
          (res, st) = executeTx tx initialTxState
      case res of
        Right (Left (TxUserError "intentional failure")) -> True `shouldBe` True
        _ -> expectationFailure ("Expected sub-transaction failure, got: " ++ show res)
      any
        ( \case
            OpRollbackSavepoint 0 "svp_0" -> True
            _ -> False
        )
        (txOperations st)
        `shouldBe` True

    it "supports nested savepoints with localized recovery" $ do
      let tx :: Tx Postgres 'ReadWrite (UserT 'Result)
          tx = do
            _ <- insert userTable (validUser "level_0")
            _ <- withSavepoint $ do
              _ <- insert userTable (validUser "level_1")
              _ <- withSavepoint $ do
                _ <- insert userTable (validUser "level_2_fail")
                txFail (TxUserError "level 2 failed")
              insert userTable (validUser "level_1_committed")
            insert userTable (validUser "level_0_committed")
          (res, st) = executeTx tx initialTxState
          keys = reverse (map snd (txStore st))
      res `shouldSatisfy` isRight
      keys `shouldContain` ["level_0", "level_1", "level_1_committed", "level_0_committed"]
      keys `shouldNotContain` ["level_2_fail"]

    it "renders standard PostgreSQL savepoint commands" $ do
      let (sp, nextSeq) = allocateSavepoint 0
      renderSavepoint sp `shouldBe` "SAVEPOINT \"svp_0\""
      renderReleaseSavepoint sp `shouldBe` "RELEASE SAVEPOINT \"svp_0\""
      renderRollbackSavepoint sp `shouldBe` "ROLLBACK TO SAVEPOINT \"svp_0\""
      nextSeq `shouldBe` 1
