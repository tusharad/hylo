{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{- |
Module      : Hylo.PG.Mutation.ChangesetSpec
Description : Test suite for User Story 5: Changeset execution and constraint error mapping
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Mutation.ChangesetSpec (spec) where

import Data.Foldable (toList)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Core
import Hylo.Core.Query (PGVersion (..), Postgres, findOptimisticLockCol)
import Hylo.PG
import Hylo.Test.ShouldNotTypecheck (shouldNotTypecheckWith)
import Test.Hspec
import Test.QuickCheck

default (Text, Int64)

--------------------------------------------------------------------------------
-- 5-Column HKD Entity for Partial Updates & Codec Testing
--------------------------------------------------------------------------------

data AccountT (ctx :: Ctx) = Account
  { id :: !(Col ctx (Id AccountT Int64))
  , name :: !(Col ctx Text)
  , email :: !(Col ctx Text)
  , score :: !(Col ctx Int64)
  , active :: !(Col ctx Bool)
  }
  deriving (Generic)

deriving instance Eq (AccountT 'Result)
deriving instance Show (AccountT 'Result)
deriving instance Eq (AccountT 'Patch)
deriving instance Show (AccountT 'Patch)

type Account = AccountT 'Result

accountSchema :: TableSchema AccountT
accountSchema =
  tableSchema "accounts" $
    Account
      { id = primaryKey "id" "bigint"
      , name = column "name" "text"
      , email = column "email" "text"
      , score = column "score" "bigint"
      , active = column "active" "boolean"
      }

instance Table AccountT where
  toTableExpr alias meta =
    Account
      { id = colSpecExpr alias meta.id
      , name = colSpecExpr alias meta.name
      , email = colSpecExpr alias meta.email
      , score = colSpecExpr alias meta.score
      , active = colSpecExpr alias meta.active
      }
  diffTable old new =
    Account
      { id = diffField old.id new.id
      , name = diffField old.name new.name
      , email = diffField old.email new.email
      , score = diffField old.score new.score
      , active = diffField old.active new.active
      }
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.name
    , eraseColDef m.email
    , eraseColDef m.score
    , eraseColDef m.active
    ]
  patchEntries meta patch =
    concat
      [ patchEntry meta.id patch.id
      , patchEntry meta.name patch.name
      , patchEntry meta.email patch.email
      , patchEntry meta.score patch.score
      , patchEntry meta.active patch.active
      ]
  dirtyFieldsTable patch =
    Map.fromList
      [ (colName c, case v of ValSetTo t -> t; ValSetDefault -> "DEFAULT")
      | (c, v) <- patchEntries (tableMeta accountSchema) patch
      ]

accountCodec :: Codec Account Account
accountCodec =
  Account
    <$> field (.id) (idCodec int64Codec)
    <*> field (.name) textCodec
    <*> field (.email) textCodec
    <*> field (.score) int64Codec
    <*> field (.active) boolCodec

instance TableCodec AccountT where
  tableCodec = accountCodec

--------------------------------------------------------------------------------
-- Entity with non-matching Haskell field selector (userEmail) and ColDef ("email")
--------------------------------------------------------------------------------

data CustomUserT (ctx :: Ctx) = CustomUser
  { userId :: !(Col ctx (Id CustomUserT Int64))
  , userEmail :: !(Col ctx Text)
  }

customUserSchema :: TableSchema CustomUserT
customUserSchema =
  tableSchema "users" $
    CustomUser
      { userId = primaryKey "id" "bigint"
      , userEmail = column "email" "text"
      }

instance Table CustomUserT where
  toTableExpr alias meta =
    CustomUser
      { userId = colSpecExpr alias meta.userId
      , userEmail = colSpecExpr alias meta.userEmail
      }
  diffTable old new =
    CustomUser
      { userId = diffField old.userId new.userId
      , userEmail = diffField old.userEmail new.userEmail
      }
  tableColDefs m =
    [ eraseColDef m.userId
    , eraseColDef m.userEmail
    ]
  patchEntries meta patch =
    patchEntry meta.userId patch.userId
      ++ patchEntry meta.userEmail patch.userEmail
  dirtyFieldsTable patch =
    Map.fromList
      [ (colName c, case v of ValSetTo t -> t; ValSetDefault -> "DEFAULT")
      | (c, v) <- patchEntries (tableMeta customUserSchema) patch
      ]

aliceRecord :: Account
aliceRecord =
  Account
    { id = toId 1
    , name = "Alice"
    , email = "alice@example.com"
    , score = 100
    , active = True
    }

--------------------------------------------------------------------------------
-- Versioned Document HKD Entity for Optimistic Locking Tests
--------------------------------------------------------------------------------

data VersionedDocT (ctx :: Ctx) = VersionedDoc
  { docId :: !(Col ctx (Id VersionedDocT Int64))
  , title :: !(Col ctx Text)
  , version :: !(Col ctx Int64)
  }
  deriving (Generic)

deriving instance Eq (VersionedDocT 'Result)
deriving instance Show (VersionedDocT 'Result)
deriving instance Eq (VersionedDocT 'Patch)
deriving instance Show (VersionedDocT 'Patch)

type VersionedDoc = VersionedDocT 'Result

versionedDocSchema :: TableSchema VersionedDocT
versionedDocSchema =
  tableSchema "documents" $
    VersionedDoc
      { docId = primaryKey "id" "bigint"
      , title = column "title" "text"
      , version = optimisticLockColumn "version" "bigint"
      }

instance Table VersionedDocT where
  toTableExpr alias meta =
    VersionedDoc
      { docId = colSpecExpr alias meta.docId
      , title = colSpecExpr alias meta.title
      , version = colSpecExpr alias meta.version
      }
  diffTable old new =
    VersionedDoc
      { docId = diffField old.docId new.docId
      , title = diffField old.title new.title
      , version = diffField old.version new.version
      }
  tableColDefs m =
    [ eraseColDef m.docId
    , eraseColDef m.title
    , eraseColDef m.version
    ]
  patchEntries meta patch =
    concat
      [ patchEntry meta.docId patch.docId
      , patchEntry meta.title patch.title
      , patchEntry meta.version patch.version
      ]
  dirtyFieldsTable patch =
    Map.fromList
      [ (colName c, case v of ValSetTo t -> t; ValSetDefault -> "DEFAULT")
      | (c, v) <- patchEntries (tableMeta versionedDocSchema) patch
      ]
  tableOptimisticLock old = Just ("version", old.version)

versionedDocCodec :: Codec VersionedDoc VersionedDoc
versionedDocCodec =
  VersionedDoc
    <$> field (.docId) (idCodec int64Codec)
    <*> field (.title) textCodec
    <*> field (.version) int64Codec

instance TableCodec VersionedDocT where
  tableCodec = versionedDocCodec

--------------------------------------------------------------------------------
-- Test Specs
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "User Story 5: Wire Changeset into hylo-pg Binary Execution & Error Mapping" $ do
  describe "1. Partial UPDATE SQL Synthesis & Minimal Parameter Binding" $ do
    it
      "Acceptance Criterion 1: Updating 1 modified field on a 5-column table sets ONLY that column and binds 1 parameter (+ PK)"
      $ do
        let oldAcc = aliceRecord
            newAcc = oldAcc {email = "alice_new@example.com"}
            cs = diffRecord accountSchema oldAcc newAcc
            (sql, params) = renderUpdateChangesetSQL accountSchema cs

        -- Must set only the 1 modified column
        T.unpack sql `shouldContain` "UPDATE \"accounts\" SET \"email\" = $1 WHERE \"id\" = $2 RETURNING *"
        T.unpack sql `shouldNotContain` "\"name\" ="
        T.unpack sql `shouldNotContain` "\"score\" ="
        T.unpack sql `shouldNotContain` "\"active\" ="

        -- Binds only 1 parameter (+ primary key)
        length params `shouldBe` 2
        params `shouldBe` [BoundParam 1 "alice_new@example.com" "text", BoundParam 2 "1" "integer"]

    it "binds only modified fields when multiple fields change" $ do
      let oldAcc = aliceRecord
          newAcc = oldAcc {email = "alice_new@example.com", score = 250}
          cs = diffRecord accountSchema oldAcc newAcc
          (sql, params) = renderUpdateChangesetSQL accountSchema cs

      T.unpack sql
        `shouldContain` "UPDATE \"accounts\" SET \"email\" = $1, \"score\" = $2 WHERE \"id\" = $3 RETURNING *"
      length params `shouldBe` 3
      params
        `shouldBe` [ BoundParam 1 "alice_new@example.com" "text"
                   , BoundParam 2 "250" "integer"
                   , BoundParam 3 "1" "integer"
                   ]

    it "emits DEFAULT without binding parameters for fields marked SetDefault" $ do
      let patch =
            Account
              { id = Unchanged
              , name = Unchanged
              , email = SetTo "alice@reset.com"
              , score = SetDefault
              , active = Unchanged
              }
          cs = withUniqueKey "1" (change @'UpdateOp patch)
          (sql, params) = renderUpdateChangesetSQL accountSchema cs

      T.unpack sql `shouldContain` "\"email\" = $1"
      T.unpack sql `shouldContain` "\"score\" = DEFAULT"
      -- Only 1 param for email + 1 param for PK
      length params `shouldBe` 2
      params `shouldBe` [BoundParam 1 "alice@reset.com" "text", BoundParam 2 "1" "integer"]

    it "handles all Unchanged fields with self-assignment fallback" $ do
      let cs = diffRecord accountSchema aliceRecord aliceRecord
          (sql, params) = renderUpdateChangesetSQL accountSchema cs

      T.unpack sql `shouldContain` "UPDATE \"accounts\" SET \"id\" = \"id\" WHERE \"id\" = $1 RETURNING *"
      length params `shouldBe` 1

  describe "2. INSERT SQL Synthesis & RETURNING * Clause" $ do
    it "renders parameterized INSERT with RETURNING * for modified fields" $ do
      let patch =
            Account
              { id = SetTo (toId 2)
              , name = SetTo "Bob"
              , email = SetTo "bob@example.com"
              , score = SetTo 50
              , active = SetTo True
              }
          cs = change @'InsertOp patch
          (sql, params) = renderInsertChangesetSQL accountSchema cs

      T.unpack sql
        `shouldContain` "INSERT INTO \"accounts\" (\"id\", \"name\", \"email\", \"score\", \"active\") VALUES ($1, $2, $3, $4, $5) RETURNING *"
      length params `shouldBe` 5

    it "renders DEFAULT VALUES RETURNING * when all fields are Unchanged" $ do
      let patch =
            Account
              { id = Unchanged
              , name = Unchanged
              , email = Unchanged
              , score = Unchanged
              , active = Unchanged
              }
          cs = change @'InsertOp patch
          (sql, params) = renderInsertChangesetSQL accountSchema cs

      sql `shouldBe` "INSERT INTO \"accounts\" DEFAULT VALUES RETURNING *"
      params `shouldBe` []

  describe "3. Database Constraint Error Translation into Typed FieldErrors" $ do
    it
      "Acceptance Criterion 2: maps SQLSTATE 23505 unique_violation to structured FieldError matching registered constraint"
      $ do
        let patch =
              Account
                { id = Unchanged
                , name = Unchanged
                , email = SetTo "duplicate@example.com"
                , score = Unchanged
                , active = Unchanged
                }
            cs = uniqueConstraint "email" (change @'InsertOp patch)
            dbErr =
              mockSqlStateErrorFull
                "23505"
                "accounts_email_key"
                (Just "duplicate key")
                Nothing
                (Just "accounts")
                Nothing
            ChangesetError (err :| rest) = translatePgConstraintError accountSchema cs dbErr

        null rest `shouldBe` True
        err.fieldPath `shouldBe` "email"
        err.errorCode `shouldBe` "unique_violation"
        err.message `shouldBe` "has already been taken"

    it "maps named unique constraint violations accurately" $ do
      let patch =
            Account
              { id = Unchanged
              , name = Unchanged
              , email = SetTo "duplicate@example.com"
              , score = Unchanged
              , active = Unchanged
              }
          cs = uniqueConstraintNamed "email" "custom_email_unique_idx" (change @'InsertOp patch)
          dbErr =
            mockSqlStateErrorFull
              "23505"
              "custom_email_unique_idx"
              (Just "duplicate key")
              Nothing
              (Just "accounts")
              Nothing
          ChangesetError (err :| _) = translatePgConstraintError accountSchema cs dbErr

      err.fieldPath `shouldBe` "email"
      err.errorCode `shouldBe` "unique_violation"
      err.message `shouldBe` "has already been taken"

    it
      "maps SQLSTATE 23503 foreign_key_violation to structured FieldError matching registered constraint"
      $ do
        let patch =
              Account
                { id = Unchanged
                , name = Unchanged
                , email = Unchanged
                , score = SetTo 999
                , active = Unchanged
                }
            cs = foreignKeyConstraint "score" "levels" (change @'UpdateOp patch)
            dbErr =
              mockSqlStateErrorFull
                "23503"
                "accounts_score_fkey"
                (Just "foreign key violation")
                Nothing
                (Just "accounts")
                Nothing
            ChangesetError (err :| _) = translatePgConstraintError accountSchema cs dbErr

        err.fieldPath `shouldBe` "score"
        err.errorCode `shouldBe` "foreign_key_violation"
        err.message `shouldBe` "does not exist"

    it "maps PostgreSQL 18 SQLSTATE 23001 restrict_violation to structured FieldError" $ do
      let patch =
            Account
              { id = Unchanged
              , name = Unchanged
              , email = Unchanged
              , score = Unchanged
              , active = SetTo False
              }
          cs =
            foreignKeyConstraintNamed
              "active"
              "subscriptions"
              "accounts_active_restrict"
              (change @'UpdateOp patch)
          dbErr =
            mockSqlStateErrorFull
              "23001"
              "accounts_active_restrict"
              (Just "restrict violation")
              Nothing
              (Just "accounts")
              Nothing
          ChangesetError (err :| _) = translatePgConstraintError accountSchema cs dbErr

      err.fieldPath `shouldBe` "active"
      err.errorCode `shouldBe` "restrict_violation"

    it "translates low-level TxConstraintViolation without throwing unhandled exceptions" $ do
      let cs = uniqueConstraint "email" (change @'InsertOp (diffTable aliceRecord aliceRecord))
          txErr =
            TxConstraintViolation "23505" "accounts_email_key" "Key (email)=(alice@example.com) already exists."
          ChangesetError (err :| _) = translateTxConstraintViolation accountSchema cs txErr

      err.fieldPath `shouldBe` "email"
      err.errorCode `shouldBe` "unique_violation"
      err.message `shouldBe` "has already been taken"

  describe "4. Pre-Execution Validation Check" $ do
    it "returns all FieldErrors immediately without database execution when changeset is invalid" $ do
      let patch =
            Account
              { id = Unchanged
              , name = SetTo ""
              , email = SetTo "invalid-email"
              , score = Unchanged
              , active = Unchanged
              }
          csReq = validateRequired "name" (.name) (change @'InsertOp patch)
          cs = validateFormat "email" (.email) "^[^@]+@[^@]+$" csReq

      cs.changesetValid `shouldBe` False
      length cs.changesetErrors `shouldBe` 2

      let tx =
            insertChangeset accountSchema cs :: Tx (Postgres 'PG16) 'ReadWrite (Either ChangesetError Account)
          (res, st) = executeTx tx initialTxState

      case res of
        Right (Left (ChangesetError errs)) -> do
          length errs `shouldBe` 2
          map (\e -> e.fieldPath) (toList errs) `shouldBe` ["name", "email"]
        other -> expectationFailure ("Expected Left ChangesetError, got: " ++ show other)

      -- Verified zero database operations recorded
      txOperations st `shouldBe` []

  describe "5. RETURNING * Binary Wire Protocol Decoding" $ do
    it "decodes RETURNING * binary wire output via Hylo.PG.Codec.Binary" $ do
      let encodedAlice = runBinaryEncoder accountCodec aliceRecord
      decodeReturningRow accountCodec encodedAlice `shouldBe` Right aliceRecord

    it "executes updateChangeset with binary wire row payload and returns hydrated record" $ do
      let oldAcc = aliceRecord
          newAcc = oldAcc {email = "alice_new@example.com"}
          cs = diffRecord accountSchema oldAcc newAcc
          encodedNew = runBinaryEncoder accountCodec newAcc

      let tx =
            updateChangesetWithRow accountSchema accountCodec encodedNew cs ::
              Tx (Postgres 'PG16) 'ReadWrite (Either ChangesetError Account)
          (res, st) = executeTx tx initialTxState

      res `shouldBe` Right (Right newAcc)
      length (txOperations st) `shouldBe` 1

    it "catches binary wire decoding errors safely" $ do
      let cs = diffRecord accountSchema aliceRecord aliceRecord
          truncatedBytes = "short"
          tx =
            updateChangesetWithRow accountSchema accountCodec truncatedBytes cs ::
              Tx (Postgres 'PG16) 'ReadWrite (Either ChangesetError Account)
          (res, _) = executeTx tx initialTxState

      case res of
        Right (Left (ChangesetError (err :| _))) -> do
          err.errorCode `shouldBe` "decode_error"
        other -> expectationFailure ("Expected decode_error, got: " ++ show other)

  describe "6. ReadOnly Compile-Time Safety ([Hylo Error E005])" $ do
    it "statically rejects insertChangeset in ReadOnly mode with [Hylo Error E005]" $ do
      shouldNotTypecheckWith
        "[Hylo Error E005]"
        ( let invalidTx =
                insertChangeset accountSchema (change @'InsertOp (diffTable aliceRecord aliceRecord)) ::
                  Tx (Postgres 'PG16) 'ReadOnly (Either ChangesetError Account)
           in invalidTx
        )

    it "statically rejects updateChangeset in ReadOnly mode with [Hylo Error E005]" $ do
      shouldNotTypecheckWith
        "[Hylo Error E005]"
        ( let invalidTx =
                updateChangeset accountSchema (diffRecord accountSchema aliceRecord aliceRecord) ::
                  Tx (Postgres 'PG16) 'ReadOnly (Either ChangesetError Account)
           in invalidTx
        )

  describe "7. QuickCheck Invariants" $ do
    it "bound parameters index is strictly monotonic 1..N+1 on partial updates" $ do
      property $ \(Positive (scoreVal :: Int64)) ->
        scoreVal /= 100 ==>
          let newAcc = aliceRecord {score = scoreVal}
              cs = diffRecord accountSchema aliceRecord newAcc
              (_, params) = renderUpdateChangesetSQL accountSchema cs
           in map (\p -> p.paramIndex) params == [1, 2]

    it "parameter count matches modified fields count + 1 on updates" $ do
      property $ \(Positive (scoreVal :: Int64)) (activeVal :: Bool) ->
        let newAcc = aliceRecord {score = scoreVal, active = activeVal}
            cs = diffRecord accountSchema aliceRecord newAcc
            setCount = Map.size (Map.delete "id" (dirtyFields cs))
            (_, params) = renderUpdateChangesetSQL accountSchema cs
         in length params == setCount + 1

  describe "8. User Story 8: Acceptance Criteria" $ do
    it
      "Acceptance Criterion 1: Table with Haskell record field userEmail and ColDef name 'email' generates INSERT and UPDATE using 'email' without Generic constraints"
      $ do
        let csInsert = change @'InsertOp (CustomUser Unchanged (SetTo "alice@example.com"))
            (insertSql, insertParams) = renderInsertChangesetSQL customUserSchema csInsert
        insertSql `shouldBe` "INSERT INTO \"users\" (\"email\") VALUES ($1) RETURNING *"
        insertParams `shouldBe` [BoundParam 1 "alice@example.com" "text"]

        let csUpdate = change @'UpdateOp (CustomUser Unchanged (SetTo "alice@example.com"))
            (updateSql, updateParams) = renderUpdateChangesetSQL customUserSchema csUpdate
        updateSql `shouldBe` "UPDATE \"users\" SET \"email\" = $1 WHERE \"id\" = $2 RETURNING *"
        updateParams `shouldBe` [BoundParam 1 "alice@example.com" "text", BoundParam 2 "1" "integer"]

  describe "9. User Story 2: First-Class Optimistic Locking (E009)" $ do
    it "marks optimisticLockColumn with colIsOptimisticLock = True" $ do
      let cols = tableColDefs (tableMeta versionedDocSchema)
      [colName c | c <- cols, colIsOptimisticLock c] `shouldBe` ["version"]
      [colName c | c <- cols, not (colIsOptimisticLock c)] `shouldBe` ["id", "title"]

    it "findOptimisticLockCol finds version column from schema metadata" $ do
      findOptimisticLockCol versionedDocSchema `shouldBe` Just "version"
      findOptimisticLockCol customUserSchema `shouldBe` Nothing

    it "renderUpdateChangesetSQL emits \"version\" = \"version\" + 1 and AND \"version\" = $expected" $ do
      let cs =
            optimisticLock "version" 1
              $ change @'UpdateOp
              $ VersionedDoc
                { docId = SetTo (toId 10)
                , title = SetTo "Concurrency In Depth"
                , version = Unchanged
                }
          (sql, params) = renderUpdateChangesetSQL versionedDocSchema cs
      sql
        `shouldBe` "UPDATE \"documents\" SET \"title\" = $1, \"version\" = \"version\" + 1 WHERE \"id\" = $2 AND \"version\" = $3 RETURNING *"
      params
        `shouldBe` [ BoundParam 1 "Concurrency In Depth" "text"
                   , BoundParam 2 "10" "integer"
                   , BoundParam 3 "1" "bigint"
                   ]

    it "renderUpdateChangesetSQL emits \"version\" = \"version\" + 1 when no other fields are modified" $ do
      let cs =
            optimisticLock "version" 3
              $ change @'UpdateOp
              $ VersionedDoc
                { docId = SetTo (toId 5)
                , title = Unchanged
                , version = Unchanged
                }
          (sql, params) = renderUpdateChangesetSQL versionedDocSchema cs
      sql
        `shouldBe` "UPDATE \"documents\" SET \"version\" = \"version\" + 1 WHERE \"id\" = $1 AND \"version\" = $2 RETURNING *"
      params
        `shouldBe` [ BoundParam 1 "5" "integer"
                   , BoundParam 2 "3" "bigint"
                   ]

    it "renderUpdateChangesetSQL infers column name via optimisticLockVersion" $ do
      let cs =
            optimisticLockVersion 9
              $ change @'UpdateOp
              $ VersionedDoc
                { docId = SetTo (toId 20)
                , title = SetTo "Auto-Inferred"
                , version = Unchanged
                }
          (sql, params) = renderUpdateChangesetSQL versionedDocSchema cs
      sql
        `shouldBe` "UPDATE \"documents\" SET \"title\" = $1, \"version\" = \"version\" + 1 WHERE \"id\" = $2 AND \"version\" = $3 RETURNING *"
      params
        `shouldBe` [ BoundParam 1 "Auto-Inferred" "text"
                   , BoundParam 2 "20" "integer"
                   , BoundParam 3 "9" "bigint"
                   ]

    it "diffRecord automatically populates changesetOptimisticLock via tableOptimisticLock" $ do
      let oldDoc = VersionedDoc (toId 1) "Initial" 4
          newDoc = oldDoc {title = "Modified"}
          cs = diffRecord versionedDocSchema oldDoc newDoc
      changesetOptimisticLock cs `shouldBe` Just ("version", 4)
      let (sql, params) = renderUpdateChangesetSQL versionedDocSchema cs
      sql
        `shouldBe` "UPDATE \"documents\" SET \"title\" = $1, \"version\" = \"version\" + 1 WHERE \"id\" = $2 AND \"version\" = $3 RETURNING *"
      params
        `shouldBe` [ BoundParam 1 "Modified" "text"
                   , BoundParam 2 "1" "integer"
                   , BoundParam 3 "4" "bigint"
                   ]

    it "updateChangesetWithFallback rejects stale version with stale_object error" $ do
      let doc1 = VersionedDoc (toId 1) "Initial" 1
          csStale =
            withUniqueKey "sim_stale"
              $ optimisticLock "version" 1
              $ change @'UpdateOp
              $ VersionedDoc (SetTo (toId 1)) (SetTo "Conflict") Unchanged
          tx =
            updateChangesetWithFallback versionedDocSchema doc1 csStale ::
              Tx (Postgres 'PG16) 'ReadWrite (Either ChangesetError VersionedDoc)
          res = runTxPure tx
      case res of
        Right (Left err) ->
          changesetErrorFields err
            `shouldBe` (FieldError "version" "stale_object" "Record was modified by another transaction" :| [])
        _ -> expectationFailure ("Expected Left stale_object, got: " ++ show res)

    it "updateChangesetWithFallback detects successive modification conflict on identical entity" $ do
      let doc2 = VersionedDoc (toId 1) "V2" 2
          cs1 =
            withUniqueKey "doc_1"
              $ optimisticLock "version" 1
              $ change @'UpdateOp
              $ VersionedDoc (SetTo (toId 1)) (SetTo "V2") Unchanged
          cs2 =
            withUniqueKey "doc_1"
              $ optimisticLock "version" 1
              $ change @'UpdateOp
              $ VersionedDoc (SetTo (toId 1)) (SetTo "V2_Conflict") Unchanged
          txCompound ::
            Tx
              (Postgres 'PG16)
              'ReadWrite
              (Either ChangesetError VersionedDoc, Either ChangesetError VersionedDoc)
          txCompound = do
            res1 <- updateChangesetWithFallback versionedDocSchema doc2 cs1
            res2 <- updateChangesetWithFallback versionedDocSchema doc2 cs2
            pure (res1, res2)
          res = runTxPure txCompound
      case res of
        Right (Right _, Left err) ->
          changesetErrorFields err
            `shouldBe` (FieldError "version" "stale_object" "Record was modified by another transaction" :| [])
        _ -> expectationFailure ("Expected first update success, second failure, got: " ++ show res)
