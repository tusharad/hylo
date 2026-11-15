{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{-# HLINT ignore "Avoid restricted function" #-}

{- |
Module      : Hylo.PG.Mutation.BulkSpec
Description : Test suite for Epic 14: Bulk Operations, Binary Array UNNEST Ingestion & DB Constraint Translation
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Mutation.BulkSpec (spec) where

import Control.DeepSeq (rnf)
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core.Context
  ( Col
  , ColDef (..)
  , Ctx (..)
  , PatchField (..)
  )
import Hylo.Core.Key (Id (..), toId)
import Hylo.Core.Query
  ( Changeset (..)
  , OpKind (..)
  , Postgres
  , delete
  , emptyChangeset
  , insert
  , withUniqueKey
  )
import Hylo.Core.Table
  ( Table (..)
  , TableSchema (..)
  , colSpecExpr
  , column
  , eraseColDef
  , patchEntry
  , primaryKey
  , tableSchema
  )
import Hylo.Core.Transaction
  ( BoundParam (..)
  , Tx (..)
  , TxMode (..)
  , TxOp (..)
  , TxState (..)
  , executeTx
  , initialTxState
  )
import Hylo.PG.Error.Translation
  ( ConstraintRule (..)
  , FieldError (..)
  , extractColumnFromConstraint
  , mockSqlStateError
  , mockSqlStateErrorFull
  , translateDbError
  , translateDbErrorEither
  , translateDbErrorWith
  )
import Hylo.PG.Mutation.Bulk
  ( BulkInsertable (..)
  , bulkInsertParamCount
  , chunkList
  , chunkedInsertMany
  , formatPgArrayElement
  , insertMany
  , insertMany_
  , renderBulkInsertChangesets
  , renderBulkInsertSQL
  )
import Hylo.SQL.AST (renderExpr)
import Hylo.Test.ShouldNotTypecheck (shouldNotTypecheckWith)
import Test.Hspec
import Test.QuickCheck

default (Text, Int64, Double)

-- | HKD Table: Document entity for bulk UNNEST ingestion tests.
data DocumentT (ctx :: Ctx) = Document
  { id :: !(Col ctx (Id DocumentT Int64))
  , title :: !(Col ctx Text)
  , createdAt :: !(Col ctx Text)
  }

instance Table DocumentT where
  toTableExpr alias m =
    Document
      { id = colSpecExpr alias m.id
      , title = colSpecExpr alias m.title
      , createdAt = colSpecExpr alias m.createdAt
      }
  tableExprColumns d =
    [ renderExpr d.id
    , renderExpr d.title
    , renderExpr d.createdAt
    ]
  tableColumnsMeta m =
    [ (colName m.id, colSqlType m.id)
    , (colName m.title, colSqlType m.title)
    , (colName m.createdAt, colSqlType m.createdAt)
    ]
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.title
    , eraseColDef m.createdAt
    ]
  patchEntries m p =
    patchEntry m.id p.id
      ++ patchEntry m.title p.title
      ++ patchEntry m.createdAt p.createdAt

instance BulkInsertable DocumentT

docTable :: TableSchema DocumentT
docTable =
  tableSchema "documents" $
    Document
      { id = primaryKey "id" "bigint"
      , title = column "title" "text"
      , createdAt = column "created_at" "timestamptz"
      }

validDocumentChangeset :: Changeset DocumentT 'InsertOp
validDocumentChangeset =
  withUniqueKey "doc-1" $
    emptyChangeset
      Document
        { id = SetTo (Id 1)
        , title = SetTo "Architecture Spec"
        , createdAt = SetTo "2026-01-01 00:00:00Z"
        }

-- | HKD Table: User entity for constraint translation tests.
data UserT (ctx :: Ctx) = User
  { id :: !(Col ctx (Id UserT Int64))
  , email :: !(Col ctx Text)
  , name :: !(Col ctx Text)
  }

instance Table UserT where
  toTableExpr alias m =
    User
      { id = colSpecExpr alias m.id
      , email = colSpecExpr alias m.email
      , name = colSpecExpr alias m.name
      }
  tableExprColumns u =
    [ renderExpr u.id
    , renderExpr u.email
    , renderExpr u.name
    ]
  tableColumnsMeta m =
    [ (colName m.id, colSqlType m.id)
    , (colName m.email, colSqlType m.email)
    , (colName m.name, colSqlType m.name)
    ]
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.email
    , eraseColDef m.name
    ]
  patchEntries m p =
    patchEntry m.id p.id
      ++ patchEntry m.email p.email
      ++ patchEntry m.name p.name

instance BulkInsertable UserT

userTable :: TableSchema UserT
userTable =
  tableSchema "users" $
    User
      { id = primaryKey "id" "bigint"
      , email = column "email" "text"
      , name = column "name" "text"
      }

-- | 2-column HKD Table for User Story 16 acceptance test.
data SimpleUserT (ctx :: Ctx) = SimpleUser
  { id :: !(Col ctx (Id SimpleUserT Int64))
  , email :: !(Col ctx Text)
  }

instance Table SimpleUserT where
  toTableExpr alias m =
    SimpleUser
      { id = colSpecExpr alias m.id
      , email = colSpecExpr alias m.email
      }
  tableExprColumns u =
    [ renderExpr u.id
    , renderExpr u.email
    ]
  tableColumnsMeta m =
    [ (colName m.id, colSqlType m.id)
    , (colName m.email, colSqlType m.email)
    ]
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.email
    ]
  patchEntries m p =
    patchEntry m.id p.id
      ++ patchEntry m.email p.email

instance BulkInsertable SimpleUserT

userSchema :: TableSchema SimpleUserT
userSchema =
  tableSchema "users" $
    SimpleUser
      { id = primaryKey "id" "integer"
      , email = column "email" "text"
      }

-- | Helper to create a document changeset with a specific key.
makeDocChangeset :: Text -> Changeset DocumentT 'InsertOp
makeDocChangeset k =
  withUniqueKey k $
    emptyChangeset
      Document
        { id = SetTo (Id 1)
        , title = SetTo ("Doc " <> k)
        , createdAt = SetTo "2026-01-01 00:00:00Z"
        }

spec :: Spec
spec = describe "Epic 14: Bulk UNNEST Ingestion & Constraint Translation" $ do
  describe "14.4 Executable Oracle Specification" $ do
    it "compiles insertMany using binary UNNEST arrays regardless of batch size" $ do
      let records = replicate 10000 validDocumentChangeset
          sql = renderBulkInsertSQL docTable records
      sql `shouldContain` "SELECT * FROM UNNEST($1::bigint[], $2::text[], $3::timestamptz[])"

    it "translates SQLSTATE 23505 into a structured FieldError" $ do
      let pgError = mockSqlStateError "23505" "users_email_key"
          translated = translateDbError userTable pgError
      translated
        `shouldBe` Just (FieldError "email" "unique_violation" "A record with this email already exists")

  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 1: Bulk Ingestion Equivalence Law (FinalState equivalence on valid keys)" $ do
      property $ \(Positive (n :: Int)) -> do
        let count = min n 25
            rawKeys = map (\i -> "doc-" <> T.pack (show i)) [1 .. count]
            records = map makeDocChangeset rawKeys
            bulkTx = insertMany docTable records :: Tx Postgres 'ReadWrite [DocumentT 'Result]
            seqTx = traverse (insert docTable) records :: Tx Postgres 'ReadWrite [DocumentT 'Result]
            (resBulk, stBulk) = executeTx bulkTx initialTxState
            (resSeq, stSeq) = executeTx seqTx initialTxState
        isRight resBulk `shouldBe` True
        isRight resSeq `shouldBe` True
        txStore stBulk `shouldBe` txStore stSeq

    it "Law 1: Bulk Ingestion Equivalence Law (Mutual failure on duplicate keys)" $ do
      property $ \(Positive (n :: Int)) -> do
        let count = min n 10
            keys = map (\i -> "dup-" <> T.pack (show i)) [1 .. count] ++ ["dup-1"]
            records = map makeDocChangeset keys
            bulkTx = insertMany docTable records :: Tx Postgres 'ReadWrite [DocumentT 'Result]
            seqTx = traverse (insert docTable) records :: Tx Postgres 'ReadWrite [DocumentT 'Result]
            (resBulk, _) = executeTx bulkTx initialTxState
            (resSeq, _) = executeTx seqTx initialTxState
        isLeft resBulk `shouldBe` True
        isLeft resSeq `shouldBe` True

    it "Law 2: Parameter Independence Invariant (ParamCount(N, C) == C for all N)" $ do
      property $ \(NonNegative (n :: Int)) -> do
        let boundedN = min n 500
            records = replicate boundedN validDocumentChangeset
            paramCount = bulkInsertParamCount docTable records
        paramCount `shouldBe` 3

    it "Law 3: Constraint Error Invariance (All target SQLSTATEs translate to structured FieldError)" $ do
      property $ \(codeIdx :: Int) -> do
        let targetCodes = ["23505", "23503", "23514", "22001", "23502"]
            selectedCode = targetCodes !! (abs codeIdx `mod` length targetCodes)
            err = mockSqlStateError selectedCode "users_email_key"
            mTrans = translateDbError userTable err
            eitherTrans = translateDbErrorEither userTable err :: Either FieldError ()
        isJust mTrans `shouldBe` True
        isLeft eitherTrans `shouldBe` True

  describe "Comprehensive Bulk UNNEST Ingestion & Invariant Combinators" $ do
    it "renders the exact expected bulk INSERT statement with UNNEST parameters" $ do
      let sql = renderBulkInsertSQL docTable [validDocumentChangeset]
      sql
        `shouldBe` "INSERT INTO \"documents\" (\"id\", \"title\", \"created_at\") SELECT * FROM UNNEST($1::bigint[], $2::text[], $3::timestamptz[])"

    it "statically rejects insertMany in ReadOnly transactions with [Hylo Error E005]" $ do
      shouldNotTypecheckWith "[Hylo Error E005]" $
        let forbidden =
              ( insertMany
                  docTable
                  [validDocumentChangeset] ::
                  Tx Postgres 'ReadOnly [DocumentT 'Result]
              )
         in rnf forbidden

    it "discards hydrated results using insertMany_" $ do
      let tx = insertMany_ docTable [validDocumentChangeset] :: Tx Postgres 'ReadWrite ()
          (res, st) = executeTx tx initialTxState
      isRight res `shouldBe` True
      txStore st `shouldBe` [("documents", "doc-1")]

    it "supports chunkedInsertMany to partition large batches into sub-batches" $ do
      let keys = map (\i -> "chunk-" <> T.pack (show i)) [1 .. 50 :: Int]
          records = map makeDocChangeset keys
          tx = chunkedInsertMany 10 docTable records :: Tx Postgres 'ReadWrite [DocumentT 'Result]
          (res, st) = executeTx tx initialTxState
      case res of
        Right hydrated -> length hydrated `shouldBe` 50
        Left err -> expectationFailure ("Expected success but got: " ++ show err)
      length (txStore st) `shouldBe` 50
      length (txOperations st) `shouldBe` 5

    it "correctly chunks lists using chunkList" $ do
      chunkList 3 [1 .. 10 :: Int] `shouldBe` [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10]]
      chunkList 0 [1, 2, 3 :: Int] `shouldBe` [[1, 2, 3]]
      chunkList 5 ([] :: [Int]) `shouldBe` []

  describe "Comprehensive Database Constraint Error Translation" $ do
    it "translates foreign key violation (SQLSTATE 23503)" $ do
      let err = mockSqlStateError "23503" "orders_customer_id_fkey"
      case translateDbError userTable err of
        Just fe -> do
          fe.fieldPath `shouldBe` "customer_id"
          fe.errorCode `shouldBe` "foreign_key_violation"
          fe.message `shouldBe` "Referenced customer_id does not exist"
        Nothing -> expectationFailure "Expected translation but got Nothing"

    it "translates check constraint violation (SQLSTATE 23514)" $ do
      let err = mockSqlStateError "23514" "products_price_check"
      case translateDbError userTable err of
        Just fe -> do
          fe.fieldPath `shouldBe` "price"
          fe.errorCode `shouldBe` "check_violation"
          fe.message `shouldBe` "Value violates check constraint for price"
        Nothing -> expectationFailure "Expected translation but got Nothing"

    it "translates string data right truncation / overflow (SQLSTATE 22001)" $ do
      let err = mockSqlStateError "22001" "users_name_key"
      case translateDbError userTable err of
        Just fe -> do
          fe.fieldPath `shouldBe` "name"
          fe.errorCode `shouldBe` "string_overflow"
          fe.message `shouldBe` "Value exceeds maximum allowed length for name"
        Nothing -> expectationFailure "Expected translation but got Nothing"

    it "translates not-null violation (SQLSTATE 23502)" $ do
      let err = mockSqlStateError "23502" "users_email_key"
      case translateDbError userTable err of
        Just fe -> do
          fe.fieldPath `shouldBe` "email"
          fe.errorCode `shouldBe` "not_null_violation"
          fe.message `shouldBe` "Column email cannot be null"
        Nothing -> expectationFailure "Expected translation but got Nothing"

    it "supports custom ConstraintRule definitions via translateDbErrorWith" $ do
      let customRules =
            [ ConstraintRule
                "custom_email_uniq"
                "email"
                "duplicate_email"
                "This email address is already in use by another account"
            ]
          err = mockSqlStateError "23505" "custom_email_uniq"
      case translateDbErrorWith customRules userTable err of
        Just fe -> do
          fe.fieldPath `shouldBe` "email"
          fe.errorCode `shouldBe` "duplicate_email"
          fe.message `shouldBe` "This email address is already in use by another account"
        Nothing -> expectationFailure "Expected translation but got Nothing"

    it "prefers explicit dbColumn over constraint name heuristics when present" $ do
      let err =
            mockSqlStateErrorFull
              "23505"
              "obscure_constraint_9874"
              Nothing
              Nothing
              Nothing
              (Just "email")
      case translateDbError userTable err of
        Just fe -> fe.fieldPath `shouldBe` "email"
        Nothing -> expectationFailure "Expected translation but got Nothing"

    it "returns Nothing for unknown SQLSTATE codes" $ do
      let err = mockSqlStateError "99999" "unknown_constraint"
      translateDbError userTable err `shouldBe` Nothing

    it "extracts column names correctly across various naming conventions" $ do
      extractColumnFromConstraint "users" "users_email_key" `shouldBe` "email"
      extractColumnFromConstraint "users" "users_profile_id_fkey" `shouldBe` "profile_id"
      extractColumnFromConstraint "products" "products_price_check" `shouldBe` "price"
      extractColumnFromConstraint "accounts" "accounts_slug_unique" `shouldBe` "slug"
      extractColumnFromConstraint "orders" "status_check" `shouldBe` "status"

  describe
    "User Story 16: Real Columnar Parameter Binding in Bulk.insertMany & Parameterized Mutations"
    $ do
      it
        "Acceptance Criterion 1: Calling insertMany with 3 changesets on a 2-column table records an OpMutate with 2 array BoundParams (each containing 3 elements)"
        $ do
          let cs1 =
                withUniqueKey "u1" $
                  emptyChangeset SimpleUser {id = SetTo (toId 1), email = SetTo "alice@example.com"}
              cs2 =
                withUniqueKey "u2" $
                  emptyChangeset SimpleUser {id = SetTo (toId 2), email = SetTo "bob@example.com"}
              cs3 =
                withUniqueKey "u3" $
                  emptyChangeset SimpleUser {id = SetTo (toId 3), email = SetTo "charlie@example.com"}
              tx = insertMany userSchema [cs1, cs2, cs3] :: Tx Postgres 'ReadWrite [SimpleUserT 'Result]
              (res, st) = executeTx tx initialTxState
          isRight res `shouldBe` True
          case txOperations st of
            [OpMutate (sql, params)] -> do
              T.unpack sql
                `shouldContain` "INSERT INTO \"users\" (\"id\", \"email\") SELECT * FROM UNNEST($1::integer[], $2::text[])"
              length params `shouldBe` 2
              params
                `shouldBe` [ BoundParam 1 "{1,2,3}" "integer[]"
                           , BoundParam 2 "{alice@example.com,bob@example.com,charlie@example.com}" "text[]"
                           ]
            ops ->
              expectationFailure
                ("Expected [OpMutate (sql, params)], but got: " ++ show (length ops) ++ " operations")

      it
        "Acceptance Criterion 2: Calling delete userSchema (toId 42) records OpMutate (\"DELETE FROM \\\"users\\\" WHERE \\\"id\\\" = $1\", [BoundParam 1 \"42\" \"integer\"])"
        $ do
          let tx = delete userSchema (toId 42) :: Tx Postgres 'ReadWrite ()
              (res, st) = executeTx tx initialTxState
          isRight res `shouldBe` True
          txOperations st
            `shouldBe` [OpMutate ("DELETE FROM \"users\" WHERE \"id\" = $1", [BoundParam 1 "42" "integer"])]

      it "formats special characters in array elements correctly (quotes, spaces, commas)" $ do
        let cs1 =
              withUniqueKey "d1" $
                emptyChangeset
                  Document
                    { id = SetTo (toId 1)
                    , title = SetTo "Doc, with comma"
                    , createdAt = SetTo "2026-01-01 00:00:00Z"
                    }
            cs2 =
              withUniqueKey "d2" $
                emptyChangeset
                  Document
                    { id = SetTo (toId 2)
                    , title = SetTo "Doc \"quoted\""
                    , createdAt = SetTo "2026-01-02 00:00:00Z"
                    }
            (sql, params) = renderBulkInsertChangesets docTable [cs1, cs2]
        T.unpack sql `shouldContain` "SELECT * FROM UNNEST"
        length params `shouldBe` 3
        case params of
          [p1, p2, p3] -> do
            paramValue p1 `shouldBe` "{1,2}"
            paramValue p2 `shouldBe` "{\"Doc, with comma\",\"Doc \\\"quoted\\\"\"}"
            paramType p2 `shouldBe` "text[]"
            paramType p3 `shouldBe` "timestamptz[]"
          _ -> expectationFailure "Expected 3 params"

      it "formats individual array elements via formatPgArrayElement" $ do
        formatPgArrayElement "hello" `shouldBe` "hello"
        formatPgArrayElement "hello, world" `shouldBe` "\"hello, world\""
        formatPgArrayElement "quote\"test" `shouldBe` "\"quote\\\"test\""
        formatPgArrayElement "" `shouldBe` "\"\""
        formatPgArrayElement "NULL" `shouldBe` "NULL"
