{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}

{- |
Module      : Hylo.Core.TableSpec
Description : Formal specification test suite for Epic 1: HKD Schema Algebra
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.TableSpec (spec) where

import Data.Int (Int64)
import Data.Text (Text)
import Hylo.Core
import Test.Hspec
import Test.QuickCheck

-- | Test Table A: User entity with various column types
data UserT (ctx :: Ctx) = User
  { id :: !(Col ctx (Id UserT Int64))
  , email :: !(Col ctx Text)
  , age :: !(Col ctx Int)
  , note :: !(Col ctx (Maybe Text))
  }

type User = UserT 'Result
type UserExpr = UserT 'Expr
type UserPatch = UserT 'Patch
type UserMeta = UserT 'Meta

-- | Test Table B: Organization entity with distinct phantom ID
data OrgT (ctx :: Ctx) = Org
  { id :: !(Col ctx (Id OrgT Int64))
  , name :: !(Col ctx Text)
  }

type Org = OrgT 'Result

-- | Schema definition for UserT
userSchema :: TableSchema UserT
userSchema =
  tableSchema "users" $
    User
      { id = primaryKey "id" "bigint"
      , email = column "email" "text" & describeCol "Unique user email address"
      , age = column "age" "integer"
      , note = nullableColumn "note" "text"
      }
  where
    (&) x f = f x

spec :: Spec
spec = describe "Epic 1: HKD Relational Schema Algebra & Dual Record Dot Projection" $ do
  describe "1. 'Result Context: Hydrated Domain Values" $ do
    it "projects plain Haskell values using standard OverloadedRecordDot" $ do
      let user :: User
          user =
            User
              { id = toId 42
              , email = "tushar@example.com"
              , age = 30
              , note = Just "Engineering lead"
              }
      user.id.unId `shouldBe` 42
      user.email `shouldBe` "tushar@example.com"
      user.age `shouldBe` 30
      user.note `shouldBe` Just "Engineering lead"

  describe "2. 'Expr Context: Typed SQL AST Expressions" $ do
    it "projects typed QExpr nodes using standard OverloadedRecordDot" $ do
      let userExpr :: UserExpr
          userExpr =
            User
              { id = colExpr "users_0" "id"
              , email = colExpr "users_0" "email"
              , age = colExpr "users_0" "age"
              , note = colExpr "users_0" "note"
              }
      userExpr.email `shouldBe` colExpr "users_0" "email"
      userExpr.age `shouldBe` colExpr "users_0" "age"
      userExpr.note `shouldBe` colExpr "users_0" "note"

  describe "3. 'NullableTable Context: Automatic Idempotent Nullability Lifting" $ do
    it "lifts non-nullable columns to QExpr (Maybe a) on LEFT JOIN" $ do
      let userExpr :: UserExpr
          userExpr =
            User
              { id = colExpr "users_0" "id"
              , email = colExpr "users_0" "email"
              , age = colExpr "users_0" "age"
              , note = colExpr "users_0" "note"
              }
          nullUser :: NullableTable UserExpr
          nullUser = NullableTable userExpr

      -- Non-nullable columns are lifted to Maybe
      let emailExpr :: QExpr (Maybe Text)
          emailExpr = nullUser.email
      emailExpr `shouldBe` liftNullableExpr userExpr.email

      let ageExpr :: QExpr (Maybe Int)
          ageExpr = nullUser.age
      ageExpr `shouldBe` liftNullableExpr userExpr.age

    it "preserves already-nullable columns as QExpr (Maybe a) without double-wrapping" $ do
      let userExpr :: UserExpr
          userExpr =
            User
              { id = colExpr "users_0" "id"
              , email = colExpr "users_0" "email"
              , age = colExpr "users_0" "age"
              , note = colExpr "users_0" "note"
              }
          nullUser :: NullableTable UserExpr
          nullUser = NullableTable userExpr

      -- Already nullable column remains QExpr (Maybe Text), NOT QExpr (Maybe (Maybe Text))
      let noteExpr :: QExpr (Maybe Text)
          noteExpr = nullUser.note
      noteExpr `shouldBe` liftNullableExpr userExpr.note

    it "satisfies the idempotence law of LiftMay" $ do
      let e1 :: QExpr Int
          e1 = lit (100 :: Int)
          eLifted = liftNullableExpr e1
          eDoubleLifted = liftNullableExpr eLifted
      eDoubleLifted `shouldBe` eLifted

  describe "4. 'Patch Context: Differential Mutation Tracking" $ do
    it "allows expressing SetTo, Unchanged, and SetDefault states" $ do
      let patch :: UserPatch
          patch =
            User
              { id = Unchanged
              , email = SetTo "updated@example.com"
              , age = Unchanged
              , note = SetDefault
              }
      patch.id `shouldBe` Unchanged
      patch.email `shouldBe` SetTo "updated@example.com"
      patch.age `shouldBe` Unchanged
      patch.note `shouldBe` SetDefault

    it "satisfies the Functor identity law on PatchField" $
      property $ \(val :: Int) ->
        fmap id (SetTo val) == SetTo val
          && fmap id (Unchanged :: PatchField Int) == Unchanged
          && fmap id (SetDefault :: PatchField Int) == SetDefault

    it "satisfies the Functor composition law on PatchField" $
      property $ \(val :: Int) ->
        let f x = x + 10
            g x = x * 2
         in fmap (f . g) (SetTo val) == (fmap f . fmap g) (SetTo val)

  describe "5. 'Meta Context: Table Schema & Column Specifications" $ do
    it "pairs table name and column metadata correctly" $ do
      let schema = userSchema
          meta :: UserMeta
          meta = schema.tableMeta
      schema.tableName `shouldBe` "users"
      meta.id.colName `shouldBe` "id"
      meta.id.colIsPrimaryKey `shouldBe` True
      meta.email.colName `shouldBe` "email"
      meta.email.colIsNullable `shouldBe` False
      meta.email.colDescription `shouldBe` Just "Unique user email address"
      meta.note.colIsNullable `shouldBe` True

  describe "6. Phantom Key Isolation" $ do
    it "constructs distinct phantom keys for distinct tables with identical values" $ do
      let uKey = toId 1 :: Id UserT Int64
          oKey = toId 1 :: Id OrgT Int64
          org :: Org
          org = Org {id = oKey, name = "Acme Corp"}
      uKey.unId `shouldBe` oKey.unId
      org.name `shouldBe` "Acme Corp"

    it "preserves underlying key equality while preventing cross-table mixing" $
      property $ \(k :: Int64) ->
        let uKey = toId k :: Id UserT Int64
         in uKey.unId == k

    it "satisfies Eq and Ord for identical table keys" $ do
      let k1 = toId 100 :: Id UserT Int64
          k2 = toId 100 :: Id UserT Int64
          k3 = toId 200 :: Id UserT Int64
      k1 `shouldBe` k2
      k1 `shouldSatisfy` (< k3)
