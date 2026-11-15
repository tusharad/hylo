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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{- |
Module      : Hylo.Core.ChangesetSpec
Description : Test suite for User Story 3: Pure Hylo.Core.Changeset Engine
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.ChangesetSpec (spec) where

import Data.Function ((&))
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Core
import Test.Hspec
import Test.QuickCheck hiding (Failure)

default (Text, Int64)

--------------------------------------------------------------------------------
-- 10-Column HKD Entity for Record Diffing & Partial Updates
--------------------------------------------------------------------------------

data AccountProfileT (ctx :: Ctx) = AccountProfile
  { id :: !(Col ctx (Id AccountProfileT Int64))
  , username :: !(Col ctx Text)
  , email :: !(Col ctx Text)
  , age :: !(Col ctx Int64)
  , bio :: !(Col ctx Text)
  , country :: !(Col ctx Text)
  , phone :: !(Col ctx Text)
  , role :: !(Col ctx Text)
  , score :: !(Col ctx Int64)
  , active :: !(Col ctx Bool)
  }
  deriving (Generic)

deriving instance Eq (AccountProfileT 'Result)
deriving instance Show (AccountProfileT 'Result)
deriving instance Eq (AccountProfileT 'Patch)
deriving instance Show (AccountProfileT 'Patch)

accountProfileSchema :: TableSchema AccountProfileT
accountProfileSchema =
  tableSchema "account_profiles" $
    AccountProfile
      { id = primaryKey "id" "bigint"
      , username = column "username" "text"
      , email = column "email" "text"
      , age = column "age" "bigint"
      , bio = column "bio" "text"
      , country = column "country" "text"
      , phone = column "phone" "text"
      , role = column "role" "text"
      , score = column "score" "bigint"
      , active = column "active" "boolean"
      }

instance Table AccountProfileT where
  toTableExpr alias meta =
    AccountProfile
      { id = colSpecExpr alias meta.id
      , username = colSpecExpr alias meta.username
      , email = colSpecExpr alias meta.email
      , age = colSpecExpr alias meta.age
      , bio = colSpecExpr alias meta.bio
      , country = colSpecExpr alias meta.country
      , phone = colSpecExpr alias meta.phone
      , role = colSpecExpr alias meta.role
      , score = colSpecExpr alias meta.score
      , active = colSpecExpr alias meta.active
      }
  diffTable old new =
    AccountProfile
      { id = diffField old.id new.id
      , username = diffField old.username new.username
      , email = diffField old.email new.email
      , age = diffField old.age new.age
      , bio = diffField old.bio new.bio
      , country = diffField old.country new.country
      , phone = diffField old.phone new.phone
      , role = diffField old.role new.role
      , score = diffField old.score new.score
      , active = diffField old.active new.active
      }
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.username
    , eraseColDef m.email
    , eraseColDef m.age
    , eraseColDef m.bio
    , eraseColDef m.country
    , eraseColDef m.phone
    , eraseColDef m.role
    , eraseColDef m.score
    , eraseColDef m.active
    ]
  patchEntries meta patch =
    concat
      [ patchEntry meta.id patch.id
      , patchEntry meta.username patch.username
      , patchEntry meta.email patch.email
      , patchEntry meta.age patch.age
      , patchEntry meta.bio patch.bio
      , patchEntry meta.country patch.country
      , patchEntry meta.phone patch.phone
      , patchEntry meta.role patch.role
      , patchEntry meta.score patch.score
      , patchEntry meta.active patch.active
      ]
  dirtyFieldsTable patch =
    Map.fromList
      [ (colName c, case v of ValSetTo t -> t; ValSetDefault -> "DEFAULT")
      | (c, v) <- patchEntries (tableMeta accountProfileSchema) patch
      ]

sampleAccount1 :: AccountProfileT 'Result
sampleAccount1 =
  AccountProfile
    { id = toId 42
    , username = "tushar"
    , email = "tushar@example.com"
    , age = 30
    , bio = "Haskell ORM Architect"
    , country = "India"
    , phone = "+919876543210"
    , role = "lead"
    , score = 100
    , active = True
    }

spec :: Spec
spec = describe "User Story 3: Pure Hylo.Core.Changeset Engine" $ do
  describe "Acceptance Criterion 1: Multi-field error accumulation in a single pass" $ do
    it "returns all 3 FieldError values simultaneously when validating 3 broken fields" $ do
      let brokenPatch =
            AccountProfile
              { id = SetDefault
              , username = SetTo "   "
              , email = SetTo "not-a-valid-email"
              , age = SetTo 15
              , bio = SetTo "Haskell enthusiast"
              , country = SetTo "India"
              , phone = SetTo "+919876543210"
              , role = SetTo "developer"
              , score = SetTo 75
              , active = SetTo True
              }
          cs =
            change brokenPatch
              & validateRequired "username" (\p -> p.username)
              & validateFormat "email" (\p -> p.email) "^[^@]+@[^@]+\\.[^@]+$"
              & validateNumber "age" (\p -> p.age) (greaterThanOrEqualTo 18)

      cs.changesetValid `shouldBe` False
      case cs.changesetErrors of
        [errUsername, errEmail, errAge] -> do
          errUsername.fieldPath `shouldBe` "username"
          errUsername.errorCode `shouldBe` "required"
          errUsername.message `shouldBe` "can't be blank"

          errEmail.fieldPath `shouldBe` "email"
          errEmail.errorCode `shouldBe` "format"
          errEmail.message `shouldBe` "has invalid format"

          errAge.fieldPath `shouldBe` "age"
          errAge.errorCode `shouldBe` "number"
          errAge.message `shouldBe` "must be greater than or equal to 18"
        errs -> expectationFailure ("Expected 3 errors, got: " <> show errs)

    it "Validation applicative functor accumulates all 3 failures simultaneously via (<*>)" $ do
      let vUsername :: Validation Text
          vUsername = Failure [fieldError "username" "required" "can't be blank"]
          vEmail :: Validation Text
          vEmail = Failure [fieldError "email" "format" "has invalid format"]
          vAge :: Validation Int64
          vAge = Failure [fieldError "age" "number" "must be greater than or equal to 18"]

          combined = (,,) <$> vUsername <*> vEmail <*> vAge

      combined
        `shouldBe` Failure
          [ fieldError "username" "required" "can't be blank"
          , fieldError "email" "format" "has invalid format"
          , fieldError "age" "number" "must be greater than or equal to 18"
          ]

    it "marks changeset valid when all validations pass" $ do
      let validPatch =
            AccountProfile
              { id = SetDefault
              , username = SetTo "tushar"
              , email = SetTo "tushar@example.com"
              , age = SetTo 30
              , bio = SetTo "Haskell architect"
              , country = SetTo "India"
              , phone = SetTo "+919876543210"
              , role = SetTo "lead"
              , score = SetTo 100
              , active = SetTo True
              }
          cs =
            change validPatch
              & validateRequired "username" (\p -> p.username)
              & validateFormat "email" (\p -> p.email) "^[^@]+@[^@]+\\.[^@]+$"
              & validateNumber "age" (\p -> p.age) (greaterThanOrEqualTo 18)

      cs.changesetValid `shouldBe` True
      cs.changesetErrors `shouldBe` []

  describe "Acceptance Criterion 2: Minimal dirty-field map on single-column change" $ do
    it "produces a dirty map containing only the 1 modified column out of 10" $ do
      let r1 = sampleAccount1
          r2 = r1 {bio = "Senior Haskell ORM Architect"}
          cs = diffRecord accountProfileSchema r1 r2

      -- Exactly 1 field is SetTo, 9 are Unchanged
      (changesetPatch cs).id `shouldBe` Unchanged
      (changesetPatch cs).username `shouldBe` Unchanged
      (changesetPatch cs).email `shouldBe` Unchanged
      (changesetPatch cs).age `shouldBe` Unchanged
      (changesetPatch cs).bio `shouldBe` SetTo "Senior Haskell ORM Architect"
      (changesetPatch cs).country `shouldBe` Unchanged
      (changesetPatch cs).phone `shouldBe` Unchanged
      (changesetPatch cs).role `shouldBe` Unchanged
      (changesetPatch cs).score `shouldBe` Unchanged
      (changesetPatch cs).active `shouldBe` Unchanged

      -- Dirty field maps contain only the single modified column
      let expectedMap = Map.singleton "bio" "Senior Haskell ORM Architect"
      dirtyFields cs `shouldBe` expectedMap
      dirtyFieldMap cs `shouldBe` expectedMap
      changesetDirtyFields cs `shouldBe` expectedMap
      Map.size (dirtyFields cs) `shouldBe` 1

    it "produces a minimal dirty map when only email changes" $ do
      let r1 = sampleAccount1
          r2 = r1 {email = "new.email@example.com"}
          cs = diffRecord accountProfileSchema r1 r2

      dirtyFields cs `shouldBe` Map.singleton "email" "new.email@example.com"
      (changesetPatch cs).email `shouldBe` SetTo "new.email@example.com"
      (changesetPatch cs).bio `shouldBe` Unchanged

    it "produces a minimal dirty map when only score changes" $ do
      let r1 = sampleAccount1
          r2 = r1 {score = 250}
          cs = diffRecord accountProfileSchema r1 r2

      dirtyFields cs `shouldBe` Map.singleton "score" "250"
      (changesetPatch cs).score `shouldBe` SetTo 250

    it "produces a minimal dirty map when only active boolean changes" $ do
      let r1 = sampleAccount1
          r2 = r1 {active = False}
          cs = diffRecord accountProfileSchema r1 r2

      dirtyFields cs `shouldBe` Map.singleton "active" "False"
      (changesetPatch cs).active `shouldBe` SetTo False

  describe "Diffing Records Edge Cases" $ do
    it "produces an empty dirty-field map when diffing identical records" $ do
      let r1 = sampleAccount1
          cs = diffRecord accountProfileSchema r1 r1

      dirtyFields cs `shouldBe` Map.empty
      (changesetPatch cs).id `shouldBe` Unchanged
      (changesetPatch cs).username `shouldBe` Unchanged
      (changesetPatch cs).email `shouldBe` Unchanged
      (changesetPatch cs).age `shouldBe` Unchanged
      (changesetPatch cs).bio `shouldBe` Unchanged
      (changesetPatch cs).country `shouldBe` Unchanged
      (changesetPatch cs).phone `shouldBe` Unchanged
      (changesetPatch cs).role `shouldBe` Unchanged
      (changesetPatch cs).score `shouldBe` Unchanged
      (changesetPatch cs).active `shouldBe` Unchanged

    it "tracks multiple dirty fields when multiple columns change" $ do
      let r1 = sampleAccount1
          r2 = r1 {email = "updated@example.com", score = 150, active = False}
          cs = diffRecord accountProfileSchema r1 r2

      Map.size (dirtyFields cs) `shouldBe` 3
      dirtyFields cs
        `shouldBe` Map.fromList
          [ ("email", "updated@example.com")
          , ("score", "150")
          , ("active", "False")
          ]

  describe "Validators & Options" $ do
    it "validateLength enforces min, max, and exact length constraints" $ do
      let p =
            AccountProfile
              { id = SetDefault
              , username = SetTo "ab"
              , email = SetTo "too-long-text-exceeding-limit"
              , age = SetTo 25
              , bio = SetTo "12345"
              , country = SetTo "US"
              , phone = SetTo "+1"
              , role = SetTo "user"
              , score = SetTo 10
              , active = SetTo True
              }
          cs =
            change p
              & validateLength "username" (\x -> x.username) (hasMinLength 3)
              & validateLength "email" (\x -> x.email) (hasMaxLength 10)
              & validateLength "bio" (\x -> x.bio) (hasExactLength 5)
              & validateLength "country" (\x -> x.country) (hasLengthRange 2 4)

      cs.changesetValid `shouldBe` False
      length cs.changesetErrors `shouldBe` 2
      map (\e -> e.fieldPath) cs.changesetErrors `shouldBe` ["username", "email"]

    it "validateFormatWith allows custom validation predicates" $ do
      let p =
            AccountProfile
              { id = SetDefault
              , username = SetTo "admin_user"
              , email = SetTo "valid@test.com"
              , age = SetTo 25
              , bio = SetTo "bio"
              , country = SetTo "IN"
              , phone = SetTo "+919876543210"
              , role = SetTo "admin"
              , score = SetTo 100
              , active = SetTo True
              }
          cs =
            change p
              & validateFormatWith "phone" (\x -> x.phone) (T.isPrefixOf "+91") "must have India country code"

      cs.changesetValid `shouldBe` True

    it "validateNumber checks upper and lower bounds" $ do
      let p =
            AccountProfile
              { id = SetDefault
              , username = SetTo "user"
              , email = SetTo "u@test.com"
              , age = SetTo 10
              , bio = SetTo "bio"
              , country = SetTo "US"
              , phone = SetTo "123"
              , role = SetTo "dev"
              , score = SetTo 200
              , active = SetTo True
              }
          cs =
            change p
              & validateNumber "age" (\x -> x.age) (lessThan 18)
              & validateNumber "score" (\x -> x.score) (numberRange 0 100)

      cs.changesetValid `shouldBe` False
      map (\e -> e.fieldPath) cs.changesetErrors `shouldBe` ["score"]

  describe "Constraint Declarations" $ do
    it "registers unique and foreign key constraints with metadata" $ do
      let p =
            AccountProfile
              { id = SetDefault
              , username = SetTo "alice"
              , email = SetTo "alice@example.com"
              , age = SetTo 28
              , bio = SetTo "bio"
              , country = SetTo "UK"
              , phone = SetTo "+44123"
              , role = SetTo "engineer"
              , score = SetTo 90
              , active = SetTo True
              }
          cs =
            change p
              & uniqueConstraint "email"
              & uniqueConstraintNamed "username" "uq_account_profiles_username"
              & foreignKeyConstraint "country" "countries"
              & foreignKeyConstraintNamed "role" "roles" "fk_account_profiles_role"

      case changesetConstraints cs of
        [c1, c2, c3, c4] -> do
          constraintField c1 `shouldBe` "email"
          constraintName c1 `shouldBe` Nothing
          constraintMessage c1 `shouldBe` "has already been taken"

          constraintField c2 `shouldBe` "username"
          constraintName c2 `shouldBe` Just "uq_account_profiles_username"

          constraintField c3 `shouldBe` "country"
          constraintRefTable c3 `shouldBe` Just "countries"

          constraintField c4 `shouldBe` "role"
          constraintRefTable c4 `shouldBe` Just "roles"
          constraintName c4 `shouldBe` Just "fk_account_profiles_role"
        cs' -> expectationFailure ("Expected 4 constraints, got: " <> show (length cs'))

  describe "QuickCheck Invariants" $ do
    it "diffing identical generated records always yields zero dirty fields" $ do
      property $ \(ageVal :: Int64, scoreVal :: Int64, activeVal :: Bool) ->
        let r =
              sampleAccount1
                { age = ageVal
                , score = scoreVal
                , active = activeVal
                }
            cs = diffRecord accountProfileSchema r r
         in Map.null (dirtyFields cs)

    it "error accumulation is strictly monotonic" $ do
      property $ \(Positive (n :: Int)) ->
        let emptyCs = emptyChangeset (diffTable sampleAccount1 sampleAccount1)
            csWithErrors = foldr (\i acc -> addError (T.pack ("field_" <> show i)) "code" "error" acc) emptyCs [1 .. n]
         in length (changesetErrors csWithErrors) == n && not (changesetValid csWithErrors)
