{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.Core.Changeset
Description : Pure Ecto-style Applicative Changeset, field validation, and diffing engine
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides pure, database-agnostic changesets for tracking mutation operations (Insert, Update, Delete).
Tracks modified columns via 'PatchField', accumulates validation 'FieldError's simultaneously,
registers database constraint declarations (unique, foreign key), and computes minimal dirty-field
maps for high-performance partial updates.
-}
module Hylo.Core.Changeset
  ( -- * Operations & Changeset Types
    OpKind (..)
  , Changeset (..)
  , emptyChangeset
  , change
  , withUniqueKey
  , castChangesetOp

    -- * Differential Field Tracking
  , PatchField (..)

    -- * Structured Field Errors & Validation Applicative
  , FieldError (..)
  , fieldError
  , Validation (..)

    -- * Constraints Declarations
  , ConstraintDeclaration (..)
  , constraintField
  , constraintName
  , constraintMessage
  , constraintRefTable
  , uniqueConstraint
  , uniqueConstraintNamed
  , foreignKeyConstraint
  , foreignKeyConstraintNamed

    -- * Pure Composable Validators
  , addError
  , addFieldError
  , validateRequired
  , CheckRequired (..)
  , validateLength
  , LengthOpts (..)
  , hasMinLength
  , hasMaxLength
  , hasLengthRange
  , hasExactLength
  , validateFormat
  , validateFormatWith
  , validateNumber
  , NumberOpts (..)
  , greaterThan
  , greaterThanOrEqualTo
  , lessThan
  , lessThanOrEqualTo
  , numberRange

    -- * Record Diffing & Dirty Field Tracking
  , diffRecord
  , diffField
  , dirtyFields
  , dirtyFieldMap
  , changesetDirtyFields
  , ToFieldValue (..)
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Core.Context (Ctx (..), PatchField (..))
import Hylo.Core.Error (FieldError (..))
import Hylo.Core.Key (Id (..))
import Hylo.Core.Table (Table (..), TableSchema (..), ToFieldValue (..), diffField)

--------------------------------------------------------------------------------
-- Operations & Changeset Types
--------------------------------------------------------------------------------

-- | Mutation operation intent for the changeset.
data OpKind
  = InsertOp
  | UpdateOp
  | DeleteOp
  deriving (Eq, Show, Enum, Bounded, Generic)

-- | Constraint declaration registered on the changeset prior to database execution.
data ConstraintDeclaration
  = -- | Field name, optional constraint name, error message
    UniqueConstraint !Text !(Maybe Text) !Text
  | -- | Field name, referenced table, optional constraint name, error message
    ForeignKeyConstraint !Text !Text !(Maybe Text) !Text
  deriving (Eq, Show, Generic)

-- | Get the target field of a constraint declaration.
constraintField :: ConstraintDeclaration -> Text
constraintField = \case
  UniqueConstraint f _ _ -> f
  ForeignKeyConstraint f _ _ _ -> f

-- | Get the optional constraint name.
constraintName :: ConstraintDeclaration -> Maybe Text
constraintName = \case
  UniqueConstraint _ n _ -> n
  ForeignKeyConstraint _ _ n _ -> n

-- | Get the error message for the constraint.
constraintMessage :: ConstraintDeclaration -> Text
constraintMessage = \case
  UniqueConstraint _ _ m -> m
  ForeignKeyConstraint _ _ _ m -> m

-- | Get the referenced table if this is a foreign key constraint.
constraintRefTable :: ConstraintDeclaration -> Maybe Text
constraintRefTable = \case
  UniqueConstraint {} -> Nothing
  ForeignKeyConstraint _ r _ _ -> Just r

-- | Ecto-style mutation changeset parameterized by HKD table and operation kind.
data Changeset (table :: Ctx -> Type) (op :: OpKind) = Changeset
  { changesetPatch :: !(table 'Patch)
  , changesetErrors :: ![FieldError]
  , changesetConstraints :: ![ConstraintDeclaration]
  , changesetKey :: !Text
  , changesetValid :: !Bool
  , changesetParams :: !(Map Text Text)
  }
  deriving (Generic)

-- | Construct a fresh, valid changeset from a patch record.
emptyChangeset :: forall (op :: OpKind) table. table 'Patch -> Changeset table op
emptyChangeset patch =
  Changeset
    { changesetPatch = patch
    , changesetErrors = []
    , changesetConstraints = []
    , changesetKey = ""
    , changesetValid = True
    , changesetParams = Map.empty
    }

-- | Synonym for 'emptyChangeset'.
change :: forall (op :: OpKind) table. table 'Patch -> Changeset table op
change = emptyChangeset

-- | Attach a simulated unique/primary key for conflict resolution and transaction logs.
withUniqueKey :: Text -> Changeset table op -> Changeset table op
withUniqueKey key cs = cs {changesetKey = key}

-- | Cast the phantom operation kind of a changeset.
castChangesetOp :: Changeset table op -> Changeset table op'
castChangesetOp cs =
  Changeset
    { changesetPatch = changesetPatch cs
    , changesetErrors = changesetErrors cs
    , changesetConstraints = changesetConstraints cs
    , changesetKey = changesetKey cs
    , changesetValid = changesetValid cs
    , changesetParams = changesetParams cs
    }

--------------------------------------------------------------------------------
-- Structured Field Errors & Validation Applicative
--------------------------------------------------------------------------------

-- | Smart constructor for 'FieldError'.
fieldError :: Text -> Text -> Text -> FieldError
fieldError = FieldError

-- | Applicative validation functor that accumulates all errors in a single pass.
data Validation a
  = Failure ![FieldError]
  | Success !a
  deriving (Eq, Show, Functor, Generic)

instance Applicative Validation where
  pure = Success
  Success f <*> Success x = Success (f x)
  Failure e1 <*> Success _ = Failure e1
  Success _ <*> Failure e2 = Failure e2
  Failure e1 <*> Failure e2 = Failure (e1 ++ e2)

-- | Append a validation error to the changeset, marking it as invalid.
addError :: Text -> Text -> Text -> Changeset table op -> Changeset table op
addError field code msg cs =
  let err = FieldError field code msg
   in cs
        { changesetErrors = changesetErrors cs ++ [err]
        , changesetValid = False
        }

-- | Append a pre-constructed 'FieldError' to the changeset.
addFieldError :: FieldError -> Changeset table op -> Changeset table op
addFieldError err cs =
  cs
    { changesetErrors = changesetErrors cs ++ [err]
    , changesetValid = False
    }

--------------------------------------------------------------------------------
-- Pure Composable Validators
--------------------------------------------------------------------------------

-- | Class for checking whether a column value is blank or empty.
class CheckRequired a where
  isBlank :: a -> Bool

instance CheckRequired Text where
  isBlank = T.null . T.strip

instance CheckRequired [a] where
  isBlank = null

instance (CheckRequired a) => CheckRequired (Maybe a) where
  isBlank Nothing = True
  isBlank (Just a) = isBlank a

instance CheckRequired Int where
  isBlank _ = False

instance CheckRequired Int64 where
  isBlank _ = False

instance CheckRequired Integer where
  isBlank _ = False

instance CheckRequired Double where
  isBlank _ = False

instance CheckRequired Float where
  isBlank _ = False

instance CheckRequired Bool where
  isBlank _ = False

instance CheckRequired (Id table a) where
  isBlank _ = False

-- | Validate that a field is set and not blank.
validateRequired ::
  (CheckRequired a) =>
  Text ->
  (table 'Patch -> PatchField a) ->
  Changeset table op ->
  Changeset table op
validateRequired field getter cs =
  case getter (changesetPatch cs) of
    Unchanged -> addError field "required" "can't be blank" cs
    SetDefault -> cs
    SetTo val
      | isBlank val -> addError field "required" "can't be blank" cs
      | otherwise -> cs

-- | Length validation options.
data LengthOpts = LengthOpts
  { minLength :: !(Maybe Int)
  , maxLength :: !(Maybe Int)
  , exactLength :: !(Maybe Int)
  }
  deriving (Eq, Show, Generic)

-- | Require a minimum length.
hasMinLength :: Int -> LengthOpts
hasMinLength n = LengthOpts (Just n) Nothing Nothing

-- | Require a maximum length.
hasMaxLength :: Int -> LengthOpts
hasMaxLength n = LengthOpts Nothing (Just n) Nothing

-- | Require length within a closed range.
hasLengthRange :: Int -> Int -> LengthOpts
hasLengthRange minL maxL = LengthOpts (Just minL) (Just maxL) Nothing

-- | Require an exact length.
hasExactLength :: Int -> LengthOpts
hasExactLength n = LengthOpts Nothing Nothing (Just n)

-- | Validate textual length constraints.
validateLength ::
  Text ->
  (table 'Patch -> PatchField Text) ->
  LengthOpts ->
  Changeset table op ->
  Changeset table op
validateLength field getter opts cs =
  case getter (changesetPatch cs) of
    SetTo val ->
      let len = T.length val
          cs1 = case minLength opts of
            Just m
              | len < m ->
                  addError field "length" ("should be at least " <> T.pack (show m) <> " character(s)") cs
            _ -> cs
          cs2 = case maxLength opts of
            Just m
              | len > m ->
                  addError field "length" ("should be at most " <> T.pack (show m) <> " character(s)") cs1
            _ -> cs1
          cs3 = case exactLength opts of
            Just m
              | len /= m ->
                  addError field "length" ("should be exactly " <> T.pack (show m) <> " character(s)") cs2
            _ -> cs2
       in cs3
    _ -> cs

-- | Validate that a text field satisfies a format pattern.
validateFormat ::
  Text ->
  (table 'Patch -> PatchField Text) ->
  Text ->
  Changeset table op ->
  Changeset table op
validateFormat field getter pat cs =
  case getter (changesetPatch cs) of
    SetTo val
      | not (matchesFormat pat val) ->
          addError field "format" "has invalid format" cs
      | otherwise -> cs
    _ -> cs

-- | Validate format using a custom predicate function.
validateFormatWith ::
  Text ->
  (table 'Patch -> PatchField Text) ->
  (Text -> Bool) ->
  Text ->
  Changeset table op ->
  Changeset table op
validateFormatWith field getter predicate errMsg cs =
  case getter (changesetPatch cs) of
    SetTo val
      | not (predicate val) -> addError field "format" errMsg cs
      | otherwise -> cs
    _ -> cs

matchesFormat :: Text -> Text -> Bool
matchesFormat pat val
  | pat == "@" || pat == "email" || "^[^@]+@" `T.isPrefixOf` pat =
      case T.splitOn "@" val of
        [user, domain] ->
          not (T.null user) && not (T.null domain) && T.isInfixOf "." domain
        _ -> False
  | pat == "^[0-9]+$" || pat == "numeric" =
      not (T.null val) && T.all isDigit val
  | pat == "^[a-zA-Z]+$" || pat == "alpha" =
      not (T.null val) && T.all (\c -> isAsciiLower c || isAsciiUpper c) val
  | pat == "^[a-zA-Z0-9]+$" || pat == "alphanumeric" =
      not (T.null val) && T.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c) val
  | otherwise =
      pat `T.isInfixOf` val

-- | Numeric boundary validation options.
data NumberOpts a = NumberOpts
  { numGreaterThan :: !(Maybe a)
  , numGreaterThanOrEqualTo :: !(Maybe a)
  , numLessThan :: !(Maybe a)
  , numLessThanOrEqualTo :: !(Maybe a)
  , numEqualTo :: !(Maybe a)
  }
  deriving (Eq, Show, Generic)

-- | Require value strictly greater than bound.
greaterThan :: a -> NumberOpts a
greaterThan x = NumberOpts (Just x) Nothing Nothing Nothing Nothing

-- | Require value greater than or equal to bound.
greaterThanOrEqualTo :: a -> NumberOpts a
greaterThanOrEqualTo x = NumberOpts Nothing (Just x) Nothing Nothing Nothing

-- | Require value strictly less than bound.
lessThan :: a -> NumberOpts a
lessThan x = NumberOpts Nothing Nothing (Just x) Nothing Nothing

-- | Require value less than or equal to bound.
lessThanOrEqualTo :: a -> NumberOpts a
lessThanOrEqualTo x = NumberOpts Nothing Nothing Nothing (Just x) Nothing

-- | Require numeric value within closed range.
numberRange :: a -> a -> NumberOpts a
numberRange minV maxV = NumberOpts Nothing (Just minV) Nothing (Just maxV) Nothing

-- | Validate numeric bounds on a column.
validateNumber ::
  (Ord a, Show a) =>
  Text ->
  (table 'Patch -> PatchField a) ->
  NumberOpts a ->
  Changeset table op ->
  Changeset table op
validateNumber field getter opts cs =
  case getter (changesetPatch cs) of
    SetTo val ->
      let cs1 = case numGreaterThan opts of
            Just bound | val <= bound -> addError field "number" ("must be greater than " <> T.pack (show bound)) cs
            _ -> cs
          cs2 = case numGreaterThanOrEqualTo opts of
            Just bound
              | val < bound ->
                  addError field "number" ("must be greater than or equal to " <> T.pack (show bound)) cs1
            _ -> cs1
          cs3 = case numLessThan opts of
            Just bound | val >= bound -> addError field "number" ("must be less than " <> T.pack (show bound)) cs2
            _ -> cs2
          cs4 = case numLessThanOrEqualTo opts of
            Just bound
              | val > bound ->
                  addError field "number" ("must be less than or equal to " <> T.pack (show bound)) cs3
            _ -> cs3
          cs5 = case numEqualTo opts of
            Just bound | val /= bound -> addError field "number" ("must be equal to " <> T.pack (show bound)) cs4
            _ -> cs4
       in cs5
    _ -> cs

--------------------------------------------------------------------------------
-- Database Constraints Declarations
--------------------------------------------------------------------------------

-- | Register a unique constraint on a column.
uniqueConstraint ::
  Text ->
  Changeset table op ->
  Changeset table op
uniqueConstraint field cs =
  let c = UniqueConstraint field Nothing "has already been taken"
   in cs {changesetConstraints = changesetConstraints cs ++ [c]}

-- | Register a named unique constraint on a column.
uniqueConstraintNamed ::
  Text ->
  Text ->
  Changeset table op ->
  Changeset table op
uniqueConstraintNamed field cName cs =
  let c = UniqueConstraint field (Just cName) "has already been taken"
   in cs {changesetConstraints = changesetConstraints cs ++ [c]}

-- | Register a foreign key constraint on a column.
foreignKeyConstraint ::
  Text ->
  Text ->
  Changeset table op ->
  Changeset table op
foreignKeyConstraint field refTable cs =
  let c = ForeignKeyConstraint field refTable Nothing "does not exist"
   in cs {changesetConstraints = changesetConstraints cs ++ [c]}

-- | Register a named foreign key constraint on a column.
foreignKeyConstraintNamed ::
  Text ->
  Text ->
  Text ->
  Changeset table op ->
  Changeset table op
foreignKeyConstraintNamed field refTable cName cs =
  let c = ForeignKeyConstraint field refTable (Just cName) "does not exist"
   in cs {changesetConstraints = changesetConstraints cs ++ [c]}

--------------------------------------------------------------------------------
-- Record Diffing & Dirty Field Tracking
--------------------------------------------------------------------------------

-- | Purely diff two hydrated records, marking modified fields as SetTo and identical fields as Unchanged.
diffRecord ::
  (Table table) =>
  TableSchema table ->
  table 'Result ->
  table 'Result ->
  Changeset table 'UpdateOp
diffRecord schema old new =
  Changeset
    { changesetPatch = diffTable old new
    , changesetErrors = []
    , changesetConstraints = []
    , changesetKey = tableName schema
    , changesetValid = True
    , changesetParams = Map.empty
    }

-- | Extract a map of dirty (modified) column names to their formatted textual values.
dirtyFields :: (Table table) => Changeset table op -> Map Text Text
dirtyFields cs = dirtyFieldsTable (changesetPatch cs)

-- | Synonym for 'dirtyFields'.
dirtyFieldMap :: (Table table) => Changeset table op -> Map Text Text
dirtyFieldMap = dirtyFields

-- | Synonym for 'dirtyFields'.
changesetDirtyFields :: (Table table) => Changeset table op -> Map Text Text
changesetDirtyFields = dirtyFields
