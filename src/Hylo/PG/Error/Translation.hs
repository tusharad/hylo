{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.PG.Error.Translation
Description : PostgreSQL DB Constraint Error Translation to FieldError
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Captures PostgreSQL SQLSTATE errors (23505 unique_violation, 23503 foreign_key_violation,
23514 check_violation, 22001 string_data_right_truncation, 23502 not_null_violation)
and translates them into structured 'FieldError' values attached to the originating
schema columns without throwing unhandled driver exceptions.
-}
module Hylo.PG.Error.Translation
  ( -- * Structured Error Representation
    FieldError (..)
  , DbError (..)
  , mockSqlStateError
  , mockSqlStateErrorFull

    -- * Custom Constraint Mapping Rules
  , ConstraintRule (..)

    -- * Error Translation Engine
  , translateDbError
  , translateDbErrorWith
  , translateDbErrorEither

    -- * Constraint Identifier Parser
  , extractColumnFromConstraint
  ) where

import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Core.Error (FieldError (..))
import Hylo.Core.Table (TableSchema (..))

-- | Raw database driver error descriptor.
data DbError = DbError
  { dbSqlState :: !Text
  , dbConstraintName :: !Text
  , dbMessage :: !(Maybe Text)
  , dbDetail :: !(Maybe Text)
  , dbTable :: !(Maybe Text)
  , dbColumn :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

instance NFData DbError

-- | Construct a mock SQLSTATE database error with a constraint name.
mockSqlStateError :: Text -> Text -> DbError
mockSqlStateError sqlState constraint =
  DbError
    { dbSqlState = sqlState
    , dbConstraintName = constraint
    , dbMessage = Nothing
    , dbDetail = Nothing
    , dbTable = Nothing
    , dbColumn = Nothing
    }

-- | Construct a comprehensive database error with all metadata.
mockSqlStateErrorFull ::
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  DbError
mockSqlStateErrorFull = DbError

-- | User-configurable rule for translating named database constraints into field errors.
data ConstraintRule = ConstraintRule
  { crConstraintName :: !Text
  , crField :: !Text
  , crErrorCode :: !Text
  , crMessage :: !Text
  }
  deriving (Eq, Show, Generic)

instance NFData ConstraintRule

{- | Extract the likely column name from a PostgreSQL constraint identifier.

Supported PostgreSQL naming conventions:
* @{table}_{column}_key@ -> @{column}@ (standard unique constraint)
* @{table}_{column}_fkey@ -> @{column}@ (standard foreign key constraint)
* @{table}_{column}_check@ -> @{column}@ (standard check constraint)
* @{table}_{column}_unique@ -> @{column}@
* @{column}_key@ -> @{column}@
* @{column}_fkey@ -> @{column}@
* @{column}_check@ -> @{column}@
-}
extractColumnFromConstraint :: Text -> Text -> Text
extractColumnFromConstraint tbl constraint =
  let pfx = tbl <> "_"
      hasTablePfx = pfx `T.isPrefixOf` constraint
      withoutTablePrefix =
        if hasTablePfx
          then T.drop (T.length pfx) constraint
          else constraint
      stripKnownSuffix str
        | "_key" `T.isSuffixOf` str = (True, T.dropEnd 4 str)
        | "_fkey" `T.isSuffixOf` str = (True, T.dropEnd 5 str)
        | "_check" `T.isSuffixOf` str = (True, T.dropEnd 6 str)
        | "_unique" `T.isSuffixOf` str = (True, T.dropEnd 7 str)
        | "_pkey" `T.isSuffixOf` str = (True, T.dropEnd 5 str)
        | otherwise = (False, str)
      (hadSuffix, stripped) = stripKnownSuffix withoutTablePrefix
      res
        | hasTablePfx = stripped
        | hadSuffix =
            let parts = T.splitOn "_" stripped
             in case parts of
                  (_ : c : rest) -> T.intercalate "_" (c : rest)
                  _ -> stripped
        | otherwise = stripped
   in if T.null res then constraint else res

-- | Translate a database driver error into a structured 'FieldError' using optional custom rules.
translateDbErrorWith ::
  [ConstraintRule] ->
  TableSchema table ->
  DbError ->
  Maybe FieldError
translateDbErrorWith rules schema err =
  let constraint = dbConstraintName err
      tName = tableName schema
      col = case dbColumn err of
        Just c | not (T.null c) -> c
        _ -> extractColumnFromConstraint tName constraint
      matchCustom =
        [ FieldError (crField r) (crErrorCode r) (crMessage r)
        | r <- rules
        , crConstraintName r == constraint
        ]
   in case matchCustom of
        (r : _) -> Just r
        [] -> case dbSqlState err of
          "23505" ->
            Just
              ( FieldError
                  col
                  "unique_violation"
                  ("A record with this " <> col <> " already exists")
              )
          "23503" ->
            Just
              ( FieldError
                  col
                  "foreign_key_violation"
                  ("Referenced " <> col <> " does not exist")
              )
          "23514" ->
            Just
              ( FieldError
                  col
                  "check_violation"
                  ("Value violates check constraint for " <> col)
              )
          "22001" ->
            Just
              ( FieldError
                  col
                  "string_overflow"
                  ("Value exceeds maximum allowed length for " <> col)
              )
          "23502" ->
            Just
              ( FieldError
                  col
                  "not_null_violation"
                  ("Column " <> col <> " cannot be null")
              )
          _ -> Nothing

-- | Translate a database driver error into a structured 'FieldError' using standard PostgreSQL conventions.
translateDbError :: TableSchema table -> DbError -> Maybe FieldError
translateDbError = translateDbErrorWith []

-- | Translate a database driver error into an 'Either FieldError a' result.
translateDbErrorEither :: TableSchema table -> DbError -> Either FieldError a
translateDbErrorEither schema err =
  case translateDbError schema err of
    Just fe -> Left fe
    Nothing ->
      Left
        ( FieldError
            (dbConstraintName err)
            (dbSqlState err)
            ("Database error with SQLSTATE " <> dbSqlState err)
        )
