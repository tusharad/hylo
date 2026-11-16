{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.Migrate.Linter
Description : Static DDL Safety Linter & Production Lock Contention Prevention
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Migrate.Linter
  ( -- * Lint Error Types
    LintError (..)

    -- * Linter Configuration
  , LintConfig (..)
  , defaultLintConfig

    -- * Core Linter API
  , lintMigrationSQL
  , lintMigrationSQLWith
  , lintDDLStatements

    -- * Two-Phase Constraint Safety (Law 2)
  , splitTwoPhaseConstraint
  , renderTwoPhaseConstraint

    -- * Statement & Preamble Utilities
  , attachLockTimeoutPreamble
  , splitStatements
  , isSetStmt
  , isIndexCreationStmt
  ) where

import Control.DeepSeq (NFData)
import Data.Char (isDigit)
import Data.Maybe (isNothing)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Migrate.AST
import Text.Read (readMaybe)

-- | 12 PostgreSQL production locking, contention, and data-loss hazards.
data LintError
  = IndexCreatedWithoutConcurrently !Text
  | MissingLockTimeoutPreamble
  | UnsafeNotNullAdded !Text !Text
  | UnsafeForeignKeyAdded !Text !Text
  | DestructiveDropTable !Text
  | DestructiveDropColumn !Text !Text
  | UnsafeColumnTypeChange !Text !Text
  | UnsafeRename !Text
  | VolatileColumnDefault !Text !Text
  | MissingStatementTimeout
  | UnsafeMaintenanceCommand !Text
  | LockTimeoutTooHigh !Int
  deriving (Eq, Show, Generic, NFData)

-- | Configurable thresholds and safety flags for migration linting.
data LintConfig = LintConfig
  { cfgRequireLockTimeout :: !Bool
  , cfgMaxLockTimeoutSec :: !Int
  , cfgRequireStatementTimeout :: !Bool
  , cfgAllowDestructive :: !Bool
  , cfgRequireConcurrentIndexes :: !Bool
  , cfgRequireTwoPhaseConstraints :: !Bool
  }
  deriving (Eq, Show, Generic, NFData)

-- | Default strict zero-downtime production configuration.
defaultLintConfig :: LintConfig
defaultLintConfig =
  LintConfig
    { cfgRequireLockTimeout = True
    , cfgMaxLockTimeoutSec = 5
    , cfgRequireStatementTimeout = False
    , cfgAllowDestructive = False
    , cfgRequireConcurrentIndexes = True
    , cfgRequireTwoPhaseConstraints = True
    }

{- | Lint raw SQL migration text using default zero-downtime safety rules.
Enforces CONCURRENT index builds and lock_timeout preambles on table mutations.
-}
lintMigrationSQL :: Text -> Either [LintError] ()
lintMigrationSQL = lintMigrationSQLWith defaultLintConfig

{- | Lint raw SQL migration text with custom configuration.
Analyzes lock timeouts, concurrent index creation, and destructive operations.
-}
lintMigrationSQLWith :: LintConfig -> Text -> Either [LintError] ()
lintMigrationSQLWith cfg rawSql =
  let stmts = splitStatements rawSql
      isOnlyIndexStmts = all isIndexCreationStmt (filter (not . isSetStmt) stmts)
      lockTimeoutErrors =
        if cfgRequireLockTimeout cfg && not isOnlyIndexStmts && not (null stmts)
          then checkLockTimeoutPreamble (cfgMaxLockTimeoutSec cfg) rawSql
          else []
      stmtErrors = concatMap (lintSingleStmtSQL cfg) stmts
      allErrors = lockTimeoutErrors ++ stmtErrors
   in if null allErrors
        then Right ()
        else Left allErrors

-- | Check whether an explicit lock_timeout preamble is present and within bounds.
checkLockTimeoutPreamble :: Int -> Text -> [LintError]
checkLockTimeoutPreamble maxSec sql =
  let upper = T.toUpper sql
   in if "LOCK_TIMEOUT" `T.isInfixOf` upper
        then case extractLockTimeoutSec sql of
          Just sec
            | sec > maxSec -> [LockTimeoutTooHigh sec]
            | otherwise -> []
          Nothing -> []
        else [MissingLockTimeoutPreamble]

-- | Extract lock_timeout value in seconds from SET LOCAL lock_timeout = 'Xs' or 'Xms'.
extractLockTimeoutSec :: Text -> Maybe Int
extractLockTimeoutSec sql =
  let upper = T.toUpper sql
   in case T.breakOn "LOCK_TIMEOUT" upper of
        (_, rest)
          | not (T.null rest) ->
              let afterEquals = T.dropWhile (\c -> c == ' ' || c == '=') (T.drop 12 rest)
                  valPart = T.takeWhile (\c -> c /= ';' && c /= '\n' && c /= '\r') afterEquals
                  cleanVal = T.filter (\c -> c /= '\'' && c /= '"' && c /= ' ') valPart
                  digits = T.takeWhile isDigit cleanVal
                  unitStr = T.toLower (T.dropWhile isDigit cleanVal)
               in case readMaybe (T.unpack digits) of
                    Just n ->
                      if unitStr == "s" || T.null unitStr
                        then Just n
                        else
                          if unitStr == "ms"
                            then Just (max 1 (n `div` 1000))
                            else
                              if unitStr == "min"
                                then Just (n * 60)
                                else Just n
                    Nothing -> Nothing
        _ -> Nothing

-- | Split SQL script into individual statements by semicolon.
splitStatements :: Text -> [Text]
splitStatements raw =
  [ T.strip s
  | s <- T.splitOn ";" raw
  , not (T.null (T.strip s))
  ]

-- | Check if statement is a SET configuration command.
isSetStmt :: Text -> Bool
isSetStmt stmt =
  let upper = T.toUpper (T.strip stmt)
   in "SET " `T.isPrefixOf` upper

-- | Check if statement is an index creation statement.
isIndexCreationStmt :: Text -> Bool
isIndexCreationStmt stmt =
  let upper = T.toUpper (T.strip stmt)
   in "CREATE INDEX" `T.isPrefixOf` upper
        || "CREATE UNIQUE INDEX" `T.isPrefixOf` upper

-- | Lint a single SQL statement for safety violations.
lintSingleStmtSQL :: LintConfig -> Text -> [LintError]
lintSingleStmtSQL cfg stmt =
  let upper = T.toUpper stmt
      cleaned = T.unwords (T.words upper)
      indexErrors =
        if cfgRequireConcurrentIndexes cfg
          then checkIndexStmt stmt cleaned
          else []
      destructiveErrors =
        if not (cfgAllowDestructive cfg)
          then checkDestructiveStmt stmt cleaned
          else []
      constraintErrors =
        if cfgRequireTwoPhaseConstraints cfg
          then checkConstraintStmt stmt cleaned
          else []
      typeChangeErrors = checkTypeChangeStmt stmt cleaned
      maintenanceErrors = checkMaintenanceStmt cleaned
   in indexErrors ++ destructiveErrors ++ constraintErrors ++ typeChangeErrors ++ maintenanceErrors

-- | Hazard 1: Check for CREATE INDEX without CONCURRENTLY.
checkIndexStmt :: Text -> Text -> [LintError]
checkIndexStmt raw cleaned =
  if ("CREATE INDEX" `T.isPrefixOf` cleaned || "CREATE UNIQUE INDEX" `T.isPrefixOf` cleaned)
    && not ("CONCURRENTLY" `T.isInfixOf` cleaned)
    then
      let iName = extractIndexNameFromSQL raw
       in [IndexCreatedWithoutConcurrently iName]
    else []

-- | Extract index name from CREATE [UNIQUE] INDEX [CONCURRENTLY] idxName ON ...
extractIndexNameFromSQL :: Text -> Text
extractIndexNameFromSQL raw =
  let w = T.words raw
      skipTokens = ["CREATE", "create", "UNIQUE", "unique", "INDEX", "index", "CONCURRENTLY", "concurrently"]
      rest = dropWhile (\tok -> T.toUpper tok `elem` skipTokens) w
   in case rest of
        (name : _) -> T.filter (\c -> c /= '"' && c /= ';') name
        [] -> "unknown_index"

-- | Hazard 5 & 6: Check for DROP TABLE or DROP COLUMN without safety flag.
checkDestructiveStmt :: Text -> Text -> [LintError]
checkDestructiveStmt raw cleaned
  | "DROP TABLE" `T.isInfixOf` cleaned =
      let tName = extractObjectNameAfter "DROP TABLE" raw
       in [DestructiveDropTable tName]
  | "DROP COLUMN" `T.isInfixOf` cleaned =
      let tName = extractTableNameFromAlter raw
          cName = extractObjectNameAfter "DROP COLUMN" raw
       in [DestructiveDropColumn tName cName]
  | otherwise = []

-- | Hazard 4: Check for foreign keys or check constraints added without NOT VALID.
checkConstraintStmt :: Text -> Text -> [LintError]
checkConstraintStmt raw cleaned
  | "ADD CONSTRAINT" `T.isInfixOf` cleaned
      && ("FOREIGN KEY" `T.isInfixOf` cleaned || "CHECK" `T.isInfixOf` cleaned)
      && not ("NOT VALID" `T.isInfixOf` cleaned) =
      let tName = extractTableNameFromAlter raw
          conName = extractObjectNameAfter "ADD CONSTRAINT" raw
       in [UnsafeForeignKeyAdded tName conName]
  | "ADD COLUMN" `T.isInfixOf` cleaned
      && "NOT NULL" `T.isInfixOf` cleaned
      && not ("DEFAULT" `T.isInfixOf` cleaned) =
      let tName = extractTableNameFromAlter raw
          cName = extractObjectNameAfter "ADD COLUMN" raw
       in [UnsafeNotNullAdded tName cName]
  | otherwise = []

-- | Hazard 7: Column type conversions triggering full table rewrites.
checkTypeChangeStmt :: Text -> Text -> [LintError]
checkTypeChangeStmt raw cleaned
  | "ALTER COLUMN" `T.isInfixOf` cleaned && "TYPE" `T.isInfixOf` cleaned =
      let tName = extractTableNameFromAlter raw
          cName = extractObjectNameAfter "ALTER COLUMN" raw
       in [UnsafeColumnTypeChange tName cName]
  | otherwise = []

-- | Hazard 11: Vacuum full or cluster usage in migration transactions.
checkMaintenanceStmt :: Text -> [LintError]
checkMaintenanceStmt cleaned
  | "VACUUM FULL" `T.isInfixOf` cleaned = [UnsafeMaintenanceCommand "VACUUM FULL"]
  | "CLUSTER " `T.isPrefixOf` cleaned = [UnsafeMaintenanceCommand "CLUSTER"]
  | otherwise = []

-- | Extract table name from ALTER TABLE "tbl" ...
extractTableNameFromAlter :: Text -> Text
extractTableNameFromAlter raw =
  case T.breakOn "ALTER TABLE " (T.strip raw) of
    (_, rest)
      | not (T.null rest) ->
          let afterAlter = T.drop 12 rest
           in case T.words afterAlter of
                (token : _) -> T.filter (\c -> c /= '"' && c /= ';') token
                [] -> "unknown_table"
    _ -> "unknown_table"

-- | Extract identifier token following a specific keyword phrase.
extractObjectNameAfter :: Text -> Text -> Text
extractObjectNameAfter phrase raw =
  case T.breakOn phrase raw of
    (_, rest)
      | not (T.null rest) ->
          let after = T.drop (T.length phrase) rest
              wordsAfter = T.words (T.strip after)
           in case wordsAfter of
                (w : _) -> T.filter (\c -> c /= '"' && c /= ';') w
                [] -> "unknown_object"
    _ -> "unknown_object"

{- | Lint typed DDL AST statements against zero-downtime safety rules.
Directly inspects IndexDef, AddConstraintStmt, and DropTableStmt nodes.
-}
lintDDLStatements :: LintConfig -> [DDLStatement] -> Either [LintError] ()
lintDDLStatements cfg stmts =
  let errors = concatMap (lintSingleStatement cfg) stmts
   in if null errors
        then Right ()
        else Left errors

-- | Lint a single DDLStatement AST node.
lintSingleStatement :: LintConfig -> DDLStatement -> [LintError]
lintSingleStatement cfg = \case
  CreateIndexStmt idx ->
    [ IndexCreatedWithoutConcurrently (unIndexName (idxName idx))
    | cfgRequireConcurrentIndexes cfg && not (idxIsConcurrent idx)
    ]
  DropTableStmt name _ ->
    [ DestructiveDropTable (unTableName name)
    | not (cfgAllowDestructive cfg)
    ]
  DropColumnStmt tbl col _ ->
    [ DestructiveDropColumn (unTableName tbl) (unColumnName col)
    | not (cfgAllowDestructive cfg)
    ]
  AddConstraintStmt tbl con notValid ->
    if cfgRequireTwoPhaseConstraints cfg && not notValid
      then case con of
        ForeignKeyConstraint conName _ _ _ _ _ ->
          [UnsafeForeignKeyAdded (unTableName tbl) (unConstraintName conName)]
        CheckConstraint conName _ ->
          [UnsafeForeignKeyAdded (unTableName tbl) (unConstraintName conName)]
        _ -> []
      else []
  AddColumnStmt tbl col ->
    [ UnsafeNotNullAdded (unTableName tbl) (unColumnName (colName col))
    | colNullability col == NotNull && isNothing (colDefault col)
    ]
  AlterColumnTypeStmt tbl col _ _ ->
    [UnsafeColumnTypeChange (unTableName tbl) (unColumnName col)]
  _ -> []

{- | Law 2: Two-Phase Constraint Safety.
Splits a constraint addition into safe 2-step statements:
Step 1: ADD CONSTRAINT ... NOT VALID (acquires short lock, zero table scan).
Step 2: VALIDATE CONSTRAINT ... (background validation scan without write lock).
Primary keys and unique constraints are emitted in a single statement without NOT VALID.
-}
splitTwoPhaseConstraint :: TableName -> ConstraintDef -> [DDLStatement]
splitTwoPhaseConstraint tbl con =
  case con of
    ForeignKeyConstraint {} ->
      [ AddConstraintStmt tbl con True
      , ValidateConstraintStmt tbl (constraintDefName con)
      ]
    CheckConstraint {} ->
      [ AddConstraintStmt tbl con True
      , ValidateConstraintStmt tbl (constraintDefName con)
      ]
    _ -> [AddConstraintStmt tbl con False]

-- | Render 2-phase constraint addition into two standalone SQL statements.
renderTwoPhaseConstraint :: (IsString s) => TableName -> ConstraintDef -> (s, s)
renderTwoPhaseConstraint tbl con =
  case splitTwoPhaseConstraint tbl con of
    [step1, step2] ->
      ( fromString (T.unpack (renderDDLStatement step1))
      , fromString (T.unpack (renderDDLStatement step2))
      )
    _ -> ("", "")

{- | Prepend a 'lock_timeout' preamble if table-altering statements are present
and an explicit lock_timeout is not already specified.
-}
attachLockTimeoutPreamble :: Text -> Text
attachLockTimeoutPreamble sql
  | T.null (T.strip sql) = sql
  | "LOCK_TIMEOUT" `T.isInfixOf` T.toUpper sql = sql
  | isOnlyIndexStmts = sql
  | otherwise = "SET LOCAL lock_timeout = '2s';\n\n" <> sql
  where
    stmts = splitStatements sql
    isOnlyIndexStmts = all isIndexCreationStmt (filter (not . isSetStmt) stmts)

-- | Extract constraint name from ConstraintDef.
constraintDefName :: ConstraintDef -> ConstraintName
constraintDefName = \case
  PrimaryKeyConstraint name _ -> name
  UniqueConstraint name _ -> name
  ForeignKeyConstraint name _ _ _ _ _ -> name
  CheckConstraint name _ -> name
