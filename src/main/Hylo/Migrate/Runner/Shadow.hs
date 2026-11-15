{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.Migrate.Runner.Shadow
Description : Ephemeral Shadow Database Verification Runner for Reversibility & Idempotence
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Migrate.Runner.Shadow
  ( -- * Verification Results
    ShadowVerificationResult (..)

    -- * Shadow Verification Runners
  , verifyMigrationSequence
  , verifyCatalogDiff
  , verifyReversibilityAndIdempotence

    -- * Statement Simulation
  , applyDDLStatement
  , applyDDLSequence
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Migrate.AST
import Hylo.Migrate.Catalog
import Hylo.Migrate.Diff

-- | Outcome of shadow database verification across up -> down -> up cycles.
data ShadowVerificationResult
  = ShadowSuccess
  | ShadowUpFailed !Text
  | ShadowDownFailed !Text
  | ShadowIdempotenceFailed !Text
  deriving (Eq, Show, Generic, NFData)

{- | Verify a bidirectional migration sequence against a baseline catalog.
Executes the full 3-phase shadow verification cycle:
1. Apply up.sql: S0 -> S1
2. Apply down.sql: S1 -> S0' (must match S0)
3. Re-apply up.sql: S0' -> S1' (must match S1)
-}
verifyMigrationSequence :: Catalog -> [DDLStatement] -> [DDLStatement] -> ShadowVerificationResult
verifyMigrationSequence s0 upStmts downStmts =
  case applyDDLSequence upStmts s0 of
    Left err -> ShadowUpFailed ("Failed to apply up statements: " <> err)
    Right s1 ->
      case applyDDLSequence downStmts s1 of
        Left err -> ShadowDownFailed ("Failed to apply down statements: " <> err)
        Right s0'
          | s0' /= s0 ->
              ShadowDownFailed
                ( "Rollback state S0' does not match initial baseline S0.\nDiff: "
                    <> T.pack (show (computeCatalogDiff s0 s0'))
                )
          | otherwise ->
              case applyDDLSequence upStmts s0' of
                Left err -> ShadowIdempotenceFailed ("Failed to re-apply up statements: " <> err)
                Right s1'
                  | s1' /= s1 ->
                      ShadowIdempotenceFailed
                        ( "Re-applied state S1' does not match first-pass target state S1.\nDiff: "
                            <> T.pack (show (computeCatalogDiff s1 s1'))
                        )
                  | otherwise -> ShadowSuccess

-- | Verify that a CatalogDiff produces fully reversible and idempotent migrations.
verifyCatalogDiff :: Catalog -> CatalogDiff -> ShadowVerificationResult
verifyCatalogDiff s0 diff =
  let (upStmts, downStmts) = diffToStatements diff
   in verifyMigrationSequence s0 upStmts downStmts

-- | Convenience verification helper directly taking source and target catalogs.
verifyReversibilityAndIdempotence :: Catalog -> Catalog -> ShadowVerificationResult
verifyReversibilityAndIdempotence source target =
  let diff = computeCatalogDiff source target
   in verifyCatalogDiff source diff

-- | Apply an ordered sequence of DDLStatements in-memory against a Catalog.
applyDDLSequence :: [DDLStatement] -> Catalog -> Either Text Catalog
applyDDLSequence [] cat = Right cat
applyDDLSequence (stmt : rest) cat =
  case applyDDLStatement stmt cat of
    Left err -> Left err
    Right cat' -> applyDDLSequence rest cat'

-- | Purely simulate the effect of a single DDL statement on an in-memory Catalog.
applyDDLStatement :: DDLStatement -> Catalog -> Either Text Catalog
applyDDLStatement stmt cat = case stmt of
  CreateTableStmt tbl ->
    if Map.member (tblName tbl) (catalogTables cat)
      then Left ("Table already exists: " <> unTableName (tblName tbl))
      else Right (addTable tbl cat)
  DropTableStmt name _cascade ->
    if Map.member name (catalogTables cat)
      then Right (removeTable name cat)
      else Left ("Table does not exist to drop: " <> unTableName name)
  AddColumnStmt tName col ->
    case lookupTable tName cat of
      Nothing -> Left ("Table does not exist for column addition: " <> unTableName tName)
      Just tbl ->
        let cols = tblColumns tbl
         in if any (\c -> colName c == colName col) cols
              then Left ("Column already exists: " <> unColumnName (colName col))
              else Right (addTable (tbl {tblColumns = cols ++ [col]}) cat)
  DropColumnStmt tName targetColName _cascade ->
    case lookupTable tName cat of
      Nothing -> Left ("Table does not exist for column drop: " <> unTableName tName)
      Just tbl ->
        let cols = tblColumns tbl
            remaining = filter (\c -> colName c /= targetColName) cols
         in Right (addTable (tbl {tblColumns = remaining}) cat)
  AlterColumnTypeStmt tName targetColName newType _ ->
    case lookupTable tName cat of
      Nothing -> Left ("Table does not exist for alter column: " <> unTableName tName)
      Just tbl ->
        let cols = tblColumns tbl
            updated = map (\c -> if colName c == targetColName then c {colType = newType} else c) cols
         in Right (addTable (tbl {tblColumns = updated}) cat)
  AlterColumnNullabilityStmt tName targetColName newNull ->
    case lookupTable tName cat of
      Nothing -> Left ("Table does not exist for alter column: " <> unTableName tName)
      Just tbl ->
        let cols = tblColumns tbl
            updated = map (\c -> if colName c == targetColName then c {colNullability = newNull} else c) cols
         in Right (addTable (tbl {tblColumns = updated}) cat)
  AlterColumnDefaultStmt tName targetColName newDef ->
    case lookupTable tName cat of
      Nothing -> Left ("Table does not exist for alter column: " <> unTableName tName)
      Just tbl ->
        let cols = tblColumns tbl
            updated = map (\c -> if colName c == targetColName then c {colDefault = newDef} else c) cols
         in Right (addTable (tbl {tblColumns = updated}) cat)
  CreateIndexStmt idx ->
    case lookupTable (idxTable idx) cat of
      Nothing -> Left ("Table does not exist for index creation: " <> unTableName (idxTable idx))
      Just tbl ->
        let idxs = tblIndexes tbl
         in Right (addTable (tbl {tblIndexes = idxs ++ [idx]}) cat)
  DropIndexStmt iName _ _ ->
    let updatedTables =
          Map.map
            ( \tbl ->
                tbl {tblIndexes = filter (\i -> idxName i /= iName) (tblIndexes tbl)}
            )
            (catalogTables cat)
     in Right (cat {catalogTables = updatedTables})
  AddConstraintStmt tName con _notValid ->
    case lookupTable tName cat of
      Nothing -> Left ("Table does not exist for constraint addition: " <> unTableName tName)
      Just tbl ->
        let cons = tblConstraints tbl
         in Right (addTable (tbl {tblConstraints = cons ++ [con]}) cat)
  ValidateConstraintStmt _tName _conName ->
    -- Validation preserves existing constraint state in catalog
    Right cat
  DropConstraintStmt tName conName _cascade ->
    case lookupTable tName cat of
      Nothing -> Left ("Table does not exist for constraint drop: " <> unTableName tName)
      Just tbl ->
        let cons = tblConstraints tbl
            remaining = filter (\c -> constraintDefName c /= conName) cons
         in Right (addTable (tbl {tblConstraints = remaining}) cat)
  RawDDLStmt _ ->
    Right cat

-- | Extract constraint name.
constraintDefName :: ConstraintDef -> ConstraintName
constraintDefName = \case
  PrimaryKeyConstraint name _ -> name
  UniqueConstraint name _ -> name
  ForeignKeyConstraint name _ _ _ _ _ -> name
  CheckConstraint name _ -> name
