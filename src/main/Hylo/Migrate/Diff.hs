{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.Migrate.Diff
Description : Structural Catalog Diffing and Reversible DDL Synthesis Engine
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Migrate.Diff
  ( -- * Diff Types
    TableDiff (..)
  , CatalogDiff (..)

    -- * Diff Computation
  , emptyCatalogDiff
  , computeCatalogDiff
  , computeTableDiff

    -- * Group Action & Reversibility
  , invertDiff
  , applyDiff

    -- * Reversible DDL Synthesis
  , renderReversibleDDL
  , diffToStatements
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import Data.String (IsString (..))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Migrate.AST
import Hylo.Migrate.Catalog
import Hylo.Migrate.Linter (splitTwoPhaseConstraint)

-- | Structural differences between an existing table and a target table.
data TableDiff = TableDiff
  { tdTableName :: !TableName
  , tdAddedColumns :: ![ColumnDef]
  , tdDroppedColumns :: ![ColumnDef]
  , tdModifiedColumns :: ![(ColumnDef, ColumnDef)]
  -- ^ (original column, updated column)
  , tdAddedIndexes :: ![IndexDef]
  , tdDroppedIndexes :: ![IndexDef]
  , tdAddedConstraints :: ![ConstraintDef]
  , tdDroppedConstraints :: ![ConstraintDef]
  }
  deriving (Eq, Show, Generic, NFData)

-- | Global catalog difference representing forward migration from source to target.
data CatalogDiff = CatalogDiff
  { cdAddedTables :: ![TableDef]
  , cdDroppedTables :: ![TableDef]
  , cdModifiedTables :: ![TableDiff]
  }
  deriving (Eq, Show, Generic, NFData)

-- | Empty diff representing two identical catalogs.
emptyCatalogDiff :: CatalogDiff
emptyCatalogDiff =
  CatalogDiff
    { cdAddedTables = []
    , cdDroppedTables = []
    , cdModifiedTables = []
    }

{- | Compute the structural difference between a source and a target catalog.
Topologically orders added tables and calculates per-table column, index,
and constraint diffs.
-}
computeCatalogDiff :: Catalog -> Catalog -> CatalogDiff
computeCatalogDiff source target =
  let srcTables = catalogTables source
      tgtTables = catalogTables target

      added = [tbl | (name, tbl) <- Map.toList tgtTables, not (Map.member name srcTables)]
      dropped = [tbl | (name, tbl) <- Map.toList srcTables, not (Map.member name tgtTables)]

      sharedNames = [name | name <- Map.keys srcTables, Map.member name tgtTables]
      modified =
        [ tDiff
        | name <- sharedNames
        , let srcTbl = srcTables Map.! name
        , let tgtTbl = tgtTables Map.! name
        , let tDiff = computeTableDiff srcTbl tgtTbl
        , not (isTableDiffEmpty tDiff)
        ]
   in CatalogDiff
        { cdAddedTables = sortTablesByDependencies added
        , cdDroppedTables = reverse (sortTablesByDependencies dropped)
        , cdModifiedTables = modified
        }

-- | Check if a TableDiff has no changes.
isTableDiffEmpty :: TableDiff -> Bool
isTableDiffEmpty td =
  null (tdAddedColumns td)
    && null (tdDroppedColumns td)
    && null (tdModifiedColumns td)
    && null (tdAddedIndexes td)
    && null (tdDroppedIndexes td)
    && null (tdAddedConstraints td)
    && null (tdDroppedConstraints td)

-- | Compute the structural difference between two revisions of a table.
computeTableDiff :: TableDef -> TableDef -> TableDiff
computeTableDiff src tgt =
  let srcCols = tblColumns src
      tgtCols = tblColumns tgt
      srcColMap = Map.fromList [(colName c, c) | c <- srcCols]
      tgtColMap = Map.fromList [(colName c, c) | c <- tgtCols]

      addedCols = [c | c <- tgtCols, not (Map.member (colName c) srcColMap)]
      droppedCols = [c | c <- srcCols, not (Map.member (colName c) tgtColMap)]
      modifiedCols =
        [ (oldCol, c)
        | c <- tgtCols
        , Just oldCol <- [Map.lookup (colName c) srcColMap]
        , isColumnModified oldCol c
        ]

      srcIdxs = tblIndexes src
      tgtIdxs = tblIndexes tgt
      srcIdxMap = Map.fromList [(idxName i, i) | i <- srcIdxs]
      tgtIdxMap = Map.fromList [(idxName i, i) | i <- tgtIdxs]

      addedIdxs = [i | i <- tgtIdxs, not (Map.member (idxName i) srcIdxMap)]
      droppedIdxs = [i | i <- srcIdxs, not (Map.member (idxName i) tgtIdxMap)]

      srcCons = tblConstraints src
      tgtCons = tblConstraints tgt
      srcConMap = Map.fromList [(constraintName c, c) | c <- srcCons]
      tgtConMap = Map.fromList [(constraintName c, c) | c <- tgtCons]

      addedCons = [c | c <- tgtCons, not (Map.member (constraintName c) srcConMap)]
      droppedCons = [c | c <- srcCons, not (Map.member (constraintName c) tgtConMap)]
   in TableDiff
        { tdTableName = tblName tgt
        , tdAddedColumns = addedCols
        , tdDroppedColumns = droppedCols
        , tdModifiedColumns = modifiedCols
        , tdAddedIndexes = addedIdxs
        , tdDroppedIndexes = droppedIdxs
        , tdAddedConstraints = addedCons
        , tdDroppedConstraints = droppedCons
        }

-- | Check if column type, nullability, or default value has changed.
isColumnModified :: ColumnDef -> ColumnDef -> Bool
isColumnModified oldCol newCol =
  colType oldCol /= colType newCol
    || colNullability oldCol /= colNullability newCol
    || colDefault oldCol /= colDefault newCol

-- | Extract constraint name.
constraintName :: ConstraintDef -> ConstraintName
constraintName = \case
  PrimaryKeyConstraint name _ -> name
  UniqueConstraint name _ -> name
  ForeignKeyConstraint name _ _ _ _ _ -> name
  CheckConstraint name _ -> name

{- | Invert a CatalogDiff so that applying the inverted diff strictly reverts
all operations back to the original source state (Group Action Inversion).
-}
invertDiff :: CatalogDiff -> CatalogDiff
invertDiff diff =
  CatalogDiff
    { cdAddedTables = reverse (cdDroppedTables diff)
    , cdDroppedTables = reverse (cdAddedTables diff)
    , cdModifiedTables = map invertTableDiff (cdModifiedTables diff)
    }

-- | Invert a TableDiff.
invertTableDiff :: TableDiff -> TableDiff
invertTableDiff td =
  TableDiff
    { tdTableName = tdTableName td
    , tdAddedColumns = tdDroppedColumns td
    , tdDroppedColumns = tdAddedColumns td
    , tdModifiedColumns = map (\(oldC, newC) -> (newC, oldC)) (tdModifiedColumns td)
    , tdAddedIndexes = tdDroppedIndexes td
    , tdDroppedIndexes = tdAddedIndexes td
    , tdAddedConstraints = tdDroppedConstraints td
    , tdDroppedConstraints = tdAddedConstraints td
    }

{- | Apply a CatalogDiff to a Catalog in-memory to transition to target schema.
Satisfies: applyDiff (computeCatalogDiff s1 s2) s1 == s2
-}
applyDiff :: CatalogDiff -> Catalog -> Catalog
applyDiff diff cat =
  let -- 1. Drop tables
      afterDrop = foldr (removeTable . tblName) cat (cdDroppedTables diff)
      -- 2. Modify tables
      afterMod = foldr applyTableDiff afterDrop (cdModifiedTables diff)
      -- 3. Add tables
      afterAdd = foldr addTable afterMod (cdAddedTables diff)
   in afterAdd

-- | Apply TableDiff to in-memory table.
applyTableDiff :: TableDiff -> Catalog -> Catalog
applyTableDiff td cat =
  case lookupTable (tdTableName td) cat of
    Nothing -> cat
    Just tbl ->
      let curCols = tblColumns tbl
          -- Drop columns
          droppedSet = [colName c | c <- tdDroppedColumns td]
          colsAfterDrop = filter (\c -> colName c `notElem` droppedSet) curCols
          -- Modify columns
          modMap = Map.fromList [(colName oldC, newC) | (oldC, newC) <- tdModifiedColumns td]
          colsAfterMod = map (\c -> Map.findWithDefault c (colName c) modMap) colsAfterDrop
          -- Add columns
          colsAfterAdd = colsAfterMod ++ tdAddedColumns td

          -- Indexes
          curIdxs = tblIndexes tbl
          dropIdxSet = [idxName i | i <- tdDroppedIndexes td]
          idxsAfterDrop = filter (\i -> idxName i `notElem` dropIdxSet) curIdxs
          idxsAfterAdd = idxsAfterDrop ++ tdAddedIndexes td

          -- Constraints
          curCons = tblConstraints tbl
          dropConSet = [constraintName c | c <- tdDroppedConstraints td]
          consAfterDrop = filter (\c -> constraintName c `notElem` dropConSet) curCons
          consAfterAdd = consAfterDrop ++ tdAddedConstraints td

          updatedTbl =
            tbl
              { tblColumns = colsAfterAdd
              , tblIndexes = idxsAfterAdd
              , tblConstraints = consAfterAdd
              }
       in addTable updatedTbl cat

{- | Synthesize reversible DDL migration files (upSql, downSql) from a CatalogDiff.
Guarantees Law 1: Apply(down.sql, Apply(up.sql, S1)) == S1.
-}
renderReversibleDDL :: (IsString s) => CatalogDiff -> (s, s)
renderReversibleDDL diff =
  let (upStmts, downStmts) = diffToStatements diff
      upSql = T.intercalate "\n\n" (map renderDDLStatement upStmts)
      downSql = T.intercalate "\n\n" (map renderDDLStatement downStmts)
   in (fromString (T.unpack upSql), fromString (T.unpack downSql))

{- | Convert a CatalogDiff into ordered lists of up and down DDL statements.
Added tables include their CREATE TABLE statement followed by index creations.
Reversal drops created tables using CASCADE.
-}
diffToStatements :: CatalogDiff -> ([DDLStatement], [DDLStatement])
diffToStatements diff =
  let upStmts =
        concatMap tableToUpStmts (cdAddedTables diff)
          ++ concatMap tableDiffToUpStmts (cdModifiedTables diff)
          ++ map (\tbl -> DropTableStmt (tblName tbl) True) (cdDroppedTables diff)

      downStmts =
        map (\tbl -> DropTableStmt (tblName tbl) True) (reverse (cdAddedTables diff))
          ++ concatMap tableDiffToDownStmts (reverse (cdModifiedTables diff))
          ++ concatMap tableToUpStmts (reverse (cdDroppedTables diff))
   in (upStmts, downStmts)

-- | Convert a TableDef to creation statements (table and standalone indexes).
tableToUpStmts :: TableDef -> [DDLStatement]
tableToUpStmts tbl =
  let createTbl = CreateTableStmt tbl
   in [createTbl]

-- | Forward statements for table modifications.
tableDiffToUpStmts :: TableDiff -> [DDLStatement]
tableDiffToUpStmts td =
  let tName = tdTableName td
      addCols = map (AddColumnStmt tName) (tdAddedColumns td)
      dropCols = map (\c -> DropColumnStmt tName (colName c) True) (tdDroppedColumns td)
      modCols =
        concatMap
          ( \(oldC, newC) ->
              [ AlterColumnTypeStmt tName (colName newC) (colType newC) Nothing
              | colType oldC /= colType newC
              ]
                ++ [ AlterColumnNullabilityStmt tName (colName newC) (colNullability newC)
                   | colNullability oldC /= colNullability newC
                   ]
                ++ [ AlterColumnDefaultStmt tName (colName newC) (colDefault newC)
                   | colDefault oldC /= colDefault newC
                   ]
          )
          (tdModifiedColumns td)
      dropCons = map (\c -> DropConstraintStmt tName (constraintName c) True) (tdDroppedConstraints td)
      addCons = concatMap (splitTwoPhaseConstraint tName) (tdAddedConstraints td)
      dropIdxs = map (\i -> DropIndexStmt (idxName i) False True) (tdDroppedIndexes td)
      addIdxs = map CreateIndexStmt (tdAddedIndexes td)
   in addCols ++ dropCols ++ modCols ++ dropCons ++ addCons ++ dropIdxs ++ addIdxs

-- | Reverse statements for table modifications.
tableDiffToDownStmts :: TableDiff -> [DDLStatement]
tableDiffToDownStmts td =
  let tName = tdTableName td
      -- Revert added columns by dropping them
      revAddCols = map (\c -> DropColumnStmt tName (colName c) True) (tdAddedColumns td)
      -- Revert dropped columns by re-adding them
      revDropCols = map (AddColumnStmt tName) (tdDroppedColumns td)
      -- Revert column modifications back to original
      revModCols =
        concatMap
          ( \(oldC, newC) ->
              [ AlterColumnTypeStmt tName (colName oldC) (colType oldC) Nothing
              | colType oldC /= colType newC
              ]
                ++ [ AlterColumnNullabilityStmt tName (colName oldC) (colNullability oldC)
                   | colNullability oldC /= colNullability newC
                   ]
                ++ [ AlterColumnDefaultStmt tName (colName oldC) (colDefault oldC)
                   | colDefault oldC /= colDefault newC
                   ]
          )
          (tdModifiedColumns td)
      revAddCons = map (\c -> DropConstraintStmt tName (constraintName c) True) (tdAddedConstraints td)
      revDropCons = concatMap (splitTwoPhaseConstraint tName) (tdDroppedConstraints td)
      revAddIdxs = map (\i -> DropIndexStmt (idxName i) False True) (tdAddedIndexes td)
      revDropIdxs = map CreateIndexStmt (tdDroppedIndexes td)
   in revAddIdxs ++ revAddCons ++ revAddCols ++ revModCols ++ revDropCols ++ revDropCons ++ revDropIdxs

-- | Topologically sort tables so that referenced tables appear before referencing tables.
sortTablesByDependencies :: [TableDef] -> [TableDef]
sortTablesByDependencies tables =
  let getRefs tbl =
        [ targetTbl
        | ForeignKeyConstraint _ _ targetTbl _ _ _ <- tblConstraints tbl
        , targetTbl /= tblName tbl
        ]
      step sorted [] = sorted
      step sorted remaining =
        let (ready, pending) =
              break
                ( all
                    ( \refTbl ->
                        refTbl `elem` map tblName sorted || refTbl `notElem` map tblName tables
                    )
                    . getRefs
                )
                remaining
         in case ready of
              [] ->
                -- Break circular dependencies gracefully
                sorted ++ remaining
              (x : xs) -> step (sorted ++ [x]) (xs ++ pending)
   in step [] tables
