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
  , EnumTypeChange (..)

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
  , diffEnumLabelHazards
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Migrate.AST
import Hylo.Migrate.Catalog
import Hylo.Migrate.Linter (LintError (..), splitTwoPhaseConstraint)

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
  , cdEnumChanges :: ![EnumTypeChange]
  }
  deriving (Eq, Show, Generic, NFData)

{- | Evolution of a single PostgreSQL enum type between two catalogs
(User Story 19). Label additions emit @ALTER TYPE ... ADD VALUE@; label
removals are impossible in PostgreSQL and surface as linter hazards.
-}
data EnumTypeChange = EnumTypeChange
  { etcType :: !Text
  , etcSourceLabels :: ![Text]
  , etcTargetLabels :: ![Text]
  , etcIsNewType :: !Bool
  , etcUsages :: ![(Text, Text)]
  }
  deriving (Eq, Show, Generic, NFData)

-- | Empty diff representing two identical catalogs.
emptyCatalogDiff :: CatalogDiff
emptyCatalogDiff =
  CatalogDiff
    { cdAddedTables = []
    , cdDroppedTables = []
    , cdModifiedTables = []
    , cdEnumChanges = []
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
        , cdEnumChanges = diffEnumTypes source target
        }

{- | Compute enum type evolution between two catalogs. A type is new when it
appears in the target with no source evidence (neither in @catalogEnums@ nor
as a table column). Types known to the source only through introspected
@TypeCustom@ columns have unknown label sets, so no additions or removals are
assumed for them. Usage sites are tracked per (table, column) so removal
hazards can name the offending column.
-}
diffEnumTypes :: Catalog -> Catalog -> [EnumTypeChange]
diffEnumTypes source target =
  let tgtUsages =
        [ (unTableName (tblName tbl), unColumnName (colName c), ty, ls)
        | tbl <- listTables target
        , c <- tblColumns tbl
        , EnumType ty ls <- [colType c]
        ]
      usagesByType =
        Map.fromListWith
          (flip (++))
          [(ty, [(t, cl)]) | (t, cl, ty, _) <- tgtUsages]
      labelsByType = Map.fromList [(ty, ls) | (_, _, ty, ls) <- tgtUsages]
      srcEnumCols =
        [ (ty, ls)
        | tbl <- listTables source
        , c <- tblColumns tbl
        , EnumType ty ls <- [colType c]
        ]
      srcCustomTypes = [ty | tbl <- listTables source, c <- tblColumns tbl, TypeCustom ty <- [colType c]]
      changes =
        [ change
        | (ty, ls) <- Map.toList labelsByType
        , let usages = Map.findWithDefault [] ty usagesByType
        , change <- case (Map.lookup ty (catalogEnums source), [sl | (sty, sl) <- srcEnumCols, sty == ty]) of
            (Just sl, _) -> mkChange ty sl ls usages
            (Nothing, sl : _) -> mkChange ty sl ls usages
            (Nothing, [])
              | ty `elem` srcCustomTypes -> []
              | otherwise -> [EnumTypeChange ty [] ls True usages]
        ]
   in changes
  where
    mkChange ty sl ls usages
      | sl == ls = []
      | otherwise = [EnumTypeChange ty sl ls False usages]

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

{- | Check if column type, nullability, or default value has changed.
Enum columns compare by type name only: label evolution is tracked through
'EnumTypeChange' records rather than column diffs, and introspected custom
types unify with same-named enum types (User Story 19).
-}
isColumnModified :: ColumnDef -> ColumnDef -> Bool
isColumnModified oldCol newCol =
  not (sameColumnType (colType oldCol) (colType newCol))
    || colNullability oldCol /= colNullability newCol
    || colDefault oldCol /= colDefault newCol

-- | Enum-aware column type equality (User Story 19).
sameColumnType :: ColumnType -> ColumnType -> Bool
sameColumnType (EnumType a _) (EnumType b _) = a == b
sameColumnType (EnumType a _) (TypeCustom b) = a == b
sameColumnType (TypeCustom a) (EnumType b _) = a == b
sameColumnType a b = a == b

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
    , cdEnumChanges = map invertEnumChange (cdEnumChanges diff)
    }

{- | Invert a single enum type change. Source and target label sets swap and
the type is never treated as newly created in the inverted direction (the
inverted source is the forward target, where the type already exists).
-}
invertEnumChange :: EnumTypeChange -> EnumTypeChange
invertEnumChange ch =
  ch
    { etcSourceLabels = etcTargetLabels ch
    , etcTargetLabels = etcSourceLabels ch
    , etcIsNewType = False
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
      -- 4. Propagate enum label evolution into columns and the enum registry
      afterEnums = applyEnumChanges (cdEnumChanges diff) afterAdd
   in recomputeEnums afterEnums

{- | Rewrite all enum-typed columns to the target label set of their type and
refresh 'catalogEnums' from the resulting table definitions. Label evolution
never alters table structure, so it is applied uniformly across every table
referencing the type (User Story 19).
-}
applyEnumChanges :: [EnumTypeChange] -> Catalog -> Catalog
applyEnumChanges changes cat = cat {catalogTables = Map.map updTable (catalogTables cat)}
  where
    targetLabelsByType = Map.fromList [(etcType ch, etcTargetLabels ch) | ch <- changes]
    updTable tbl = tbl {tblColumns = map updColumn (tblColumns tbl)}
    updColumn c = case colType c of
      EnumType ty _ | Just ls <- Map.lookup ty targetLabelsByType -> c {colType = EnumType ty ls}
      _ -> c

-- | Recompute 'catalogEnums' from the enum-typed columns of all tables.
recomputeEnums :: Catalog -> Catalog
recomputeEnums cat =
  cat
    { catalogEnums =
        Map.fromList
          [ (ty, ls)
          | tbl <- Map.elems (catalogTables cat)
          , c <- tblColumns tbl
          , EnumType ty ls <- [colType c]
          ]
    }

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

{- | Convert a TableDef to creation statements (table and standalone indexes).
CREATE TYPE statements for the table's enum columns are emitted before the
CREATE TABLE statement (User Story 19).
-}
tableToUpStmts :: TableDef -> [DDLStatement]
tableToUpStmts tbl =
  let createTbl = CreateTableStmt tbl
      enumTypes =
        dedupEnumTypes
          [ (ty, ls)
          | c <- tblColumns tbl
          , EnumType ty ls <- [colType c]
          ]
      creates = [CreateEnumTypeStmt ty ls | (ty, ls) <- enumTypes]
   in creates ++ [createTbl]

-- | Deduplicate (type, labels) pairs by type name, keeping first occurrence.
dedupEnumTypes :: [(Text, [Text])] -> [(Text, [Text])]
dedupEnumTypes = go []
  where
    go _ [] = []
    go seen ((ty, ls) : rest)
      | ty `elem` seen = go seen rest
      | otherwise = (ty, ls) : go (ty : seen) rest

-- | Order-preserving list difference.
diffLabels :: (Eq a) => [a] -> [a] -> [a]
diffLabels xs ys = [x | x <- xs, x `notElem` ys]

{- | Convert a CatalogDiff into ordered lists of up and down DDL statements.
Added tables include their CREATE TABLE statement preceded by CREATE TYPE
statements for their enum columns. Enum label additions on existing types
emit ALTER TYPE ... ADD VALUE; label removals emit no DDL (PostgreSQL cannot
drop enum values) and surface as linter hazards via 'diffEnumLabelHazards'.
-}
diffToStatements :: CatalogDiff -> ([DDLStatement], [DDLStatement])
diffToStatements diff =
  let rawUp =
        concatMap tableToUpStmts (cdAddedTables diff)
          ++ concatMap tableDiffToUpStmts (cdModifiedTables diff)
          ++ map (\tbl -> DropTableStmt (tblName tbl) True) (cdDroppedTables diff)
      coveredCreates = [ty | CreateEnumTypeStmt ty _ <- rawUp]
      extraCreates =
        [ CreateEnumTypeStmt (etcType ch) (etcTargetLabels ch)
        | ch <- cdEnumChanges diff
        , etcIsNewType ch
        , etcType ch `notElem` coveredCreates
        ]
      addValues =
        [ AlterEnumTypeAddValueStmt (etcType ch) lbl
        | ch <- cdEnumChanges diff
        , not (etcIsNewType ch)
        , lbl <- diffLabels (etcTargetLabels ch) (etcSourceLabels ch)
        ]
      upStmts = extraCreates ++ addValues ++ dedupeEnumCreates rawUp

      addedNames = map (unTableName . tblName) (cdAddedTables diff)
      dropTableStmts = map (\tbl -> DropTableStmt (tblName tbl) True) (reverse (cdAddedTables diff))
      enumDrops =
        [ DropEnumTypeStmt (etcType ch) False
        | ch <- cdEnumChanges diff
        , etcIsNewType ch
        , all (\(tbl, _) -> tbl `elem` addedNames) (etcUsages ch)
        ]
      downStmts =
        dropTableStmts
          ++ enumDrops
          ++ concatMap tableDiffToDownStmts (reverse (cdModifiedTables diff))
          ++ concatMap tableToUpStmts (reverse (cdDroppedTables diff))
   in (upStmts, downStmts)

{- | Remove duplicate CREATE TYPE statements for the same type name, keeping
the first occurrence (multiple added tables may share an enum type).
-}
dedupeEnumCreates :: [DDLStatement] -> [DDLStatement]
dedupeEnumCreates = go []
  where
    go _ [] = []
    go seen (stmt@(CreateEnumTypeStmt ty _) : rest)
      | ty `elem` seen = go seen rest
      | otherwise = stmt : go (ty : seen) rest
    go seen (stmt : rest) = stmt : go seen rest

{- | Enum label removal hazards from a catalog diff. PostgreSQL cannot DROP
enum values, so migrations whose enum columns lose labels must be rewritten
manually and are flagged by the migration linter.
-}
diffEnumLabelHazards :: CatalogDiff -> [LintError]
diffEnumLabelHazards diff =
  [ UnsafeEnumLabelRemoved tbl col removed
  | ch <- cdEnumChanges diff
  , let removed = diffLabels (etcSourceLabels ch) (etcTargetLabels ch)
  , not (null removed)
  , (tbl, col) <- etcUsages ch
  ]

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
