{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.Migrate.Catalog
Description : In-memory representation of database schema catalogs and combinators
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Migrate.Catalog
  ( -- * Catalog Representation
    Catalog (..)

    -- * Catalog Construction & Manipulation
  , emptyCatalog
  , addTable
  , removeTable
  , lookupTable
  , listTables

    -- * Bridge from TableSchema to Catalog (User Story 11)
  , AnyTableSchema (..)
  , tableSchemaToDef
  , anyTableSchemaToDef
  , schemasToCatalog
  , anySchemasToCatalog

    -- * Sample Catalog Definitions (Test Oracles)
  , sampleDocumentCatalogDef

    -- * Re-exported Combinators
  , (&)
  ) where

import Control.DeepSeq (NFData)
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.TypeLits (natVal)
import Hylo.Core.Context (ColDef (..), LogicalType (..))
import Hylo.Core.Table (AnyTableSchema (..), Table (..), TableSchema (..))
import Hylo.Migrate.AST
import Text.Read (readMaybe)

-- | Database schema catalog state containing all defined tables, extensions, and enums.
data Catalog = Catalog
  { catalogTables :: !(Map TableName TableDef)
  , catalogExtensions :: !(Set Text)
  , catalogEnums :: !(Map Text [Text])
  }
  deriving (Eq, Show, Generic, NFData)

-- | Construct an empty catalog with no tables, extensions, or enum types.
emptyCatalog :: Catalog
emptyCatalog =
  Catalog
    { catalogTables = Map.empty
    , catalogExtensions = Set.empty
    , catalogEnums = Map.empty
    }

-- | Insert or replace a table definition in the catalog.
addTable :: TableDef -> Catalog -> Catalog
addTable tbl cat =
  cat {catalogTables = Map.insert (tblName tbl) tbl (catalogTables cat)}

-- | Remove a table definition from the catalog by table name.
removeTable :: TableName -> Catalog -> Catalog
removeTable name cat =
  cat {catalogTables = Map.delete name (catalogTables cat)}

-- | Lookup a table definition in the catalog by table name.
lookupTable :: TableName -> Catalog -> Maybe TableDef
lookupTable name cat = Map.lookup name (catalogTables cat)

-- | List all table definitions currently registered in the catalog.
listTables :: Catalog -> [TableDef]
listTables = Map.elems . catalogTables

-- | Bridge an in-memory Haskell 'TableSchema' to a migration 'TableDef'.
tableSchemaToDef :: (Table table) => TableSchema table -> TableDef
tableSchemaToDef schema =
  let colDefs = tableColDefs (tableMeta schema)
      pkCols = [ColumnName c.colName | c <- colDefs, c.colIsPrimaryKey]
      pkConstraints = case pkCols of
        [] -> []
        cols -> [PrimaryKeyConstraint (ConstraintName (tableName schema <> "_pkey")) cols]
   in TableDef
        { tblName = TableName (tableName schema)
        , tblColumns = map colDefToColumnDef colDefs
        , tblIndexes = []
        , tblConstraints = pkConstraints
        , tblComment = Nothing
        }

-- | Bridge an existential 'AnyTableSchema' to a migration 'TableDef'.
anyTableSchemaToDef :: AnyTableSchema -> TableDef
anyTableSchemaToDef (AnyTableSchema s) = tableSchemaToDef s

-- | Construct a 'Catalog' from a list of 'AnyTableSchema' registries.
anySchemasToCatalog :: [AnyTableSchema] -> Catalog
anySchemasToCatalog schemas = schemasToCatalog (map anyTableSchemaToDef schemas)

-- | Construct a 'Catalog' from a list of 'TableDef' definitions.
schemasToCatalog :: [TableDef] -> Catalog
schemasToCatalog defs =
  Catalog
    { catalogTables = Map.fromList [(tblName def, def) | def <- defs]
    , catalogExtensions = Set.empty
    , catalogEnums =
        Map.fromList [(ty, ls) | def <- defs, c <- tblColumns def, EnumType ty ls <- [colType c]]
    }

-- | Convert a core 'ColDef ()' to an AST 'ColumnDef'.
colDefToColumnDef :: ColDef () -> ColumnDef
colDefToColumnDef c =
  ColumnDef
    { colName = ColumnName c.colName
    , colType =
        if c.colIsEnum
          then EnumType c.colName c.colEnumLabels
          else logicalAndSqlToColumnType c.colLogicalType c.colSqlType
    , colNullability = if c.colIsNullable then Nullable else NotNull
    , colDefault = Nothing
    , colIsPrimaryKey = c.colIsPrimaryKey
    , colComment = c.colDescription
    }

-- | Resolve logical type and SQL type string to AST 'ColumnType'.
logicalAndSqlToColumnType :: LogicalType a -> Text -> ColumnType
logicalAndSqlToColumnType lt rawSql =
  let sql = T.toLower (T.strip rawSql)
   in case lt of
        LInt64
          | sql == "integer" || sql == "int4" || sql == "int" -> TypeInteger
          | sql == "smallint" || sql == "int2" -> TypeSmallInt
          | otherwise -> TypeBigInt
        LText
          | "varchar(" `T.isPrefixOf` sql && ")" `T.isSuffixOf` sql ->
              let lenStr = T.dropEnd 1 (T.drop 8 sql)
               in TypeVarChar (readMaybe (T.unpack lenStr))
          | sql == "varchar" -> TypeVarChar Nothing
          | otherwise -> TypeText
        LBool -> TypeBoolean
        LUTCTime
          | sql == "timestamp" -> TypeTimestamp
          | sql == "timestamptz" -> TypeTimestamptz
          | sql == "date" -> TypeDate
          | sql == "time" -> TypeTime
          | otherwise -> TypeTimestamptz
        LUUID -> TypeUuid
        LJson
          | sql == "json" -> TypeJson
          | sql == "jsonb" -> TypeJsonb
          | otherwise -> TypeJsonb
        LVector p -> TypeVector (Just (fromIntegral (natVal p)))
        LCustom _ -> case sql of
          "smallint" -> TypeSmallInt
          "int2" -> TypeSmallInt
          "integer" -> TypeInteger
          "int4" -> TypeInteger
          "int" -> TypeInteger
          "bigint" -> TypeBigInt
          "int8" -> TypeBigInt
          "text" -> TypeText
          "varchar" -> TypeVarChar Nothing
          "boolean" -> TypeBoolean
          "bool" -> TypeBoolean
          "timestamp" -> TypeTimestamp
          "timestamptz" -> TypeTimestamptz
          "date" -> TypeDate
          "time" -> TypeTime
          "json" -> TypeJson
          "jsonb" -> TypeJsonb
          "uuid" -> TypeUuid
          other
            | "vector(" `T.isPrefixOf` other && ")" `T.isSuffixOf` other ->
                let dimStr = T.dropEnd 1 (T.drop 7 other)
                 in TypeVector (readMaybe (T.unpack dimStr))
            | "varchar(" `T.isPrefixOf` other && ")" `T.isSuffixOf` other ->
                let lenStr = T.dropEnd 1 (T.drop 8 other)
                 in TypeVarChar (readMaybe (T.unpack lenStr))
            | otherwise -> TypeCustom rawSql

{- | Standard sample catalog definition for documents with vector embedding and HNSW index.
Matches Oracle specification 18.4.
-}
sampleDocumentCatalogDef :: TableDef
sampleDocumentCatalogDef =
  TableDef
    { tblName = TableName "documents"
    , tblColumns =
        [ ColumnDef
            { colName = ColumnName "id"
            , colType = TypeBigInt
            , colNullability = NotNull
            , colDefault = Nothing
            , colIsPrimaryKey = True
            , colComment = Just "Primary identifier"
            }
        , ColumnDef
            { colName = ColumnName "title"
            , colType = TypeText
            , colNullability = NotNull
            , colDefault = Nothing
            , colIsPrimaryKey = False
            , colComment = Just "Document title"
            }
        , ColumnDef
            { colName = ColumnName "content"
            , colType = TypeText
            , colNullability = Nullable
            , colDefault = Nothing
            , colIsPrimaryKey = False
            , colComment = Just "Document raw text body"
            }
        , ColumnDef
            { colName = ColumnName "embedding"
            , colType = TypeVector (Just 1536)
            , colNullability = Nullable
            , colDefault = Nothing
            , colIsPrimaryKey = False
            , colComment = Just "OpenAI text-embedding-3-small vector (1536-dim)"
            }
        ]
    , tblIndexes =
        [ IndexDef
            { idxName = IndexName "docs_embedding_hnsw_idx"
            , idxTable = TableName "documents"
            , idxColumns = [ColumnName "embedding"]
            , idxMethod = HNSW
            , idxIsUnique = False
            , idxIsConcurrent = False
            , idxWhereClause = Nothing
            , idxStorageParams = [("m", "16"), ("ef_construction", "64")]
            , idxOpClass = Just "vector_cosine_ops"
            }
        ]
    , tblConstraints =
        [ PrimaryKeyConstraint (ConstraintName "documents_pkey") [ColumnName "id"]
        ]
    , tblComment = Just "Indexed document corpus with pgvector HNSW search"
    }
