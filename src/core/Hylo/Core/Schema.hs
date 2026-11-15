{- |
Module      : Hylo.Core.Schema
Description : Structural schema metadata and catalog descriptors
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Structural schema catalog definitions (TableCatalogDef, ColumnCatalogDef) for
diffing, DDL generation, migrations, and AI tooling.
-}
module Hylo.Core.Schema
  ( -- * Re-exports from Table
    TableSchema (..)
  , tableSchema
  , column
  , nullableColumn
  , primaryKey
  , foreignKey
  , describeCol

    -- * Catalog AST Descriptors
  , TableCatalogDef (..)
  , ColumnCatalogDef (..)
  , toColumnCatalogDef
  , tableCatalog
  ) where

import Data.Text (Text)
import Hylo.Core.Context (ColSpec (..))
import Hylo.Core.Table
  ( TableSchema (..)
  , column
  , describeCol
  , foreignKey
  , nullableColumn
  , primaryKey
  , tableSchema
  )

-- | Structural catalog definition for a database table.
data TableCatalogDef = TableCatalogDef
  { catalogTableName :: !Text
  , catalogColumns :: ![ColumnCatalogDef]
  , catalogPrimaryKey :: ![Text]
  }
  deriving (Eq, Show)

-- | Structural catalog definition for a single column.
data ColumnCatalogDef = ColumnCatalogDef
  { colDefName :: !Text
  , colDefSqlType :: !Text
  , colDefNullable :: !Bool
  , colDefPrimaryKey :: !Bool
  , colDefDescription :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | Convert a column 'ColSpec' into a 'ColumnCatalogDef'.
toColumnCatalogDef :: ColSpec a -> ColumnCatalogDef
toColumnCatalogDef spec =
  ColumnCatalogDef
    { colDefName = colName spec
    , colDefSqlType = colSqlType spec
    , colDefNullable = colIsNullable spec
    , colDefPrimaryKey = colIsPrimaryKey spec
    , colDefDescription = colDescription spec
    }

-- | Construct a 'TableCatalogDef' from column catalog definitions.
tableCatalog :: Text -> [ColumnCatalogDef] -> TableCatalogDef
tableCatalog name cols =
  TableCatalogDef
    { catalogTableName = name
    , catalogColumns = cols
    , catalogPrimaryKey =
        [colDefName col | col <- cols, colDefPrimaryKey col]
    }
