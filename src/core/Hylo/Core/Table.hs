{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      : Hylo.Core.Table
Description : TableSchema and column definition combinators
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.Table
  ( TableSchema (..)
  , tableSchema
  , Table (..)
  , colSpecExpr
  , column
  , nullableColumn
  , primaryKey
  , foreignKey
  , describeCol
  , Changeset (..)
  , emptyChangeset
  , withUniqueKey
  , ColDef (..)
  , ColSpec
  , LogicalType (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Hylo.Core.Context
  ( ColDef (..)
  , ColSpec
  , Ctx (..)
  , LogicalType (..)
  , castLogicalType
  , renderLogicalType
  )
import Hylo.Core.Expr (QExpr, colExpr)

-- | Class for HKD table schemas that can project their fields into 'Expr context.
class Table (table :: Ctx -> Type) where
  toTableExpr :: Text -> table 'Meta -> table 'Expr
  tableExprColumns :: table 'Expr -> [Text]
  tableExprColumns _ = []
  tableColumnsMeta :: table 'Meta -> [(Text, Text)]
  tableColumnsMeta _ = []

-- | Construct a typed SQL column expression for an alias and a column specification.
colSpecExpr :: Text -> ColDef a -> QExpr a
colSpecExpr alias spec = colExpr alias (colName spec)

-- | Schema definition for an HKD table pairing the table name with its 'Meta record.
data TableSchema (table :: Ctx -> Type) = TableSchema
  { tableName :: !Text
  , tableMeta :: !(table 'Meta)
  }

-- | Construct a 'TableSchema' given a table name and its 'Meta record.
tableSchema :: Text -> table 'Meta -> TableSchema table
tableSchema = TableSchema

-- | Construct a standard non-nullable column descriptor.
column :: Text -> LogicalType a -> ColDef a
column name lt =
  ColDef
    { colName = name
    , colLogicalType = lt
    , colIsNullable = False
    , colIsPrimaryKey = False
    , colDescription = Nothing
    , colSqlType = renderLogicalType lt
    }

-- | Construct a nullable column descriptor.
nullableColumn :: Text -> LogicalType a -> ColDef (Maybe a)
nullableColumn name lt =
  ColDef
    { colName = name
    , colLogicalType = castLogicalType lt
    , colIsNullable = True
    , colIsPrimaryKey = False
    , colDescription = Nothing
    , colSqlType = renderLogicalType lt
    }

-- | Construct a primary key column descriptor.
primaryKey :: Text -> LogicalType a -> ColDef a
primaryKey name lt =
  ColDef
    { colName = name
    , colLogicalType = lt
    , colIsNullable = False
    , colIsPrimaryKey = True
    , colDescription = Nothing
    , colSqlType = renderLogicalType lt
    }

-- | Construct a foreign key column descriptor.
foreignKey :: Text -> LogicalType a -> ColDef a
foreignKey name lt =
  ColDef
    { colName = name
    , colLogicalType = lt
    , colIsNullable = False
    , colIsPrimaryKey = False
    , colDescription = Nothing
    , colSqlType = renderLogicalType lt
    }

-- | Attach an AI/documentation description to a column specification.
describeCol :: Text -> ColDef a -> ColDef a
describeCol desc colDef = colDef {colDescription = Just desc}

-- | Changeset descriptor for table records.
data Changeset (table :: Ctx -> Type) = Changeset
  { changesetPatch :: table 'Patch
  , changesetKey :: !Text
  }

-- | Construct an empty changeset.
emptyChangeset :: table 'Patch -> Changeset table
emptyChangeset patch = Changeset patch ""

-- | Attach a simulated unique key for conflict detection in transactions.
withUniqueKey :: Text -> Changeset table -> Changeset table
withUniqueKey key cs = cs {changesetKey = key}
