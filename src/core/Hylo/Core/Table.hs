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
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Hylo.Core.Context (ColSpec (..), Ctx (..))
import Hylo.Core.Expr (QExpr, colExpr)

-- | Class for HKD table schemas that can project their fields into 'Expr context.
class Table (table :: Ctx -> Type) where
  toTableExpr :: Text -> table 'Meta -> table 'Expr
  tableExprColumns :: table 'Expr -> [Text]
  tableExprColumns _ = []
  tableColumnsMeta :: table 'Meta -> [(Text, Text)]
  tableColumnsMeta _ = []

-- | Construct a typed SQL column expression for an alias and a column specification.
colSpecExpr :: Text -> ColSpec a -> QExpr a
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
column :: Text -> Text -> ColSpec a
column name sqlType =
  ColSpec
    { colName = name
    , colSqlType = sqlType
    , colIsNullable = False
    , colIsPrimaryKey = False
    , colDescription = Nothing
    }

-- | Construct a nullable column descriptor.
nullableColumn :: Text -> Text -> ColSpec (Maybe a)
nullableColumn name sqlType =
  ColSpec
    { colName = name
    , colSqlType = sqlType
    , colIsNullable = True
    , colIsPrimaryKey = False
    , colDescription = Nothing
    }

-- | Construct a primary key column descriptor.
primaryKey :: Text -> Text -> ColSpec a
primaryKey name sqlType =
  ColSpec
    { colName = name
    , colSqlType = sqlType
    , colIsNullable = False
    , colIsPrimaryKey = True
    , colDescription = Nothing
    }

-- | Construct a foreign key column descriptor.
foreignKey :: Text -> Text -> ColSpec a
foreignKey name sqlType =
  ColSpec
    { colName = name
    , colSqlType = sqlType
    , colIsNullable = False
    , colIsPrimaryKey = False
    , colDescription = Nothing
    }

-- | Attach an AI/documentation description to a column specification.
describeCol :: Text -> ColSpec a -> ColSpec a
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
