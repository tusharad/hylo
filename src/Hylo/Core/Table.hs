{-# LANGUAGE DataKinds #-}
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
  , column
  , nullableColumn
  , primaryKey
  , foreignKey
  , describeCol
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Hylo.Core.Context (ColSpec (..), Ctx (..))

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
