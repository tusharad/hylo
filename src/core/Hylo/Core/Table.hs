{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.Core.Table
Description : TableSchema, Table typeclass, patch entries, and column definition combinators
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.Table
  ( -- * Table Schema & Typeclass
    TableSchema (..)
  , tableSchema
  , AnyTableSchema (..)
  , Table (..)
  , RedisCachePolicy (..)
  , withRedisCache
  , withTableDescription

    -- * Column Specification Combinators
  , colSpecExpr
  , column
  , nullableColumn
  , primaryKey
  , foreignKey
  , optimisticLockColumn
  , describeCol
  , eraseColDef
  , ColDef (..)
  , ColSpec
  , LogicalType (..)

    -- * Patch Value Descriptors & Field Formatting
  , PatchVal (..)
  , ToFieldValue (..)
  , patchEntry
  , diffField
  ) where

import Control.DeepSeq (NFData)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Core.Context
  ( ColDef (..)
  , ColSpec
  , Ctx (..)
  , LogicalType (..)
  , PatchField (..)
  , castLogicalType
  , renderLogicalType
  )
import Hylo.Core.Expr (QExpr, colExpr)
import Hylo.Core.Key (Id (..))

-- | Patch value representation for SQL synthesis and dirty field maps.
data PatchVal
  = ValSetTo !Text
  | ValSetDefault
  deriving (Eq, Show, Generic)

-- | Class for HKD table schemas that can project their fields and metadata.
class Table (table :: Ctx -> Type) where
  toTableExpr :: Text -> table 'Meta -> table 'Expr
  tableExprColumns :: table 'Expr -> [Text]
  tableExprColumns _ = []
  tableColumnsMeta :: table 'Meta -> [(Text, Text)]
  tableColumnsMeta _ = []
  tableColDefs :: table 'Meta -> [ColDef ()]
  tableColDefs _ = []
  diffTable :: table 'Result -> table 'Result -> table 'Patch
  diffTable _ _ = errorWithoutStackTrace "diffTable: not implemented for table"
  patchEntries :: table 'Meta -> table 'Patch -> [(ColDef (), PatchVal)]
  patchEntries _ _ = []
  dirtyFieldsTable :: table 'Patch -> Map Text Text
  dirtyFieldsTable _ = Map.empty
  tableOptimisticLock :: table 'Result -> Maybe (Text, Int64)
  tableOptimisticLock _ = Nothing
  tablePrimaryKey :: table 'Result -> Maybe Text
  tablePrimaryKey _ = Nothing

-- | Erase the phantom type parameter of a column definition to '()'.
eraseColDef :: ColDef a -> ColDef ()
eraseColDef c = c {colLogicalType = castLogicalType (colLogicalType c)}

-- | Construct a patch entry for a column definition and patch field.
patchEntry :: (ToFieldValue a) => ColDef a -> PatchField a -> [(ColDef (), PatchVal)]
patchEntry col = \case
  SetTo v -> [(eraseColDef col, ValSetTo (toFieldValue v))]
  SetDefault -> [(eraseColDef col, ValSetDefault)]
  Unchanged -> []

-- | Pure diff combinator for a single field comparison.
diffField :: (Eq a) => a -> a -> PatchField a
diffField old new
  | old == new = Unchanged
  | otherwise = SetTo new

-- | Typeclass for formatting column values into textual representations.
class ToFieldValue a where
  toFieldValue :: a -> Text

instance {-# OVERLAPPABLE #-} (Show a) => ToFieldValue a where
  toFieldValue = T.pack . show

instance {-# OVERLAPPING #-} ToFieldValue Text where
  toFieldValue = id

instance {-# OVERLAPPING #-} ToFieldValue String where
  toFieldValue = T.pack

instance {-# OVERLAPPING #-} ToFieldValue Int64 where
  toFieldValue = T.pack . show

instance {-# OVERLAPPING #-} ToFieldValue Int where
  toFieldValue = T.pack . show

instance {-# OVERLAPPING #-} ToFieldValue Bool where
  toFieldValue = T.pack . show

instance (ToFieldValue a) => ToFieldValue (Maybe a) where
  toFieldValue Nothing = "NULL"
  toFieldValue (Just a) = toFieldValue a

instance (ToFieldValue a) => ToFieldValue (Id table a) where
  toFieldValue (Id a) = toFieldValue a

-- | Construct a typed SQL column expression for an alias and a column specification.
colSpecExpr :: Text -> ColDef a -> QExpr a
colSpecExpr alias spec = colExpr alias (colName spec)

-- | Cache policy metadata for Redis read-through caching and onCommit invalidation.
data RedisCachePolicy = RedisCachePolicy
  { rcpKeyPrefix :: !Text
  , rcpTtlSeconds :: !Int
  }
  deriving (Eq, Show, Generic)

instance NFData RedisCachePolicy

-- | Schema definition for an HKD table pairing the table name with its 'Meta record.
data TableSchema (table :: Ctx -> Type) = TableSchema
  { tableName :: !Text
  , tableMeta :: !(table 'Meta)
  , tableCachePolicy :: !(Maybe RedisCachePolicy)
  , tableDescription :: !(Maybe Text)
  }
  deriving (Generic)

{- | Construct a 'TableSchema' given a table name and its 'Meta record with no
cache policy and no description.
-}
tableSchema :: Text -> table 'Meta -> TableSchema table
tableSchema name meta = TableSchema name meta Nothing Nothing

-- | Attach a 'RedisCachePolicy' to a 'TableSchema'.
withRedisCache :: RedisCachePolicy -> TableSchema table -> TableSchema table
withRedisCache policy s = s {tableCachePolicy = Just policy}

-- | Attach a human-readable description consumed by AI context exporters.
withTableDescription :: Text -> TableSchema table -> TableSchema table
withTableDescription desc s = s {tableDescription = Just desc}

-- | Existential wrapper around 'TableSchema' for heterogeneous schema registries.
data AnyTableSchema where
  AnyTableSchema :: (Table table) => TableSchema table -> AnyTableSchema

instance Show AnyTableSchema where
  showsPrec d (AnyTableSchema s) =
    showParen (d > 10) $
      showString "AnyTableSchema " . shows (tableName s)

-- | Construct a standard non-nullable column descriptor.
column :: Text -> LogicalType a -> ColDef a
column name lt =
  ColDef
    { colName = name
    , colLogicalType = lt
    , colIsNullable = False
    , colIsPrimaryKey = False
    , colIsOptimisticLock = False
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
    , colIsOptimisticLock = False
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
    , colIsOptimisticLock = False
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
    , colIsOptimisticLock = False
    , colDescription = Nothing
    , colSqlType = renderLogicalType lt
    }

-- | Construct an optimistic locking version column descriptor.
optimisticLockColumn :: Text -> LogicalType a -> ColDef a
optimisticLockColumn name lt =
  ColDef
    { colName = name
    , colLogicalType = lt
    , colIsNullable = False
    , colIsPrimaryKey = False
    , colIsOptimisticLock = True
    , colDescription = Nothing
    , colSqlType = renderLogicalType lt
    }

-- | Attach an AI/documentation description to a column specification.
describeCol :: Text -> ColDef a -> ColDef a
describeCol desc colDef = colDef {colDescription = Just desc}
