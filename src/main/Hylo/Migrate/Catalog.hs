{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
import GHC.Generics (Generic)
import Hylo.Migrate.AST

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
