{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.Migrate.Catalog.PG
Description : PostgreSQL System Catalog Introspection Queries and Parsers
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Migrate.Catalog.PG
  ( -- * Catalog Introspection Queries
    introspectTablesQuery
  , introspectColumnsQuery
  , introspectIndexesQuery
  , introspectConstraintsQuery

    -- * Raw Row Types
  , PGTableRow (..)
  , PGColumnRow (..)
  , PGIndexRow (..)
  , PGConstraintRow (..)

    -- * Parsers & Builders
  , parsePGType
  , parseIndexMethod
  , buildCatalogFromRows
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Migrate.AST
import Hylo.Migrate.Catalog
import Text.Read (readMaybe)

-- | Raw table metadata row returned from pg_catalog.
data PGTableRow = PGTableRow
  { pgRelName :: !Text
  , pgRelComment :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, NFData)

-- | Raw column metadata row returned from pg_catalog.
data PGColumnRow = PGColumnRow
  { pgColTableName :: !Text
  , pgColName :: !Text
  , pgColType :: !Text
  , pgColNotNull :: !Bool
  , pgColDefault :: !(Maybe Text)
  , pgColComment :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, NFData)

-- | Raw index metadata row returned from pg_catalog.
data PGIndexRow = PGIndexRow
  { pgIdxTableName :: !Text
  , pgIdxName :: !Text
  , pgIdxMethod :: !Text
  , pgIdxIsUnique :: !Bool
  , pgIdxIsPrimary :: !Bool
  , pgIdxDef :: !Text
  }
  deriving (Eq, Show, Generic, NFData)

-- | Raw constraint metadata row returned from pg_catalog.
data PGConstraintRow = PGConstraintRow
  { pgConName :: !Text
  , pgConTableName :: !Text
  , pgConType :: !Text
  , pgConDef :: !Text
  }
  deriving (Eq, Show, Generic, NFData)

-- | SQL query inspecting tables and comments in pg_catalog.
introspectTablesQuery :: Text
introspectTablesQuery =
  T.strip
    "SELECT c.relname AS table_name, \
    \       d.description AS table_comment \
    \FROM pg_catalog.pg_class c \
    \JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace \
    \LEFT JOIN pg_catalog.pg_description d ON d.objoid = c.oid AND d.objsubid = 0 \
    \WHERE n.nspname = 'public' AND c.relkind = 'r' \
    \ORDER BY c.relname;"

-- | SQL query inspecting columns, types, nullability, and defaults in pg_catalog.
introspectColumnsQuery :: Text
introspectColumnsQuery =
  T.strip
    "SELECT c.relname AS table_name, \
    \       a.attname AS column_name, \
    \       pg_catalog.format_type(a.atttypid, a.atttypmod) AS column_type, \
    \       a.attnotnull AS is_not_null, \
    \       pg_catalog.pg_get_expr(def.adbin, def.adrelid) AS column_default, \
    \       d.description AS column_comment \
    \FROM pg_catalog.pg_attribute a \
    \JOIN pg_catalog.pg_class c ON c.oid = a.attrelid \
    \JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace \
    \LEFT JOIN pg_catalog.pg_attrdef def ON def.adrelid = a.attrelid AND def.adnum = a.attnum \
    \LEFT JOIN pg_catalog.pg_description d ON d.objoid = c.oid AND d.objsubid = a.attnum \
    \WHERE n.nspname = 'public' AND c.relkind = 'r' AND a.attnum > 0 AND NOT a.attisdropped \
    \ORDER BY c.relname, a.attnum;"

-- | SQL query inspecting table indexes including pgvector HNSW/IVFFlat access methods.
introspectIndexesQuery :: Text
introspectIndexesQuery =
  T.strip
    "SELECT c.relname AS table_name, \
    \       i.relname AS index_name, \
    \       am.amname AS index_method, \
    \       idx.indisunique AS is_unique, \
    \       idx.indisprimary AS is_primary, \
    \       pg_catalog.pg_get_indexdef(idx.indexrelid, 0, true) AS index_def \
    \FROM pg_catalog.pg_index idx \
    \JOIN pg_catalog.pg_class c ON c.oid = idx.indrelid \
    \JOIN pg_catalog.pg_class i ON i.oid = idx.indexrelid \
    \JOIN pg_catalog.pg_am am ON am.oid = i.relam \
    \JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace \
    \WHERE n.nspname = 'public' AND c.relkind = 'r' \
    \ORDER BY c.relname, i.relname;"

-- | SQL query inspecting primary key, unique, check, and foreign key constraints.
introspectConstraintsQuery :: Text
introspectConstraintsQuery =
  T.strip
    "SELECT con.conname AS constraint_name, \
    \       c.relname AS table_name, \
    \       con.contype AS constraint_type, \
    \       pg_catalog.pg_get_constraintdef(con.oid, true) AS constraint_def \
    \FROM pg_catalog.pg_constraint con \
    \JOIN pg_catalog.pg_class c ON c.oid = con.conrelid \
    \JOIN pg_catalog.pg_namespace n ON n.oid = con.connamespace \
    \WHERE n.nspname = 'public' AND c.relkind = 'r' \
    \ORDER BY c.relname, con.conname;"

-- | Parse PostgreSQL type names formatted by format_type into Hylo ColumnType.
parsePGType :: Text -> ColumnType
parsePGType raw =
  let t = T.toLower (T.strip raw)
   in case t of
        "smallint" -> TypeSmallInt
        "int2" -> TypeSmallInt
        "integer" -> TypeInteger
        "int" -> TypeInteger
        "int4" -> TypeInteger
        "bigint" -> TypeBigInt
        "int8" -> TypeBigInt
        "text" -> TypeText
        "character varying" -> TypeVarChar Nothing
        "varchar" -> TypeVarChar Nothing
        "boolean" -> TypeBoolean
        "bool" -> TypeBoolean
        "timestamp without time zone" -> TypeTimestamp
        "timestamp" -> TypeTimestamp
        "timestamp with time zone" -> TypeTimestamptz
        "timestamptz" -> TypeTimestamptz
        "date" -> TypeDate
        "time without time zone" -> TypeTime
        "time" -> TypeTime
        "json" -> TypeJson
        "jsonb" -> TypeJsonb
        "uuid" -> TypeUuid
        "vector" -> TypeVector Nothing
        _
          | "vector(" `T.isPrefixOf` t && ")" `T.isSuffixOf` t ->
              let inner = T.dropEnd 1 (T.drop 7 t)
               in TypeVector (readMaybe (T.unpack inner))
          | "character varying(" `T.isPrefixOf` t && ")" `T.isSuffixOf` t ->
              let inner = T.dropEnd 1 (T.drop 18 t)
               in TypeVarChar (readMaybe (T.unpack inner))
          | "varchar(" `T.isPrefixOf` t && ")" `T.isSuffixOf` t ->
              let inner = T.dropEnd 1 (T.drop 8 t)
               in TypeVarChar (readMaybe (T.unpack inner))
          | otherwise -> TypeCustom raw

-- | Parse index access method string.
parseIndexMethod :: Text -> IndexMethod
parseIndexMethod raw =
  case T.toLower (T.strip raw) of
    "btree" -> BTree
    "hash" -> Hash
    "gin" -> GIN
    "gist" -> GiST
    "brin" -> BRIN
    "hnsw" -> HNSW
    "ivfflat" -> IVFFlat
    other -> CustomMethod other

{- | Purely build a typed in-memory Catalog from introspected raw metadata rows.
Groups columns, indexes, and constraints under their corresponding tables.
-}
buildCatalogFromRows ::
  [PGTableRow] ->
  [PGColumnRow] ->
  [PGIndexRow] ->
  [PGConstraintRow] ->
  Catalog
buildCatalogFromRows tblRows colRows idxRows conRows =
  let colMap =
        foldr
          ( \r acc ->
              let col =
                    ColumnDef
                      { colName = ColumnName (pgColName r)
                      , colType = parsePGType (pgColType r)
                      , colNullability = if pgColNotNull r then NotNull else Nullable
                      , colDefault = fmap ColumnDefault (pgColDefault r)
                      , colIsPrimaryKey = False
                      , colComment = pgColComment r
                      }
               in Map.insertWith (++) (pgColTableName r) [col] acc
          )
          Map.empty
          colRows

      idxMap =
        foldr
          ( \r acc ->
              let idx =
                    IndexDef
                      { idxName = IndexName (pgIdxName r)
                      , idxTable = TableName (pgIdxTableName r)
                      , idxColumns = parseIndexDefCols (pgIdxDef r)
                      , idxMethod = parseIndexMethod (pgIdxMethod r)
                      , idxIsUnique = pgIdxIsUnique r
                      , idxIsConcurrent = False
                      , idxWhereClause = parseIndexWhere (pgIdxDef r)
                      , idxStorageParams = parseIndexStorageParams (pgIdxDef r)
                      , idxOpClass = parseIndexOpClass (pgIdxDef r)
                      }
               in if pgIdxIsPrimary r
                    then acc
                    else Map.insertWith (++) (pgIdxTableName r) [idx] acc
          )
          Map.empty
          idxRows

      conMap =
        foldr
          ( \r acc ->
              let con = parseConstraint (pgConName r) (pgConTableName r) (pgConType r) (pgConDef r)
               in Map.insertWith (++) (pgConTableName r) [con] acc
          )
          Map.empty
          conRows

      tables =
        Map.fromList
          [ ( TableName (pgRelName r)
            , TableDef
                { tblName = TableName (pgRelName r)
                , tblColumns = Map.findWithDefault [] (pgRelName r) colMap
                , tblIndexes = Map.findWithDefault [] (pgRelName r) idxMap
                , tblConstraints = Map.findWithDefault [] (pgRelName r) conMap
                , tblComment = pgRelComment r
                }
            )
          | r <- tblRows
          ]
   in Catalog
        { catalogTables = tables
        , catalogExtensions = Set.empty
        , catalogEnums = Map.empty
        }

-- | Parse column names from pg_get_indexdef string.
parseIndexDefCols :: Text -> [ColumnName]
parseIndexDefCols def =
  case T.breakOn "USING " def of
    (_, rest)
      | not (T.null rest) ->
          let afterUsing = T.drop 6 rest
           in case T.breakOn "(" afterUsing of
                (_, withParen)
                  | not (T.null withParen) ->
                      let inside = T.takeWhile (/= ')') (T.drop 1 withParen)
                          parts = T.splitOn "," inside
                       in map (ColumnName . cleanColName) parts
                _ -> []
    _ -> []

-- | Extract column name from raw index column fragment (handling opclasses).
cleanColName :: Text -> Text
cleanColName raw =
  let w = T.words (T.strip raw)
   in case w of
        (c : _) -> T.filter (/= '"') c
        [] -> ""

-- | Extract optional WHERE predicate from index definition.
parseIndexWhere :: Text -> Maybe Text
parseIndexWhere def =
  case T.breakOn " WHERE " def of
    (_, rest)
      | not (T.null rest) -> Just (T.strip (T.drop 7 rest))
    _ -> Nothing

-- | Extract storage parameters from WITH (...) in index definition.
parseIndexStorageParams :: Text -> [(Text, Text)]
parseIndexStorageParams def =
  case T.breakOn " WITH (" def of
    (_, rest)
      | not (T.null rest) ->
          let inside = T.takeWhile (/= ')') (T.drop 7 rest)
              items = T.splitOn "," inside
           in [ (T.strip k, T.strip v)
              | item <- items
              , [k, v] <- [T.splitOn "=" item]
              ]
    _ -> []

-- | Extract operator class if present (e.g. vector_cosine_ops).
parseIndexOpClass :: Text -> Maybe Text
parseIndexOpClass def =
  let lower = T.toLower def
   in if "vector_cosine_ops" `T.isInfixOf` lower
        then Just "vector_cosine_ops"
        else
          if "vector_l2_ops" `T.isInfixOf` lower
            then Just "vector_l2_ops"
            else
              if "vector_ip_ops" `T.isInfixOf` lower
                then Just "vector_ip_ops"
                else Nothing

-- | Parse constraint definition based on contype (p: primary key, u: unique, f: foreign key, c: check).
parseConstraint :: Text -> Text -> Text -> Text -> ConstraintDef
parseConstraint conName _tblName conType conDef =
  case conType of
    "p" -> PrimaryKeyConstraint (ConstraintName conName) (extractParenCols conDef)
    "u" -> UniqueConstraint (ConstraintName conName) (extractParenCols conDef)
    "f" ->
      let (srcCols, refTbl, refCols) = parseForeignKeyDef conDef
       in ForeignKeyConstraint
            (ConstraintName conName)
            srcCols
            refTbl
            refCols
            NoAction
            NoAction
    _ -> CheckConstraint (ConstraintName conName) conDef

-- | Extract column names enclosed in parentheses from constraint definition.
extractParenCols :: Text -> [ColumnName]
extractParenCols def =
  case T.breakOn "(" def of
    (_, rest)
      | not (T.null rest) ->
          let inside = T.takeWhile (/= ')') (T.drop 1 rest)
              cols = T.splitOn "," inside
           in map (ColumnName . T.filter (/= '"') . T.strip) cols
    _ -> []

-- | Parse foreign key definition string.
parseForeignKeyDef :: Text -> ([ColumnName], TableName, [ColumnName])
parseForeignKeyDef def =
  let srcCols = extractParenCols def
      afterRef = case T.breakOn "REFERENCES " def of
        (_, rest) | not (T.null rest) -> T.drop 11 rest
        _ -> ""
      refTblName = TableName (T.filter (/= '"') (T.takeWhile (\c -> c /= ' ' && c /= '(') (T.strip afterRef)))
      refCols = extractParenCols afterRef
   in (srcCols, refTblName, refCols)
