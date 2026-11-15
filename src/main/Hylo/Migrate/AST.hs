{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.Migrate.AST
Description : Abstract Syntax Tree for Database Migrations, DDL & Schema Definitions
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Migrate.AST
  ( -- * Identifiers
    TableName (..)
  , ColumnName (..)
  , IndexName (..)
  , ConstraintName (..)

    -- * Column Types & Attributes
  , ColumnType (..)
  , ColumnNullability (..)
  , ColumnDefault (..)
  , ColumnDef (..)

    -- * Indexes
  , IndexMethod (..)
  , IndexDef (..)

    -- * Constraints
  , OnDeleteAction (..)
  , OnUpdateAction
  , ConstraintDef (..)

    -- * Tables
  , TableDef (..)

    -- * DDL Statements
  , DDLStatement (..)

    -- * SQL Rendering
  , renderDDLStatement
  , renderColumnDef
  , renderColumnType
  , renderIndexMethod
  , renderOnDeleteAction
  ) where

import Control.DeepSeq (NFData)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Database table name with quoted identifier representation.
newtype TableName = TableName {unTableName :: Text}
  deriving (Eq, Ord, Show, Generic, NFData)

instance IsString TableName where
  fromString = TableName . T.pack

-- | Database column name with quoted identifier representation.
newtype ColumnName = ColumnName {unColumnName :: Text}
  deriving (Eq, Ord, Show, Generic, NFData)

instance IsString ColumnName where
  fromString = ColumnName . T.pack

-- | Database index name with quoted identifier representation.
newtype IndexName = IndexName {unIndexName :: Text}
  deriving (Eq, Ord, Show, Generic, NFData)

instance IsString IndexName where
  fromString = IndexName . T.pack

-- | Database constraint name with quoted identifier representation.
newtype ConstraintName = ConstraintName {unConstraintName :: Text}
  deriving (Eq, Ord, Show, Generic, NFData)

instance IsString ConstraintName where
  fromString = ConstraintName . T.pack

-- | Supported PostgreSQL column data types including pgvector vectors.
data ColumnType
  = TypeSmallInt
  | TypeInteger
  | TypeBigInt
  | TypeText
  | TypeVarChar (Maybe Int)
  | TypeBoolean
  | TypeTimestamp
  | TypeTimestamptz
  | TypeDate
  | TypeTime
  | TypeJson
  | TypeJsonb
  | TypeUuid
  | TypeVector (Maybe Int)
  | TypeCustom Text
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Column nullability constraint.
data ColumnNullability
  = Nullable
  | NotNull
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Column default value expression in SQL.
newtype ColumnDefault = ColumnDefault {unColumnDefault :: Text}
  deriving (Eq, Ord, Show, Generic, NFData)

instance IsString ColumnDefault where
  fromString = ColumnDefault . T.pack

-- | Definition of a column in a table.
data ColumnDef = ColumnDef
  { colName :: !ColumnName
  , colType :: !ColumnType
  , colNullability :: !ColumnNullability
  , colDefault :: !(Maybe ColumnDefault)
  , colIsPrimaryKey :: !Bool
  , colComment :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Index access methods supported by PostgreSQL.
data IndexMethod
  = BTree
  | Hash
  | GIN
  | GiST
  | BRIN
  | HNSW
  | IVFFlat
  | CustomMethod !Text
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Index definition with vector parameters and predicate support.
data IndexDef = IndexDef
  { idxName :: !IndexName
  , idxTable :: !TableName
  , idxColumns :: ![ColumnName]
  , idxMethod :: !IndexMethod
  , idxIsUnique :: !Bool
  , idxIsConcurrent :: !Bool
  , idxWhereClause :: !(Maybe Text)
  , idxStorageParams :: ![(Text, Text)]
  , idxOpClass :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Referential action for foreign key constraints on deletion or update.
data OnDeleteAction
  = NoAction
  | Restrict
  | Cascade
  | SetNull
  | SetDefault
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Alias for foreign key update action.
type OnUpdateAction = OnDeleteAction

-- | Table constraints supported in catalog diffing and DDL generation.
data ConstraintDef
  = PrimaryKeyConstraint !ConstraintName ![ColumnName]
  | UniqueConstraint !ConstraintName ![ColumnName]
  | ForeignKeyConstraint
      !ConstraintName
      ![ColumnName]
      !TableName
      ![ColumnName]
      !OnDeleteAction
      !OnUpdateAction
  | CheckConstraint !ConstraintName !Text
  deriving (Eq, Ord, Show, Generic, NFData)

-- | In-memory structural definition of a database table.
data TableDef = TableDef
  { tblName :: !TableName
  , tblColumns :: ![ColumnDef]
  , tblIndexes :: ![IndexDef]
  , tblConstraints :: ![ConstraintDef]
  , tblComment :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Reversible DDL statements.
data DDLStatement
  = CreateTableStmt !TableDef
  | DropTableStmt !TableName !Bool
  | AddColumnStmt !TableName !ColumnDef
  | DropColumnStmt !TableName !ColumnName !Bool
  | AlterColumnTypeStmt !TableName !ColumnName !ColumnType !(Maybe Text)
  | AlterColumnNullabilityStmt !TableName !ColumnName !ColumnNullability
  | AlterColumnDefaultStmt !TableName !ColumnName !(Maybe ColumnDefault)
  | CreateIndexStmt !IndexDef
  | DropIndexStmt !IndexName !Bool !Bool
  | AddConstraintStmt !TableName !ConstraintDef !Bool
  | ValidateConstraintStmt !TableName !ConstraintName
  | DropConstraintStmt !TableName !ConstraintName !Bool
  | RawDDLStmt !Text
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Render a column data type into standard PostgreSQL DDL.
renderColumnType :: ColumnType -> Text
renderColumnType = \case
  TypeSmallInt -> "smallint"
  TypeInteger -> "integer"
  TypeBigInt -> "bigint"
  TypeText -> "text"
  TypeVarChar Nothing -> "character varying"
  TypeVarChar (Just n) -> "character varying(" <> T.pack (show n) <> ")"
  TypeBoolean -> "boolean"
  TypeTimestamp -> "timestamp without time zone"
  TypeTimestamptz -> "timestamp with time zone"
  TypeDate -> "date"
  TypeTime -> "time without time zone"
  TypeJson -> "json"
  TypeJsonb -> "jsonb"
  TypeUuid -> "uuid"
  TypeVector Nothing -> "vector"
  TypeVector (Just dim) -> "vector(" <> T.pack (show dim) <> ")"
  TypeCustom t -> t

-- | Render a column definition line for CREATE TABLE or ALTER TABLE ADD COLUMN.
renderColumnDef :: ColumnDef -> Text
renderColumnDef col =
  let nameStr = "\"" <> unColumnName (colName col) <> "\""
      typeStr = renderColumnType (colType col)
      nullStr = case colNullability col of
        NotNull -> " NOT NULL"
        Nullable -> ""
      defStr = case colDefault col of
        Just (ColumnDefault d) -> " DEFAULT " <> d
        Nothing -> ""
   in nameStr <> " " <> typeStr <> nullStr <> defStr

-- | Render an index access method.
renderIndexMethod :: IndexMethod -> Text
renderIndexMethod = \case
  BTree -> "btree"
  Hash -> "hash"
  GIN -> "gin"
  GiST -> "gist"
  BRIN -> "brin"
  HNSW -> "hnsw"
  IVFFlat -> "ivfflat"
  CustomMethod m -> m

-- | Render referential delete action.
renderOnDeleteAction :: OnDeleteAction -> Text
renderOnDeleteAction = \case
  NoAction -> "NO ACTION"
  Restrict -> "RESTRICT"
  Cascade -> "CASCADE"
  SetNull -> "SET NULL"
  SetDefault -> "SET DEFAULT"

-- | Render a single DDL statement into valid PostgreSQL SQL.
renderDDLStatement :: DDLStatement -> Text
renderDDLStatement = \case
  CreateTableStmt tbl ->
    let header = "CREATE TABLE \"" <> unTableName (tblName tbl) <> "\" ("
        colLines = map (\c -> "  " <> renderColumnDef c) (tblColumns tbl)
        conLines = map (\c -> "  " <> renderTableConstraint c) (tblConstraints tbl)
        allLines = colLines ++ conLines
        body = T.intercalate ",\n" allLines
        footer = ");"
        tableSql = header <> "\n" <> body <> "\n" <> footer
        idxSqls = map (\idx -> "\n" <> renderIndexStmt idx) (tblIndexes tbl)
     in tableSql <> T.concat idxSqls
  DropTableStmt name cascade ->
    let cascadeStr = if cascade then " CASCADE" else ""
     in "DROP TABLE \"" <> unTableName name <> "\"" <> cascadeStr <> ";"
  AddColumnStmt tbl col ->
    "ALTER TABLE \"" <> unTableName tbl <> "\" ADD COLUMN " <> renderColumnDef col <> ";"
  DropColumnStmt tbl col cascade ->
    let cascadeStr = if cascade then " CASCADE" else ""
     in "ALTER TABLE \""
          <> unTableName tbl
          <> "\" DROP COLUMN \""
          <> unColumnName col
          <> "\""
          <> cascadeStr
          <> ";"
  AlterColumnTypeStmt tbl col newType mUsing ->
    let usingStr = case mUsing of
          Just expr -> " USING " <> expr
          Nothing -> ""
     in "ALTER TABLE \""
          <> unTableName tbl
          <> "\" ALTER COLUMN \""
          <> unColumnName col
          <> "\" TYPE "
          <> renderColumnType newType
          <> usingStr
          <> ";"
  AlterColumnNullabilityStmt tbl col nullability ->
    let actionStr = case nullability of
          NotNull -> "SET NOT NULL"
          Nullable -> "DROP NOT NULL"
     in "ALTER TABLE \""
          <> unTableName tbl
          <> "\" ALTER COLUMN \""
          <> unColumnName col
          <> "\" "
          <> actionStr
          <> ";"
  AlterColumnDefaultStmt tbl col mDef ->
    let actionStr = case mDef of
          Just (ColumnDefault d) -> "SET DEFAULT " <> d
          Nothing -> "DROP DEFAULT"
     in "ALTER TABLE \""
          <> unTableName tbl
          <> "\" ALTER COLUMN \""
          <> unColumnName col
          <> "\" "
          <> actionStr
          <> ";"
  CreateIndexStmt idx ->
    renderIndexStmt idx
  DropIndexStmt idx isConcurrent cascade ->
    let concStr = if isConcurrent then " CONCURRENTLY" else ""
        cascadeStr = if cascade then " CASCADE" else ""
     in "DROP INDEX" <> concStr <> " \"" <> unIndexName idx <> "\"" <> cascadeStr <> ";"
  AddConstraintStmt tbl con notValid ->
    let nvStr = if notValid then " NOT VALID" else ""
     in "ALTER TABLE \"" <> unTableName tbl <> "\" ADD " <> renderTableConstraint con <> nvStr <> ";"
  ValidateConstraintStmt tbl conName ->
    "ALTER TABLE \""
      <> unTableName tbl
      <> "\" VALIDATE CONSTRAINT \""
      <> unConstraintName conName
      <> "\";"
  DropConstraintStmt tbl conName cascade ->
    let cascadeStr = if cascade then " CASCADE" else ""
     in "ALTER TABLE \""
          <> unTableName tbl
          <> "\" DROP CONSTRAINT \""
          <> unConstraintName conName
          <> "\""
          <> cascadeStr
          <> ";"
  RawDDLStmt raw -> raw

-- | Render a table constraint into SQL snippet.
renderTableConstraint :: ConstraintDef -> Text
renderTableConstraint = \case
  PrimaryKeyConstraint cName cols ->
    "CONSTRAINT \"" <> unConstraintName cName <> "\" PRIMARY KEY (" <> renderColList cols <> ")"
  UniqueConstraint cName cols ->
    "CONSTRAINT \"" <> unConstraintName cName <> "\" UNIQUE (" <> renderColList cols <> ")"
  ForeignKeyConstraint cName srcCols targetTbl targetCols onDel onUpd ->
    "CONSTRAINT \""
      <> unConstraintName cName
      <> "\" FOREIGN KEY ("
      <> renderColList srcCols
      <> ") REFERENCES \""
      <> unTableName targetTbl
      <> "\" ("
      <> renderColList targetCols
      <> ") ON DELETE "
      <> renderOnDeleteAction onDel
      <> " ON UPDATE "
      <> renderOnDeleteAction onUpd
  CheckConstraint cName expr ->
    "CONSTRAINT \"" <> unConstraintName cName <> "\" CHECK (" <> expr <> ")"

-- | Render index definition statement.
renderIndexStmt :: IndexDef -> Text
renderIndexStmt idx =
  let uniqueStr = if idxIsUnique idx then "UNIQUE " else ""
      concStr = if idxIsConcurrent idx then "CONCURRENTLY " else ""
      methodStr = renderIndexMethod (idxMethod idx)
      colsWithOp = case idxOpClass idx of
        Just opCls ->
          T.intercalate ", " $
            map (\c -> "\"" <> unColumnName c <> "\" " <> opCls) (idxColumns idx)
        Nothing -> renderColList (idxColumns idx)
      paramsStr =
        if null (idxStorageParams idx)
          then ""
          else
            " WITH ("
              <> T.intercalate ", " (map (\(k, v) -> k <> " = " <> v) (idxStorageParams idx))
              <> ")"
      whereStr = case idxWhereClause idx of
        Just p -> " WHERE " <> p
        Nothing -> ""
   in "CREATE "
        <> uniqueStr
        <> "INDEX "
        <> concStr
        <> "\""
        <> unIndexName (idxName idx)
        <> "\" ON \""
        <> unTableName (idxTable idx)
        <> "\" USING "
        <> methodStr
        <> " ("
        <> colsWithOp
        <> ")"
        <> paramsStr
        <> whereStr
        <> ";"

-- | Helper to render comma-separated quoted column list.
renderColList :: [ColumnName] -> Text
renderColList cols = T.intercalate ", " (map (\c -> "\"" <> unColumnName c <> "\"") cols)
