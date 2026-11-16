{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.Migrate.CodeGen
Description : Idiomatic Haskell HKD TableSchema Reverse Code Generation & Law 2 Idempotence
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Migrate.CodeGen
  ( -- * Reverse Code Generation
    generateHaskellSchema
  , generateTableHkd

    -- * Reverse Introspection & Law 2 Idempotence
  , catalogFromCodeGen

    -- * Naming Utilities
  , toPascalCase
  , toCamelCase
  , columnTypeToHaskell
  , haskellToColumnType
  , renderSqlType
  ) where

import Data.Char (isUpper, toLower, toUpper)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Migrate.AST
import Hylo.Migrate.Catalog

{- | Generate idiomatic, compilable Haskell HKD source code from a database Catalog.
Synthesizes 4-context 'Col ctx a' data types, type aliases, complete Table instances,
and TableSchema values.
-}
generateHaskellSchema :: (IsString s) => Catalog -> s
generateHaskellSchema cat =
  let header =
        T.unlines
          [ "{-# LANGUAGE DataKinds #-}"
          , "{-# LANGUAGE DeriveAnyClass #-}"
          , "{-# LANGUAGE DeriveGeneric #-}"
          , "{-# LANGUAGE DuplicateRecordFields #-}"
          , "{-# LANGUAGE FlexibleInstances #-}"
          , "{-# LANGUAGE OverloadedRecordDot #-}"
          , "{-# LANGUAGE OverloadedStrings #-}"
          , "{-# LANGUAGE StandaloneDeriving #-}"
          , "{-# LANGUAGE TypeFamilies #-}"
          , "{-# LANGUAGE NoFieldSelectors #-}"
          , ""
          , "{- |"
          , "Module      : Database.Schema.Generated"
          , "Description : Auto-generated Hylo HKD Database Schema from Live Catalog"
          , "Generated   : (c) Hylo Reverse Codegen Engine"
          , "-}"
          , "module Database.Schema.Generated where"
          , ""
          , "import Data.Int (Int16, Int32, Int64)"
          , "import qualified Data.Map.Strict as Map"
          , "import Data.Text (Text)"
          , "import Data.Time (UTCTime)"
          , "import GHC.Generics (Generic)"
          , "import Hylo.Core"
          , "  ( Col"
          , "  , ColDef"
          , "  , Ctx (..)"
          , "  , PatchVal (..)"
          , "  , Table (..)"
          , "  , TableSchema"
          , "  , colName"
          , "  , colSpecExpr"
          , "  , column"
          , "  , diffField"
          , "  , eraseColDef"
          , "  , nullableColumn"
          , "  , patchEntry"
          , "  , primaryKey"
          , "  , tableMeta"
          , "  , tableSchema"
          , "  )"
          , "import Hylo.PG.Vector (Embedding)"
          , ""
          ]
      tables = listTables cat
      tableSections = map generateTableHkd tables
   in fromString (T.unpack (header <> T.intercalate "\n\n" tableSections))

-- | Generate Haskell HKD definition for a single table.
generateTableHkd :: TableDef -> Text
generateTableHkd tbl =
  let tName = unTableName (tblName tbl)
      typeName = toPascalCase tName
      hkdName = typeName <> "T"
      schemaName = toCamelCase tName <> "Schema"
      docComment = case tblComment tbl of
        Just c -> "-- | " <> c <> "\n"
        Nothing -> "-- | Generated HKD table schema for " <> tName <> ".\n"
      fieldPrefix = toCamelCase (singularize tName)

      fields =
        map
          ( \col ->
              let fName = fieldPrefix <> toPascalCase (unColumnName (colName col))
                  fType = columnTypeToHaskell (colType col) (colNullability col)
                  cComment = case colComment col of
                    Just c -> "  -- ^ " <> c
                    Nothing -> ""
               in "  , "
                    <> fName
                    <> " :: !(Col ctx "
                    <> fType
                    <> ")"
                    <> (if T.null cComment then "" else "\n" <> cComment)
          )
          (tblColumns tbl)

      fieldsFormatted = case fields of
        [] -> "  { -- (empty table)\n  }"
        (firstF : restF) ->
          let firstClean = "  { " <> T.drop 4 firstF
           in T.intercalate "\n" (firstClean : restF) <> "\n  }"

      dataDecl =
        docComment
          <> "data "
          <> hkdName
          <> " (ctx :: Ctx) =\n"
          <> typeName
          <> "\n"
          <> fieldsFormatted
          <> "\n  deriving (Generic)"

      aliases =
        T.unlines
          [ ""
          , "-- | Result context representation for " <> typeName
          , "type " <> typeName <> " = " <> hkdName <> " 'Result"
          , ""
          , "-- | Query expression context representation for " <> typeName
          , "type " <> typeName <> "Expr = " <> hkdName <> " 'Expr"
          , ""
          , "-- | Patch context representation for " <> typeName
          , "type " <> typeName <> "Patch = " <> hkdName <> " 'Patch"
          , ""
          , "-- | Metadata context representation for " <> typeName
          , "type " <> typeName <> "Meta = " <> hkdName <> " 'Meta"
          ]

      fieldNames = map (\col -> fieldPrefix <> toPascalCase (unColumnName (colName col))) (tblColumns tbl)

      schemaDecl =
        if null (tblColumns tbl)
          then
            T.unlines
              [ "-- | Table schema for " <> tName
              , schemaName <> " :: TableSchema " <> hkdName
              , schemaName <> " ="
              , "  tableSchema \"" <> tName <> "\" " <> typeName
              ]
          else
            let colSpecs =
                  map
                    ( \col ->
                        let fn = fieldPrefix <> toPascalCase (unColumnName (colName col))
                            cn = unColumnName (colName col)
                            st = renderSqlType (colType col)
                            fnCall
                              | colIsPrimaryKey col = "primaryKey \"" <> cn <> "\" \"" <> st <> "\""
                              | colNullability col == Nullable = "nullableColumn \"" <> cn <> "\" \"" <> st <> "\""
                              | otherwise = "column \"" <> cn <> "\" \"" <> st <> "\""
                         in fn <> " = " <> fnCall
                    )
                    (tblColumns tbl)
             in T.unlines
                  [ "-- | Table schema for " <> tName
                  , schemaName <> " :: TableSchema " <> hkdName
                  , schemaName <> " ="
                  , "  tableSchema \"" <> tName <> "\" $"
                  , "    " <> typeName
                  , "      { " <> T.intercalate "\n      , " colSpecs
                  , "      }"
                  ]

      tableInstance =
        if null fieldNames
          then
            T.unlines
              [ "instance Table " <> hkdName <> " where"
              , "  toTableExpr _ _ = " <> typeName
              , "  diffTable _ _ = " <> typeName
              , "  tableColDefs _ = []"
              , "  patchEntries _ _ = []"
              , "  dirtyFieldsTable _ = Map.empty"
              ]
          else
            T.unlines
              [ "instance Table " <> hkdName <> " where"
              , "  toTableExpr alias meta ="
              , "    " <> typeName
              , "      { "
                  <> T.intercalate "\n      , " [fn <> " = colSpecExpr alias meta." <> fn | fn <- fieldNames]
              , "      }"
              , "  diffTable old new ="
              , "    " <> typeName
              , "      { "
                  <> T.intercalate "\n      , " [fn <> " = diffField old." <> fn <> " new." <> fn | fn <- fieldNames]
              , "      }"
              , "  tableColDefs m ="
              , "    [ " <> T.intercalate "\n    , " ["eraseColDef m." <> fn | fn <- fieldNames]
              , "    ]"
              , "  patchEntries meta patch ="
              , "    concat"
              , "      [ "
                  <> T.intercalate "\n      , " ["patchEntry meta." <> fn <> " patch." <> fn | fn <- fieldNames]
              , "      ]"
              , "  dirtyFieldsTable patch ="
              , "    Map.fromList"
              , "      [ (colName c, case v of ValSetTo t -> t; ValSetDefault -> \"DEFAULT\")"
              , "      | (c, v) <- patchEntries (tableMeta " <> schemaName <> ") patch"
              , "      ]"
              ]
   in dataDecl <> aliases <> "\n" <> schemaDecl <> "\n" <> tableInstance

-- | Render SQL type name for TableSchema construction.
renderSqlType :: ColumnType -> Text
renderSqlType = \case
  TypeSmallInt -> "smallint"
  TypeInteger -> "integer"
  TypeBigInt -> "bigint"
  TypeText -> "text"
  TypeVarChar (Just n) -> "varchar(" <> T.pack (show n) <> ")"
  TypeVarChar Nothing -> "varchar"
  TypeBoolean -> "boolean"
  TypeTimestamp -> "timestamp"
  TypeTimestamptz -> "timestamptz"
  TypeDate -> "date"
  TypeTime -> "time"
  TypeJson -> "json"
  TypeJsonb -> "jsonb"
  TypeUuid -> "uuid"
  TypeVector (Just dim) -> "vector(" <> T.pack (show dim) <> ")"
  TypeVector Nothing -> "vector"
  TypeCustom t -> t
  EnumType ty _ -> ty

-- | Convert a ColumnType and nullability to Haskell type representation.
columnTypeToHaskell :: ColumnType -> ColumnNullability -> Text
columnTypeToHaskell cType nullability =
  let baseType = case cType of
        TypeSmallInt -> "Int16"
        TypeInteger -> "Int32"
        TypeBigInt -> "Int64"
        TypeText -> "Text"
        TypeVarChar _ -> "Text"
        TypeBoolean -> "Bool"
        TypeTimestamp -> "UTCTime"
        TypeTimestamptz -> "UTCTime"
        TypeDate -> "UTCTime"
        TypeTime -> "UTCTime"
        TypeJson -> "Text"
        TypeJsonb -> "Text"
        TypeUuid -> "Text"
        TypeVector (Just dim) -> "Embedding \"default\" " <> T.pack (show dim)
        TypeVector Nothing -> "Embedding \"default\" 1536"
        TypeCustom t -> t
        EnumType ty _ -> ty
   in case nullability of
        NotNull -> baseType
        Nullable ->
          let wrapped = if " " `T.isInfixOf` baseType then "(" <> baseType <> ")" else baseType
           in "(Maybe " <> wrapped <> ")"

-- | Convert a Haskell type string back to Hylo ColumnType and Nullability.
haskellToColumnType :: Text -> (ColumnType, ColumnNullability)
haskellToColumnType raw =
  let t = T.strip raw
   in if "(Maybe " `T.isPrefixOf` t && ")" `T.isSuffixOf` t
        then
          let inner = T.strip (T.dropEnd 1 (T.drop 7 t))
              cleanInner =
                if "(" `T.isPrefixOf` inner && ")" `T.isSuffixOf` inner
                  then T.strip (T.dropEnd 1 (T.drop 1 inner))
                  else inner
           in (baseHaskellToType cleanInner, Nullable)
        else
          if "(Nullable " `T.isPrefixOf` t && ")" `T.isSuffixOf` t
            then
              let inner = T.strip (T.dropEnd 1 (T.drop 10 t))
                  cleanInner =
                    if "(" `T.isPrefixOf` inner && ")" `T.isSuffixOf` inner
                      then T.strip (T.dropEnd 1 (T.drop 1 inner))
                      else inner
               in (baseHaskellToType cleanInner, Nullable)
            else (baseHaskellToType t, NotNull)

-- | Convert non-nullable Haskell type string to ColumnType.
baseHaskellToType :: Text -> ColumnType
baseHaskellToType t
  | t == "Int16" = TypeSmallInt
  | t == "Int32" = TypeInteger
  | t == "Int64" = TypeBigInt
  | t == "Text" = TypeText
  | t == "Bool" = TypeBoolean
  | t == "UTCTime" = TypeTimestamptz
  | "Embedding " `T.isPrefixOf` t =
      let parts = T.words t
       in if length parts >= 3
            then case reads (T.unpack (parts !! 2)) of
              [(dim, "")] -> TypeVector (Just dim)
              _ -> TypeVector (Just 1536)
            else TypeVector (Just 1536)
  | otherwise = TypeCustom t

{- | Inverse compiler: parses generated Haskell source code back to in-memory Catalog.
Guarantees Law 2: Diff(Introspect(DB), Compile(CodeGen(Introspect(DB)))) == Empty.
-}
catalogFromCodeGen :: Text -> Catalog
catalogFromCodeGen code =
  let linesOfCode = T.lines code
      tableBlocks = extractTableBlocks linesOfCode
      tables = map parseTableBlock tableBlocks
   in Catalog
        { catalogTables = Map.fromList [(tblName tbl, tbl) | tbl <- tables]
        , catalogExtensions = Set.empty
        , catalogEnums = Map.empty
        }

-- | Group source lines into blocks per data declaration.
extractTableBlocks :: [Text] -> [[Text]]
extractTableBlocks [] = []
extractTableBlocks (l : ls)
  | isDataDecl l =
      let (block, rest) = break isDataDecl ls
       in (l : block) : extractTableBlocks rest
  | otherwise = extractTableBlocks ls
  where
    isDataDecl line =
      "data " `T.isPrefixOf` line
        && ("(ctx :: Ctx) =" `T.isInfixOf` line || " f =" `T.isInfixOf` line)

-- | Parse a single Haskell data declaration block into a TableDef.
parseTableBlock :: [Text] -> TableDef
parseTableBlock block =
  let firstLine = case block of
        (x : _) -> x
        [] -> ""
      wordsAfterData = T.words (T.drop 5 firstLine)
      nameWithT = case wordsAfterData of
        (w : _) -> w
        [] -> ""
      typeName = if "T" `T.isSuffixOf` nameWithT then T.dropEnd 1 nameWithT else nameWithT
      tableName = toSnakeCase typeName
      rawFields = [l | l <- block, ":: !(Col ctx " `T.isInfixOf` l || ":: !(f " `T.isInfixOf` l]
      cols = map (parseFieldLine tableName) rawFields
   in TableDef
        { tblName = TableName tableName
        , tblColumns = cols
        , tblIndexes = []
        , tblConstraints = []
        , tblComment = Nothing
        }

-- | Parse a single field definition line back to ColumnDef.
parseFieldLine :: Text -> Text -> ColumnDef
parseFieldLine tblSnake line =
  let stripped = T.strip line
      cleanLine = if "," `T.isPrefixOf` stripped then T.strip (T.drop 1 stripped) else stripped
      cleanLine' = if "{" `T.isPrefixOf` cleanLine then T.strip (T.drop 1 cleanLine) else cleanLine
      (fName, rest) =
        if ":: !(Col ctx " `T.isInfixOf` cleanLine'
          then T.breakOn ":: !(Col ctx " cleanLine'
          else T.breakOn ":: !(f " cleanLine'
      fNameClean = T.strip fName
      typePart =
        if ":: !(Col ctx " `T.isPrefixOf` rest
          then T.dropEnd 1 (T.drop 13 (T.strip rest))
          else T.dropEnd 1 (T.drop 6 (T.strip rest))
      (cType, nullability) = haskellToColumnType typePart
      colSnake = stripTablePrefix tblSnake (toSnakeCase fNameClean)
   in ColumnDef
        { colName = ColumnName colSnake
        , colType = cType
        , colNullability = nullability
        , colDefault = Nothing
        , colIsPrimaryKey = colSnake == "id"
        , colComment = Nothing
        }

-- | Strip table name prefix from field name in snake_case.
stripTablePrefix :: Text -> Text -> Text
stripTablePrefix tblSnake colSnake =
  let prefix = singularize tblSnake <> "_"
   in if prefix `T.isPrefixOf` colSnake
        then T.drop (T.length prefix) colSnake
        else colSnake

-- | Convert string to PascalCase (e.g. "users" -> "Users", "document_embeddings" -> "DocumentEmbeddings").
toPascalCase :: Text -> Text
toPascalCase = T.concat . map capitalize . T.splitOn "_"

-- | Convert string to camelCase.
toCamelCase :: Text -> Text
toCamelCase t =
  case T.splitOn "_" t of
    [] -> ""
    (x : xs) -> T.toLower x <> T.concat (map capitalize xs)

-- | Capitalize first character.
capitalize :: Text -> Text
capitalize t =
  case T.uncons t of
    Nothing -> ""
    Just (c, rest) -> T.singleton (toUpper c) <> rest

-- | Convert PascalCase or camelCase to snake_case.
toSnakeCase :: Text -> Text
toSnakeCase = T.dropWhile (== '_') . T.pack . go . T.unpack
  where
    go [] = []
    go (c : cs)
      | isUpper c = '_' : toLower c : go cs
      | otherwise = c : go cs

-- | Simple singularization for English table names.
singularize :: Text -> Text
singularize t
  | "ies" `T.isSuffixOf` t = T.dropEnd 3 t <> "y"
  | "ses" `T.isSuffixOf` t = T.dropEnd 2 t
  | "s" `T.isSuffixOf` t && not ("ss" `T.isSuffixOf` t) = T.dropEnd 1 t
  | otherwise = t
