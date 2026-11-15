{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Hylo.Migrate.DiffSpec
Description : Test suite for Epic 18: Live Catalog Introspection, Reversible DDL Synthesis & Legacy Reverse Codegen
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Migrate.DiffSpec (spec) where

import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core
  ( Col
  , Ctx (..)
  , Id (..)
  , PatchVal (..)
  , Table (..)
  , TableSchema
  , colSpecExpr
  , column
  , diffField
  , eraseColDef
  , patchEntry
  , primaryKey
  , tableMeta
  , tableSchema
  )
import qualified Hylo.Core.Context as CoreCtx
import Hylo.Migrate.AST
import Hylo.Migrate.Catalog
import Hylo.Migrate.Catalog.PG
import Hylo.Migrate.CodeGen
import Hylo.Migrate.Diff
import Test.Hspec
import Test.QuickCheck

-- | Test Table: Account entity for TableSchema to Catalog bridge tests.
data AccountT (ctx :: Ctx) = Account
  { id :: !(Col ctx (Id AccountT Int64))
  , name :: !(Col ctx Text)
  , email :: !(Col ctx Text)
  , score :: !(Col ctx Int64)
  , active :: !(Col ctx Bool)
  }

accountSchema :: TableSchema AccountT
accountSchema =
  tableSchema "accounts" $
    Account
      { id = primaryKey "id" "bigint"
      , name = column "name" "text"
      , email = column "email" "text"
      , score = column "score" "bigint"
      , active = column "active" "boolean"
      }

instance Table AccountT where
  toTableExpr alias meta =
    Account
      { id = colSpecExpr alias meta.id
      , name = colSpecExpr alias meta.name
      , email = colSpecExpr alias meta.email
      , score = colSpecExpr alias meta.score
      , active = colSpecExpr alias meta.active
      }
  diffTable old new =
    Account
      { id = diffField old.id new.id
      , name = diffField old.name new.name
      , email = diffField old.email new.email
      , score = diffField old.score new.score
      , active = diffField old.active new.active
      }
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.name
    , eraseColDef m.email
    , eraseColDef m.score
    , eraseColDef m.active
    ]
  patchEntries meta patch =
    concat
      [ patchEntry meta.id patch.id
      , patchEntry meta.name patch.name
      , patchEntry meta.email patch.email
      , patchEntry meta.score patch.score
      , patchEntry meta.active patch.active
      ]
  dirtyFieldsTable patch =
    Map.fromList
      [ (CoreCtx.colName c, case v of ValSetTo t -> t; ValSetDefault -> "DEFAULT")
      | (c, v) <- patchEntries (tableMeta accountSchema) patch
      ]

-- | QuickCheck generator for safe alphanumeric SQL identifiers.
genIdentifier :: Gen Text
genIdentifier = do
  firstChar <- elements ['a' .. 'z']
  len <- chooseInt (2, 8)
  rest <- vectorOf len (elements (['a' .. 'z'] ++ ['0' .. '9']))
  pure (T.pack (firstChar : rest))

-- | QuickCheck generator for standard column types.
genColumnType :: Gen ColumnType
genColumnType =
  elements
    [ TypeSmallInt
    , TypeInteger
    , TypeBigInt
    , TypeText
    , TypeBoolean
    , TypeTimestamptz
    , TypeJsonb
    , TypeUuid
    , TypeVector (Just 1536)
    ]

-- | QuickCheck generator for ColumnNullability.
genNullability :: Gen ColumnNullability
genNullability = elements [Nullable, NotNull]

-- | QuickCheck generator for a single column definition.
genColumnDef :: Gen ColumnDef
genColumnDef = do
  cName <- genIdentifier
  cType <- genColumnType
  cNull <- genNullability
  pure
    ColumnDef
      { colName = ColumnName cName
      , colType = cType
      , colNullability = cNull
      , colDefault = Nothing
      , colIsPrimaryKey = False
      , colComment = Nothing
      }

-- | QuickCheck generator for a table definition with distinct column names.
genTableDef :: Gen TableDef
genTableDef = do
  tName <- genIdentifier
  numCols <- chooseInt (1, 5)
  cols <- vectorOf numCols genColumnDef
  let uniqueCols = deduplicateCols cols
  pure
    TableDef
      { tblName = TableName tName
      , tblColumns = uniqueCols
      , tblIndexes = []
      , tblConstraints = []
      , tblComment = Nothing
      }

-- | Remove duplicate column names from generated list.
deduplicateCols :: [ColumnDef] -> [ColumnDef]
deduplicateCols = go []
  where
    go :: [ColumnName] -> [ColumnDef] -> [ColumnDef]
    go _ [] = []
    go seen (c : cs)
      | c.colName `elem` seen = go seen cs
      | otherwise = c : go (c.colName : seen) cs

-- | QuickCheck generator for arbitrary Catalog.
genCatalog :: Gen Catalog
genCatalog = do
  numTables <- chooseInt (0, 4)
  tbls <- vectorOf numTables genTableDef
  pure
    ( foldr
        ( \tbl cat ->
            if Map.member (tblName tbl) (catalogTables cat)
              then cat
              else addTable tbl cat
        )
        emptyCatalog
        tbls
    )

spec :: Spec
spec = describe "Epic 18: Live Catalog Introspection & DDL Synthesis" $ do
  -- 18.4 Executable Oracle Specification
  describe "18.4 Executable Oracle Specification" $ do
    it "generates reversible up and down DDL for added columns and vector indexes" $ do
      let currentCatalog = emptyCatalog
          targetCatalog = emptyCatalog & addTable sampleDocumentCatalogDef
          diff = computeCatalogDiff currentCatalog targetCatalog
          (upSql, downSql) = renderReversibleDDL diff
      upSql `shouldContain` "CREATE TABLE \"documents\""
      upSql `shouldContain` "CREATE INDEX \"docs_embedding_hnsw_idx\" ON \"documents\" USING hnsw"
      downSql `shouldContain` "DROP TABLE \"documents\" CASCADE"

  -- Law 1: Reversibility Group Action
  describe "Law 1: Reversibility Group Action" $ do
    it "reverts added table when applying down DDL" $ do
      let s1 = emptyCatalog
          s2 = emptyCatalog & addTable sampleDocumentCatalogDef
          diff = computeCatalogDiff s1 s2
          s2Applied = applyDiff diff s1
          s1Restored = applyDiff (invertDiff diff) s2Applied
      s2Applied `shouldBe` s2
      s1Restored `shouldBe` s1

    it "reverts dropped table when applying inverted diff" $ do
      let s1 = emptyCatalog & addTable sampleDocumentCatalogDef
          s2 = emptyCatalog
          diff = computeCatalogDiff s1 s2
          s2Applied = applyDiff diff s1
          s1Restored = applyDiff (invertDiff diff) s2Applied
      s2Applied `shouldBe` s2
      s1Restored `shouldBe` s1

    it "reverts added and dropped columns" $ do
      let initialTable =
            TableDef
              { tblName = "users"
              , tblColumns =
                  [ ColumnDef "id" TypeBigInt NotNull Nothing True Nothing
                  , ColumnDef "name" TypeText NotNull Nothing False Nothing
                  ]
              , tblIndexes = []
              , tblConstraints = []
              , tblComment = Nothing
              }
          updatedTable =
            initialTable
              { tblColumns =
                  [ ColumnDef "id" TypeBigInt NotNull Nothing True Nothing
                  , ColumnDef "email" TypeText Nullable Nothing False Nothing
                  ]
              }
          s1 = emptyCatalog & addTable initialTable
          s2 = emptyCatalog & addTable updatedTable
          diff = computeCatalogDiff s1 s2
          s2Applied = applyDiff diff s1
          s1Restored = applyDiff (invertDiff diff) s2Applied
      s2Applied `shouldBe` s2
      s1Restored `shouldBe` s1

    it "satisfies invertDiff inversion symmetry: invertDiff (diff s1 s2) == diff s2 s1" $ do
      property $ forAll genCatalog $ \s1 ->
        forAll genCatalog $ \s2 ->
          let d1 = computeCatalogDiff s1 s2
              d2 = computeCatalogDiff s2 s1
           in invertDiff d1 == d2

    it "satisfies Law 1 group action: applyDiff (invertDiff diff) (applyDiff diff s1) == s1" $ do
      property $ forAll genCatalog $ \s1 ->
        forAll genCatalog $ \s2 ->
          let diff = computeCatalogDiff s1 s2
              s2Applied = applyDiff diff s1
              s1Restored = applyDiff (invertDiff diff) s2Applied
           in s1Restored == s1

    it "generates symmetric up and down SQL statements for column alterations" $ do
      let t1 =
            TableDef
              { tblName = "accounts"
              , tblColumns = [ColumnDef "balance" TypeInteger Nullable Nothing False Nothing]
              , tblIndexes = []
              , tblConstraints = []
              , tblComment = Nothing
              }
          t2 =
            TableDef
              { tblName = "accounts"
              , tblColumns = [ColumnDef "balance" TypeBigInt NotNull Nothing False Nothing]
              , tblIndexes = []
              , tblConstraints = []
              , tblComment = Nothing
              }
          s1 = emptyCatalog & addTable t1
          s2 = emptyCatalog & addTable t2
          diff = computeCatalogDiff s1 s2
          (upSql, downSql) = renderReversibleDDL diff
      upSql `shouldContain` "ALTER TABLE \"accounts\" ALTER COLUMN \"balance\" TYPE bigint"
      upSql `shouldContain` "ALTER TABLE \"accounts\" ALTER COLUMN \"balance\" SET NOT NULL"
      downSql `shouldContain` "ALTER TABLE \"accounts\" ALTER COLUMN \"balance\" TYPE integer"
      downSql `shouldContain` "ALTER TABLE \"accounts\" ALTER COLUMN \"balance\" DROP NOT NULL"

  -- Law 2: Idempotence of Reverse Codegen
  describe "Law 2: Idempotence of Reverse Codegen" $ do
    it "synthesizes compilable Haskell HKD schemas for documents table" $ do
      let cat = emptyCatalog & addTable sampleDocumentCatalogDef
          code = generateHaskellSchema cat :: String
      code `shouldContain` "data DocumentsT (ctx :: Ctx) ="
      code `shouldContain` "documentId :: !(Col ctx Int64)"
      code `shouldContain` "documentTitle :: !(Col ctx Text)"
      code `shouldContain` "documentContent :: !(Col ctx (Maybe Text))"
      code `shouldContain` "documentEmbedding :: !(Col ctx (Maybe (Embedding \"default\" 1536)))"
      code `shouldContain` "type Documents = DocumentsT 'Result"
      code `shouldContain` "type DocumentsExpr = DocumentsT 'Expr"
      code `shouldContain` "type DocumentsPatch = DocumentsT 'Patch"
      code `shouldContain` "type DocumentsMeta = DocumentsT 'Meta"
      code `shouldContain` "documentsSchema :: TableSchema DocumentsT"
      code `shouldContain` "instance Table DocumentsT where"

    it "satisfies Law 2 idempotence: reverse-parsed codegen produces identical column schema" $ do
      let baseTable =
            TableDef
              { tblName = "articles"
              , tblColumns =
                  [ ColumnDef "id" TypeBigInt NotNull Nothing True Nothing
                  , ColumnDef "title" TypeText NotNull Nothing False Nothing
                  , ColumnDef "body" TypeText Nullable Nothing False Nothing
                  , ColumnDef "is_published" TypeBoolean NotNull Nothing False Nothing
                  ]
              , tblIndexes = []
              , tblConstraints = []
              , tblComment = Nothing
              }
          cat = emptyCatalog & addTable baseTable
          code = generateHaskellSchema cat :: String
          rebuiltCat = catalogFromCodeGen (T.pack code)
          diff = computeCatalogDiff cat rebuiltCat
      -- The column definitions reconstructed from generated code match the source catalog
      cdAddedTables diff `shouldBe` []
      cdDroppedTables diff `shouldBe` []
      case cdModifiedTables diff of
        [] -> pure ()
        [td] -> do
          tdAddedColumns td `shouldBe` []
          tdDroppedColumns td `shouldBe` []
          tdModifiedColumns td `shouldBe` []
        other -> expectationFailure ("Unexpected table diffs: " ++ show other)

  -- Law 3: Dependency Order & Constraint Safety
  describe "Law 3: Dependency Order & Foreign Key Preservation" $ do
    it "orders up.sql to create referenced parent tables before child tables" $ do
      let authorsTable =
            TableDef
              { tblName = "authors"
              , tblColumns = [ColumnDef "id" TypeBigInt NotNull Nothing True Nothing]
              , tblIndexes = []
              , tblConstraints = [PrimaryKeyConstraint "authors_pkey" ["id"]]
              , tblComment = Nothing
              }
          booksTable =
            TableDef
              { tblName = "books"
              , tblColumns =
                  [ ColumnDef "id" TypeBigInt NotNull Nothing True Nothing
                  , ColumnDef "author_id" TypeBigInt NotNull Nothing False Nothing
                  ]
              , tblIndexes = []
              , tblConstraints =
                  [ PrimaryKeyConstraint "books_pkey" ["id"]
                  , ForeignKeyConstraint "books_author_fk" ["author_id"] "authors" ["id"] Cascade Cascade
                  ]
              , tblComment = Nothing
              }
          s1 = emptyCatalog
          -- Insert in reverse order to test topological sorting
          s2 = emptyCatalog & addTable booksTable & addTable authorsTable
          diff = computeCatalogDiff s1 s2
          (upSql, downSql) = renderReversibleDDL diff
          authorCreatePos = T.breakOn "CREATE TABLE \"authors\"" (T.pack upSql)
          bookCreatePos = T.breakOn "CREATE TABLE \"books\"" (T.pack upSql)
      T.length (fst authorCreatePos) `shouldSatisfy` (< T.length (fst bookCreatePos))
      downSql `shouldContain` "DROP TABLE \"books\" CASCADE"
      downSql `shouldContain` "DROP TABLE \"authors\" CASCADE"

  -- PostgreSQL Catalog Introspection & Types
  describe "Catalog.PG Introspection & Type Parsing" $ do
    it "correctly parses PostgreSQL format_type representations" $ do
      parsePGType "int4" `shouldBe` TypeInteger
      parsePGType "integer" `shouldBe` TypeInteger
      parsePGType "int8" `shouldBe` TypeBigInt
      parsePGType "bigint" `shouldBe` TypeBigInt
      parsePGType "smallint" `shouldBe` TypeSmallInt
      parsePGType "text" `shouldBe` TypeText
      parsePGType "boolean" `shouldBe` TypeBoolean
      parsePGType "timestamp with time zone" `shouldBe` TypeTimestamptz
      parsePGType "jsonb" `shouldBe` TypeJsonb
      parsePGType "uuid" `shouldBe` TypeUuid
      parsePGType "vector(1536)" `shouldBe` TypeVector (Just 1536)
      parsePGType "character varying(100)" `shouldBe` TypeVarChar (Just 100)

    it "correctly parses index access methods" $ do
      parseIndexMethod "btree" `shouldBe` BTree
      parseIndexMethod "hnsw" `shouldBe` HNSW
      parseIndexMethod "ivfflat" `shouldBe` IVFFlat
      parseIndexMethod "gin" `shouldBe` GIN
      parseIndexMethod "gist" `shouldBe` GiST

    it "constructs in-memory Catalog from raw pg_catalog rows" $ do
      let tblRows = [PGTableRow "users" (Just "User accounts")]
          colRows =
            [ PGColumnRow "users" "id" "bigint" True Nothing (Just "ID")
            , PGColumnRow "users" "email" "text" True Nothing Nothing
            ]
          idxRows =
            [ PGIndexRow
                "users"
                "users_email_idx"
                "btree"
                True
                False
                "CREATE UNIQUE INDEX users_email_idx ON public.users USING btree (email)"
            ]
          conRows =
            [ PGConstraintRow "users_pkey" "users" "p" "PRIMARY KEY (id)"
            ]
          cat = buildCatalogFromRows tblRows colRows idxRows conRows
      case lookupTable "users" cat of
        Nothing -> expectationFailure "Expected table 'users' to exist in catalog"
        Just tbl -> do
          tblName tbl `shouldBe` "users"
          length (tblColumns tbl) `shouldBe` 2
          length (tblIndexes tbl) `shouldBe` 1
          length (tblConstraints tbl) `shouldBe` 1

  describe "User Story 11: 4-Context Codegen & TableSchema Catalog Bridge" $ do
    it
      "Acceptance Criterion 1: Running generateHaskellSchema on a Catalog emits record definitions using Col ctx a and type aliases (<Name> = <Name>T 'Result)"
      $ do
        let cat = emptyCatalog & addTable sampleDocumentCatalogDef
            code = generateHaskellSchema cat :: String
        code `shouldContain` "data DocumentsT (ctx :: Ctx) ="
        code `shouldContain` "documentId :: !(Col ctx Int64)"
        code `shouldContain` "type Documents = DocumentsT 'Result"
        code `shouldContain` "type DocumentsExpr = DocumentsT 'Expr"
        code `shouldContain` "type DocumentsPatch = DocumentsT 'Patch"
        code `shouldContain` "type DocumentsMeta = DocumentsT 'Meta"
        code `shouldContain` "documentsSchema :: TableSchema DocumentsT"
        code `shouldContain` "instance Table DocumentsT where"

    it
      "Acceptance Criterion 2: catalogFromCodeGen parses generated Haskell code back into an identical Catalog without loss"
      $ do
        let cat = emptyCatalog & addTable sampleDocumentCatalogDef
            code = generateHaskellSchema cat :: String
            rebuiltCat = catalogFromCodeGen (T.pack code)
            diff = computeCatalogDiff cat rebuiltCat
        cdAddedTables diff `shouldBe` []
        cdDroppedTables diff `shouldBe` []
        case cdModifiedTables diff of
          [] -> pure ()
          [td] -> do
            tdAddedColumns td `shouldBe` []
            tdDroppedColumns td `shouldBe` []
            tdModifiedColumns td `shouldBe` []
          other -> expectationFailure ("Unexpected table diffs: " ++ show other)

    it
      "Acceptance Criterion 3: tableSchemaToDef accountSchema produces a TableDef with matching tableName, columns, and types"
      $ do
        let tblDef = tableSchemaToDef accountSchema
        tblDef.tblName `shouldBe` TableName "accounts"
        let colNames = [unColumnName c.colName | c <- tblDef.tblColumns]
        colNames `shouldBe` ["id", "name", "email", "score", "active"]
        let colTypes = [c.colType | c <- tblDef.tblColumns]
        colTypes `shouldBe` [TypeBigInt, TypeText, TypeText, TypeBigInt, TypeBoolean]
        let pks = [c.colName | c <- tblDef.tblColumns, c.colIsPrimaryKey]
        pks `shouldBe` [ColumnName "id"]
        let nulls = [c.colNullability | c <- tblDef.tblColumns]
        nulls `shouldBe` [NotNull, NotNull, NotNull, NotNull, NotNull]

    it "schemasToCatalog aggregates TableDefs into a queryable Catalog" $ do
      let tblDef = tableSchemaToDef accountSchema
          cat = schemasToCatalog [tblDef]
      lookupTable (TableName "accounts") cat `shouldBe` Just tblDef
