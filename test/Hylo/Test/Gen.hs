{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Hylo.Test.Gen
Description : Hedgehog generators for Hylo Query AST, expressions, schemas, records, and catalogs
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Test.Gen
  ( AccountProfileT (..)
  , accountProfileSchema
  , sampleAccount1
  , genAccountProfile
  , genIdentifier
  , genColumnType
  , genNullability
  , genColumnDef
  , genTableDef
  , deduplicateCols
  , genCatalog
  , genQExprInt
  , genQExprBool
  , genLimit
  , genOffset
  , genPagination
  , genOrderDirection
  , genNullsOrder
  , genOrderSpec
  , genOrderSpecs
  , genFilters
  , genQueryAST
  , genExprQueryAST
  , genRRFK
  , genRRFRank
  , genRRFRankPair
  ) where

import Data.Foldable (for_)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hylo.Core
  ( Col
  , Ctx (..)
  , Id
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
  , toId
  )
import qualified Hylo.Core.Context as CoreCtx
import Hylo.Core.Expr
  ( QExpr
  , colExpr
  , in_
  , lit
  , (&&.)
  , (/=.)
  , (<.)
  , (<=.)
  , (==.)
  , (>.)
  , (>=.)
  , (||.)
  )
import Hylo.Core.Query
  ( NullsOrder (..)
  , OrderDirection (..)
  , OrderSpec
  , PGVersion (..)
  , Postgres
  , Query
  , asc
  , desc
  , each
  , limit
  , nullsFirst
  , nullsLast
  , offset
  , orderBy
  , where_
  )
import Hylo.Migrate.AST
  ( ColumnDef (..)
  , ColumnName (..)
  , ColumnNullability (..)
  , ColumnType (..)
  , TableDef (..)
  , TableName (..)
  )
import Hylo.Migrate.Catalog (Catalog, addTable, catalogTables, emptyCatalog)
import Hylo.SQL.AST (Projectable (..), renderExpr)

--------------------------------------------------------------------------------
-- Test Table: AccountProfile Entity with 10 Columns
--------------------------------------------------------------------------------

data AccountProfileT (ctx :: Ctx) = AccountProfile
  { id :: !(Col ctx (Id AccountProfileT Int64))
  , username :: !(Col ctx Text)
  , email :: !(Col ctx Text)
  , age :: !(Col ctx Int64)
  , bio :: !(Col ctx Text)
  , country :: !(Col ctx Text)
  , phone :: !(Col ctx Text)
  , role :: !(Col ctx Text)
  , score :: !(Col ctx Int64)
  , active :: !(Col ctx Bool)
  }
  deriving (Generic)

deriving instance Eq (AccountProfileT 'Result)
deriving instance Show (AccountProfileT 'Result)
deriving instance Eq (AccountProfileT 'Patch)
deriving instance Show (AccountProfileT 'Patch)

accountProfileSchema :: TableSchema AccountProfileT
accountProfileSchema =
  tableSchema "account_profiles" $
    AccountProfile
      { id = primaryKey "id" "bigint"
      , username = column "username" "text"
      , email = column "email" "text"
      , age = column "age" "bigint"
      , bio = column "bio" "text"
      , country = column "country" "text"
      , phone = column "phone" "text"
      , role = column "role" "text"
      , score = column "score" "bigint"
      , active = column "active" "boolean"
      }

instance Table AccountProfileT where
  toTableExpr alias meta =
    AccountProfile
      { id = colSpecExpr alias meta.id
      , username = colSpecExpr alias meta.username
      , email = colSpecExpr alias meta.email
      , age = colSpecExpr alias meta.age
      , bio = colSpecExpr alias meta.bio
      , country = colSpecExpr alias meta.country
      , phone = colSpecExpr alias meta.phone
      , role = colSpecExpr alias meta.role
      , score = colSpecExpr alias meta.score
      , active = colSpecExpr alias meta.active
      }
  diffTable old new =
    AccountProfile
      { id = diffField old.id new.id
      , username = diffField old.username new.username
      , email = diffField old.email new.email
      , age = diffField old.age new.age
      , bio = diffField old.bio new.bio
      , country = diffField old.country new.country
      , phone = diffField old.phone new.phone
      , role = diffField old.role new.role
      , score = diffField old.score new.score
      , active = diffField old.active new.active
      }
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.username
    , eraseColDef m.email
    , eraseColDef m.age
    , eraseColDef m.bio
    , eraseColDef m.country
    , eraseColDef m.phone
    , eraseColDef m.role
    , eraseColDef m.score
    , eraseColDef m.active
    ]
  tableExprColumns acc =
    [ renderExpr acc.id
    , renderExpr acc.username
    , renderExpr acc.email
    , renderExpr acc.age
    , renderExpr acc.bio
    , renderExpr acc.country
    , renderExpr acc.phone
    , renderExpr acc.role
    , renderExpr acc.score
    , renderExpr acc.active
    ]
  patchEntries meta patch =
    concat
      [ patchEntry meta.id patch.id
      , patchEntry meta.username patch.username
      , patchEntry meta.email patch.email
      , patchEntry meta.age patch.age
      , patchEntry meta.bio patch.bio
      , patchEntry meta.country patch.country
      , patchEntry meta.phone patch.phone
      , patchEntry meta.role patch.role
      , patchEntry meta.score patch.score
      , patchEntry meta.active patch.active
      ]
  dirtyFieldsTable patch =
    Map.fromList
      [ (CoreCtx.colName c, case v of ValSetTo t -> t; ValSetDefault -> "DEFAULT")
      | (c, v) <- patchEntries (tableMeta accountProfileSchema) patch
      ]

instance Projectable (AccountProfileT 'Expr) where
  projectColumns = tableExprColumns

sampleAccount1 :: AccountProfileT 'Result
sampleAccount1 =
  AccountProfile
    { id = toId 42
    , username = "tushar"
    , email = "tushar@example.com"
    , age = 30
    , bio = "Haskell ORM Architect"
    , country = "India"
    , phone = "+919876543210"
    , role = "lead"
    , score = 100
    , active = True
    }

--------------------------------------------------------------------------------
-- Record Generator
--------------------------------------------------------------------------------

-- | Hedgehog generator for arbitrary 10-column domain record.
genAccountProfile :: Gen (AccountProfileT 'Result)
genAccountProfile =
  AccountProfile . toId
    <$> Gen.int64 (Range.linear 1 100000)
    <*> genIdentifier
    <*> ((<> "@example.com") <$> genIdentifier)
    <*> Gen.int64 (Range.linear 18 90)
    <*> Gen.text (Range.linear 0 30) Gen.alpha
    <*> Gen.element ["USA", "Germany", "India", "UK", "Canada", "Japan"]
    <*> ((\n -> "+1" <> T.pack (show n)) <$> Gen.int (Range.linear 1000000 9999999))
    <*> Gen.element ["admin", "user", "editor", "moderator"]
    <*> Gen.int64 (Range.linear 0 1000)
    <*> Gen.bool

--------------------------------------------------------------------------------
-- Expression Generators
--------------------------------------------------------------------------------

-- | Hedgehog generator for integer expressions with arithmetic operators.
genQExprInt :: Gen (QExpr Int64)
genQExprInt =
  Gen.recursive
    Gen.choice
    [ lit <$> Gen.int64 (Range.linear (-1000) 1000)
    , Gen.element
        [ colExpr "t0" "id"
        , colExpr "t0" "score"
        , colExpr "t0" "age"
        ]
    ]
    [ Gen.subterm2 genQExprInt genQExprInt (+)
    , Gen.subterm2 genQExprInt genQExprInt (-)
    , Gen.subterm2 genQExprInt genQExprInt (*)
    , Gen.subterm genQExprInt negate
    , Gen.subterm genQExprInt abs
    ]

-- | Hedgehog generator for boolean expressions with comparisons and logical connectives.
genQExprBool :: Gen (QExpr Bool)
genQExprBool =
  Gen.recursive
    Gen.choice
    [ lit <$> Gen.bool
    , (==.) <$> genQExprInt <*> genQExprInt
    , (/=.) <$> genQExprInt <*> genQExprInt
    , (<.) <$> genQExprInt <*> genQExprInt
    , (<=.) <$> genQExprInt <*> genQExprInt
    , (>.) <$> genQExprInt <*> genQExprInt
    , (>=.) <$> genQExprInt <*> genQExprInt
    , in_ (colExpr "t0" "score" :: QExpr Int64)
        <$> Gen.list (Range.linear 1 4) (Gen.int64 (Range.linear 0 100))
    ]
    [ Gen.subterm2 genQExprBool genQExprBool (&&.)
    , Gen.subterm2 genQExprBool genQExprBool (||.)
    ]

--------------------------------------------------------------------------------
-- Query Pagination and Ordering Generators
--------------------------------------------------------------------------------

-- | Hedgehog generator for query LIMIT clauses.
genLimit :: Gen Int
genLimit = Gen.int (Range.linear 1 500)

-- | Hedgehog generator for query OFFSET clauses.
genOffset :: Gen Int
genOffset = Gen.int (Range.linear 0 500)

-- | Hedgehog generator for combined pagination (limit, offset).
genPagination :: Gen (Maybe Int, Maybe Int)
genPagination = (,) <$> Gen.maybe genLimit <*> Gen.maybe genOffset

-- | Hedgehog generator for sorting directions.
genOrderDirection :: Gen OrderDirection
genOrderDirection = Gen.element [Asc, Desc]

-- | Hedgehog generator for NULLS order placement.
genNullsOrder :: Gen NullsOrder
genNullsOrder = Gen.element [NullsDefault, NullsFirst, NullsLast]

-- | Hedgehog generator for a single OrderSpec.
genOrderSpec :: Gen OrderSpec
genOrderSpec = do
  expr <-
    Gen.element
      [ colExpr "t0" "id" :: QExpr Int64
      , colExpr "t0" "score"
      , colExpr "t0" "age"
      ]
  dir <- genOrderDirection
  nulls <- genNullsOrder
  let baseSpec = case dir of
        Asc -> asc expr
        Desc -> desc expr
  pure $ case nulls of
    NullsDefault -> baseSpec
    NullsFirst -> nullsFirst baseSpec
    NullsLast -> nullsLast baseSpec

-- | Hedgehog generator for a list of OrderSpecs.
genOrderSpecs :: Gen [OrderSpec]
genOrderSpecs = Gen.list (Range.linear 0 3) genOrderSpec

-- | Hedgehog generator for a list of query WHERE filter predicates.
genFilters :: Gen [QExpr Bool]
genFilters = Gen.list (Range.linear 0 3) genQExprBool

--------------------------------------------------------------------------------
-- Query AST Generators
--------------------------------------------------------------------------------

-- | Hedgehog generator for full Table Query ASTs.
genQueryAST :: Gen (Query (Postgres 'PG16) (AccountProfileT 'Expr))
genQueryAST = do
  filters <- genFilters
  orderSpecs <- genOrderSpecs
  (mLim, mOff) <- genPagination
  pure $ do
    acc <- each accountProfileSchema
    mapM_ where_ filters
    case orderSpecs of
      [] -> pure ()
      specs -> orderBy specs
    for_ mLim limit
    for_ mOff offset
    pure acc

-- | Hedgehog generator for expression-returning Query ASTs.
genExprQueryAST :: Gen (Query (Postgres 'PG16) (QExpr Int64))
genExprQueryAST = do
  e <- genQExprInt
  filters <- genFilters
  orderSpecs <- genOrderSpecs
  (mLim, mOff) <- genPagination
  pure $ do
    _ <- each accountProfileSchema
    mapM_ where_ filters
    case orderSpecs of
      [] -> pure ()
      specs -> orderBy specs
    for_ mLim limit
    for_ mOff offset
    pure e

--------------------------------------------------------------------------------
-- Catalog & Migration Generators
--------------------------------------------------------------------------------

-- | Hedgehog generator for lower-case alphanumeric SQL identifiers.
genIdentifier :: Gen Text
genIdentifier = do
  firstChar <- Gen.lower
  rest <- Gen.string (Range.linear 2 7) (Gen.element (['a' .. 'z'] ++ ['0' .. '9']))
  pure (T.pack (firstChar : rest))

-- | Hedgehog generator for standard database column types.
genColumnType :: Gen ColumnType
genColumnType =
  Gen.element
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

-- | Hedgehog generator for column nullability constraints.
genNullability :: Gen ColumnNullability
genNullability = Gen.element [Nullable, NotNull]

-- | Hedgehog generator for column definition metadata.
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

-- | Remove duplicate column names preserving order.
deduplicateCols :: [ColumnDef] -> [ColumnDef]
deduplicateCols = go []
  where
    go _ [] = []
    go seen (c : cs)
      | c.colName `elem` seen = go seen cs
      | otherwise = c : go (c.colName : seen) cs

-- | Hedgehog generator for table definitions with distinct column names.
genTableDef :: Gen TableDef
genTableDef = do
  tName <- genIdentifier
  numCols <- Gen.int (Range.linear 1 5)
  cols <- Gen.list (Range.singleton numCols) genColumnDef
  let uniqueCols = deduplicateCols cols
  pure
    TableDef
      { tblName = TableName tName
      , tblColumns = uniqueCols
      , tblIndexes = []
      , tblConstraints = []
      , tblComment = Nothing
      }

-- | Hedgehog generator for arbitrary database catalogs.
genCatalog :: Gen Catalog
genCatalog = do
  numTables <- Gen.int (Range.linear 0 4)
  tbls <- Gen.list (Range.singleton numTables) genTableDef
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

--------------------------------------------------------------------------------
-- RRF Rank Generators
--------------------------------------------------------------------------------

-- | Hedgehog generator for positive smoothing factor @k@.
genRRFK :: Gen Double
genRRFK = Gen.double (Range.linearFrac 0.0 1000.0)

-- | Hedgehog generator for positive 1-based ranks.
genRRFRank :: Gen Int
genRRFRank = Gen.int (Range.linear 1 100000)

-- | Hedgehog generator for rank pairs.
genRRFRankPair :: Gen (Int, Int)
genRRFRankPair = (,) <$> genRRFRank <*> genRRFRank
