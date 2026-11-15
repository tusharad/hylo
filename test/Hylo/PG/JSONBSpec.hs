{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.JSONBSpec
Description : Unit and property specifications for PostgreSQL JSONB combinators
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.JSONBSpec (spec) where

import qualified Data.Aeson as Aeson
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Backend.Capability (Postgres)
import Hylo.Core.Context (Col, Ctx (..))
import Hylo.Core.Expr (colExpr, lit, (==.))
import Hylo.Core.Key (Id (..))
import Hylo.Core.Query (Query, each, where_)
import Hylo.Core.Table
  ( Table (..)
  , TableSchema (..)
  , colName
  , colSqlType
  , colSpecExpr
  , column
  , eraseColDef
  , primaryKey
  , tableSchema
  )
import Hylo.PG.Compiler.SQL (compilePostgresQuery, previewPostgresSQL)
import Hylo.PG.JSONB
import Hylo.SQL.AST (Projectable (..), renderExpr)
import Hylo.SQL.Builder (BoundParam (..), CompiledQuery (..))
import Test.Hspec

data MockDocT (ctx :: Ctx) = MockDoc
  { docId :: !(Col ctx (Id MockDocT Int64))
  , metadata :: !(Col ctx Aeson.Value)
  }

instance Table MockDocT where
  toTableExpr alias m =
    MockDoc
      { docId = colSpecExpr alias m.docId
      , metadata = colSpecExpr alias m.metadata
      }
  tableExprColumns d =
    [ renderExpr d.docId
    , renderExpr d.metadata
    ]
  tableColDefs m = [eraseColDef m.docId, eraseColDef m.metadata]
  tableColumnsMeta m =
    [ (colName m.docId, colSqlType m.docId)
    , (colName m.metadata, colSqlType m.metadata)
    ]
  patchEntries _ _ = []

instance Projectable (MockDocT 'Expr) where
  projectColumns = tableExprColumns

docTableSchema :: TableSchema MockDocT
docTableSchema =
  tableSchema "documents" $
    MockDoc
      { docId = primaryKey "id" "bigint"
      , metadata = column "metadata" "jsonb"
      }

spec :: Spec
spec = describe "User Story 17: PostgreSQL JSONB Expression Algebra" $ do
  it "binds litJsonb as parameter $1 with type jsonb" $ do
    let payload = Aeson.object ["role" Aeson..= ("admin" :: Text), "level" Aeson..= (42 :: Int)]
        q :: Query (Postgres v) (MockDocT 'Expr)
        q = do
          d <- each docTableSchema
          where_ (d.metadata @> litJsonb payload)
          pure d
        cq = compilePostgresQuery q
    cq.compiledSQL `shouldSatisfy` T.isInfixOf "(\"t0\".\"metadata\" @> $1)"
    case toList cq.compiledParams of
      [p1] -> do
        p1.paramIndex `shouldBe` 1
        p1.paramType `shouldBe` "jsonb"
        p1.paramValue `shouldSatisfy` T.isInfixOf "\"role\":\"admin\""
      other -> expectationFailure ("Expected exactly 1 param but got: " ++ show other)

  it "supports jsonbContainedBy (<@ and <@.) with bound parameters" $ do
    let payload = Aeson.object ["allowed" Aeson..= True]
        q :: Query (Postgres v) (MockDocT 'Expr)
        q = do
          d <- each docTableSchema
          where_ (d.metadata <@. litJsonb payload)
          pure d
        cq = compilePostgresQuery q
    cq.compiledSQL `shouldSatisfy` T.isInfixOf "(\"t0\".\"metadata\" <@ $1)"
    case toList cq.compiledParams of
      [p1] -> p1.paramType `shouldBe` "jsonb"
      other -> expectationFailure ("Expected exactly 1 param but got: " ++ show other)

  it "supports jsonbHasKey (?.) with bound text key" $ do
    let q :: Query (Postgres v) (MockDocT 'Expr)
        q = do
          d <- each docTableSchema
          where_ (d.metadata ?. lit ("verified" :: Text))
          pure d
        cq = compilePostgresQuery q
    cq.compiledSQL `shouldSatisfy` T.isInfixOf "(\"t0\".\"metadata\" ? $1)"
    case toList cq.compiledParams of
      [p1] -> p1.paramValue `shouldBe` "verified"
      other -> expectationFailure ("Expected exactly 1 param but got: " ++ show other)

  it "supports jsonbGet (->.) and jsonbGetText (->> and ->>.) field extraction" $ do
    let q :: Query (Postgres v) (MockDocT 'Expr)
        q = do
          d <- each docTableSchema
          where_ (d.metadata ->> lit ("username" :: Text) ==. lit ("alice" :: Text))
          pure d
        cq = compilePostgresQuery q
    cq.compiledSQL `shouldSatisfy` T.isInfixOf "(\"t0\".\"metadata\" ->> $1)"
    case toList cq.compiledParams of
      [p1, p2] -> do
        p1.paramValue `shouldBe` "username"
        p2.paramValue `shouldBe` "alice"
      other -> expectationFailure ("Expected exactly 2 params but got: " ++ show other)

  it "supports jsonbGetPath (#>.) and jsonbGetPathText (#>>.)" $ do
    let pathExpr = colExpr "" "ARRAY['address', 'city']"
        q :: Query (Postgres v) (MockDocT 'Expr)
        q = do
          d <- each docTableSchema
          where_ (d.metadata #>>. pathExpr ==. lit ("New York" :: Text))
          pure d
        cq = compilePostgresQuery q
    cq.compiledSQL `shouldSatisfy` T.isInfixOf "(\"t0\".\"metadata\" #>>"

  it "interpolates JSONB literals safely in previewPostgresSQL" $ do
    let payload = Aeson.object ["env" Aeson..= ("production" :: Text)]
        q :: Query (Postgres v) (MockDocT 'Expr)
        q = do
          d <- each docTableSchema
          where_ (d.metadata @> litJsonb payload)
          pure d
        sql = previewPostgresSQL q
    sql `shouldSatisfy` T.isInfixOf "(\"t0\".\"metadata\" @> '{\"env\":\"production\"}')"
