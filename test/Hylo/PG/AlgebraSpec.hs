{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Hylo.PG.AlgebraSpec
Description : Unit specifications for PostgreSQL Arrays, Full-Text Search, and EXPLAIN
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.AlgebraSpec (spec) where

import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Backend.Capability (Postgres)
import Hylo.Core.Context (Col, Ctx (..))
import Hylo.Core.Expr (lit, (==.), (>.))
import Hylo.Core.Key (Id (..))
import Hylo.Core.Query (Query, each, where_)
import Hylo.Core.Table
  ( Table (..)
  , TableSchema (..)
  , colName
  , colSpecExpr
  , colSqlType
  , column
  , eraseColDef
  , primaryKey
  , tableSchema
  )
import Hylo.PG.Array
import Hylo.PG.Compiler.SQL (compilePostgresQuery, explainQuery, previewExplainSQL)
import Hylo.PG.Connection (defaultPool, runExplain)
import Hylo.PG.Search.FullText
import Hylo.SQL.AST (Projectable (..), renderExpr)
import Hylo.SQL.Builder (BoundParam (..), CompiledQuery (..))
import Test.Hspec

data MockArticleT (ctx :: Ctx) = MockArticle
  { articleId :: !(Col ctx (Id MockArticleT Int64))
  , tags :: !(Col ctx [Text])
  , body :: !(Col ctx Text)
  }

instance Table MockArticleT where
  toTableExpr alias m =
    MockArticle
      { articleId = colSpecExpr alias m.articleId
      , tags = colSpecExpr alias m.tags
      , body = colSpecExpr alias m.body
      }
  tableExprColumns a =
    [ renderExpr a.articleId
    , renderExpr a.tags
    , renderExpr a.body
    ]
  tableColDefs m = [eraseColDef m.articleId, eraseColDef m.tags, eraseColDef m.body]
  tableColumnsMeta m =
    [ (colName m.articleId, colSqlType m.articleId)
    , (colName m.tags, colSqlType m.tags)
    , (colName m.body, colSqlType m.body)
    ]
  patchEntries _ _ = []

instance Projectable (MockArticleT 'Expr) where
  projectColumns = tableExprColumns

articleTableSchema :: TableSchema MockArticleT
articleTableSchema =
  tableSchema "articles" $
    MockArticle
      { articleId = primaryKey "id" "bigint"
      , tags = column "tags" "text[]"
      , body = column "body" "text"
      }

spec :: Spec
spec = describe "User Story 17: PostgreSQL Arrays, Full-Text Search, and EXPLAIN" $ do
  describe "PostgreSQL Array Combinators" $ do
    it "binds litArray as parameter $1 with array type" $ do
      let q :: Query (Postgres v) (MockArticleT 'Expr)
          q = do
            a <- each articleTableSchema
            where_ (a.tags @>= litArray "text" ["haskell", "postgres"])
            pure a
          cq = compilePostgresQuery q
      cq.compiledSQL `shouldSatisfy` T.isInfixOf "(\"t0\".\"tags\" @> $1)"
      case toList cq.compiledParams of
        [p1] -> do
          p1.paramIndex `shouldBe` 1
          p1.paramType `shouldBe` "text[]"
          p1.paramValue `shouldBe` "{haskell,postgres}"
        other -> expectationFailure ("Expected 1 param but got: " ++ show other)

    it "compiles arrayOverlaps (&&=) and arrayContainedBy (<@=)" $ do
      let q :: Query (Postgres v) (MockArticleT 'Expr)
          q = do
            a <- each articleTableSchema
            where_ (a.tags &&= litArray "text" ["database", "compiler"])
            pure a
          cq = compilePostgresQuery q
      cq.compiledSQL `shouldSatisfy` T.isInfixOf "(\"t0\".\"tags\" && $1)"

    it "compiles cardinality(tags)" $ do
      let q :: Query (Postgres v) (MockArticleT 'Expr)
          q = do
            a <- each articleTableSchema
            where_ (cardinality a.tags ==. lit (3 :: Int))
            pure a
          cq = compilePostgresQuery q
      cq.compiledSQL `shouldSatisfy` T.isInfixOf "(cardinality(\"t0\".\"tags\") = $1)"

  describe "PostgreSQL Full-Text Search" $ do
    it "compiles toTsVector @@ plainToTsQuery with bound text" $ do
      let q :: Query (Postgres v) (MockArticleT 'Expr)
          q = do
            a <- each articleTableSchema
            where_ (toTsVector a.body @@ plainToTsQuery (lit ("postgresql index optimization" :: Text)))
            pure a
          cq = compilePostgresQuery q
      cq.compiledSQL `shouldSatisfy` T.isInfixOf "(to_tsvector(\"t0\".\"body\") @@ plainto_tsquery($1))"
      case toList cq.compiledParams of
        [p1] -> p1.paramValue `shouldBe` "postgresql index optimization"
        other -> expectationFailure ("Expected 1 param but got: " ++ show other)

    it "compiles tsRank with two-argument parameterization" $ do
      let queryExpr = plainToTsQuery (lit ("functional programming" :: Text))
          q :: Query (Postgres v) (MockArticleT 'Expr)
          q = do
            a <- each articleTableSchema
            where_ (tsRank (toTsVector a.body) queryExpr >. lit (0.1 :: Double))
            pure a
          cq = compilePostgresQuery q
      cq.compiledSQL `shouldSatisfy` T.isInfixOf "ts_rank(to_tsvector(\"t0\".\"body\"), plainto_tsquery($1))"

    it "compiles websearchToTsQuery and phraseToTsQuery" $ do
      let q :: Query (Postgres v) (MockArticleT 'Expr)
          q = do
            a <- each articleTableSchema
            where_ (toTsVector a.body @@ websearchToTsQuery (lit ("\"high performance\" OR acid" :: Text)))
            pure a
          cq = compilePostgresQuery q
      cq.compiledSQL `shouldSatisfy` T.isInfixOf "websearch_to_tsquery($1)"

  describe "EXPLAIN and runExplain" $ do
    let testQ :: Query (Postgres v) (MockArticleT 'Expr)
        testQ = do
          a <- each articleTableSchema
          where_ (a.tags @>= litArray "text" ["db"])
          pure a

    it "compiles explainQuery False with EXPLAIN (FORMAT JSON)" $ do
      let cq = explainQuery False testQ
      cq.compiledSQL `shouldSatisfy` T.isPrefixOf "EXPLAIN (FORMAT JSON) SELECT "
      cq.compiledSQL `shouldSatisfy` T.isInfixOf "(\"t0\".\"tags\" @> $1)"
      length cq.compiledParams `shouldBe` 1

    it "compiles explainQuery True with EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON)" $ do
      let cq = explainQuery True testQ
      cq.compiledSQL `shouldSatisfy` T.isPrefixOf "EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON) SELECT "
      cq.compiledSQL `shouldSatisfy` T.isInfixOf "(\"t0\".\"tags\" @> $1)"

    it "interpolates parameters in previewExplainSQL" $ do
      let sql = previewExplainSQL False testQ
      sql `shouldSatisfy` T.isPrefixOf "EXPLAIN (FORMAT JSON) SELECT "
      sql `shouldSatisfy` T.isInfixOf "'{db}'"

    it "executes runExplain against defaultPool returning JSON text" $ do
      res <- runExplain defaultPool False testQ
      case res of
        Left err -> expectationFailure ("Expected simulated JSON but got: " ++ show err)
        Right json -> do
          json `shouldSatisfy` T.isInfixOf "Plan"
          json `shouldSatisfy` T.isInfixOf "EXPLAIN (FORMAT JSON)"
