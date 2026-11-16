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
Module      : Hylo.SQLiteSpec
Description : Test suite for User Story 1: SQLite 3.38+ Relational Dialect & Correlated JSON Subqueries
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.SQLiteSpec (spec) where

import Control.Concurrent.STM (newTVarIO, readTVarIO, writeTVar)
import Control.Monad.STM (atomically)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Backend.Capability (PGVersion (..), Postgres, SQLite, SupportsLateralSubqueries)
import Hylo.Core
  ( Col
  , Ctx (..)
  , Id (..)
  , NullableTable (..)
  , Table (..)
  , TableSchema
  , colSpecExpr
  , column
  , eraseColDef
  , just
  , lit
  , onCommit
  , onRollback
  , primaryKey
  , setTxExtension
  , tableSchema
  , txFail
  , (==.)
  )
import Hylo.Core.Query (NestedList (..), Query, each, limit, many, optional, where_)
import Hylo.Core.Transaction (TxError (..))
import Hylo.PG.Codec.Binary
  ( TableCodec (..)
  , boolCodec
  , field
  , idCodec
  , int64Codec
  , jsonField
  , textCodec
  )
import Hylo.SQL.AST (Projectable (..), renderExpr)
import Hylo.SQL.Builder
  ( BoundParam (..)
  , CompiledQuery (..)
  , Dialect (..)
  , compileQuery
  , quoteIdentifier
  , quoteScopedCol
  )
import Hylo.SQLite
  ( closePool
  , compileSQLiteQuery
  , createMemoryPool
  , executeSql
  , runTxReadWrite
  , selectRows
  )
import Test.Hspec

--------------------------------------------------------------------------------
-- Domain Tables for Testing
--------------------------------------------------------------------------------

-- | 1. Organization Table
data OrgT (ctx :: Ctx) = Org
  { id :: !(Col ctx (Id OrgT Int64))
  , name :: !(Col ctx Text)
  }
  deriving (Generic)

type Org = OrgT 'Result

deriving instance Eq Org
deriving instance Show Org

orgTable :: TableSchema OrgT
orgTable =
  tableSchema "orgs" $
    Org
      { id = primaryKey "id" "bigint"
      , name = column "name" "text"
      }

instance Table OrgT where
  toTableExpr alias m =
    Org
      { id = colSpecExpr alias m.id
      , name = colSpecExpr alias m.name
      }
  tableExprColumns o =
    [ renderExpr o.id
    , renderExpr o.name
    ]
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.name
    ]

instance Projectable (OrgT 'Expr) where
  projectColumns = tableExprColumns

instance TableCodec OrgT where
  tableCodec =
    Org
      <$> field (.id) (idCodec int64Codec)
      <*> field (.name) textCodec
  tableJsonDecoder =
    Org
      <$> jsonField
      <*> jsonField

-- | 2. User Table
data SQLiteUserT (ctx :: Ctx) = SQLiteUser
  { id :: !(Col ctx (Id SQLiteUserT Int64))
  , orgId :: !(Col ctx (Id OrgT Int64))
  , name :: !(Col ctx Text)
  , active :: !(Col ctx Bool)
  }
  deriving (Generic)

type SQLiteUser = SQLiteUserT 'Result

deriving instance Eq SQLiteUser
deriving instance Show SQLiteUser

userTable :: TableSchema SQLiteUserT
userTable =
  tableSchema "users" $
    SQLiteUser
      { id = primaryKey "id" "bigint"
      , orgId = column "org_id" "bigint"
      , name = column "name" "text"
      , active = column "active" "boolean"
      }

instance Table SQLiteUserT where
  toTableExpr alias m =
    SQLiteUser
      { id = colSpecExpr alias m.id
      , orgId = colSpecExpr alias m.orgId
      , name = colSpecExpr alias m.name
      , active = colSpecExpr alias m.active
      }
  tableExprColumns u =
    [ renderExpr u.id
    , renderExpr u.orgId
    , renderExpr u.name
    , renderExpr u.active
    ]
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.orgId
    , eraseColDef m.name
    , eraseColDef m.active
    ]

instance Projectable (SQLiteUserT 'Expr) where
  projectColumns = tableExprColumns

instance TableCodec SQLiteUserT where
  tableCodec =
    SQLiteUser
      <$> field (.id) (idCodec int64Codec)
      <*> field (.orgId) (idCodec int64Codec)
      <*> field (.name) textCodec
      <*> field (.active) boolCodec
  tableJsonDecoder =
    SQLiteUser
      <$> jsonField
      <*> jsonField
      <*> jsonField
      <*> jsonField

-- | 3. Profile Table (for optional LEFT JOIN)
data SQLiteProfileT (ctx :: Ctx) = SQLiteProfile
  { id :: !(Col ctx (Id SQLiteProfileT Int64))
  , userId :: !(Col ctx (Id SQLiteUserT Int64))
  , bio :: !(Col ctx Text)
  }
  deriving (Generic)

type SQLiteProfile = SQLiteProfileT 'Result

deriving instance Eq SQLiteProfile
deriving instance Show SQLiteProfile

profileTable :: TableSchema SQLiteProfileT
profileTable =
  tableSchema "profiles" $
    SQLiteProfile
      { id = primaryKey "id" "bigint"
      , userId = column "user_id" "bigint"
      , bio = column "bio" "text"
      }

instance Table SQLiteProfileT where
  toTableExpr alias m =
    SQLiteProfile
      { id = colSpecExpr alias m.id
      , userId = colSpecExpr alias m.userId
      , bio = colSpecExpr alias m.bio
      }
  tableExprColumns p =
    [ renderExpr p.id
    , renderExpr p.userId
    , renderExpr p.bio
    ]
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.userId
    , eraseColDef m.bio
    ]

instance Projectable (SQLiteProfileT 'Expr) where
  projectColumns = tableExprColumns

instance TableCodec SQLiteProfileT where
  tableCodec =
    SQLiteProfile
      <$> field (.id) (idCodec int64Codec)
      <*> field (.userId) (idCodec int64Codec)
      <*> field (.bio) textCodec
  tableJsonDecoder =
    SQLiteProfile
      <$> jsonField
      <*> jsonField
      <*> jsonField

-- | 4. Document Table (for nested collections via many)
data SQLiteDocT (ctx :: Ctx) = SQLiteDoc
  { id :: !(Col ctx (Id SQLiteDocT Int64))
  , userId :: !(Col ctx (Id SQLiteUserT Int64))
  , title :: !(Col ctx Text)
  }
  deriving (Generic)

type SQLiteDoc = SQLiteDocT 'Result

deriving instance Eq SQLiteDoc
deriving instance Show SQLiteDoc

docTable :: TableSchema SQLiteDocT
docTable =
  tableSchema "documents" $
    SQLiteDoc
      { id = primaryKey "id" "bigint"
      , userId = column "user_id" "bigint"
      , title = column "title" "text"
      }

instance Table SQLiteDocT where
  toTableExpr alias m =
    SQLiteDoc
      { id = colSpecExpr alias m.id
      , userId = colSpecExpr alias m.userId
      , title = colSpecExpr alias m.title
      }
  tableExprColumns d =
    [ renderExpr d.id
    , renderExpr d.userId
    , renderExpr d.title
    ]
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.userId
    , eraseColDef m.title
    ]

instance Projectable (SQLiteDocT 'Expr) where
  projectColumns = tableExprColumns

instance TableCodec SQLiteDocT where
  tableCodec =
    SQLiteDoc
      <$> field (.id) (idCodec int64Codec)
      <*> field (.userId) (idCodec int64Codec)
      <*> field (.title) textCodec
  tableJsonDecoder =
    SQLiteDoc
      <$> jsonField
      <*> jsonField
      <*> jsonField

--------------------------------------------------------------------------------
-- Polymorphic Query (Acceptance Criterion 1)
--------------------------------------------------------------------------------

{- | Polymorphic query containing relational joins (each), optional left join (optional),
and nested collections (many). Compiles identically on PostgreSQL and SQLite!
-}
polymorphicQuery ::
  forall db.
  SupportsLateralSubqueries db =>
  Query db (SQLiteUserT 'Expr, NullableTable (SQLiteProfileT 'Expr), NestedList (SQLiteDocT 'Expr))
polymorphicQuery = do
  u <- each userTable
  p <- optional profileTable (\prof -> prof.userId ==. just u.id)
  docs <- many $ do
    d <- each docTable
    where_ (d.userId ==. u.id)
    limit 5
    pure d
  pure (u, p, docs)

--------------------------------------------------------------------------------
-- Test Suite Specification
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "User Story 1: SQLite 3.38+ Relational Dialect & Correlated JSON Subqueries (hylo-sqlite)" $ do
  describe "1. SQLite Dialect & SQL Builder" $ do
    it "quotes identifiers using standard double quotes and uses ?1, ?2 parameter syntax" $ do
      quoteIdentifier SQLiteDialect "table" `shouldBe` "\"table\""
      quoteIdentifier SQLiteDialect "column_name" `shouldBe` "\"column_name\""
      quoteScopedCol SQLiteDialect "t0" "id" `shouldBe` "\"t0\".\"id\""

    it "compiles single-table queries with SQLite double-quoting and ?1 parameter binding" $ do
      let q :: Query SQLite (SQLiteUserT 'Expr)
          q = do
            u <- each userTable
            where_ (u.name ==. lit ("Alice" :: Text))
            pure u
      let cq = compileSQLiteQuery q
      let sql = T.unpack cq.compiledSQL
      sql
        `shouldContain` "SELECT \"t0\".\"id\", \"t0\".\"org_id\", \"t0\".\"name\", \"t0\".\"active\" FROM \"users\" AS \"t0\""
      sql `shouldContain` "WHERE (\"t0\".\"name\" = ?1)"
      toList cq.compiledParams `shouldBe` [BoundParam 1 "Alice" "text"]

  describe "2. Correlated JSON Subquery Lowering for many on SQLite" $ do
    it
      "lowers many into COALESCE((SELECT json_group_array(json_array(...)) ...), '[]') in the SELECT list"
      $ do
        let q :: Query SQLite (SQLiteUserT 'Expr, NestedList (SQLiteDocT 'Expr))
            q = do
              u <- each userTable
              docs <- many $ do
                d <- each docTable
                where_ (d.userId ==. u.id)
                limit 5
                pure d
              pure (u, docs)
        let cq = compileSQLiteQuery q
        let sql = T.unpack cq.compiledSQL

        -- Must NOT emit LEFT JOIN LATERAL in FROM clause for SQLite!
        sql `shouldNotContain` "LATERAL"
        sql `shouldNotContain` "jsonb_agg"

        -- Must emit correlated scalar subquery with json_group_array and json_array in SELECT list
        sql
          `shouldContain` "COALESCE((SELECT json_group_array(json_array(\"t1\".\"id\", \"t1\".\"user_id\", \"t1\".\"title\")) FROM \"documents\" AS \"t1\" WHERE (\"t1\".\"user_id\" = \"t0\".\"id\") LIMIT 5), '[]')"
        sql `shouldContain` "FROM \"users\" AS \"t0\""

    it "handles multi-level nested queries with json(...) wrapping inner subqueries" $ do
      let q :: Query SQLite (OrgT 'Expr, NestedList (SQLiteUserT 'Expr, NestedList (SQLiteDocT 'Expr)))
          q = do
            o <- each orgTable
            usersWithDocs <- many $ do
              u <- each userTable
              where_ (u.orgId ==. o.id)
              docs <- many $ do
                d <- each docTable
                where_ (d.userId ==. u.id)
                pure d
              pure (u, docs)
            pure (o, usersWithDocs)
      let cq = compileSQLiteQuery q
      let sql = T.unpack cq.compiledSQL

      -- Outer query projects org columns and users subquery
      sql `shouldContain` "FROM \"orgs\" AS \"t0\""
      sql
        `shouldContain` "json_group_array(json_array(\"t1\".\"id\", \"t1\".\"org_id\", \"t1\".\"name\", \"t1\".\"active\", json(COALESCE((SELECT json_group_array(json_array(\"t2\".\"id\", \"t2\".\"user_id\", \"t2\".\"title\")) FROM \"documents\" AS \"t2\" WHERE (\"t2\".\"user_id\" = \"t1\".\"id\")), '[]'))))"

  describe "3. Acceptance Criterion 1: Polymorphic Query Compilation & Execution" $ do
    it
      "compiles the same polymorphic query to PostgreSQL (LATERAL) and SQLite (correlated JSON subquery)"
      $ do
        -- Compile for PostgreSQL
        let pgCompiled =
              compileQuery
                PostgresDialect
                ( polymorphicQuery ::
                    Query
                      (Postgres 'PG17)
                      (SQLiteUserT 'Expr, NullableTable (SQLiteProfileT 'Expr), NestedList (SQLiteDocT 'Expr))
                )
        let pgSql = T.unpack pgCompiled.compiledSQL
        pgSql `shouldContain` "LEFT JOIN \"profiles\" AS \"t1\""
        pgSql `shouldContain` "LEFT JOIN LATERAL ("
        pgSql `shouldContain` "COALESCE(jsonb_agg(jsonb_build_array("
        pgSql `shouldContain` " ON TRUE"

        -- Compile for SQLite
        let sqliteCompiled =
              compileQuery
                SQLiteDialect
                ( polymorphicQuery ::
                    Query
                      SQLite
                      (SQLiteUserT 'Expr, NullableTable (SQLiteProfileT 'Expr), NestedList (SQLiteDocT 'Expr))
                )
        let sqliteSql = T.unpack sqliteCompiled.compiledSQL
        sqliteSql `shouldContain` "LEFT JOIN \"profiles\" AS \"t1\""
        sqliteSql `shouldNotContain` "LATERAL"
        sqliteSql
          `shouldContain` "COALESCE((SELECT json_group_array(json_array(\"t2\".\"id\", \"t2\".\"user_id\", \"t2\".\"title\")) FROM \"documents\" AS \"t2\" WHERE (\"t2\".\"user_id\" = \"t0\".\"id\") LIMIT 5), '[]')"

    it "executes polymorphicQuery against in-memory SQLite and hydrates domain results" $ do
      p <- createMemoryPool
      _ <-
        executeSql
          p
          "CREATE TABLE users (id INTEGER PRIMARY KEY, org_id INTEGER NOT NULL, name TEXT NOT NULL, active INTEGER NOT NULL);\
          \CREATE TABLE profiles (id INTEGER PRIMARY KEY, user_id INTEGER NOT NULL, bio TEXT NOT NULL);\
          \CREATE TABLE documents (id INTEGER PRIMARY KEY, user_id INTEGER NOT NULL, title TEXT NOT NULL);\
          \INSERT INTO users VALUES (1, 10, 'Alice', 1), (2, 10, 'Bob', 0);\
          \INSERT INTO profiles VALUES (101, 1, 'Functional programmer');\
          \INSERT INTO documents VALUES (1001, 1, 'Architecture Doc'), (1002, 1, 'API Spec');"

      res <-
        selectRows
          p
          ( polymorphicQuery ::
              Query
                SQLite
                (SQLiteUserT 'Expr, NullableTable (SQLiteProfileT 'Expr), NestedList (SQLiteDocT 'Expr))
          )
      closePool p

      case res of
        Left err -> expectationFailure ("selectRows failed: " ++ show err)
        Right rows -> case rows of
          [(alice, aliceProfile, aliceDocs), (bob, bobProfile, bobDocs)] -> do
            alice `shouldBe` SQLiteUser (Id 1) (Id 10) "Alice" True
            aliceProfile `shouldBe` Just (SQLiteProfile (Id 101) (Id 1) "Functional programmer")
            aliceDocs
              `shouldBe` [ SQLiteDoc (Id 1001) (Id 1) "Architecture Doc"
                         , SQLiteDoc (Id 1002) (Id 1) "API Spec"
                         ]
            bob `shouldBe` SQLiteUser (Id 2) (Id 10) "Bob" False
            bobProfile `shouldBe` Nothing
            bobDocs `shouldBe` []
          _ -> expectationFailure ("Expected 2 rows, got: " ++ show (length rows))

  describe "4. Acceptance Criterion 2: Multi-Level Nested Structure Hydration" $ do
    it "correctly hydrates multi-level (Org -> Users -> Docs) structures from in-memory SQLite" $ do
      p <- createMemoryPool
      _ <-
        executeSql
          p
          "CREATE TABLE orgs (id INTEGER PRIMARY KEY, name TEXT NOT NULL);\
          \CREATE TABLE users (id INTEGER PRIMARY KEY, org_id INTEGER NOT NULL, name TEXT NOT NULL, active INTEGER NOT NULL);\
          \CREATE TABLE documents (id INTEGER PRIMARY KEY, user_id INTEGER NOT NULL, title TEXT NOT NULL);\
          \INSERT INTO orgs VALUES (1, 'Acme Corp'), (2, 'Startup Inc');\
          \INSERT INTO users VALUES (1, 1, 'Alice', 1), (2, 1, 'Bob', 1), (3, 2, 'Charlie', 1);\
          \INSERT INTO documents VALUES (101, 1, 'Doc 1'), (102, 1, 'Doc 2'), (103, 3, 'Doc 3');"

      let multiLevelQuery ::
            Query SQLite (OrgT 'Expr, NestedList (SQLiteUserT 'Expr, NestedList (SQLiteDocT 'Expr)))
          multiLevelQuery = do
            o <- each orgTable
            usersWithDocs <- many $ do
              u <- each userTable
              where_ (u.orgId ==. o.id)
              docs <- many $ do
                d <- each docTable
                where_ (d.userId ==. u.id)
                pure d
              pure (u, docs)
            pure (o, usersWithDocs)

      res <- selectRows p multiLevelQuery
      closePool p

      case res of
        Left err -> expectationFailure ("selectRows failed: " ++ show err)
        Right rows -> case rows of
          [(acme, [(alice, aliceDocs), (bob, bobDocs)]), (startup, [(charlie, charlieDocs)])] -> do
            acme `shouldBe` Org (Id 1) "Acme Corp"
            alice `shouldBe` SQLiteUser (Id 1) (Id 1) "Alice" True
            aliceDocs `shouldBe` [SQLiteDoc (Id 101) (Id 1) "Doc 1", SQLiteDoc (Id 102) (Id 1) "Doc 2"]
            bob `shouldBe` SQLiteUser (Id 2) (Id 1) "Bob" True
            bobDocs `shouldBe` []

            startup `shouldBe` Org (Id 2) "Startup Inc"
            charlie `shouldBe` SQLiteUser (Id 3) (Id 2) "Charlie" True
            charlieDocs `shouldBe` [SQLiteDoc (Id 103) (Id 3) "Doc 3"]
          _ -> expectationFailure ("Expected 2 nested org rows, got: " ++ show (length rows))

  describe "5. SQLite Connection & Transaction Hooks" $ do
    it "executes commit hooks on successful runTx and rollback hooks on failure" $ do
      p <- createMemoryPool
      _ <- executeSql p "CREATE TABLE items (id INTEGER PRIMARY KEY, val TEXT);"

      commitHookCalled <- newTVarIO False
      rollbackHookCalled <- newTVarIO False

      -- Successful transaction
      let successfulTx = do
            setTxExtension "hook" ("test" :: Text)
            onCommit (atomically (writeTVar commitHookCalled True))
            onRollback (atomically (writeTVar rollbackHookCalled True))

      res1 <- runTxReadWrite p successfulTx
      res1 `shouldBe` Right ()

      cCalled <- readTVarIO commitHookCalled
      rCalled <- readTVarIO rollbackHookCalled
      cCalled `shouldBe` True
      rCalled `shouldBe` False

      -- Failing transaction
      commitHookCalled2 <- newTVarIO False
      rollbackHookCalled2 <- newTVarIO False

      let failingTx = do
            onCommit (atomically (writeTVar commitHookCalled2 True))
            onRollback (atomically (writeTVar rollbackHookCalled2 True))
            txFail (TxUserError "intentional failure")

      res2 <- runTxReadWrite p failingTx
      case res2 of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected transaction to fail"

      cCalled2 <- readTVarIO commitHookCalled2
      rCalled2 <- readTVarIO rollbackHookCalled2
      cCalled2 `shouldBe` False
      rCalled2 `shouldBe` True

      closePool p
