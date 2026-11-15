{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{- |
Module      : Hylo.SQL.BuilderSpec
Description : Test suite for User Story 4: Multi-Dialect Parameterized SqlBuilder
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.SQL.BuilderSpec (spec) where

import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import GHC.Generics (Generic)
import Hylo.Core
import Hylo.MySQL.Compiler (compileMySQLQuery)
import Hylo.SQL
import Test.Hspec
import Test.QuickCheck

default (Text, Int64)

--------------------------------------------------------------------------------
-- Test HKD Table for Queries & Self-Joins
--------------------------------------------------------------------------------

data EmployeeT (ctx :: Ctx) = Employee
  { id :: !(Col ctx (Id EmployeeT Int64))
  , name :: !(Col ctx Text)
  , managerId :: !(Col ctx (Id EmployeeT Int64))
  , age :: !(Col ctx Int64)
  }
  deriving (Generic)

deriving instance Eq (EmployeeT 'Result)
deriving instance Show (EmployeeT 'Result)

instance Table EmployeeT where
  toTableExpr alias meta =
    Employee
      { id = colSpecExpr alias meta.id
      , name = colSpecExpr alias meta.name
      , managerId = colSpecExpr alias meta.managerId
      , age = colSpecExpr alias meta.age
      }
  tableExprColumns expr =
    [ renderExpr expr.id
    , renderExpr expr.name
    , renderExpr expr.managerId
    , renderExpr expr.age
    ]

employeeTable :: TableSchema EmployeeT
employeeTable =
  tableSchema "employees" $
    Employee
      { id = primaryKey "id" "bigint"
      , name = column "name" "text"
      , managerId = foreignKey "manager_id" "bigint"
      , age = column "age" "bigint"
      }

data UserT (ctx :: Ctx) = User
  { id :: !(Col ctx Int64)
  , name :: !(Col ctx Text)
  , createdAt :: !(Col ctx UTCTime)
  }
  deriving (Generic)

instance Table UserT where
  toTableExpr alias meta =
    User
      { id = colSpecExpr alias meta.id
      , name = colSpecExpr alias meta.name
      , createdAt = colSpecExpr alias meta.createdAt
      }
  tableExprColumns expr =
    [ renderExpr expr.id
    , renderExpr expr.name
    ]

userTable :: TableSchema UserT
userTable =
  tableSchema "users" $
    User
      { id = column "id" "bigint"
      , name = column "name" "text"
      , createdAt = column "created_at" "timestamp"
      }

--------------------------------------------------------------------------------
-- Test Specs
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "User Story 4: Parameterized Multi-Dialect SqlBuilder" $ do
  describe "1. Dialect Identifier Quoting" $ do
    it "quotes identifiers with double quotes for PostgreSQL and SQLite" $ do
      quoteIdentifier PostgresDialect "col" `shouldBe` "\"col\""
      quoteIdentifier SQLiteDialect "col" `shouldBe` "\"col\""
      quoteIdentifier PostgresDialect "employees" `shouldBe` "\"employees\""
      quoteIdentifier SQLiteDialect "employees" `shouldBe` "\"employees\""

    it "quotes identifiers with backticks for MySQL" $ do
      quoteIdentifier MySQLDialect "col" `shouldBe` "`col`"
      quoteIdentifier MySQLDialect "employees" `shouldBe` "`employees`"

    it "quotes scoped column references appropriately per dialect" $ do
      quoteScopedCol PostgresDialect "t0" "name" `shouldBe` "\"t0\".\"name\""
      quoteScopedCol SQLiteDialect "t0" "name" `shouldBe` "\"t0\".\"name\""
      quoteScopedCol MySQLDialect "t0" "name" `shouldBe` "`t0`.`name`"

    it "preserves wildcard selector without quotes" $ do
      quoteIdentifier PostgresDialect "*" `shouldBe` "*"
      quoteIdentifier MySQLDialect "*" `shouldBe` "*"
      quoteIdentifier SQLiteDialect "*" `shouldBe` "*"

  describe "2. Bind Parameter Placeholders" $ do
    it "emits $1, $2, $3 for PostgreSQL" $ do
      bindPlaceholder PostgresDialect 1 `shouldBe` "$1"
      bindPlaceholder PostgresDialect 2 `shouldBe` "$2"
      bindPlaceholder PostgresDialect 42 `shouldBe` "$42"

    it "emits positional ? for MySQL" $ do
      bindPlaceholder MySQLDialect 1 `shouldBe` "?"
      bindPlaceholder MySQLDialect 2 `shouldBe` "?"
      bindPlaceholder MySQLDialect 42 `shouldBe` "?"

    it "emits ?1, ?2, ?3 for SQLite" $ do
      bindPlaceholder SQLiteDialect 1 `shouldBe` "?1"
      bindPlaceholder SQLiteDialect 2 `shouldBe` "?2"
      bindPlaceholder SQLiteDialect 42 `shouldBe` "?42"

  describe "3. Strict Parameter Accumulation on Expressions" $ do
    it "appends typed bind parameters and emits dialect-specific placeholders" $ do
      let expr :: QExpr Bool
          expr = colExpr "t0" "age" >. lit (18 :: Int64)

      let (pgSql, pgParams) = compileExpr PostgresDialect expr
      pgSql `shouldBe` "(\"t0\".\"age\" > $1)"
      toList pgParams `shouldBe` [BoundParam 1 "18" "integer"]

      let (mySql, myParams) = compileExpr MySQLDialect expr
      mySql `shouldBe` "(`t0`.`age` > ?)"
      toList myParams `shouldBe` [BoundParam 1 "18" "integer"]

      let (sqliteSql, sqliteParams) = compileExpr SQLiteDialect expr
      sqliteSql `shouldBe` "(\"t0\".\"age\" > ?1)"
      toList sqliteParams `shouldBe` [BoundParam 1 "18" "integer"]

    it "accumulates multiple parameters monotonically in left-to-right order" $ do
      let expr :: QExpr Bool
          expr =
            ((colExpr "t0" "name" :: QExpr Text) ==. lit ("Alice" :: Text))
              &&. (colExpr "t0" "age" >=. lit (21 :: Int64))

      let (pgSql, pgParams) = compileExpr PostgresDialect expr
      pgSql `shouldBe` "((\"t0\".\"name\" = $1) AND (\"t0\".\"age\" >= $2))"
      toList pgParams `shouldBe` [BoundParam 1 "Alice" "text", BoundParam 2 "21" "integer"]

      let (mySql, myParams) = compileExpr MySQLDialect expr
      mySql `shouldBe` "((`t0`.`name` = ?) AND (`t0`.`age` >= ?))"
      toList myParams `shouldBe` [BoundParam 1 "Alice" "text", BoundParam 2 "21" "integer"]

      let (sqliteSql, sqliteParams) = compileExpr SQLiteDialect expr
      sqliteSql `shouldBe` "((\"t0\".\"name\" = ?1) AND (\"t0\".\"age\" >= ?2))"
      toList sqliteParams `shouldBe` [BoundParam 1 "Alice" "text", BoundParam 2 "21" "integer"]

  describe "4. Collision-Free Self-Joins with Deterministic t0, t1 Aliasing" $ do
    it "compiles a self-join query emitting distinct t0 and t1 aliases across all dialects" $ do
      let q :: Query Postgres (Col 'Expr Text, Col 'Expr Text)
          q = do
            e1 <- each employeeTable
            e2 <- each employeeTable
            where_ (e1.managerId ==. e2.id)
            where_ (e1.name ==. lit ("Alice" :: Text))
            pure (e1.name, e2.name)

      -- PostgreSQL Dialect
      let pgCompiled = compileQuery PostgresDialect q
      let pgSql = T.unpack (compiledSQL pgCompiled)
      pgSql
        `shouldContain` "FROM \"employees\" AS \"t0\" INNER JOIN \"employees\" AS \"t1\" ON (\"t0\".\"manager_id\" = \"t1\".\"id\")"
      pgSql `shouldContain` "SELECT \"t0\".\"name\", \"t1\".\"name\""
      pgSql `shouldContain` "WHERE (\"t0\".\"name\" = $1)"
      toList (compiledParams pgCompiled) `shouldBe` [BoundParam 1 "Alice" "text"]

      -- MySQL Dialect
      let myCompiled = compileQuery MySQLDialect q
      let mySql = T.unpack (compiledSQL myCompiled)
      mySql
        `shouldContain` "FROM `employees` AS `t0` INNER JOIN `employees` AS `t1` ON (`t0`.`manager_id` = `t1`.`id`)"
      mySql `shouldContain` "SELECT `t0`.`name`, `t1`.`name`"
      mySql `shouldContain` "WHERE (`t0`.`name` = ?)"
      toList (compiledParams myCompiled) `shouldBe` [BoundParam 1 "Alice" "text"]

      -- SQLite Dialect
      let sqliteCompiled = compileQuery SQLiteDialect q
      let sqliteSql = T.unpack (compiledSQL sqliteCompiled)
      sqliteSql
        `shouldContain` "FROM \"employees\" AS \"t0\" INNER JOIN \"employees\" AS \"t1\" ON (\"t0\".\"manager_id\" = \"t1\".\"id\")"
      sqliteSql `shouldContain` "SELECT \"t0\".\"name\", \"t1\".\"name\""
      sqliteSql `shouldContain` "WHERE (\"t0\".\"name\" = ?1)"
      toList (compiledParams sqliteCompiled) `shouldBe` [BoundParam 1 "Alice" "text"]

    it "handles three-way self-joins with deterministic t0, t1, t2 aliases" $ do
      let q :: Query Postgres (Col 'Expr Text, Col 'Expr Text, Col 'Expr Text)
          q = do
            e1 <- each employeeTable
            e2 <- each employeeTable
            e3 <- each employeeTable
            where_ (e1.managerId ==. e2.id)
            where_ (e2.managerId ==. e3.id)
            pure (e1.name, e2.name, e3.name)

      let pgCompiled = compileQuery PostgresDialect q
      let pgSql = T.unpack (compiledSQL pgCompiled)
      pgSql
        `shouldContain` "FROM \"employees\" AS \"t0\" INNER JOIN \"employees\" AS \"t1\" ON (\"t0\".\"manager_id\" = \"t1\".\"id\") INNER JOIN \"employees\" AS \"t2\" ON (\"t1\".\"manager_id\" = \"t2\".\"id\")"
      pgSql `shouldContain` "SELECT \"t0\".\"name\", \"t1\".\"name\", \"t2\".\"name\""

      let myCompiled = compileQuery MySQLDialect q
      let mySql = T.unpack (compiledSQL myCompiled)
      mySql
        `shouldContain` "FROM `employees` AS `t0` INNER JOIN `employees` AS `t1` ON (`t0`.`manager_id` = `t1`.`id`) INNER JOIN `employees` AS `t2` ON (`t1`.`manager_id` = `t2`.`id`)"
      mySql `shouldContain` "SELECT `t0`.`name`, `t1`.`name`, `t2`.`name`"

  describe "5. QuickCheck Invariants" $ do
    it "bind parameters indices are strictly 1..N monotonic" $ do
      property $ \(Positive (n :: Int)) ->
        let count = min n 25
            exprs =
              [ (colExpr "t0" (T.pack ("col" <> show i)) :: QExpr Int64) ==. lit (fromIntegral i :: Int64)
              | i <- [1 .. count]
              ]
            combinedExpr = foldl1 (&&.) exprs
            (_, params) = compileExpr PostgresDialect combinedExpr
            paramList = toList params
         in map paramIndex paramList == [1 .. count]

    it "MySQL placeholders are always exactly '?'" $ do
      property $ \(Positive (idx :: Int)) ->
        bindPlaceholder MySQLDialect idx == "?"

    it "Postgres placeholders start with '$' and end with index string" $ do
      property $ \(Positive (idx :: Int)) ->
        bindPlaceholder PostgresDialect idx == "$" <> T.pack (show idx)

    it "SQLite placeholders start with '?' and end with index string" $ do
      property $ \(Positive (idx :: Int)) ->
        bindPlaceholder SQLiteDialect idx == "?" <> T.pack (show idx)

  describe "User Story 7: Wire Hylo.SQL.Builder Across All Dialect Compilers & Fix Pagination" $ do
    it
      "Acceptance Criterion 1: Compiling a MySQL query with where_ (u.name ==. lit \"Alice\") via Hylo.MySQL.Compiler produces SELECT `t0`.`id`, `t0`.`name` FROM `users` AS `t0` WHERE (`t0`.`name` = ?) with [BoundParam 1 \"Alice\" \"text\"]"
      $ do
        let q :: Query MySQL (Col 'Expr Int64, Col 'Expr Text)
            q = do
              u <- each userTable
              where_ (u.name ==. lit ("Alice" :: Text))
              pure (u.id, u.name)
            cq = compileMySQLQuery q
        compiledSQL cq
          `shouldBe` "SELECT `t0`.`id`, `t0`.`name` FROM `users` AS `t0` WHERE (`t0`.`name` = ?)"
        toList (compiledParams cq) `shouldBe` [BoundParam 1 "Alice" "text"]

    it
      "Acceptance Criterion 2: Calling seekAfter (u.createdAt, u.id) (t, 42) binds both t and 42 into compiledParams in the exact keyset order"
      $ do
        let t :: UTCTime
            t = UTCTime (fromGregorian 2026 11 15) (secondsToDiffTime (20 * 3600))
            q :: Query (Postgres 'PG17) (Col 'Expr Int64, Col 'Expr Text)
            q = do
              u <- each userTable
              seekAfter (u.createdAt, u.id) (t, 42 :: Int64)
              pure (u.id, u.name)
            cq = compileQuery PostgresDialect q
        toList (compiledParams cq)
          `shouldBe` [ BoundParam 1 "2026-11-15 20:00:00 UTC" "text"
                     , BoundParam 2 "42" "integer"
                     ]
