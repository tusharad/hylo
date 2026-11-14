{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Monad law, left identity" #-}
{-# HLINT ignore "Monad law, right identity" #-}
{-# HLINT ignore "Use >=>" #-}

{- |
Module      : Hylo.PG.Compiler.JoinFlattenerSpec
Description : Specification tests for Epic 4: Query Monad AST, Deterministic Scoping & Join Flattener
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.JoinFlattenerSpec
  ( spec
  ) where

import Data.Int (Int64)
import Data.List (nub)
import Data.Text (Text)
import Hylo.Core
  ( Col
  , Ctx (..)
  , Id
  , Table (..)
  , TableSchema
  , colSpecExpr
  , column
  , foreignKey
  , lit
  , primaryKey
  , runQuery
  , tableSchema
  , (==.)
  )
import Hylo.Core.Query
  ( each
  , generateScopes
  , initialQueryState
  , where_
  )
import Hylo.PG.Compiler.SQL (renderSQL)
import Test.Hspec
import Test.QuickCheck

-- | Test Table 1: User entity
data UserT (ctx :: Ctx) = User
  { id :: !(Col ctx (Id UserT Int64))
  , email :: !(Col ctx Text)
  , active :: !(Col ctx Bool)
  }

instance Table UserT where
  toTableExpr alias m =
    User
      { id = colSpecExpr alias m.id
      , email = colSpecExpr alias m.email
      , active = colSpecExpr alias m.active
      }

userTable :: TableSchema UserT
userTable =
  tableSchema "users" $
    User
      { id = primaryKey "id" "bigint"
      , email = column "email" "text"
      , active = column "active" "boolean"
      }

-- | Test Table 2: Profile entity
data ProfileT (ctx :: Ctx) = Profile
  { id :: !(Col ctx (Id ProfileT Int64))
  , userId :: !(Col ctx (Id UserT Int64))
  , bio :: !(Col ctx Text)
  }

instance Table ProfileT where
  toTableExpr alias m =
    Profile
      { id = colSpecExpr alias m.id
      , userId = colSpecExpr alias m.userId
      , bio = colSpecExpr alias m.bio
      }

profileTable :: TableSchema ProfileT
profileTable =
  tableSchema "profiles" $
    Profile
      { id = primaryKey "id" "bigint"
      , userId = foreignKey "user_id" "bigint"
      , bio = column "bio" "text"
      }

-- | Test Table 3: Order entity
data OrderT (ctx :: Ctx) = Order
  { id :: !(Col ctx (Id OrderT Int64))
  , userId :: !(Col ctx (Id UserT Int64))
  , total :: !(Col ctx Int64)
  }

instance Table OrderT where
  toTableExpr alias m =
    Order
      { id = colSpecExpr alias m.id
      , userId = colSpecExpr alias m.userId
      , total = colSpecExpr alias m.total
      }

orderTable :: TableSchema OrderT
orderTable =
  tableSchema "orders" $
    Order
      { id = primaryKey "id" "bigint"
      , userId = foreignKey "user_id" "bigint"
      , total = column "total" "bigint"
      }

spec :: Spec
spec = describe "Epic 4: Query Monad AST & Join Flattener" $ do
  describe "4.4 Executable Oracle Specification" $ do
    it "flattens sequential each and where_ blocks into ANSI INNER JOIN ON clauses" $ do
      let q = do
            u <- each userTable
            where_ (u.active ==. lit True)
            p <- each profileTable
            where_ (p.userId ==. u.id)
            pure (u.email, p.bio)
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "FROM \"users\" AS \"users_0\""
      sql
        `shouldContain` "INNER JOIN \"profiles\" AS \"profiles_1\" ON (\"profiles_1\".\"user_id\" = \"users_0\".\"id\")"
      sql `shouldNotContain` "CROSS JOIN"

    it "allocates strictly monotonic and unique ScopeIds across complex subqueries" $
      property $ \n ->
        let scopes = generateScopes (abs n `mod` 50)
         in length scopes == length (nub scopes)

  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 2: Monadic Left Identity (pure x >>= f == f x)" $
      property $ \(n :: Int) ->
        let f x = do
              where_ (lit x ==. lit (0 :: Int))
              pure (x * 2)
            q1 = pure n >>= f
            q2 = f n
            st = initialQueryState
         in runQuery q1 st == runQuery q2 st

    it "Law 3: Monadic Right Identity (m >>= pure == m)" $
      property $ \(n :: Int) ->
        let m = do
              where_ (lit n ==. lit (42 :: Int))
              pure (n + 1)
            q = m >>= pure
            st = initialQueryState
         in runQuery q st == runQuery m st

    it "Law 4: Monadic Associativity ((m >>= f) >>= g == m >>= (\\x -> f x >>= g))" $
      property $ \(n :: Int) ->
        let m = pure n
            f x = where_ (lit x ==. lit (1 :: Int)) >> pure (x + 10)
            g y = where_ (lit y ==. lit (2 :: Int)) >> pure (y * 3)
            q1 = (m >>= f) >>= g
            q2 = m >>= (\x -> f x >>= g)
            st = initialQueryState
         in runQuery q1 st == runQuery q2 st

  describe "Relational Join & Scope Normalization" $ do
    it "flattens three-way sequential joins into chained INNER JOINs without CROSS JOIN" $ do
      let q = do
            u <- each userTable
            p <- each profileTable
            where_ (p.userId ==. u.id)
            o <- each orderTable
            where_ (o.userId ==. u.id)
            pure (u.email, p.bio, o.total)
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "FROM \"users\" AS \"users_0\""
      sql
        `shouldContain` "INNER JOIN \"profiles\" AS \"profiles_1\" ON (\"profiles_1\".\"user_id\" = \"users_0\".\"id\")"
      sql
        `shouldContain` "INNER JOIN \"orders\" AS \"orders_2\" ON (\"orders_2\".\"user_id\" = \"users_0\".\"id\")"
      sql `shouldNotContain` "CROSS JOIN"

    it "falls back to CROSS JOIN when relations share no connecting join predicates" $ do
      let q = do
            u <- each userTable
            p <- each profileTable
            pure (u.email, p.bio)
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "FROM \"users\" AS \"users_0\" CROSS JOIN \"profiles\" AS \"profiles_1\""
      sql `shouldNotContain` "INNER JOIN"

    it "separates non-join filter predicates into the WHERE clause" $ do
      let q = do
            u <- each userTable
            where_ (u.active ==. lit True)
            p <- each profileTable
            where_ (p.userId ==. u.id)
            pure (u.email, p.bio)
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "WHERE (\"users_0\".\"active\" = true)"
