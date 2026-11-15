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

{- |
Module      : Hylo.PG.Compiler.LateralSpec
Description : Specification tests for Epic 5: Correlated Subquery Lowering & Single-Trip Graph Hydration
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.LateralSpec
  ( spec
  ) where

import Data.Int (Int64)
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
  , primaryKey
  , tableSchema
  , (==.)
  )
import Hylo.Core.Query
  ( Projectable (..)
  , each
  , renderExpr
  , where_
  )
import Hylo.PG.Compiler.Lateral (many)
import Hylo.PG.Compiler.SQL (renderSQL)
import Hylo.PG.Hydration.Graph
  ( hydrateGraph
  , hydrateJsonbChildren
  )
import Test.Hspec
import Test.QuickCheck

-- | Test Table 1: Organization entity
data OrgT (ctx :: Ctx) = Org
  { id :: !(Col ctx (Id OrgT Int64))
  , name :: !(Col ctx Text)
  }

instance Table OrgT where
  toTableExpr alias m =
    Org
      { id = colSpecExpr alias m.id
      , name = colSpecExpr alias m.name
      }
  tableExprColumns o = [renderExpr o.id, renderExpr o.name]

instance Projectable (OrgT 'Expr) where
  projectColumns = tableExprColumns

orgTable :: TableSchema OrgT
orgTable =
  tableSchema "orgs" $
    Org
      { id = primaryKey "id" "bigint"
      , name = column "name" "text"
      }

-- | Test Table 2: Project entity
data ProjectT (ctx :: Ctx) = Project
  { id :: !(Col ctx (Id ProjectT Int64))
  , orgId :: !(Col ctx (Id OrgT Int64))
  , title :: !(Col ctx Text)
  }

instance Table ProjectT where
  toTableExpr alias m =
    Project
      { id = colSpecExpr alias m.id
      , orgId = colSpecExpr alias m.orgId
      , title = colSpecExpr alias m.title
      }
  tableExprColumns p = [renderExpr p.id, renderExpr p.orgId, renderExpr p.title]

instance Projectable (ProjectT 'Expr) where
  projectColumns = tableExprColumns

projectTable :: TableSchema ProjectT
projectTable =
  tableSchema "projects" $
    Project
      { id = primaryKey "id" "bigint"
      , orgId = foreignKey "org_id" "bigint"
      , title = column "title" "text"
      }

-- | Test Table 3: Task entity
data TaskT (ctx :: Ctx) = Task
  { id :: !(Col ctx (Id TaskT Int64))
  , projectId :: !(Col ctx (Id ProjectT Int64))
  , content :: !(Col ctx Text)
  }

instance Table TaskT where
  toTableExpr alias m =
    Task
      { id = colSpecExpr alias m.id
      , projectId = colSpecExpr alias m.projectId
      , content = colSpecExpr alias m.content
      }
  tableExprColumns t = [renderExpr t.id, renderExpr t.projectId, renderExpr t.content]

instance Projectable (TaskT 'Expr) where
  projectColumns = tableExprColumns

taskTable :: TableSchema TaskT
taskTable =
  tableSchema "tasks" $
    Task
      { id = primaryKey "id" "bigint"
      , projectId = foreignKey "project_id" "bigint"
      , content = column "content" "text"
      }

spec :: Spec
spec = describe "Epic 5: Correlated Subquery Lowering (LATERAL N+1 Eradication)" $ do
  describe "5.4 Executable Oracle Specification" $ do
    it "compiles a nested many query into a single correlated LEFT JOIN LATERAL with jsonb_agg" $ do
      let q = do
            org <- each orgTable
            projects <- many $ do
              p <- each projectTable
              where_ (p.orgId ==. org.id)
              pure p
            pure (org, projects)
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "LEFT JOIN LATERAL ("
      sql `shouldContain` "jsonb_agg(to_jsonb(\"projects_1\".*))"
      sql `shouldContain` ") AS \"t1\" ON TRUE"
      sql `shouldNotContain` "SELECT * FROM projects WHERE org_id ="

  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 1: Graph Hydration Cardinality Invariant (|Hydrate(R ⋈ S)| == |R|)" $
      property $ \(orgIds :: [Int64]) (projOrgIds :: [Int64]) ->
        let orgs = [(oId, "Org " <> show oId) | oId <- orgIds]
            projects = [(pId, oId, "Project " <> show pId) | (pId :: Int64, oId) <- zip [1 ..] projOrgIds]
            hydrated = hydrateGraph fst (\(_, o, _) -> o) orgs projects
         in length hydrated == length orgs

    it "Law 1: Child Relationship Cardinality (count children == matching foreign keys)" $
      property $ \(orgId :: Int64) (projOrgIds :: [Int64]) ->
        let orgs = [(orgId, "Target Org" :: String)]
            projects = [(pId, oId, "Proj " <> show pId) | (pId :: Int64, oId) <- zip [1 ..] projOrgIds]
            hydrated = hydrateGraph fst (\(_, o, _) -> o) orgs projects
            expectedChildCount = length (filter (== orgId) projOrgIds)
         in case hydrated of
              [(_, children)] -> length children == expectedChildCount
              _ -> False

    it "Law 3: Empty Collection Idempotence (COALESCE(jsonb_agg(∅), '[]') == '[]')" $ do
      let emptyResult = hydrateJsonbChildren (const (Right [])) ("org1" :: String, "[]")
      emptyResult `shouldBe` Right ("org1", [] :: [String])

  describe "Multi-Tier Graph Lowering" $ do
    it "compiles a 3-tier nested graph into chained LEFT JOIN LATERAL statements in 1 query" $ do
      let q = do
            org <- each orgTable
            projects <- many $ do
              p <- each projectTable
              where_ (p.orgId ==. org.id)
              tasks <- many $ do
                t <- each taskTable
                where_ (t.projectId ==. p.id)
                pure t
              pure (p, tasks)
            pure (org, projects)
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "LEFT JOIN LATERAL ("
      sql `shouldContain` "jsonb_agg(to_jsonb(\"projects_1\".*))"
      sql `shouldContain` ") AS \"t1\" ON TRUE"
      sql `shouldContain` ") AS \"lat_0\" ON TRUE"
      sql `shouldNotContain` "SELECT * FROM tasks WHERE"
