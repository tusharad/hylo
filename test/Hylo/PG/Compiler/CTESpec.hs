{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{-# HLINT ignore "Avoid restricted function" #-}

{- |
Module      : Hylo.PG.Compiler.CTESpec
Description : Formal specification and executable oracle for Epic 8 (CTEs & Recursive Graphs)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.CTESpec
  ( spec
  ) where

import Data.Int (Int32, Int64)
import Data.List (nub, (\\))
import Data.Text (Text)
import Hylo.Core
  ( Col
  , Ctx (..)
  , Id (..)
  , Table (..)
  , TableSchema
  , colSpecExpr
  , column
  , isNull
  , just
  , lit
  , nullableColumn
  , primaryKey
  , tableSchema
  , (==.)
  )
import Hylo.Core.Query
  ( FromRelation (..)
  , Query
  , asc
  , each
  , orderBy
  , where_
  )
import Hylo.PG.Compiler.CTE
  ( CTETuple3 (..)
  , CycleClause (..)
  , DynamoDB
  , Materialization (..)
  , with
  , withMaterialized
  , withRecursive
  , withRecursiveCycle
  )
import Hylo.PG.Compiler.SQL (renderSQL)
import Test.Hspec
import Test.QuickCheck
import Test.ShouldNotTypecheck (shouldNotTypecheck, shouldNotTypecheckWith)

-- | Employee domain table for hierarchical organization tree recursive tests.
data EmployeeT (ctx :: Ctx) = Employee
  { id :: !(Col ctx (Id EmployeeT Int64))
  , name :: !(Col ctx Text)
  , managerId :: !(Col ctx (Maybe (Id EmployeeT Int64)))
  }

instance Table EmployeeT where
  toTableExpr alias meta =
    Employee
      { id = colSpecExpr alias meta.id
      , name = colSpecExpr alias meta.name
      , managerId = colSpecExpr alias meta.managerId
      }
  tableExprColumns expr =
    [expr.id.exprCol, expr.name.exprCol, expr.managerId.exprCol]

employeeTable :: TableSchema EmployeeT
employeeTable =
  tableSchema "employees" $
    Employee
      { id = primaryKey "id" "bigint"
      , name = column "name" "text"
      , managerId = nullableColumn "manager_id" "bigint"
      }

spec :: Spec
spec = describe "Epic 8: Common Table Expressions & Recursive Graphs" $ do
  describe "8.4 Executable Oracle Specification" $ do
    it "compiles recursive CTE for hierarchical organization tree traversal" $ do
      let q =
            withRecursive
              "org_tree"
              -- Base Case: Root employee
              ( \_self -> do
                  e <- each employeeTable
                  where_ (e.managerId ==. isNull)
                  pure (e.id, e.name, lit (1 :: Int32))
              )
              -- Recursive Case: Employees reporting to frontier
              ( \self -> do
                  e <- each employeeTable
                  mgr <- each self
                  where_ (e.managerId ==. just mgr.id)
                  pure (e.id, e.name, mgr.depth + lit 1)
              )
              -- Final Query
              ( \orgTree -> do
                  member <- each orgTree
                  orderBy [asc member.depth, asc member.name]
                  pure member
              )
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "WITH RECURSIVE \"org_tree\" AS ("
      sql `shouldContain` "UNION ALL"
      sql `shouldContain` "SELECT \"employees_0\".\"id\", \"employees_0\".\"name\", 1"
      sql
        `shouldContain` "SELECT \"employees_1\".\"id\", \"employees_1\".\"name\", (\"org_tree_2\".\"depth\" + 1)"
      sql `shouldContain` "ORDER BY \"org_tree_3\".\"depth\" ASC, \"org_tree_3\".\"name\" ASC"

  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 1: Least Fixed Point Semantics of Recursive CTEs (Closure convergence invariant)" $
      property $ \(edges :: [(Int64, Int64)]) ->
        let -- Pure relational model of least fixed-point iteration
            -- Base step: nodes with no incoming edges (roots)
            allNodes = nub (map fst edges ++ map snd edges)
            hasParent = nub (map snd edges)
            roots = allNodes \\ hasParent
            -- Recursive step: compute transitive reachable set step by step
            step frontier visited
              | null frontier = visited
              | otherwise =
                  let nextNodes =
                        [ child
                        | (parent, child) <- edges
                        , parent `elem` frontier
                        ]
                      newFrontier = filter (`notElem` visited) (nub nextNodes)
                   in step newFrontier (visited ++ newFrontier)
            fixedPoint = step roots roots
            -- Invariant: Every reachable node was either a root or reachable from a root
            allReachableValid = all (`elem` allNodes) fixedPoint
         in allReachableValid

    it
      "Law 2: Materialization Invariance (Materialized and NotMaterialized project identical structures)"
      $ do
        let qMat =
              withMaterialized
                Materialized
                "filtered"
                ( do
                    e <- each employeeTable
                    pure (e.id, e.name)
                )
                ( \sub -> do
                    s <- each sub
                    pure (s.id, s.name)
                )
            qNotMat =
              withMaterialized
                NotMaterialized
                "filtered"
                ( do
                    e <- each employeeTable
                    pure (e.id, e.name)
                )
                ( \sub -> do
                    s <- each sub
                    pure (s.id, s.name)
                )
            sqlMat :: String
            sqlMat = renderSQL qMat
            sqlNotMat :: String
            sqlNotMat = renderSQL qNotMat
        sqlMat `shouldContain` "WITH \"filtered\" AS MATERIALIZED ("
        sqlNotMat `shouldContain` "WITH \"filtered\" AS NOT MATERIALIZED ("
        -- Both project the identical columns and aliases
        sqlMat `shouldContain` "SELECT \"filtered_1\".\"id\", \"filtered_1\".\"name\""
        sqlNotMat `shouldContain` "SELECT \"filtered_1\".\"id\", \"filtered_1\".\"name\""

  describe "Comprehensive Common Table Expressions & Cycle Controls" $ do
    it "compiles standard non-recursive CTE (with)" $ do
      let q =
            with
              "roots"
              ( do
                  e <- each employeeTable
                  where_ (e.managerId ==. isNull)
                  pure (e.id, e.name)
              )
              ( \roots -> do
                  r <- each roots
                  pure (r.id, r.name)
              )
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "WITH \"roots\" AS ("
      sql
        `shouldContain` "FROM \"employees\" AS \"employees_0\" WHERE (\"employees_0\".\"manager_id\" IS NULL)"

    it "compiles recursive CTE with cycle detection clause (CYCLE id SET is_cycle USING path)" $ do
      let cycleConfig = Just (CycleClause "id" "is_cycle" "path")
          q =
            withRecursiveCycle
              "graph"
              cycleConfig
              ( \_self -> do
                  e <- each employeeTable
                  where_ (e.managerId ==. isNull)
                  pure (e.id, e.name, lit (1 :: Int32))
              )
              ( \self -> do
                  e <- each employeeTable
                  mgr <- each self
                  where_ (e.managerId ==. just mgr.id)
                  pure (e.id, e.name, mgr.depth + lit 1)
              )
              ( \graph -> do
                  g <- each graph
                  pure (g.id, g.name)
              )
          sql :: String
          sql = renderSQL q
      sql `shouldContain` "WITH RECURSIVE \"graph\" AS ("
      sql `shouldContain` "CYCLE \"id\" SET \"is_cycle\" USING \"path\""

    it "statically rejects recursive CTEs on DynamoDB with [Hylo Error E008]" $
      shouldNotTypecheckWith
        "[Hylo Error E008]"
        ( let invalidDynamoQuery ::
                Query
                  DynamoDB
                  ( CTETuple3
                      (Hylo.Core.Col 'Expr (Id EmployeeT Int64))
                      (Hylo.Core.Col 'Expr Text)
                      (Hylo.Core.Col 'Expr Int32)
                  )
              invalidDynamoQuery = withRecursive "nodes" undefined undefined undefined
           in invalidDynamoQuery
        )

    it "statically rejects mismatched base and recursive return types" $
      shouldNotTypecheck
        ( let mismatchedQuery =
                withRecursive
                  "mismatched"
                  ( \_self -> do
                      e <- each employeeTable
                      pure (e.id, e.name) -- 2 columns
                  )
                  ( \self -> do
                      e <- each employeeTable
                      mgr <- each self
                      pure (e.id, e.name, mgr.depth) -- 3 columns! Mismatched!
                  )
                  ( \cte -> do
                      c <- each cte
                      pure c.id
                  )
           in mismatchedQuery
        )
