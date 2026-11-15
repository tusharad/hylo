{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{-# HLINT ignore "Avoid restricted function" #-}

{- |
Module      : Hylo.AI.QueryToolSpec
Description : Test suite for Epic 17: Sandboxed Structured Agent Query Compiler & Mandatory RLS
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.AI.QueryToolSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isAsciiLower)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Hylo.AI.QueryTool
import Hylo.Core.Context (Col, Ctx (..))
import Hylo.Core.Expr (lit, (==.))
import Hylo.Core.Key (Id (..))
import Hylo.Core.Query (Changeset, Postgres, insert)
import Hylo.Core.Query.AST (Projectable (..), renderExpr)
import Hylo.Core.Table
  ( Table (..)
  , TableSchema (..)
  , colSpecExpr
  , column
  , foreignKey
  , primaryKey
  , tableSchema
  )
import Hylo.Core.Transaction
  ( Tx
  , TxMode (..)
  , TxState (..)
  , executeTx
  , initialTxState
  )
import Hylo.PG.Compiler.SQL (renderSQL)
import Hylo.Test.ShouldNotTypecheck (shouldNotTypecheckWith)
import Test.Hspec
import Test.QuickCheck

default (Text, Int64, Double)

-- | Test Table: Document entity for sandboxed agent tool querying.
data DocumentT (ctx :: Ctx) = Document
  { id :: !(Col ctx (Id DocumentT Int64))
  , createdAt :: !(Col ctx UTCTime)
  , orgId :: !(Col ctx (Id OrgT Int64))
  , title :: !(Col ctx Text)
  , salary :: !(Col ctx Int64)
  }

-- | Test Table: Organization tenant entity.
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
  tableExprColumns o =
    [ renderExpr o.id
    , renderExpr o.name
    ]
  tableColumnsMeta _ =
    [ ("id", "id")
    , ("name", "name")
    ]

instance Projectable (OrgT 'Expr) where
  projectColumns = tableExprColumns

instance Table DocumentT where
  toTableExpr alias m =
    Document
      { id = colSpecExpr alias m.id
      , createdAt = colSpecExpr alias m.createdAt
      , orgId = colSpecExpr alias m.orgId
      , title = colSpecExpr alias m.title
      , salary = colSpecExpr alias m.salary
      }
  tableExprColumns d =
    [ renderExpr d.id
    , renderExpr d.createdAt
    , renderExpr d.orgId
    , renderExpr d.title
    , renderExpr d.salary
    ]
  tableColumnsMeta _ =
    [ ("id", "id")
    , ("createdAt", "created_at")
    , ("created_at", "created_at")
    , ("orgId", "org_id")
    , ("org_id", "org_id")
    , ("title", "title")
    , ("salary", "salary")
    ]

instance Projectable (DocumentT 'Expr) where
  projectColumns = tableExprColumns

docTable :: TableSchema DocumentT
docTable =
  tableSchema "documents" $
    Document
      { id = primaryKey "id" "bigint"
      , createdAt = column "created_at" "timestamptz"
      , orgId = foreignKey "org_id" "bigint"
      , title = column "title" "text"
      , salary = column "salary" "bigint"
      }

-- | Safely compile and render an agent query into SQL text for test assertions.
compileSQL ::
  (Table table, Projectable (table 'Expr)) =>
  TableSchema table -> AgentPolicy table -> Aeson.Value -> String
compileSQL schema policy payload = case compileAgentQuery schema policy payload of
  Right q -> renderSQL q
  Left err -> errorWithoutStackTrace ("compileAgentQuery failed: " ++ show err)

spec :: Spec
spec = describe "Epic 17: Sandboxed Structured Agent Query Compiler" $ do
  describe "17.4 Executable Oracle Specification" $ do
    it "rejects tool payloads referencing un-whitelisted columns" $ do
      let maliciousPayload = [json| { "filter": { "salary": { "$gt": 100000 } } } |]
          policy = defaultPolicy {allowedFilterCols = ["title", "createdAt"]}
          result = compileAgentQuery docTable policy maliciousPayload
      result `shouldBe` Left (ForbiddenColumnError "salary")

    it "inviolably injects mandatory tenant scope into the generated SQL AST" $ do
      let userPayload = [json| { "filter": { "title": { "$eq": "Spec" } } } |]
          currentOrg = Id 42 :: Id OrgT Int64
          policy =
            defaultPolicy
              { allowedFilterCols = ["title"]
              , mandatoryScope = \doc -> doc.orgId ==. lit currentOrg
              }
      case compileAgentQuery docTable policy userPayload of
        Right q -> do
          let sql = renderSQL q
          sql `shouldContain` "(\"documents_0\".\"org_id\" = 42)"
          sql `shouldContain` "(\"documents_0\".\"title\" = 'Spec')"
        Left err -> expectationFailure ("Unexpected error: " ++ show err)

  describe "Contract Elicitation: Formal Laws" $ do
    it "Law 1: Mandatory Scope Conjunction Invariant (Filter(execute) == Filter(LLM) AND Scope)" $ do
      property $ \(Positive (orgVal :: Int64)) (titleVal :: String) -> do
        let safeTitle = filter (\c -> c /= '\'' && c /= '\\' && isAsciiLower c) titleVal
            titleText = if null safeTitle then "test" else T.pack safeTitle
            currentOrg = Id orgVal :: Id OrgT Int64
            payload =
              Aeson.object
                [
                  ( "filter"
                  , Aeson.object
                      [ ("title", Aeson.object [("$eq", Aeson.String titleText)])
                      ]
                  )
                ]
            policy =
              defaultPolicy
                { allowedFilterCols = ["title"]
                , mandatoryScope = \doc -> doc.orgId ==. lit currentOrg
                }
        case compileAgentQuery docTable policy payload of
          Right q -> do
            let sql :: String = renderSQL q
                expectedOrg = "(\"documents_0\".\"org_id\" = " ++ show orgVal ++ ")"
                expectedTitle = "(\"documents_0\".\"title\" = '" ++ T.unpack titleText ++ "')"
            sql `shouldContain` expectedOrg
            sql `shouldContain` expectedTitle
            sql `shouldContain` " AND "
          Left err -> expectationFailure ("Unexpected compilation error: " ++ show err)

    it
      "Law 2: Column Whitelist Containment (ReferencedColumns not in Whitelist => Left ForbiddenColumnError)"
      $ do
        property $ \(colNameStr :: String) -> do
          let cleanCol = filter isAsciiLower colNameStr
              targetCol = if null cleanCol then "secret" else T.pack cleanCol
              isForbidden = targetCol `notElem` ["title", "createdAt"]
          if isForbidden
            then do
              let payload =
                    Aeson.object
                      [
                        ( "filter"
                        , Aeson.object
                            [ (Key.fromText targetCol, Aeson.object [("$eq", Aeson.String "val")])
                            ]
                        )
                      ]
                  policy = defaultPolicy {allowedFilterCols = ["title", "createdAt"]}
                  res = compileAgentQuery docTable policy payload
              res `shouldBe` Left (ForbiddenColumnError targetCol)
            else True `shouldBe` True

    it "Law 3: Execution Purity (Zero Database Mutations in ReadOnly Tx)" $ do
      let userPayload = [json| { "filter": { "title": { "$eq": "Spec" } } } |]
          currentOrg = Id 42 :: Id OrgT Int64
          policy =
            defaultPolicy
              { allowedFilterCols = ["title"]
              , mandatoryScope = \doc -> doc.orgId ==. lit currentOrg
              }
          txAction = executeAgentToolQuery docTable policy userPayload
          (res, finalSt) = executeTx txAction initialTxState
      case res of
        Right (Right _) -> do
          txOperations finalSt `shouldBe` []
          txStore finalSt `shouldBe` []
        _ -> expectationFailure "Execution should succeed in ReadOnly"

    it "Law 3: Compile-Time Rejection of Mutations in ReadOnly Tx" $ do
      shouldNotTypecheckWith
        "[Hylo Error E005]"
        ( let invalidTx = insert docTable (undefined :: Changeset DocumentT)
           in (invalidTx :: Tx Postgres 'ReadOnly (DocumentT 'Result))
        )

  describe "Comprehensive Operator AST Synthesis" $ do
    it "compiles $ne inequality filter" $ do
      let payload = [json| { "filter": { "title": { "$ne": "Archived" } } } |]
          policy = defaultPolicy {allowedFilterCols = ["title"]}
          sql = compileSQL docTable policy payload
      sql `shouldContain` "(\"documents_0\".\"title\" <> 'Archived')"

    it "compiles $gt and $lt comparison filters" $ do
      let payload = [json| { "filter": { "salary": { "$gt": 50000, "$lt": 150000 } } } |]
          policy = defaultPolicy {allowedFilterCols = ["salary"]}
          sql = compileSQL docTable policy payload
      sql `shouldContain` "(\"documents_0\".\"salary\" > 50000)"
      sql `shouldContain` "(\"documents_0\".\"salary\" < 150000)"

    it "compiles $gte and $lte comparison filters" $ do
      let payload = [json| { "filter": { "salary": { "$gte": 1000, "$lte": 9999 } } } |]
          policy = defaultPolicy {allowedFilterCols = ["salary"]}
          sql = compileSQL docTable policy payload
      sql `shouldContain` "(\"documents_0\".\"salary\" >= 1000)"
      sql `shouldContain` "(\"documents_0\".\"salary\" <= 9999)"

    it "compiles $in array inclusion filter" $ do
      let payload = [json| { "filter": { "title": { "$in": ["Draft", "Published"] } } } |]
          policy = defaultPolicy {allowedFilterCols = ["title"]}
          sql = compileSQL docTable policy payload
      sql `shouldContain` "(\"documents_0\".\"title\" IN ('Draft', 'Published'))"

    it "compiles $nin array exclusion filter" $ do
      let payload = [json| { "filter": { "title": { "$nin": ["Spam", "Deleted"] } } } |]
          policy = defaultPolicy {allowedFilterCols = ["title"]}
          sql = compileSQL docTable policy payload
      sql `shouldContain` "(\"documents_0\".\"title\" NOT IN ('Spam', 'Deleted'))"

    it "compiles $like and $ilike pattern matching filters" $ do
      let payload = [json| { "filter": { "title": { "$like": "%Report%", "$ilike": "%q1%" } } } |]
          policy = defaultPolicy {allowedFilterCols = ["title"]}
          sql = compileSQL docTable policy payload
      sql `shouldContain` "(\"documents_0\".\"title\" LIKE '%Report%')"
      sql `shouldContain` "(\"documents_0\".\"title\" ILIKE '%q1%')"

    it "compiles $isNull check filters" $ do
      let payloadNull = [json| { "filter": { "title": { "$isNull": true } } } |]
          payloadNotNull = [json| { "filter": { "title": { "$isNull": false } } } |]
          policy = defaultPolicy {allowedFilterCols = ["title"]}
          sqlNull = compileSQL docTable policy payloadNull
          sqlNotNull = compileSQL docTable policy payloadNotNull
      sqlNull `shouldContain` "(\"documents_0\".\"title\" IS NULL)"
      sqlNotNull `shouldContain` "(\"documents_0\".\"title\" IS NOT NULL)"

    it "supports implicit equality shorthand filters" $ do
      let payload = [json| { "filter": { "title": "DirectMatch" } } |]
          policy = defaultPolicy {allowedFilterCols = ["title"]}
          sql = compileSQL docTable policy payload
      sql `shouldContain` "(\"documents_0\".\"title\" = 'DirectMatch')"

    it "rejects invalid operator tokens with descriptive error" $ do
      let payload = [json| { "filter": { "title": { "$hackOperator": "bad" } } } |]
          policy = defaultPolicy {allowedFilterCols = ["title"]}
          res = compileAgentQuery docTable policy payload
      res `shouldBe` Left (InvalidOperatorError "$hackOperator")

    it "rejects malformed payloads that are not JSON objects" $ do
      let payload = [json| "plain string" |]
          policy = defaultPolicy {allowedFilterCols = ["title"]}
          res = compileAgentQuery docTable policy payload
      res `shouldBe` Left (InvalidPayloadError "Query payload must be a JSON object")

  describe "Paging & Limits" $ do
    it "caps user requested limit at maxPageSize policy" $ do
      let payload = [json| { "limit": 500 } |]
          policy = defaultPolicy {maxPageSize = 50}
          sql = compileSQL docTable policy payload
      sql `shouldContain` "LIMIT 50"

    it "applies explicit offset pagination when specified" $ do
      let payload = [json| { "offset": 20 } |]
          policy = defaultPolicy {maxPageSize = 100}
          sql = compileSQL docTable policy payload
      sql `shouldContain` "OFFSET 20"

  describe "Dynamic JSON Schema Tool Derivation" $ do
    it "derives OpenAI function schema with whitelisted properties" $ do
      let policy = defaultPolicy {allowedFilterCols = ["title", "createdAt"], maxPageSize = 25}
          schema = deriveAgentToolSchema docTable policy
      case schema of
        Aeson.Object obj -> do
          KeyMap.lookup "type" obj `shouldBe` Just (Aeson.String "function")
          case KeyMap.lookup "function" obj of
            Just (Aeson.Object fn) -> do
              KeyMap.lookup "name" fn `shouldBe` Just (Aeson.String "query_documents")
              case KeyMap.lookup "parameters" fn of
                Just (Aeson.Object p) -> do
                  KeyMap.lookup "type" p `shouldBe` Just (Aeson.String "object")
                  case KeyMap.lookup "properties" p of
                    Just (Aeson.Object props) -> do
                      case KeyMap.lookup "filter" props of
                        Just (Aeson.Object f) -> do
                          case KeyMap.lookup "properties" f of
                            Just (Aeson.Object fp) -> do
                              KeyMap.member (Key.fromText "title") fp `shouldBe` True
                              KeyMap.member (Key.fromText "createdAt") fp `shouldBe` True
                              KeyMap.member (Key.fromText "salary") fp `shouldBe` False
                            _ -> expectationFailure "Expected filter properties object"
                        _ -> expectationFailure "Expected filter object"
                    _ -> expectationFailure "Expected parameters properties object"
                _ -> expectationFailure "Expected function parameters object"
            _ -> expectationFailure "Expected function object"
        _ -> expectationFailure "Expected root object"

    it "derives Anthropic tool schema with input_schema" $ do
      let policy = defaultPolicy {allowedFilterCols = ["title"]}
          schema = deriveAnthropicToolSchema docTable policy
      case schema of
        Aeson.Object obj -> do
          KeyMap.lookup "name" obj `shouldBe` Just (Aeson.String "query_documents")
          KeyMap.member (Key.fromText "input_schema") obj `shouldBe` True
        _ -> expectationFailure "Expected root object"

    it "derives MCP tool schema with inputSchema" $ do
      let policy = defaultPolicy {allowedFilterCols = ["title"]}
          schema = deriveMCPToolSchema docTable policy
      case schema of
        Aeson.Object obj -> do
          KeyMap.lookup "name" obj `shouldBe` Just (Aeson.String "query_documents")
          KeyMap.member (Key.fromText "inputSchema") obj `shouldBe` True
        _ -> expectationFailure "Expected root object"
