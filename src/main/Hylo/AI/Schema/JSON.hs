{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.AI.Schema.JSON
Description : Dynamic derivation of strict JSON Schema tool specifications for LLMs (OpenAI, Anthropic, MCP)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.AI.Schema.JSON
  ( deriveAgentToolSchema
  , deriveAnthropicToolSchema
  , deriveMCPToolSchema
  , deriveToolParametersSchema
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T
import Hylo.AI.Policy.RLS (AgentPolicy (..))
import Hylo.Core.Table (TableSchema (..))

-- | Derive standard JSON Schema parameters object restricted strictly to allowed filter columns.
deriveToolParametersSchema :: TableSchema table -> AgentPolicy table -> Aeson.Value
deriveToolParametersSchema schema policy =
  let tName = tableName schema
      filterProps =
        KeyMap.fromList
          [ ( Key.fromText col
            , Aeson.object
                [ ("type", Aeson.String "object")
                , ("description", Aeson.String ("Filter operators for column " <> col <> " in table " <> tName))
                ,
                  ( "properties"
                  , Aeson.object
                      [ ("$eq", Aeson.object [("description", Aeson.String "Exact match equality")])
                      , ("$ne", Aeson.object [("description", Aeson.String "Not equal")])
                      , ("$gt", Aeson.object [("description", Aeson.String "Greater than")])
                      , ("$gte", Aeson.object [("description", Aeson.String "Greater than or equal")])
                      , ("$lt", Aeson.object [("description", Aeson.String "Less than")])
                      , ("$lte", Aeson.object [("description", Aeson.String "Less than or equal")])
                      ,
                        ( "$in"
                        , Aeson.object
                            [ ("type", Aeson.String "array")
                            , ("description", Aeson.String "Value inclusion in list")
                            ]
                        )
                      ,
                        ( "$nin"
                        , Aeson.object
                            [ ("type", Aeson.String "array")
                            , ("description", Aeson.String "Value exclusion from list")
                            ]
                        )
                      ,
                        ( "$like"
                        , Aeson.object [("type", Aeson.String "string"), ("description", Aeson.String "SQL LIKE pattern")]
                        )
                      ,
                        ( "$ilike"
                        , Aeson.object
                            [ ("type", Aeson.String "string")
                            , ("description", Aeson.String "SQL case-insensitive ILIKE pattern")
                            ]
                        )
                      ,
                        ( "$isNull"
                        , Aeson.object
                            [("type", Aeson.String "boolean"), ("description", Aeson.String "Check for NULL or NOT NULL")]
                        )
                      ]
                  )
                ]
            )
          | col <- allowedFilterCols policy
          ]
      filterObj =
        Aeson.object
          [ ("type", Aeson.String "object")
          ,
            ( "description"
            , Aeson.String
                ("Structured filter predicates on allowed columns: " <> T.intercalate ", " (allowedFilterCols policy))
            )
          , ("properties", Aeson.Object filterProps)
          ]
      rootProps =
        KeyMap.fromList
          [ ("filter", filterObj)
          ,
            ( "limit"
            , Aeson.object
                [ ("type", Aeson.String "integer")
                , ("description", Aeson.String "Maximum number of rows to return")
                , ("maximum", Aeson.toJSON (maxPageSize policy))
                ]
            )
          ,
            ( "offset"
            , Aeson.object
                [ ("type", Aeson.String "integer")
                , ("description", Aeson.String "Number of rows to skip for offset-based pagination")
                ]
            )
          ,
            ( "orderBy"
            , Aeson.object
                [ ("type", Aeson.String "string")
                ,
                  ( "description"
                  , Aeson.String
                      ( "Column and sort direction (e.g. 'col:asc' or 'col:desc'). Allowed: "
                          <> T.intercalate ", " (allowedFilterCols policy)
                      )
                  )
                ]
            )
          ]
   in Aeson.object
        [ ("type", Aeson.String "object")
        , ("properties", Aeson.Object rootProps)
        ]

-- | Derive an OpenAI-compatible function calling tool schema from 'TableSchema' and 'AgentPolicy'.
deriveAgentToolSchema :: TableSchema table -> AgentPolicy table -> Aeson.Value
deriveAgentToolSchema schema policy =
  let tName = tableName schema
      toolName = "query_" <> tName
      description =
        "Execute a sandboxed, row-level secured read query against "
          <> tName
          <> " with strict column whitelisting."
      params = deriveToolParametersSchema schema policy
   in Aeson.object
        [ ("type", Aeson.String "function")
        ,
          ( "function"
          , Aeson.object
              [ ("name", Aeson.String toolName)
              , ("description", Aeson.String description)
              , ("parameters", params)
              ]
          )
        ]

-- | Derive an Anthropic-compatible tool specification schema.
deriveAnthropicToolSchema :: TableSchema table -> AgentPolicy table -> Aeson.Value
deriveAnthropicToolSchema schema policy =
  let tName = tableName schema
      toolName = "query_" <> tName
      description =
        "Execute a sandboxed, row-level secured read query against "
          <> tName
          <> " with strict column whitelisting."
      params = deriveToolParametersSchema schema policy
   in Aeson.object
        [ ("name", Aeson.String toolName)
        , ("description", Aeson.String description)
        , ("input_schema", params)
        ]

-- | Derive a Model Context Protocol (MCP) JSON-RPC 2.0 tool specification schema.
deriveMCPToolSchema :: TableSchema table -> AgentPolicy table -> Aeson.Value
deriveMCPToolSchema schema policy =
  let tName = tableName schema
      toolName = "query_" <> tName
      description =
        "Execute a sandboxed, row-level secured read query against "
          <> tName
          <> " with strict column whitelisting."
      params = deriveToolParametersSchema schema policy
   in Aeson.object
        [ ("name", Aeson.String toolName)
        , ("description", Aeson.String description)
        , ("inputSchema", params)
        ]
