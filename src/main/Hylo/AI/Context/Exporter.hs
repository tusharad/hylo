{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.AI.Context.Exporter
Description : Token-optimized AGENTS.md and llms.txt context exporters for AI coding assistants
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.AI.Context.Exporter
  ( AiContextConfig (..)
  , QueryPattern (..)
  , defaultAiContextConfig
  , exportAgentsMarkdown
  , exportLlmsTxt
  , estimateTokens
  , fitTokenBudget
  ) where

import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeNats (natVal)
import Hylo.Core.Context (ColDef (..), LogicalType (..))
import Hylo.Core.Table
  ( AnyTableSchema (..)
  , RedisCachePolicy (..)
  , Table (..)
  , TableSchema (..)
  )

-- | Renderable projection of an 'Hylo.AI.Policy.RLS.AgentPolicy' whitelist.
data QueryPattern = QueryPattern
  { qpTableName :: !Text
  , qpAllowedFilterCols :: ![Text]
  , qpMaxPageSize :: !Int
  }
  deriving (Eq, Show)

-- | Configuration governing AI context export rendering.
data AiContextConfig = AiContextConfig
  { acIncludeTables :: ![Text]
  , acExcludeColumns :: ![Text]
  , acOutputDir :: !FilePath
  , acTokenBudget :: !Int
  , acQueryPolicies :: ![QueryPattern]
  }
  deriving (Eq, Show)

{- | Baseline configuration: include everything, exclude nothing, write to the
project root with a 4096 token budget and no query policies.
-}
defaultAiContextConfig :: AiContextConfig
defaultAiContextConfig = AiContextConfig [] [] "." 4096 []

-- | Erased, homogeneous snapshot of one 'TableSchema' for rendering.
data TableRecord = TableRecord
  { trName :: !Text
  , trDescription :: !(Maybe Text)
  , trCache :: !(Maybe RedisCachePolicy)
  , trCols :: ![ColDef ()]
  }

-- | Project an existential schema registry entry into a 'TableRecord'.
tableRecord :: AnyTableSchema -> TableRecord
tableRecord (AnyTableSchema s) =
  TableRecord
    { trName = tableName s
    , trDescription = tableDescription s
    , trCache = tableCachePolicy s
    , trCols = tableColDefs (tableMeta s)
    }

{- | Filter registry entries by 'acIncludeTables' (empty means include all)
and drop 'acExcludeColumns' columns.
-}
selectedTables :: AiContextConfig -> [AnyTableSchema] -> [TableRecord]
selectedTables cfg schemas =
  [ dropExcluded cfg r
  | s <- schemas
  , let r = tableRecord s
  , included (acIncludeTables cfg) (trName r)
  ]
  where
    included include name = case include of
      [] -> True
      _ -> name `elem` include

dropExcluded :: AiContextConfig -> TableRecord -> TableRecord
dropExcluded cfg r =
  r {trCols = filter (\c -> colName c `notElem` acExcludeColumns cfg) (trCols r)}

-- | Name of the first primary key column, if any.
primaryKeyOf :: TableRecord -> Maybe Text
primaryKeyOf r = listToMaybe [colName c | c <- trCols r, colIsPrimaryKey c]

{- | Project a 'LogicalType' onto its canonical Haskell representation,
wrapping nullable columns in 'Maybe'.
-}
haskellTypeOf :: Bool -> LogicalType a -> Text
haskellTypeOf nullable lt = wrap (go lt)
  where
    go = \case
      LInt64 -> "Int64"
      LText -> "Text"
      LBool -> "Bool"
      LUTCTime -> "UTCTime"
      LUUID -> "UUID"
      LJson -> "Value"
      LVector p -> "Vector " <> T.pack (show (natVal p))
      LCustom t -> t
    wrap t
      | nullable = "Maybe " <> t
      | otherwise = t

{- | Deterministically estimate the GPT-4o style token count of a rendered
document (~4 characters per token).
-}
estimateTokens :: Text -> Int
estimateTokens t = (T.length t + 3) `div` 4

{- | Deterministically truncate a rendered document at line boundaries so its
estimated token count fits within the budget. Documents already within the
budget are returned unchanged (the function is idempotent). A non-positive
budget disables truncation.
-}
fitTokenBudget :: Int -> Text -> Text
fitTokenBudget budget t
  | budget <= 0 = t
  | estimateTokens t <= budget = t
  | estimateTokens marker > budget = ""
  | otherwise = takeLinesWithin (4 * budget - T.length marker - 4) t <> marker
  where
    marker = "<!-- hylo ai-context: truncated to token budget -->\n"

-- | Keep whole lines while their accumulated character count fits.
takeLinesWithin :: Int -> Text -> Text
takeLinesWithin maxChars = go 0 . T.lines
  where
    go _ [] = ""
    go acc (l : ls)
      | acc + T.length l + 1 > maxChars = ""
      | otherwise = l <> "\n" <> go (acc + T.length l + 1) ls

{- | Render a structured @AGENTS.md@ document from a schema registry. The
document contains a @Schema Overview@ table, one section per table with
column metadata and cache/optimistic-lock annotations, and a @Query
Patterns@ section derived from 'acQueryPolicies'.
-}
exportAgentsMarkdown :: [AnyTableSchema] -> AiContextConfig -> Text
exportAgentsMarkdown schemas cfg =
  fitTokenBudget (acTokenBudget cfg) (T.unlines body)
  where
    selected = selectedTables cfg schemas
    body =
      [ "# Database Schema Context"
      , ""
      , "_Generated by `hylo ai-context`. Do not edit by hand._"
      , ""
      , "## Schema Overview"
      , ""
      , "| Table | Primary Key | Row Count Hint | Description |"
      , "|---|---|---|---|"
      ]
        ++ map overviewRow selected
        ++ [""]
        ++ concatMap tableSectionLines selected
        ++ [ "## Query Patterns"
           , ""
           ]
        ++ queryPatternLines (acQueryPolicies cfg)

{- | Render a compact [llms.txt](https://llmstxt.org) document: an @H1@ title,
a blockquote summary, and a @## Schema@ section with one link per table
followed by a one-line column summary. Sized to stay under 2K tokens for a
10-table schema.
-}
exportLlmsTxt :: [AnyTableSchema] -> AiContextConfig -> Text
exportLlmsTxt schemas cfg =
  fitTokenBudget (acTokenBudget cfg) (T.unlines body)
  where
    selected = selectedTables cfg schemas
    body =
      [ "# Hylo Database Schema"
      , ""
      , "> Schema context for "
          <> T.pack (show (length selected))
          <> " table(s) generated by hylo ai-context."
      , ""
      , "## Schema"
      , ""
      ]
        ++ map linkLine selected
        ++ [""]
        ++ concatMap compactSectionLines selected

overviewRow :: TableRecord -> Text
overviewRow r =
  "| "
    <> trName r
    <> " | "
    <> fromMaybe "—" (primaryKeyOf r)
    <> " | — | "
    <> fromMaybe "—" (trDescription r)
    <> " |"

tableSectionLines :: TableRecord -> [Text]
tableSectionLines r =
  [ "## Table: " <> trName r
  , ""
  ]
    ++ maybe [] (\d -> [d, ""]) (trDescription r)
    ++ maybe [] cacheLines (trCache r)
    ++ [ "| Column | Haskell Type | SQL Type | Nullable | Default | Annotations |"
       , "|---|---|---|---|---|---|"
       ]
    ++ map columnRow (trCols r)
    ++ [""]
  where
    cacheLines p =
      [ "Cache: Redis (key prefix \""
          <> rcpKeyPrefix p
          <> "\", ttl "
          <> T.pack (show (rcpTtlSeconds p))
          <> "s)"
      , ""
      ]

columnRow :: ColDef () -> Text
columnRow c =
  "| "
    <> colName c
    <> " | "
    <> haskellTypeOf (colIsNullable c) (colLogicalType c)
    <> " | "
    <> colSqlType c
    <> " | "
    <> (if colIsNullable c then "YES" else "NO")
    <> " | "
    <> (if colIsNullable c then "NULL" else "—")
    <> " | "
    <> annotationText
    <> " |"
  where
    annotationText = case tags of
      [] -> "—"
      _ -> T.intercalate ", " tags
    tags =
      catMaybes
        [ if colIsPrimaryKey c then Just "PRIMARY KEY" else Nothing
        , if colIsOptimisticLock c then Just "OPTIMISTIC LOCK" else Nothing
        ]

queryPatternLines :: [QueryPattern] -> [Text]
queryPatternLines [] =
  [ "No `AgentPolicy` column whitelists are configured. Agents fall back to `defaultPolicy` (no filter columns, max page size 100)."
  ]
queryPatternLines policies = concatMap policyLines policies

policyLines :: QueryPattern -> [Text]
policyLines p =
  [ "### query_" <> qpTableName p
  , ""
  , "Allowed filter columns: " <> allowedText
  , ""
  , "Example tool call:"
  , ""
  , "```json"
  , examplePayload p
  , "```"
  , ""
  ]
  where
    allowedText = case qpAllowedFilterCols p of
      [] -> "_(none)_"
      cols -> T.intercalate ", " cols

{- | Hand-rendered JSON payload: deterministic key order without depending on
'Data.Aeson' object encoding.
-}
examplePayload :: QueryPattern -> Text
examplePayload p = case qpAllowedFilterCols p of
  [] ->
    "{\"tool\": \"query_"
      <> qpTableName p
      <> "\", \"args\": {\"filter\": {}}, \"limit\": "
      <> pageSize
      <> "}"
  (c : _) ->
    "{\"tool\": \"query_"
      <> qpTableName p
      <> "\", \"args\": {\"filter\": {\""
      <> c
      <> "\": {\"$eq\": \"...\"}}}, \"limit\": "
      <> pageSize
      <> "}"
  where
    pageSize = T.pack (show (qpMaxPageSize p))

linkLine :: TableRecord -> Text
linkLine r = case trDescription r of
  Just d -> "- [" <> trName r <> "](#" <> trName r <> "): " <> d
  Nothing -> "- [" <> trName r <> "](#" <> trName r <> ")"

compactSectionLines :: TableRecord -> [Text]
compactSectionLines r =
  [ "### " <> trName r
  , ""
  ]
    ++ maybe [] (\d -> [d, ""]) (trDescription r)
    ++ [columnsLine (trCols r), ""]

columnsLine :: [ColDef ()] -> Text
columnsLine [] = "Columns: —"
columnsLine cols = "Columns: " <> T.intercalate ", " (map colSummary cols)

colSummary :: ColDef () -> Text
colSummary c =
  colName c <> " (" <> colSqlType c <> suffix <> ")"
  where
    suffix = case tags of
      [] -> ""
      _ -> ", " <> T.intercalate ", " tags
    tags =
      catMaybes
        [ if colIsPrimaryKey c then Just "primary key" else Nothing
        , if colIsNullable c then Just "nullable" else Nothing
        , if colIsOptimisticLock c then Just "optimistic lock" else Nothing
        ]
