{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Hylo.AI.QueryTool
Description : Sandboxed structured agent query compiler & mandatory Row-Level Security (RLS)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.AI.QueryTool
  ( -- * Query Tool Errors
    QueryToolError (..)
  , AgentQueryError

    -- * Sandboxed AST Compilation & Execution
  , compileAgentQuery
  , executeAgentToolQuery

    -- * QuasiQuoter for JSON Payloads
  , json

    -- * Re-exports from Policy & Schema
  , AgentPolicy (..)
  , AgentQueryPolicy
  , defaultPolicy
  , withAllowedFilterCols
  , withMandatoryScope
  , withMaxPageSize
  , deriveAgentToolSchema
  , deriveAnthropicToolSchema
  , deriveMCPToolSchema
  ) where

import Control.DeepSeq (NFData (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.QQ as QQ
import Data.Char (isUpper, toLower)
import Data.Foldable (for_, toList)
import qualified Data.List as L
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.AI.Policy.RLS
  ( AgentPolicy (..)
  , AgentQueryPolicy
  , defaultPolicy
  , withAllowedFilterCols
  , withMandatoryScope
  , withMaxPageSize
  )
import Hylo.AI.Schema.JSON
  ( deriveAgentToolSchema
  , deriveAnthropicToolSchema
  , deriveMCPToolSchema
  )
import Hylo.Core.Context (Ctx (..))
import Hylo.Core.Expr (QExpr (..), colExpr)
import Hylo.Core.Query (select)
import Hylo.Core.Query.Monad
  ( Query
  , each
  , limit
  , offset
  , where_
  )
import Hylo.Core.Table (Table (..), TableSchema (..))
import Hylo.Core.Transaction (Tx, TxError (..), TxMode (..))
import Hylo.SQL.AST (ScopeId (..), formatScope)
import Language.Haskell.TH.Quote (QuasiQuoter)

-- | QuasiQuoter for compile-time verified JSON values (re-exported from aeson-qq).
json :: QuasiQuoter
json = QQ.aesonQQ

-- | Structured error diagnostics returned when an agent query payload fails validation.
data QueryToolError
  = ForbiddenColumnError !Text
  | InvalidPayloadError !Text
  | InvalidOperatorError !Text
  | InvalidValueError !Text
  | ExecutionError !TxError
  deriving (Eq, Show)

-- | Type synonym aligning with Hylo PRD specification.
type AgentQueryError = QueryToolError

instance NFData TxError

instance NFData QueryToolError where
  rnf = \case
    ForbiddenColumnError c -> rnf c
    InvalidPayloadError p -> rnf p
    InvalidOperatorError o -> rnf o
    InvalidValueError v -> rnf v
    ExecutionError e -> rnf e

-- | Compile an LLM's structured JSON query payload into a type-safe Hylo 'Query' AST.
compileAgentQuery ::
  forall db table.
  Table table =>
  TableSchema table ->
  AgentPolicy table ->
  Aeson.Value ->
  Either QueryToolError (Query db (table 'Expr))
compileAgentQuery schema policy payload = case payload of
  Aeson.Object rootObj -> do
    let tName = tableName schema
        scopeAlias = formatScope tName (ScopeId 0)
        allowed = allowedFilterCols policy
        metaCols = tableColumnsMeta (tableMeta schema)

    filterPreds <- case KeyMap.lookup "filter" rootObj of
      Nothing -> pure []
      Just (Aeson.Object filterObj) -> do
        validateWhitelist allowed (map Key.toText (KeyMap.keys filterObj))
        concat
          <$> mapM
            ( \(k, val) ->
                let colText = Key.toText k
                    sqlCol = resolveSqlColumn metaCols colText
                 in parseColumnFilter scopeAlias sqlCol val
            )
            (KeyMap.toList filterObj)
      Just _ -> Left (InvalidPayloadError "The 'filter' property must be a JSON object")

    let mLstLimit = case KeyMap.lookup "limit" rootObj of
          Just (Aeson.Number n) -> case floatingOrInteger n of
            Right (i :: Integer) -> Just (min (fromIntegral i) (maxPageSize policy))
            Left (_ :: Double) -> Just (maxPageSize policy)
          _ -> Just (maxPageSize policy)

    let mOffsetVal = case KeyMap.lookup "offset" rootObj of
          Just (Aeson.Number n) -> case floatingOrInteger n of
            Right (i :: Integer) -> Just (fromIntegral i)
            Left (_ :: Double) -> Nothing
          _ -> Nothing

    pure $ do
      record <- each schema
      where_ (mandatoryScope policy record)
      mapM_ where_ filterPreds
      for_ mLstLimit limit
      for_ mOffsetVal offset
      pure record
  _ -> Left (InvalidPayloadError "Query payload must be a JSON object")

-- | Execute an agent tool query inside the effect-pure 'Tx' monad in 'ReadOnly' mode.
executeAgentToolQuery ::
  (Table table) =>
  TableSchema table ->
  AgentPolicy table ->
  Aeson.Value ->
  Tx db 'ReadOnly (Either QueryToolError (table 'Expr))
executeAgentToolQuery schema policy payload =
  case compileAgentQuery schema policy payload of
    Left err -> pure (Left err)
    Right q -> do
      res <- select q
      pure (Right res)

-- | Validate that all referenced columns exist in the allowed column whitelist.
validateWhitelist :: [Text] -> [Text] -> Either QueryToolError ()
validateWhitelist allowed cols =
  case L.find (\c -> not (isWhitelisted c allowed)) cols of
    Just forbidden -> Left (ForbiddenColumnError forbidden)
    Nothing -> Right ()

-- | Check if a column matches any allowed column (supporting exact match or snake_case conversion).
isWhitelisted :: Text -> [Text] -> Bool
isWhitelisted col allowed =
  col `elem` allowed
    || toSnakeCase col `elem` map toSnakeCase allowed

-- | Resolve Haskell record field or payload column name to the database SQL column name.
resolveSqlColumn :: [(Text, Text)] -> Text -> Text
resolveSqlColumn metaCols colText =
  case L.lookup colText metaCols of
    Just mapped -> mapped
    Nothing -> case L.lookup (toSnakeCase colText) metaCols of
      Just mapped -> mapped
      Nothing -> toSnakeCase colText

-- | Parse a single field filter into a list of SQL 'QExpr Bool' predicates.
parseColumnFilter :: Text -> Text -> Aeson.Value -> Either QueryToolError [QExpr Bool]
parseColumnFilter alias sqlCol val = case val of
  Aeson.Object opObj ->
    mapM (parseOperator alias sqlCol) (KeyMap.toList opObj)
  Aeson.Null ->
    pure [UnaryOpExpr "IS NULL" (colExpr alias sqlCol)]
  primitiveVal -> do
    expr <- parseOperator alias sqlCol (Key.fromText "$eq", primitiveVal)
    pure [expr]

-- | Parse an operator-value pair into a 'QExpr Bool' condition.
parseOperator :: Text -> Text -> (Key.Key, Aeson.Value) -> Either QueryToolError (QExpr Bool)
parseOperator alias sqlCol (opKey, opVal) =
  let opText = Key.toText opKey
      targetCol = colExpr alias sqlCol
   in case opText of
        "$eq" -> case opVal of
          Aeson.Null -> Right (UnaryOpExpr "IS NULL" targetCol)
          _ -> BinOpExpr "=" targetCol <$> valueToExpr opVal
        "$ne" -> case opVal of
          Aeson.Null -> Right (UnaryOpExpr "IS NOT NULL" targetCol)
          _ -> BinOpExpr "<>" targetCol <$> valueToExpr opVal
        "$gt" -> BinOpExpr ">" targetCol <$> valueToExpr opVal
        "$gte" -> BinOpExpr ">=" targetCol <$> valueToExpr opVal
        "$lt" -> BinOpExpr "<" targetCol <$> valueToExpr opVal
        "$lte" -> BinOpExpr "<=" targetCol <$> valueToExpr opVal
        "$like" -> BinOpExpr "LIKE" targetCol <$> valueToExpr opVal
        "$ilike" -> BinOpExpr "ILIKE" targetCol <$> valueToExpr opVal
        "$isNull" -> case opVal of
          Aeson.Bool True -> Right (UnaryOpExpr "IS NULL" targetCol)
          Aeson.Bool False -> Right (UnaryOpExpr "IS NOT NULL" targetCol)
          _ -> Left (InvalidValueError "$isNull requires a boolean value")
        "$in" -> case opVal of
          Aeson.Array arr -> do
            rendered <- mapM valueToLiteralText (toList arr)
            let inList = "(" <> T.intercalate ", " rendered <> ")"
            Right (BinOpExpr "IN" targetCol (LitExpr inList))
          _ -> Left (InvalidValueError "$in requires an array of scalar values")
        "$nin" -> case opVal of
          Aeson.Array arr -> do
            rendered <- mapM valueToLiteralText (toList arr)
            let inList = "(" <> T.intercalate ", " rendered <> ")"
            Right (BinOpExpr "NOT IN" targetCol (LitExpr inList))
          _ -> Left (InvalidValueError "$nin requires an array of scalar values")
        _ -> Left (InvalidOperatorError opText)

-- | Convert a JSON primitive value into a typed SQL expression 'QExpr a'.
valueToExpr :: Aeson.Value -> Either QueryToolError (QExpr a)
valueToExpr = \case
  Aeson.String s -> Right (LitExpr s)
  Aeson.Number n -> Right (LitExpr (renderNumber n))
  Aeson.Bool b -> Right (LitExpr (if b then "TRUE" else "FALSE"))
  Aeson.Null -> Right (LitExpr "NULL")
  Aeson.Array _ -> Left (InvalidValueError "Nested array cannot be parsed as scalar value")
  Aeson.Object _ -> Left (InvalidValueError "Nested object cannot be parsed as scalar value")

-- | Render a JSON primitive as a SQL literal string for IN clauses.
valueToLiteralText :: Aeson.Value -> Either QueryToolError Text
valueToLiteralText = \case
  Aeson.String s -> Right ("'" <> escapeSqlString s <> "'")
  Aeson.Number n -> Right (renderNumber n)
  Aeson.Bool b -> Right (if b then "TRUE" else "FALSE")
  Aeson.Null -> Right "NULL"
  _ -> Left (InvalidValueError "Only scalar primitives allowed in $in list")

-- | Escape single quotes for SQL string literals.
escapeSqlString :: Text -> Text
escapeSqlString = T.replace "'" "''"

-- | Render a scientific number cleanly as integer or decimal text.
renderNumber :: Scientific -> Text
renderNumber n = case floatingOrInteger n of
  Right (i :: Integer) -> T.pack (show i)
  Left (d :: Double) -> T.pack (show d)

-- | Convert a camelCase or mixed-case string to snake_case.
toSnakeCase :: Text -> Text
toSnakeCase t = case T.unpack t of
  [] -> ""
  (x : xs) -> T.pack (toLower x : go xs)
  where
    go [] = []
    go (c : cs)
      | isUpper c = '_' : toLower c : go cs
      | otherwise = c : go cs
