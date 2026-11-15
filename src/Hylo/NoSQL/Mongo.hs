{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.NoSQL.Mongo
Description : MongoDB document adapter and Aggregation Pipeline compiler
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.NoSQL.Mongo
  ( -- * MongoDB Backend Marker
    MongoDB

    -- * Aggregation Pipeline Stages
  , MongoStage (..)
  , renderMongoPipeline
  , renderMongoStages
  ) where

import Control.DeepSeq (NFData (..))
import Data.Aeson
  ( ToJSON (..)
  , Value (..)
  , object
  , (.=)
  )
import qualified Data.Aeson.Key as Key
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Hylo.Backend.Capability (MongoDB)
import Hylo.Core.Expr (QExpr (..))
import Hylo.Core.Query.AST (Projectable (..))
import Hylo.Core.Query.Monad
  ( Query (..)
  , QueryState (..)
  , initialQueryState
  )

-- | MongoDB aggregation pipeline stages.
data MongoStage
  = MatchStage !Value
  | ProjectStage !Value
  | SortStage !Value
  | SkipStage !Int
  | LimitStage !Int
  deriving (Show, Eq, Generic)

instance NFData MongoStage

instance ToJSON MongoStage where
  toJSON = \case
    MatchStage v -> object ["$match" .= v]
    ProjectStage v -> object ["$project" .= v]
    SortStage v -> object ["$sort" .= v]
    SkipStage n -> object ["$skip" .= n]
    LimitStage n -> object ["$limit" .= n]

-- | Render an operational Query for MongoDB into a JSON Aggregation Pipeline array.
renderMongoPipeline :: Projectable a => Query MongoDB a -> Value
renderMongoPipeline q =
  Array (Vector.fromList (map toJSON (renderMongoStages q)))

-- | Render an operational Query for MongoDB into a structured list of pipeline stages.
renderMongoStages :: Projectable a => Query MongoDB a -> [MongoStage]
renderMongoStages q =
  let (res, st) = runQuery q initialQueryState
      mMatch = case qsPredicates st of
        [] -> Nothing
        preds -> Just (MatchStage (object (concatMap predToPairs preds)))
      cols = cleanCols (projectColumns res)
      mProject =
        if null cols
          then Nothing
          else
            let projectFields =
                  [Key.fromText c .= (1 :: Int) | c <- cols]
                    ++ [Key.fromText "_id" .= (0 :: Int) | "_id" `notElem` cols]
             in Just (ProjectStage (object projectFields))
      mSkip = fmap SkipStage (qsOffset st)
      mLimit = fmap LimitStage (qsLimit st)
   in catMaybes [mMatch, mProject, mSkip, mLimit]

-- | Clean column name from quoted SQL AST form (e.g. "\"users_0\".\"email\"" -> "email").
cleanCols :: [Text] -> [Text]
cleanCols = map extractName
  where
    extractName t =
      case reverse (T.splitOn "." t) of
        [] -> ""
        (raw : _) -> T.filter (/= '"') raw

-- | Convert AST predicate into MongoDB match key-value pairs.
predToPairs :: QExpr a -> [(Key.Key, Value)]
predToPairs = \case
  BinOpExpr "=" (ColExpr _ col) (LitExpr val) ->
    [(Key.fromText col, parseLiteral val)]
  BinOpExpr "=" (LitExpr val) (ColExpr _ col) ->
    [(Key.fromText col, parseLiteral val)]
  BinOpExpr "AND" p1 p2 ->
    predToPairs p1 ++ predToPairs p2
  ColExpr _ col ->
    [(Key.fromText col, Bool True)]
  _ -> []

-- | Convert SQL literal strings into JSON values.
parseLiteral :: Text -> Value
parseLiteral t
  | t == "TRUE" || t == "true" = Bool True
  | t == "FALSE" || t == "false" = Bool False
  | t == "NULL" || t == "null" = Null
  | T.isPrefixOf "'" t && T.isSuffixOf "'" t && T.length t >= 2 =
      String (T.drop 1 (T.dropEnd 1 t))
  | otherwise = String t
