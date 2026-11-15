{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.PG.Hydration.Graph
Description : Graph Hydration Engine & Cardinality Invariants for LATERAL Queries
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Hydration.Graph
  ( hydrateGraph
  , hydrateJsonbChildren
  ) where

import qualified Data.ByteString as B
import Data.Text (Text)

{- | Hydrate parent records and correlated child elements by foreign key mapping.

Satisfies the Graph Hydration Cardinality Invariant:

@length (hydrateGraph getPk getFk parents children) == length parents@
-}
hydrateGraph ::
  Eq key =>
  (parent -> key) ->
  (child -> key) ->
  [parent] ->
  [child] ->
  [(parent, [child])]
hydrateGraph getPk getFk parents children =
  [ (p, [c | c <- children, getFk c == getPk p])
  | p <- parents
  ]

{- | Hydrate a parent item paired with raw binary JSON representation of children.

An empty JSON array (@"[]"@) decodes strictly to an empty list without runtime exceptions,
satisfying the Empty Collection Idempotence law:

@COALESCE(jsonb_agg(∅), '[]'::jsonb) ≡ '[]'::jsonb@
-}
hydrateJsonbChildren ::
  (B.ByteString -> Either Text [child]) ->
  (parent, B.ByteString) ->
  Either Text (parent, [child])
hydrateJsonbChildren decodeChildren (parent, rawJson)
  | B.null rawJson || rawJson == "[]" = Right (parent, [])
  | otherwise = case decodeChildren rawJson of
      Left err -> Left err
      Right cs -> Right (parent, cs)
