{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.PG.Array
Description : PostgreSQL Array operators and typed combinators
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides type-safe operators and combinators for PostgreSQL array expressions,
including containment, overlap, concatenation, and cardinality.
-}
module Hylo.PG.Array
  ( -- * Literals
    litArray

    -- * Array Combinators
  , arrayContains
  , arrayContainedBy
  , arrayOverlaps
  , arrayConcat
  , cardinality

    -- * Symbolic Infix Operators
  , (@>=)
  , (<@=)
  , (&&=)
  , (++.)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core.Expr (QExpr (..), litTyped)

-- | Construct a typed PostgreSQL array literal (e.g. "{1,2,3}"), bound as a $1 parameter.
litArray :: Show a => Text -> [a] -> QExpr [a]
litArray elemType xs =
  let rendered = "{" <> T.intercalate "," (map (cleanShow . T.pack . show) xs) <> "}"
   in litTyped rendered (elemType <> "[]")
  where
    cleanShow t = case T.uncons t of
      Just ('"', rest) -> case T.unsnoc rest of
        Just (middle, '"') -> middle
        _ -> t
      _ -> t

-- | Array contains test (@>). True if first array contains all elements of the second.
arrayContains :: QExpr [a] -> QExpr [a] -> QExpr Bool
arrayContains = BinOpExpr "@>"

-- | Infix operator for 'arrayContains'.
(@>=) :: QExpr [a] -> QExpr [a] -> QExpr Bool
(@>=) = arrayContains

infix 4 @>=

-- | Array contained-by test (<@). True if first array is contained within the second.
arrayContainedBy :: QExpr [a] -> QExpr [a] -> QExpr Bool
arrayContainedBy = BinOpExpr "<@"

-- | Infix operator for 'arrayContainedBy'.
(<@=) :: QExpr [a] -> QExpr [a] -> QExpr Bool
(<@=) = arrayContainedBy

infix 4 <@=

-- | Array overlaps test (&&). True if arrays share at least one common element.
arrayOverlaps :: QExpr [a] -> QExpr [a] -> QExpr Bool
arrayOverlaps = BinOpExpr "&&"

-- | Infix operator for 'arrayOverlaps'.
(&&=) :: QExpr [a] -> QExpr [a] -> QExpr Bool
(&&=) = arrayOverlaps

infix 4 &&=

-- | Array concatenation operator (||).
arrayConcat :: QExpr [a] -> QExpr [a] -> QExpr [a]
arrayConcat = BinOpExpr "||"

-- | Infix operator for 'arrayConcat'.
(++.) :: QExpr [a] -> QExpr [a] -> QExpr [a]
(++.) = arrayConcat

infixr 5 ++.

-- | Calculate total number of elements in array using PostgreSQL @cardinality()@ function.
cardinality :: QExpr [a] -> QExpr Int
cardinality arr = AggOpExpr "cardinality" arr Nothing
