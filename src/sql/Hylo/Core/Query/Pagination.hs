{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.Core.Query.Pagination
Description : Keyset-based O(1) pagination combinators and composite tuple comparisons
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Keyset pagination provides index-backed composite row comparisons ensuring constant
response time regardless of page depth, eliminating the linear scan penalties of OFFSET pagination.
-}
module Hylo.Core.Query.Pagination
  ( -- * Keyset Seeking Combinators
    seekAfter
  , seekBefore
  , seekAfterDir
  , seekBeforeDir
  , SeekDirection (..)

    -- * Keyset Class & Representations
  , Keyset (..)
  , KeysetCursor (..)
  , KeysetPage (..)
  ) where

import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import Hylo.Core.Expr (QExpr (..), ToQExpr (..))
import Hylo.Core.Query.AST (exprAliases, renderExpr)
import Hylo.Core.Query.Monad (Query (..), where_)

-- | Direction for keyset seeking.
data SeekDirection
  = SeekAsc
  | SeekDesc
  deriving (Eq, Show)

instance NFData SeekDirection where
  rnf x = x `seq` ()

-- | Keyset cursor wrapping pagination boundary values.
newtype KeysetCursor vals = KeysetCursor
  { unKeysetCursor :: vals
  }
  deriving (Eq, Show)

instance NFData vals => NFData (KeysetCursor vals) where
  rnf (KeysetCursor v) = rnf v

-- | Page structure returned by keyset pagination queries.
data KeysetPage a vals = KeysetPage
  { pageItems :: ![a]
  , pageHasNext :: !Bool
  , pageHasPrev :: !Bool
  , pageNextCursor :: !(Maybe vals)
  , pagePrevCursor :: !(Maybe vals)
  }
  deriving (Eq, Show, Functor)

-- | Class of composite column expressions and corresponding cursor value tuples.
class Keyset cols vals | cols -> vals where
  keysetCols :: cols -> [Text]
  keysetAliases :: cols -> [Text]

instance {-# OVERLAPPABLE #-} ToQExpr e a => Keyset e a where
  keysetCols e = [renderExpr (toQExpr e)]
  keysetAliases e = exprAliases (toQExpr e)

instance (ToQExpr e1 a, ToQExpr e2 b) => Keyset (e1, e2) (a, b) where
  keysetCols (c1, c2) =
    [ renderExpr (toQExpr c1)
    , renderExpr (toQExpr c2)
    ]
  keysetAliases (c1, c2) =
    exprAliases (toQExpr c1) ++ exprAliases (toQExpr c2)

instance (ToQExpr e1 a, ToQExpr e2 b, ToQExpr e3 c) => Keyset (e1, e2, e3) (a, b, c) where
  keysetCols (c1, c2, c3) =
    [ renderExpr (toQExpr c1)
    , renderExpr (toQExpr c2)
    , renderExpr (toQExpr c3)
    ]
  keysetAliases (c1, c2, c3) =
    exprAliases (toQExpr c1)
      ++ exprAliases (toQExpr c2)
      ++ exprAliases (toQExpr c3)

instance
  (ToQExpr e1 a, ToQExpr e2 b, ToQExpr e3 c, ToQExpr e4 d) =>
  Keyset (e1, e2, e3, e4) (a, b, c, d)
  where
  keysetCols (c1, c2, c3, c4) =
    [ renderExpr (toQExpr c1)
    , renderExpr (toQExpr c2)
    , renderExpr (toQExpr c3)
    , renderExpr (toQExpr c4)
    ]
  keysetAliases (c1, c2, c3, c4) =
    exprAliases (toQExpr c1)
      ++ exprAliases (toQExpr c2)
      ++ exprAliases (toQExpr c3)
      ++ exprAliases (toQExpr c4)

instance
  (ToQExpr e1 a, ToQExpr e2 b, ToQExpr e3 c, ToQExpr e4 d, ToQExpr e5 e) =>
  Keyset (e1, e2, e3, e4, e5) (a, b, c, d, e)
  where
  keysetCols (c1, c2, c3, c4, c5) =
    [ renderExpr (toQExpr c1)
    , renderExpr (toQExpr c2)
    , renderExpr (toQExpr c3)
    , renderExpr (toQExpr c4)
    , renderExpr (toQExpr c5)
    ]
  keysetAliases (c1, c2, c3, c4, c5) =
    exprAliases (toQExpr c1)
      ++ exprAliases (toQExpr c2)
      ++ exprAliases (toQExpr c3)
      ++ exprAliases (toQExpr c4)
      ++ exprAliases (toQExpr c5)

-- | Add a keyset seek predicate for next page navigation (descending feed: @(cols) < ($1, $2, ...)@).
seekAfter :: Keyset cols vals => cols -> vals -> Query db ()
seekAfter cols _vals =
  let cs = keysetCols cols
   in where_ (RowCmpExpr "<" cs (keysetAliases cols) (length cs))

-- | Add a keyset seek predicate for previous page navigation (descending feed: @(cols) > ($1, $2, ...)@).
seekBefore :: Keyset cols vals => cols -> vals -> Query db ()
seekBefore cols _vals =
  let cs = keysetCols cols
   in where_ (RowCmpExpr ">" cs (keysetAliases cols) (length cs))

-- | Explicitly directed forward keyset seek.
seekAfterDir :: Keyset cols vals => SeekDirection -> cols -> vals -> Query db ()
seekAfterDir SeekDesc = seekAfter
seekAfterDir SeekAsc = seekBefore

-- | Explicitly directed backward keyset seek.
seekBeforeDir :: Keyset cols vals => SeekDirection -> cols -> vals -> Query db ()
seekBeforeDir SeekDesc = seekBefore
seekBeforeDir SeekAsc = seekAfter
