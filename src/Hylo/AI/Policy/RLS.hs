{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.AI.Policy.RLS
Description : Row-Level Security (RLS) policies and column whitelisting for AI agent query tools
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.AI.Policy.RLS
  ( AgentPolicy (..)
  , AgentQueryPolicy
  , defaultPolicy
  , withAllowedFilterCols
  , withMandatoryScope
  , withMaxPageSize
  ) where

import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import Hylo.Core.Context (Ctx (..))
import Hylo.Core.Expr (QExpr, lit)

-- | Row-Level Security and Column Whitelist policy governing AI agent tool query compilation.
data AgentPolicy table = AgentPolicy
  { allowedFilterCols :: ![Text]
  , maxPageSize :: !Int
  , mandatoryScope :: !(table 'Expr -> QExpr Bool)
  }

-- | Type synonym aligning with Hylo PRD specification.
type AgentQueryPolicy db table = AgentPolicy table

instance Show (AgentPolicy table) where
  showsPrec d p =
    showParen (d > 10) $
      showString "AgentPolicy { allowedFilterCols = "
        . shows (allowedFilterCols p)
        . showString ", maxPageSize = "
        . shows (maxPageSize p)
        . showString " }"

instance NFData (AgentPolicy table) where
  rnf p = rnf (allowedFilterCols p) `seq` rnf (maxPageSize p)

-- | Default baseline policy with no allowed filter columns, max page size of 100, and trivial scope.
defaultPolicy :: AgentPolicy table
defaultPolicy =
  AgentPolicy
    { allowedFilterCols = []
    , maxPageSize = 100
    , mandatoryScope = \_ -> lit True
    }

-- | Update the allowed filter column whitelist.
withAllowedFilterCols :: [Text] -> AgentPolicy table -> AgentPolicy table
withAllowedFilterCols cols p = p {allowedFilterCols = cols}

-- | Update the mandatory tenant isolation scope predicate.
withMandatoryScope :: (table 'Expr -> QExpr Bool) -> AgentPolicy table -> AgentPolicy table
withMandatoryScope scope p = p {mandatoryScope = scope}

-- | Update the maximum page size.
withMaxPageSize :: Int -> AgentPolicy table -> AgentPolicy table
withMaxPageSize size p = p {maxPageSize = size}
