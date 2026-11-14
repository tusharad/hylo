{- |
Module      : Hylo.Core
Description : Canonical re-export module for Hylo Core primitives
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core
  ( -- * Context & HKD Table Algebra
    Ctx (..)
  , Col
  , PatchField (..)
  , ColSpec (..)

    -- * Phantom Keys
  , Id (..)
  , toId

    -- * Expressions
  , QExpr (..)
  , lit
  , colExpr
  , just
  , isNull
  , coalesce
  , (==.)
  , (/=.)

    -- * Automatic Nullability Lifting
  , NullableTable (..)
  , LiftMay
  , liftNullableExpr
  , unNullableTable

    -- * Schema & Table Definitions
  , TableSchema (..)
  , tableSchema
  , column
  , nullableColumn
  , primaryKey
  , foreignKey
  , describeCol
  ) where

import Hylo.Core.Context
import Hylo.Core.Expr
import Hylo.Core.Key
import Hylo.Core.Nullable
import Hylo.Core.Table
