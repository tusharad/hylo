{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Hylo.Core.Context
Description : 4-Context Higher-Kinded Data (HKD) representation
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.Context
  ( Ctx (..)
  , Col
  , PatchField (..)
  , ColSpec (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Hylo.Core.Expr (QExpr)

{- | The 4 core contexts of a Hylo Higher-Kinded Data (HKD) table:

1. 'Result': Hydrated Haskell domain values returned from queries (e.g. @Int64@, @Text@).
2. 'Expr':   Typed SQL AST expressions used in query blocks (e.g. @QExpr Text@).
3. 'Patch':  Differential field state for changesets (@Unchanged@, @SetTo a@, @SetDefault@).
4. 'Meta':   Column metadata descriptors, constraints, and codec mapping.
-}
data Ctx
  = Result
  | Expr
  | Patch
  | Meta
  deriving (Eq, Show)

-- | Differential field state for mutation changesets.
data PatchField a
  = Unchanged
  | SetTo !a
  | SetDefault
  deriving (Eq, Show, Functor)

-- | Schema metadata descriptor for a single table column.
data ColSpec a = ColSpec
  { colName :: !Text
  , colSqlType :: !Text
  , colIsNullable :: !Bool
  , colIsPrimaryKey :: !Bool
  , colDescription :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | Open type family projecting a column type based on context.
type Col :: Ctx -> Type -> Type
type family Col ctx a where
  Col 'Result a = a
  Col 'Expr a = QExpr a
  Col 'Patch a = PatchField a
  Col 'Meta a = ColSpec a
