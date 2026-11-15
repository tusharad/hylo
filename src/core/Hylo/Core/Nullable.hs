{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.Core.Nullable
Description : Automatic idempotent nullability lifting for LEFT JOINs
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.Nullable
  ( NullableTable (..)
  , LiftMay
  , liftNullableExpr
  , unNullableTable
  ) where

import Data.Kind (Type)
import GHC.Records (HasField (..))
import Hylo.Core.Expr (QExpr (..))
import Unsafe.Coerce (unsafeCoerce)

{- | Automatic idempotent nullability lifting type family.

Satisfies:
  @LiftMay (Maybe a) = Maybe a@
  @LiftMay a         = Maybe a@

Invariant (Idempotence of LiftMay):
  @LiftMay (LiftMay a) ~ LiftMay a@

This guarantees zero nested @Maybe (Maybe a)@ types when accessing
already-nullable columns through a LEFT JOIN.
-}
type LiftMay :: Type -> Type
type family LiftMay a where
  LiftMay (Maybe a) = Maybe a
  LiftMay a = Maybe a

{- | Wrapper around an expression record @(table 'Expr)@ representing a relation
that has been joined via an optional (@LEFT JOIN@).

Notice: Defined without record field selectors to prevent conflict with GHC's
built-in 'HasField' instances.
-}
newtype NullableTable exprRecord = NullableTable exprRecord
  deriving (Eq, Show)

-- | Unwrap a 'NullableTable' into its underlying expression record.
unNullableTable :: NullableTable exprRecord -> exprRecord
unNullableTable (NullableTable r) = r

-- | Lift any 'QExpr' to its idempotent nullable form.
liftNullableExpr :: QExpr a -> QExpr (LiftMay a)
liftNullableExpr (LiftNullableExpr e) = unsafeCoerce (LiftNullableExpr e)
liftNullableExpr e = unsafeCoerce (LiftNullableExpr e)

{- | Custom 'HasField' instance enabling 'OverloadedRecordDot':
Accessing any field on @NullableTable exprRecord@ automatically projects @QExpr (Maybe a)@,
whether the underlying field was originally non-null (@QExpr a@) or already nullable (@QExpr (Maybe a)@).
-}
instance
  (HasField field exprRecord (QExpr a), lifted ~ LiftMay a) =>
  HasField field (NullableTable exprRecord) (QExpr lifted)
  where
  getField (NullableTable rec) = liftNullableExpr (getField @field rec)
