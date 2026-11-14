{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

{- |
Module      : Hylo.Core.Key
Description : Phantom-typed Primary and Foreign Keys
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.Key
  ( Id (..)
  , toId
  ) where

import Data.Kind (Type)
import Hylo.Core.Context (Ctx)

{- | Phantom-typed Primary and Foreign Key wrapper.

Parameterized by the table type @(table :: Ctx -> Type)@ and the underlying
key representation type @a@ (such as @Int64@ or @UUID@).

This guarantees at compile time that keys from different tables
(e.g. @Id UserT Int64@ and @Id OrgT Int64@) cannot be compared or mixed up.
-}
newtype Id (table :: Ctx -> Type) a = Id {unId :: a}
  deriving (Eq, Ord, Show)

-- | Construct a phantom-typed key from an underlying value.
toId :: a -> Id table a
toId = Id
