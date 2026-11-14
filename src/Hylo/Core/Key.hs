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

import Control.DeepSeq (NFData (..))

{- | Phantom-typed Primary and Foreign Key wrapper.

Parameterized by the table type @table@ and the underlying
key representation type @a@ (such as @Int64@ or @UUID@).

This guarantees at compile time that keys from different tables
(e.g. @Id UserT Int64@ and @Id OrgT Int64@) cannot be compared or mixed up.
-}
newtype Id table a = Id {unId :: a}
  deriving (Eq, Ord)

instance Show a => Show (Id table a) where
  showsPrec p (Id a) = showsPrec p a

instance NFData a => NFData (Id table a) where
  rnf (Id a) = rnf a

-- | Construct a phantom-typed key from an underlying value.
toId :: a -> Id table a
toId = Id
