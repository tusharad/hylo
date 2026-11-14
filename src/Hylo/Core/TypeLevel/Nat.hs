{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.Core.TypeLevel.Nat
Description : Type-level natural number and symbol utilities for dimensions & models
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.TypeLevel.Nat
  ( -- * Type-Level Primitives
    Nat
  , Symbol
  , KnownNat
  , KnownSymbol
  , natVal
  , symbolVal
  , natValInt
  , symbolValText
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import GHC.TypeNats (KnownNat, Nat, natVal)

-- | Extract a type-level natural number as an 'Int'.
natValInt :: forall (n :: Nat) proxy. (KnownNat n) => proxy n -> Int
natValInt p = fromIntegral (natVal p)

-- | Extract a type-level symbol as 'Text'.
symbolValText :: forall (s :: Symbol) proxy. (KnownSymbol s) => proxy s -> Text
symbolValText p = T.pack (symbolVal p)
