{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Hylo.Core.Query.Recursive
Description : Recursive CTE graph traversals, least fixed-point evaluation, and cycle prevention
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.Query.Recursive
  ( -- * Re-exports from CTE core
    module Hylo.Core.Query.CTE
  ) where

import Hylo.Core.Query.CTE hiding
  ( with
  , withMaterialized
  , withRecursive
  , withRecursiveCycle
  )
