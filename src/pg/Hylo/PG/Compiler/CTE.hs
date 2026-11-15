{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.Compiler.CTE
Description : PostgreSQL Dialect Common Table Expressions (WITH / WITH RECURSIVE) & static capability gating
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.Compiler.CTE
  ( -- * Backend Capabilities
    SupportsCTE (..)
  , SupportsRecursiveCTE (..)
  , SupportsRecursiveCTEs
  , recursiveCTEsDict
  , DynamoDB

    -- * Capability-Guarded Combinators
  , with
  , withMaterialized
  , withRecursive
  , withRecursiveCycle

    -- * Re-exports from CTE Core
  , module Hylo.SQL.Query.CTE
  ) where

import Hylo.Backend.Capability
  ( DynamoDB
  , SupportsCTE (..)
  , SupportsRecursiveCTE (..)
  , SupportsRecursiveCTEs
  , recursiveCTEsDict
  , with
  , withMaterialized
  , withRecursive
  , withRecursiveCycle
  )
import Hylo.SQL.Query.CTE hiding
  ( with
  , withMaterialized
  , withRecursive
  , withRecursiveCycle
  )
