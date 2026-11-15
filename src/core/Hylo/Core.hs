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
  , LogicalType (..)
  , lVector
  , renderLogicalType
  , textToLogicalType
  , ColDef (..)
  , ColSpec

    -- * Phantom Keys
  , Id (..)
  , toId

    -- * Expressions & Comparison Operators
  , QExpr (..)
  , lit
  , colExpr
  , just
  , isNull
  , coalesce
  , (==.)
  , (/=.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)

    -- * Closed Type Family Diagnostics (E001–E015)
  , AssertComparable (..)
  , CheckComparable
  , AssertGrouped (..)
  , CheckGrouped
  , AssertWritable (..)
  , CheckWritable
  , AssertNoIO (..)
  , CheckNoIO
  , AssertMatchingVectors (..)
  , CheckMatchingVectors
  , AssertBackendCapability (..)
  , CheckBackendCapability
  , AssertHasVersion (..)
  , CheckHasVersion
  , AssertParameterized (..)
  , CheckParameterized
  , AssertHasPrimaryKey (..)
  , CheckHasPrimaryKey
  , AssertValidForeignKey (..)
  , CheckValidForeignKey
  , AssertAcyclic (..)
  , CheckAcyclic
  , AssertPaginationValid (..)
  , CheckPaginationValid
  , AssertSafeMigration (..)
  , CheckSafeMigration

    -- * Transactions, Failure & Changesets
  , TxMode (..)
  , Tx (..)
  , TxError (..)
  , TxOp (..)
  , TxState (..)
  , initialTxState
  , txFail
  , withSavepoint
  , executeTx
  , runTxPure
  , onCommit
  , onRollback
  , Changeset (..)
  , emptyChangeset
  , withUniqueKey
  , FieldError (..)

    -- * Automatic Nullability Lifting
  , NullableTable (..)
  , LiftMay
  , liftNullableExpr
  , unNullableTable

    -- * Schema & Table Definitions
  , TableSchema (..)
  , tableSchema
  , Table (..)
  , colSpecExpr
  , column
  , nullableColumn
  , primaryKey
  , foreignKey
  , describeCol
  , TableCatalogDef (..)
  , ColumnCatalogDef (..)
  , toColumnCatalogDef
  , tableCatalog

    -- * Zero-Generics Codec Engine
  , Codec (..)
  , ValueCodec
  , RowCodec
  , Decoder (..)
  , runBinaryEncoder
  , runBinaryDecoder
  , field
  , fieldWith
  , nullable
  , idCodec
  , dimap
  , lmap
  , rmap
  , pairCodec
  , tripleCodec
  , quadCodec
  ) where

import Hylo.Core.Codec
import Hylo.Core.Context
import Hylo.Core.Error
import Hylo.Core.Expr
import Hylo.Core.Key
import Hylo.Core.Nullable
import Hylo.Core.Schema
import Hylo.Core.Table
import Hylo.Core.Transaction
import Hylo.Core.TypeError
