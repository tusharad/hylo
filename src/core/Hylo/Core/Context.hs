{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
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
  , LogicalType (..)
  , lVector
  , renderLogicalType
  , textToLogicalType
  , castLogicalType
  , ColDef (..)
  , ColSpec
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeNats (KnownNat, Nat, natVal)
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

-- | Database-agnostic logical data types for column definitions.
data LogicalType (a :: Type) where
  LInt64 :: LogicalType a
  LText :: LogicalType a
  LBool :: LogicalType a
  LUTCTime :: LogicalType a
  LUUID :: LogicalType a
  LJson :: LogicalType a
  LVector :: forall (n :: Nat) a. (KnownNat n) => Proxy n -> LogicalType a
  LCustom :: !Text -> LogicalType a

-- | Smart constructor for type-level vector dimension.
lVector :: forall (n :: Nat) a. (KnownNat n) => LogicalType a
lVector = LVector (Proxy @n)

instance Eq (LogicalType a) where
  LInt64 == LInt64 = True
  LText == LText = True
  LBool == LBool = True
  LUTCTime == LUTCTime = True
  LUUID == LUUID = True
  LJson == LJson = True
  LVector (p1 :: Proxy n1) == LVector (p2 :: Proxy n2) = natVal p1 == natVal p2
  LCustom t1 == LCustom t2 = t1 == t2
  _ == _ = False

instance Show (LogicalType a) where
  show = \case
    LInt64 -> "LInt64"
    LText -> "LText"
    LBool -> "LBool"
    LUTCTime -> "LUTCTime"
    LUUID -> "LUUID"
    LJson -> "LJson"
    LVector p -> "LVector " ++ show (natVal p)
    LCustom t -> "LCustom " ++ show t

-- | Render a logical type into standard SQL data type representation.
renderLogicalType :: LogicalType a -> Text
renderLogicalType = \case
  LInt64 -> "bigint"
  LText -> "text"
  LBool -> "boolean"
  LUTCTime -> "timestamp"
  LUUID -> "uuid"
  LJson -> "json"
  LVector p -> "vector(" <> T.pack (show (natVal p)) <> ")"
  LCustom t -> t

-- | Cast the phantom type parameter of a 'LogicalType'.
castLogicalType :: LogicalType a -> LogicalType b
castLogicalType = \case
  LInt64 -> LInt64
  LText -> LText
  LBool -> LBool
  LUTCTime -> LUTCTime
  LUUID -> LUUID
  LJson -> LJson
  LVector p -> LVector p
  LCustom t -> LCustom t

-- | Parse textual type representation into a logical type (case-insensitive).
textToLogicalType :: Text -> LogicalType a
textToLogicalType raw =
  let t = T.toLower (T.strip raw)
   in case t of
        "bigint" -> LInt64
        "int8" -> LInt64
        "integer" -> LInt64
        "int" -> LInt64
        "int4" -> LInt64
        "text" -> LText
        "varchar" -> LText
        "string" -> LText
        "boolean" -> LBool
        "bool" -> LBool
        "timestamp" -> LUTCTime
        "utc" -> LUTCTime
        "uuid" -> LUUID
        "json" -> LJson
        _ -> LCustom raw

instance IsString (LogicalType a) where
  fromString = textToLogicalType . T.pack

-- | Schema metadata descriptor for a single table column.
data ColDef a = ColDef
  { colName :: !Text
  , colLogicalType :: !(LogicalType a)
  , colIsNullable :: !Bool
  , colIsPrimaryKey :: !Bool
  , colDescription :: !(Maybe Text)
  , colSqlType :: !Text
  }
  deriving (Eq, Show)

-- | Backwards-compatibility type synonym for 'ColDef'.
type ColSpec a = ColDef a

-- | Open type family projecting a column type based on context.
type Col :: Ctx -> Type -> Type
type family Col ctx a where
  Col 'Result a = a
  Col 'Expr a = QExpr a
  Col 'Patch a = PatchField a
  Col 'Meta a = ColDef a
