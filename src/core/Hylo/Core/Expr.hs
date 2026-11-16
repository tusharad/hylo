{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.Core.Expr
Description : Core typed SQL expression AST representation
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.Expr
  ( QExpr (..)
  , ToQExpr (..)
  , lit
  , litTyped
  , func2Expr
  , colExpr
  , just
  , isNull
  , isNull_
  , IsNullToken (..)
  , coalesce
  , enumLit
  , EqOp (..)
  , (/=.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  , (&&.)
  , (||.)
  , in_
  , notIn_
  , AssertComparable
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Hylo.Core.TypeError
  ( AssertComparable (..)
  , AssertEnumLabel (..)
  , EnumLabelsOf
  , HasEnumLabels (..)
  , LabelIn
  )

-- | Class of expression types convertible to 'QExpr'.
class ToQExpr e a | e -> a where
  toQExpr :: e -> QExpr a

instance ToQExpr (QExpr a) a where
  toQExpr = id

-- | Typed SQL AST expression representing an expression evaluating to type @a@.
data QExpr (a :: Type) where
  LitExpr :: !Text -> QExpr a
  LitTypedExpr :: !Text -> !Text -> QExpr a
  ColExpr :: !Text -> !Text -> QExpr a
  BinOpExpr :: !Text -> !(QExpr a) -> !(QExpr b) -> QExpr c
  UnaryOpExpr :: !Text -> !(QExpr a) -> QExpr b
  LiftNullableExpr :: !(QExpr a) -> QExpr (Maybe a)
  CoalesceExpr :: !(QExpr (Maybe a)) -> !(QExpr a) -> QExpr a
  AggOpExpr :: !Text -> !(QExpr a) -> !(Maybe (QExpr Bool)) -> QExpr b
  Func2Expr :: !Text -> !(QExpr a) -> !(QExpr b) -> QExpr c
  StringAggExpr :: !(QExpr Text) -> !(QExpr Text) -> !(Maybe (QExpr Bool)) -> QExpr Text
  RowCmpExpr :: !Text -> ![Text] -> ![Text] -> ![Text] -> !Int -> QExpr Bool
  InListExpr :: !Bool -> !(QExpr a) -> ![Text] -> QExpr Bool

instance Show (QExpr a) where
  showsPrec d expr = case expr of
    LitExpr t -> showParen (d > 10) $ showString "LitExpr " . shows t
    LitTypedExpr val ty ->
      showParen (d > 10) $
        showString "LitTypedExpr "
          . shows val
          . showString " "
          . shows ty
    ColExpr r c -> showParen (d > 10) $ showString "ColExpr " . shows r . showString " " . shows c
    BinOpExpr op l r ->
      showParen (d > 10) $
        showString "BinOpExpr "
          . shows op
          . showString " "
          . shows l
          . showString " "
          . shows r
    UnaryOpExpr op e ->
      showParen (d > 10) $
        showString "UnaryOpExpr "
          . shows op
          . showString " "
          . shows e
    LiftNullableExpr e ->
      showParen (d > 10) $
        showString "LiftNullableExpr " . shows e
    CoalesceExpr m dflt ->
      showParen (d > 10) $
        showString "CoalesceExpr "
          . shows m
          . showString " "
          . shows dflt
    AggOpExpr fn arg mFilter ->
      showParen (d > 10) $
        showString "AggOpExpr "
          . shows fn
          . showString " "
          . shows arg
          . showString " "
          . shows mFilter
    Func2Expr fn a b ->
      showParen (d > 10) $
        showString "Func2Expr "
          . shows fn
          . showString " "
          . shows a
          . showString " "
          . shows b
    StringAggExpr arg delim mFilter ->
      showParen (d > 10) $
        showString "StringAggExpr "
          . shows arg
          . showString " "
          . shows delim
          . showString " "
          . shows mFilter
    RowCmpExpr op cols aliases vals n ->
      showParen (d > 10) $
        showString "RowCmpExpr "
          . shows op
          . showString " "
          . shows cols
          . showString " "
          . shows aliases
          . showString " "
          . shows vals
          . showString " "
          . shows n
    InListExpr isIn e items ->
      showParen (d > 10) $
        showString "InListExpr "
          . shows isIn
          . showString " "
          . shows e
          . showString " "
          . shows items

instance Eq (QExpr a) where
  LitExpr t1 == LitExpr t2 = t1 == t2
  LitTypedExpr v1 t1 == LitTypedExpr v2 t2 = v1 == v2 && t1 == t2
  ColExpr r1 c1 == ColExpr r2 c2 = r1 == r2 && c1 == c2
  BinOpExpr o1 l1 r1 == BinOpExpr o2 l2 r2 =
    o1 == o2 && show l1 == show l2 && show r1 == show r2
  UnaryOpExpr o1 e1 == UnaryOpExpr o2 e2 = o1 == o2 && show e1 == show e2
  LiftNullableExpr e1 == LiftNullableExpr e2 = show e1 == show e2
  CoalesceExpr m1 d1 == CoalesceExpr m2 d2 =
    show m1 == show m2 && show d1 == show d2
  AggOpExpr f1 a1 m1 == AggOpExpr f2 a2 m2 =
    f1 == f2 && show a1 == show a2 && show m1 == show m2
  Func2Expr f1 a1 b1 == Func2Expr f2 a2 b2 =
    f1 == f2 && show a1 == show a2 && show b1 == show b2
  StringAggExpr a1 d1 m1 == StringAggExpr a2 d2 m2 =
    show a1 == show a2 && show d1 == show d2 && show m1 == show m2
  RowCmpExpr o1 c1 a1 v1 n1 == RowCmpExpr o2 c2 a2 v2 n2 =
    o1 == o2 && c1 == c2 && a1 == a2 && v1 == v2 && n1 == n2
  InListExpr in1 e1 items1 == InListExpr in2 e2 items2 =
    in1 == in2 && show e1 == show e2 && items1 == items2
  _ == _ = False

-- | Construct a literal expression from any Show-able value.
lit :: Show a => a -> QExpr a
lit v = LitExpr (T.pack (show v))

-- | Construct an explicitly typed literal expression with a PostgreSQL logical type.
litTyped :: Text -> Text -> QExpr a
litTyped = LitTypedExpr

-- | Construct a two-argument function expression.
func2Expr :: Text -> QExpr a -> QExpr b -> QExpr c
func2Expr = Func2Expr

-- | Construct a scoped column expression from table alias and column name.
colExpr :: Text -> Text -> QExpr a
colExpr = ColExpr

-- | Explicitly lift an expression into a nullable context (SQL @JUST@).
just :: QExpr a -> QExpr (Maybe a)
just = LiftNullableExpr

-- | Sentinel token representing a SQL NULL value in comparison expressions.
data IsNullToken = IsNullToken
  deriving (Eq, Show)

-- | SQL NULL token for equality comparisons (e.g. @where_ (e.managerId ==. isNull)@).
isNull :: IsNullToken
isNull = IsNullToken

-- | Explicitly test if a nullable expression is NULL.
isNull_ :: QExpr (Maybe a) -> QExpr Bool
isNull_ = UnaryOpExpr "IS NULL"

-- | Provide a fallback value for a nullable expression.
coalesce :: QExpr (Maybe a) -> QExpr a -> QExpr a
coalesce = CoalesceExpr

{- | Compile-time-verified enum label literal for enum-typed columns.

Emits @[Hylo Error E017]@ when 'label' is not one of the labels registered
for the column's enum ADT via its 'HasEnumLabels' instance.

Usage: @where_ (order.status ==. enumLit @"pending" (Proxy @"status"))@
-}
enumLit ::
  forall (label :: Symbol) (col :: Symbol) a.
  ( HasEnumLabels a
  , KnownSymbol label
  , AssertEnumLabel col label (EnumLabelsOf a) (LabelIn label (EnumLabelsOf a))
  ) =>
  Proxy col ->
  QExpr a
enumLit _ = case hasEnumLabelsDict (Proxy @a) of
  () -> case enumLabelDict
    (Proxy @col)
    (Proxy @label)
    (Proxy @(EnumLabelsOf a))
    (Proxy @(LabelIn label (EnumLabelsOf a))) of
    () -> LitExpr (T.pack (symbolVal (Proxy @label)))

instance (Num a, Show a) => Num (QExpr a) where
  (+) = BinOpExpr "+"
  (-) = BinOpExpr "-"
  (*) = BinOpExpr "*"
  negate = UnaryOpExpr "-"
  abs = UnaryOpExpr "ABS"
  signum = UnaryOpExpr "SIGN"
  fromInteger n = lit (fromInteger n :: a)

-- | Class for equality comparisons, supporting standard typed comparisons and IS NULL tokens.
class EqOp e1 e2 where
  (==.) :: e1 -> e2 -> QExpr Bool

infix 4 ==.

instance
  {-# OVERLAPPABLE #-}
  (ToQExpr e1 a, ToQExpr e2 b, AssertComparable a b) =>
  EqOp e1 e2
  where
  l ==. r = comparableDict (Proxy :: Proxy a) (Proxy :: Proxy b) `seq` BinOpExpr "=" (toQExpr l) (toQExpr r)

instance EqOp (QExpr (Maybe a)) IsNullToken where
  e ==. _ = UnaryOpExpr "IS NULL" e

-- | Typed inequality operator guarded by 'AssertComparable'.
(/=.) ::
  forall e1 e2 a b. (ToQExpr e1 a, ToQExpr e2 b, AssertComparable a b) => e1 -> e2 -> QExpr Bool
(/=.) l r = comparableDict (Proxy :: Proxy a) (Proxy :: Proxy b) `seq` BinOpExpr "<>" (toQExpr l) (toQExpr r)

infix 4 /=.

-- | Typed less-than operator guarded by 'AssertComparable'.
(<.) ::
  forall e1 e2 a b.
  (ToQExpr e1 a, ToQExpr e2 b, AssertComparable a b, a ~ b) => e1 -> e2 -> QExpr Bool
(<.) l r = comparableDict (Proxy :: Proxy a) (Proxy :: Proxy b) `seq` BinOpExpr "<" (toQExpr l) (toQExpr r)

infix 4 <.

-- | Typed less-than-or-equal operator guarded by 'AssertComparable'.
(<=.) ::
  forall e1 e2 a b.
  (ToQExpr e1 a, ToQExpr e2 b, AssertComparable a b, a ~ b) => e1 -> e2 -> QExpr Bool
(<=.) l r = comparableDict (Proxy :: Proxy a) (Proxy :: Proxy b) `seq` BinOpExpr "<=" (toQExpr l) (toQExpr r)

infix 4 <=.

-- | Typed greater-than operator guarded by 'AssertComparable'.
(>.) ::
  forall e1 e2 a b.
  (ToQExpr e1 a, ToQExpr e2 b, AssertComparable a b, a ~ b) => e1 -> e2 -> QExpr Bool
(>.) l r = comparableDict (Proxy :: Proxy a) (Proxy :: Proxy b) `seq` BinOpExpr ">" (toQExpr l) (toQExpr r)

infix 4 >.

-- | Typed greater-than-or-equal operator guarded by 'AssertComparable'.
(>=.) ::
  forall e1 e2 a b.
  (ToQExpr e1 a, ToQExpr e2 b, AssertComparable a b, a ~ b) => e1 -> e2 -> QExpr Bool
(>=.) l r = comparableDict (Proxy :: Proxy a) (Proxy :: Proxy b) `seq` BinOpExpr ">=" (toQExpr l) (toQExpr r)

infix 4 >=.

-- | Logical AND of two boolean expressions.
(&&.) :: QExpr Bool -> QExpr Bool -> QExpr Bool
(&&.) = BinOpExpr "AND"

infixr 3 &&.

-- | Logical OR of two boolean expressions.
(||.) :: QExpr Bool -> QExpr Bool -> QExpr Bool
(||.) = BinOpExpr "OR"

infixr 2 ||.

-- | Typed SQL IN list inclusion operator (e.g. @where_ (in_ doc.title ["Draft", "Published"])@).
in_ :: (ToQExpr e a, Show a) => e -> [a] -> QExpr Bool
in_ e xs = InListExpr True (toQExpr e) (map (cleanShowVal . T.pack . show) xs)

-- | Typed SQL NOT IN list exclusion operator.
notIn_ :: (ToQExpr e a, Show a) => e -> [a] -> QExpr Bool
notIn_ e xs = InListExpr False (toQExpr e) (map (cleanShowVal . T.pack . show) xs)

-- | Helper stripping surrounding quotes from 'Show' output on string values.
cleanShowVal :: Text -> Text
cleanShowVal t = case T.uncons t of
  Just ('"', rest) -> case T.unsnoc rest of
    Just (middle, '"') -> middle
    _ -> t
  _ -> t
