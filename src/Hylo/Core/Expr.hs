{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.Core.Expr
Description : Core typed SQL expression AST representation
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.Expr
  ( QExpr (..)
  , lit
  , colExpr
  , just
  , isNull
  , coalesce
  , (==.)
  , (/=.)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T

-- | Typed SQL AST expression representing an expression evaluating to type @a@.
data QExpr (a :: Type) where
  LitExpr :: !Text -> QExpr a
  ColExpr :: !Text -> !Text -> QExpr a
  BinOpExpr :: !Text -> !(QExpr a) -> !(QExpr b) -> QExpr c
  UnaryOpExpr :: !Text -> !(QExpr a) -> QExpr b
  LiftNullableExpr :: !(QExpr a) -> QExpr (Maybe a)
  CoalesceExpr :: !(QExpr (Maybe a)) -> !(QExpr a) -> QExpr a

instance Show (QExpr a) where
  showsPrec d expr = case expr of
    LitExpr t -> showParen (d > 10) $ showString "LitExpr " . shows t
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
        showString "UnaryOpExpr " . shows op . showString " " . shows e
    LiftNullableExpr e ->
      showParen (d > 10) $ showString "LiftNullableExpr " . shows e
    CoalesceExpr m def ->
      showParen (d > 10) $
        showString "CoalesceExpr " . shows m . showString " " . shows def

instance Eq (QExpr a) where
  LitExpr t1 == LitExpr t2 = t1 == t2
  ColExpr r1 c1 == ColExpr r2 c2 = r1 == r2 && c1 == c2
  BinOpExpr o1 l1 r1 == BinOpExpr o2 l2 r2 =
    o1 == o2 && show l1 == show l2 && show r1 == show r2
  UnaryOpExpr o1 e1 == UnaryOpExpr o2 e2 = o1 == o2 && show e1 == show e2
  LiftNullableExpr e1 == LiftNullableExpr e2 = show e1 == show e2
  CoalesceExpr m1 d1 == CoalesceExpr m2 d2 =
    show m1 == show m2 && show d1 == show d2
  _ == _ = False

-- | Construct a literal expression from any Show-able value.
lit :: Show a => a -> QExpr a
lit v = LitExpr (T.pack (show v))

-- | Construct a scoped column expression from table alias and column name.
colExpr :: Text -> Text -> QExpr a
colExpr = ColExpr

-- | Explicitly lift an expression into a nullable context (SQL @JUST@).
just :: QExpr a -> QExpr (Maybe a)
just = LiftNullableExpr

-- | Test if a nullable expression is NULL.
isNull :: QExpr (Maybe a) -> QExpr Bool
isNull = UnaryOpExpr "IS NULL"

-- | Provide a fallback value for a nullable expression.
coalesce :: QExpr (Maybe a) -> QExpr a -> QExpr a
coalesce = CoalesceExpr

-- | Typed equality operator for expressions of identical type.
(==.) :: QExpr a -> QExpr a -> QExpr Bool
(==.) = BinOpExpr "="

infix 4 ==.

-- | Typed inequality operator for expressions of identical type.
(/=.) :: QExpr a -> QExpr a -> QExpr Bool
(/=.) = BinOpExpr "<>"

infix 4 /=.
