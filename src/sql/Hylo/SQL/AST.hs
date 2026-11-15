{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Hylo.SQL.AST
Description : Canonical relational SQL query AST, scope identifiers, and projectable expressions
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.SQL.AST
  ( -- * Query AST & Scopes
    ScopeId (..)
  , formatScope
  , generateScopes
  , RelationBinding (..)
  , LateralBinding (..)
  , Materialization (..)
  , CycleClause (..)
  , CTEType (..)
  , CTEBinding (..)
  , FromItem (..)
  , LockStrength (..)
  , LockWait (..)
  , LockClause (..)
  , renderLockClause
  , Projectable (..)
  , renderExpr
  , exprAliases
  ) where

import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core.Expr (QExpr (..))

instance NFData (QExpr a) where
  rnf expr = case expr of
    LitExpr t -> rnf t
    ColExpr r c -> rnf r `seq` rnf c
    BinOpExpr op l r -> rnf op `seq` rnf l `seq` rnf r
    UnaryOpExpr op e -> rnf op `seq` rnf e
    LiftNullableExpr e -> rnf e
    CoalesceExpr m dflt -> rnf m `seq` rnf dflt
    AggOpExpr fn arg mFilter -> rnf fn `seq` rnf arg `seq` rnf mFilter
    StringAggExpr arg delim mFilter -> rnf arg `seq` rnf delim `seq` rnf mFilter
    RowCmpExpr op cols aliases vals n ->
      rnf op `seq` rnf cols `seq` rnf aliases `seq` rnf vals `seq` rnf n

-- | Deterministic, collision-free relation scope identifier.
newtype ScopeId = ScopeId
  { unScopeId :: Int
  }
  deriving (Eq, Ord, Show)

instance NFData ScopeId where
  rnf (ScopeId n) = rnf n

-- | Format a table name and its monotonic ScopeId into a SQL relation alias (e.g. "users_0").
formatScope :: Text -> ScopeId -> Text
formatScope tName (ScopeId n) = tName <> "_" <> T.pack (show n)

-- | Generate a list of strictly monotonic and collision-free ScopeIds.
generateScopes :: Int -> [ScopeId]
generateScopes n = [ScopeId i | i <- [0 .. max 0 n - 1]]

-- | Internal binding descriptor tracking an instantiated relation within a Query monad.
data RelationBinding = RelationBinding
  { rbTableName :: !Text
  , rbScope :: !ScopeId
  , rbAlias :: !Text
  }
  deriving (Eq, Show)

instance NFData RelationBinding where
  rnf (RelationBinding t s a) = rnf t `seq` rnf s `seq` rnf a

-- | Internal binding descriptor tracking a correlated lateral subquery.
data LateralBinding = LateralBinding
  { lbAlias :: !Text
  , lbSubquerySql :: !Text
  }
  deriving (Eq, Show)

instance NFData LateralBinding where
  rnf (LateralBinding a s) = rnf a `seq` rnf s

-- | Common Table Expression materialization fence.
data Materialization
  = DefaultMaterialization
  | Materialized
  | NotMaterialized
  deriving (Eq, Show)

instance NFData Materialization where
  rnf m = m `seq` ()

-- | Cycle detection clause for recursive CTEs.
data CycleClause = CycleClause
  { cycleColumn :: !Text
  , cycleSetColumn :: !Text
  , cycleUsingColumn :: !Text
  }
  deriving (Eq, Show)

instance NFData CycleClause where
  rnf (CycleClause c s u) = rnf c `seq` rnf s `seq` rnf u

-- | Type of Common Table Expression.
data CTEType
  = NonRecursiveCTE !Materialization !Text !Text
  | RecursiveCTE !Text !Text !Text !(Maybe CycleClause)
  deriving (Eq, Show)

instance NFData CTEType where
  rnf (NonRecursiveCTE m n q) = rnf m `seq` rnf n `seq` rnf q
  rnf (RecursiveCTE n b r mc) = rnf n `seq` rnf b `seq` rnf r `seq` rnf mc

-- | Named CTE binding attached to query AST.
data CTEBinding = CTEBinding
  { cteName :: !Text
  , cteDefinition :: !CTEType
  }
  deriving (Eq, Show)

instance NFData CTEBinding where
  rnf (CTEBinding n d) = rnf n `seq` rnf d

-- | ANSI SQL-92 relational FROM tree constructed by the join flattener.
data FromItem
  = FromTable !Text !Text
  | InnerJoin !FromItem !FromItem !(QExpr Bool)
  | LeftJoin !FromItem !FromItem !(QExpr Bool)
  | LeftJoinLateral !FromItem !Text !Text
  | CrossJoin !FromItem !FromItem
  deriving (Eq, Show)

instance NFData FromItem where
  rnf = \case
    FromTable t a -> rnf t `seq` rnf a
    InnerJoin l r c -> rnf l `seq` rnf r `seq` rnf c
    LeftJoin l r c -> rnf l `seq` rnf r `seq` rnf c
    LeftJoinLateral l s a -> rnf l `seq` rnf s `seq` rnf a
    CrossJoin l r -> rnf l `seq` rnf r

-- | Pessimistic locking strength.
data LockStrength
  = ForUpdate
  | ForNoKeyUpdate
  | ForShare
  | ForKeyShare
  deriving (Eq, Show)

instance NFData LockStrength where
  rnf x = x `seq` ()

-- | Pessimistic locking wait policy.
data LockWait
  = LockWaitDefault
  | SkipLocked
  | NoWait
  deriving (Eq, Show)

instance NFData LockWait where
  rnf x = x `seq` ()

-- | Complete lock clause specification.
data LockClause = LockClause
  { lcStrength :: !LockStrength
  , lcWait :: !LockWait
  , lcTables :: ![Text]
  }
  deriving (Eq, Show)

instance NFData LockClause where
  rnf (LockClause s w ts) = rnf s `seq` rnf w `seq` rnf ts

-- | Render a lock clause into SQL syntax.
renderLockClause :: LockClause -> Text
renderLockClause (LockClause str wait tables) =
  let strText = case str of
        ForUpdate -> "FOR UPDATE"
        ForNoKeyUpdate -> "FOR NO KEY UPDATE"
        ForShare -> "FOR SHARE"
        ForKeyShare -> "FOR KEY SHARE"
      tablesText = case tables of
        [] -> ""
        ts -> " OF " <> T.intercalate ", " (map (\t -> "\"" <> t <> "\"") ts)
      waitText = case wait of
        LockWaitDefault -> ""
        SkipLocked -> " SKIP LOCKED"
        NoWait -> " NOWAIT"
   in strText <> tablesText <> waitText

-- | Render a typed QExpr into SQL text syntax.
renderExpr :: QExpr a -> Text
renderExpr = \case
  LitExpr t -> case T.toLower t of
    "true" -> "TRUE"
    "false" -> "FALSE"
    _ -> t
  ColExpr rel col ->
    if T.null rel
      then "\"" <> col <> "\""
      else
        if rel == "EXCLUDED"
          then "EXCLUDED.\"" <> col <> "\""
          else "\"" <> rel <> "\".\"" <> col <> "\""
  BinOpExpr op l r ->
    if op == "=" && renderExpr r == "NULL"
      then "(" <> renderExpr l <> " IS NULL)"
      else "(" <> renderExpr l <> " " <> op <> " " <> renderExpr r <> ")"
  UnaryOpExpr op e ->
    if op == "IS NULL" || op == "IS NOT NULL"
      then "(" <> renderExpr e <> " " <> op <> ")"
      else "(" <> op <> " " <> renderExpr e <> ")"
  LiftNullableExpr e -> renderExpr e
  CoalesceExpr m dflt -> "COALESCE(" <> renderExpr m <> ", " <> renderExpr dflt <> ")"
  AggOpExpr fn arg mFilter ->
    let base = fn <> "(" <> renderExpr arg <> ")"
     in case mFilter of
          Nothing -> base
          Just cond -> base <> " FILTER (WHERE " <> renderExpr cond <> ")"
  StringAggExpr arg delim mFilter ->
    let base = "string_agg(" <> renderExpr arg <> ", " <> renderExpr delim <> ")"
     in case mFilter of
          Nothing -> base
          Just cond -> base <> " FILTER (WHERE " <> renderExpr cond <> ")"
  RowCmpExpr op cols _ _ nParams ->
    let lhs = "(" <> T.intercalate ", " cols <> ")"
        rhs = "(" <> T.intercalate ", " ["$" <> T.pack (show i) | i <- [1 .. nParams]] <> ")"
     in "(" <> lhs <> " " <> op <> " " <> rhs <> ")"

-- | Extract all table relation aliases referenced in a QExpr expression.
exprAliases :: QExpr a -> [Text]
exprAliases = \case
  LitExpr _ -> []
  ColExpr rel _ -> [rel]
  BinOpExpr _ l r -> exprAliases l ++ exprAliases r
  UnaryOpExpr _ e -> exprAliases e
  LiftNullableExpr e -> exprAliases e
  CoalesceExpr m dflt -> exprAliases m ++ exprAliases dflt
  AggOpExpr _ arg mFilter -> exprAliases arg ++ maybe [] exprAliases mFilter
  StringAggExpr arg delim mFilter -> exprAliases arg ++ exprAliases delim ++ maybe [] exprAliases mFilter
  RowCmpExpr _ _ aliases _ _ -> aliases

-- | Class of types that can be projected into SQL SELECT column lists.
class Projectable a where
  projectColumns :: a -> [Text]

instance Projectable () where
  projectColumns () = ["1"]

instance Projectable (QExpr a) where
  projectColumns e = [renderExpr e]

instance (Projectable a, Projectable b) => Projectable (a, b) where
  projectColumns (a, b) = projectColumns a ++ projectColumns b

instance (Projectable a, Projectable b, Projectable c) => Projectable (a, b, c) where
  projectColumns (a, b, c) = projectColumns a ++ projectColumns b ++ projectColumns c

instance (Projectable a, Projectable b, Projectable c, Projectable d) => Projectable (a, b, c, d) where
  projectColumns (a, b, c, d) =
    projectColumns a ++ projectColumns b ++ projectColumns c ++ projectColumns d

instance
  (Projectable a, Projectable b, Projectable c, Projectable d, Projectable e) =>
  Projectable (a, b, c, d, e)
  where
  projectColumns (a, b, c, d, e) =
    projectColumns a
      ++ projectColumns b
      ++ projectColumns c
      ++ projectColumns d
      ++ projectColumns e

instance
  ( Projectable a
  , Projectable b
  , Projectable c
  , Projectable d
  , Projectable e
  , Projectable f
  ) =>
  Projectable (a, b, c, d, e, f)
  where
  projectColumns (a, b, c, d, e, f) =
    projectColumns a
      ++ projectColumns b
      ++ projectColumns c
      ++ projectColumns d
      ++ projectColumns e
      ++ projectColumns f

instance
  ( Projectable a
  , Projectable b
  , Projectable c
  , Projectable d
  , Projectable e
  , Projectable f
  , Projectable g
  ) =>
  Projectable (a, b, c, d, e, f, g)
  where
  projectColumns (a, b, c, d, e, f, g) =
    projectColumns a
      ++ projectColumns b
      ++ projectColumns c
      ++ projectColumns d
      ++ projectColumns e
      ++ projectColumns f
      ++ projectColumns g

instance
  ( Projectable a
  , Projectable b
  , Projectable c
  , Projectable d
  , Projectable e
  , Projectable f
  , Projectable g
  , Projectable h
  ) =>
  Projectable (a, b, c, d, e, f, g, h)
  where
  projectColumns (a, b, c, d, e, f, g, h) =
    projectColumns a
      ++ projectColumns b
      ++ projectColumns c
      ++ projectColumns d
      ++ projectColumns e
      ++ projectColumns f
      ++ projectColumns g
      ++ projectColumns h
