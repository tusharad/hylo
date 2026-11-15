{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.Core.Query.CTE
Description : Common Table Expressions (CTEs), recursive graphs, and materialization fences
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.Query.CTE
  ( -- * CTE References & Relational Binding
    CTERef (..)
  , mkCTERef
  , CTERow (..)

    -- * Fixed-Size CTE Row Tuples with Dot-Projection
  , CTETuple2 (..)
  , CTETuple3 (..)
  , CTETuple4 (..)
  , CTETuple5 (..)

    -- * Materialization & Cycle Controls
  , Materialization (..)
  , CycleClause (..)
  , CTEType (..)
  , CTEBinding (..)

    -- * CTE Combinators
  , with
  , withMaterialized
  , withRecursive
  , withRecursiveCycle

    -- * SQL Rendering of CTE clauses
  , renderCTEs
  , renderCTEBinding
  ) where

import Control.DeepSeq (NFData (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Records (HasField (..))
import Hylo.Core.Expr (QExpr (..), colExpr)
import Hylo.Core.Query.AST
  ( CTEBinding (..)
  , CTEType (..)
  , CycleClause (..)
  , FromItem (..)
  , LateralBinding (..)
  , Materialization (..)
  , Projectable (..)
  , RelationBinding (..)
  , ScopeId (..)
  , formatScope
  , renderExpr
  )
import Hylo.Core.Query.Monad
  ( FromRelation (..)
  , Query (..)
  , QueryState (..)
  , initialQueryState
  )
import Hylo.PG.Compiler.JoinFlattener
  ( flattenJoins
  , renderFromItem
  )

-- | Scoped reference to an in-flight Common Table Expression.
data CTERef (record :: Type) = CTERef
  { cteRefName :: !Text
  , cteRefColumns :: ![Text]
  , cteRefMkRecord :: !(Text -> record)
  }

instance NFData (CTERef record) where
  rnf (CTERef n c _) = rnf n `seq` rnf c

-- | Relational instance allowing 'CTERef' to be bound in 'Query' blocks via 'each'.
instance FromRelation (CTERef record) record where
  each cte = Query $ \s ->
    let scope = ScopeId (qsNextScope s)
        tName = cteRefName cte
        alias = formatScope tName scope
        exprRecord = cteRefMkRecord cte alias
        binding = RelationBinding tName scope alias
        s' =
          s
            { qsNextScope = qsNextScope s + 1
            , qsRelations = qsRelations s ++ [binding]
            }
     in (exprRecord, s')

-- | 2-column projected CTE record with positional and named field selectors.
data CTETuple2 a b = CTETuple2
  { ct2Col1 :: !a
  , ct2Col2 :: !b
  }
  deriving (Eq, Show)

instance (NFData a, NFData b) => NFData (CTETuple2 a b) where
  rnf (CTETuple2 a b) = rnf a `seq` rnf b

instance (Projectable a, Projectable b) => Projectable (CTETuple2 a b) where
  projectColumns (CTETuple2 a b) = projectColumns a ++ projectColumns b

instance HasField "id" (CTETuple2 a b) a where
  getField (CTETuple2 a _) = a

instance HasField "name" (CTETuple2 a b) b where
  getField (CTETuple2 _ b) = b

instance HasField "parent" (CTETuple2 a b) a where
  getField (CTETuple2 a _) = a

instance HasField "child" (CTETuple2 a b) b where
  getField (CTETuple2 _ b) = b

instance HasField "key" (CTETuple2 a b) a where
  getField (CTETuple2 a _) = a

instance HasField "val" (CTETuple2 a b) b where
  getField (CTETuple2 _ b) = b

instance HasField "_1" (CTETuple2 a b) a where
  getField (CTETuple2 a _) = a

instance HasField "_2" (CTETuple2 a b) b where
  getField (CTETuple2 _ b) = b

-- | 3-column projected CTE record with graph/tree field selectors.
data CTETuple3 a b c = CTETuple3
  { ct3Col1 :: !a
  , ct3Col2 :: !b
  , ct3Col3 :: !c
  }
  deriving (Eq, Show)

instance (NFData a, NFData b, NFData c) => NFData (CTETuple3 a b c) where
  rnf (CTETuple3 a b c) = rnf a `seq` rnf b `seq` rnf c

instance (Projectable a, Projectable b, Projectable c) => Projectable (CTETuple3 a b c) where
  projectColumns (CTETuple3 a b c) = projectColumns a ++ projectColumns b ++ projectColumns c

instance HasField "id" (CTETuple3 a b c) a where
  getField (CTETuple3 a _ _) = a

instance HasField "name" (CTETuple3 a b c) b where
  getField (CTETuple3 _ b _) = b

instance HasField "depth" (CTETuple3 a b c) c where
  getField (CTETuple3 _ _ c) = c

instance HasField "level" (CTETuple3 a b c) c where
  getField (CTETuple3 _ _ c) = c

instance HasField "parent" (CTETuple3 a b c) a where
  getField (CTETuple3 a _ _) = a

instance HasField "child" (CTETuple3 a b c) b where
  getField (CTETuple3 _ b _) = b

instance HasField "path" (CTETuple3 a b c) c where
  getField (CTETuple3 _ _ c) = c

instance HasField "_1" (CTETuple3 a b c) a where
  getField (CTETuple3 a _ _) = a

instance HasField "_2" (CTETuple3 a b c) b where
  getField (CTETuple3 _ b _) = b

instance HasField "_3" (CTETuple3 a b c) c where
  getField (CTETuple3 _ _ c) = c

-- | 4-column projected CTE record.
data CTETuple4 a b c d = CTETuple4
  { ct4Col1 :: !a
  , ct4Col2 :: !b
  , ct4Col3 :: !c
  , ct4Col4 :: !d
  }
  deriving (Eq, Show)

instance (NFData a, NFData b, NFData c, NFData d) => NFData (CTETuple4 a b c d) where
  rnf (CTETuple4 a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

instance (Projectable a, Projectable b, Projectable c, Projectable d) => Projectable (CTETuple4 a b c d) where
  projectColumns (CTETuple4 a b c d) =
    projectColumns a ++ projectColumns b ++ projectColumns c ++ projectColumns d

instance HasField "id" (CTETuple4 a b c d) a where
  getField (CTETuple4 a _ _ _) = a

instance HasField "name" (CTETuple4 a b c d) b where
  getField (CTETuple4 _ b _ _) = b

instance HasField "depth" (CTETuple4 a b c d) c where
  getField (CTETuple4 _ _ c _) = c

instance HasField "path" (CTETuple4 a b c d) d where
  getField (CTETuple4 _ _ _ d) = d

instance HasField "_1" (CTETuple4 a b c d) a where
  getField (CTETuple4 a _ _ _) = a

instance HasField "_2" (CTETuple4 a b c d) b where
  getField (CTETuple4 _ b _ _) = b

instance HasField "_3" (CTETuple4 a b c d) c where
  getField (CTETuple4 _ _ c _) = c

instance HasField "_4" (CTETuple4 a b c d) d where
  getField (CTETuple4 _ _ _ d) = d

-- | 5-column projected CTE record.
data CTETuple5 a b c d e = CTETuple5
  { ct5Col1 :: !a
  , ct5Col2 :: !b
  , ct5Col3 :: !c
  , ct5Col4 :: !d
  , ct5Col5 :: !e
  }
  deriving (Eq, Show)

instance (NFData a, NFData b, NFData c, NFData d, NFData e) => NFData (CTETuple5 a b c d e) where
  rnf (CTETuple5 a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

instance
  (Projectable a, Projectable b, Projectable c, Projectable d, Projectable e) =>
  Projectable (CTETuple5 a b c d e)
  where
  projectColumns (CTETuple5 a b c d e) =
    projectColumns a ++ projectColumns b ++ projectColumns c ++ projectColumns d ++ projectColumns e

-- | Class associating projected query expression structures with hydrated CTE dot-records.
class (Projectable ret, Projectable rec) => CTERow ret rec | ret -> rec, rec -> ret where
  defaultCTEColumns :: Proxy ret -> [Text]
  mkCTERow :: [Text] -> Text -> rec

safeHead :: Text -> [Text] -> Text
safeHead dflt = \case
  [] -> dflt
  (x : _) -> x

safeIndex :: Text -> Int -> [Text] -> Text
safeIndex dflt idx xs =
  case drop idx xs of
    (x : _) -> x
    [] -> dflt

instance CTERow (QExpr a) (QExpr a) where
  defaultCTEColumns _ = ["val"]
  mkCTERow cols alias =
    let c0 = safeHead "val" cols
     in colExpr alias c0

instance CTERow (QExpr a, QExpr b) (CTETuple2 (QExpr a) (QExpr b)) where
  defaultCTEColumns _ = ["id", "name"]
  mkCTERow cols alias =
    let c0 = safeHead "id" cols
        c1 = safeIndex "name" 1 cols
     in CTETuple2 (colExpr alias c0) (colExpr alias c1)

instance CTERow (QExpr a, QExpr b, QExpr c) (CTETuple3 (QExpr a) (QExpr b) (QExpr c)) where
  defaultCTEColumns _ = ["id", "name", "depth"]
  mkCTERow cols alias =
    let c0 = safeHead "id" cols
        c1 = safeIndex "name" 1 cols
        c2 = safeIndex "depth" 2 cols
     in CTETuple3 (colExpr alias c0) (colExpr alias c1) (colExpr alias c2)

instance CTERow (QExpr a, QExpr b, QExpr c, QExpr d) (CTETuple4 (QExpr a) (QExpr b) (QExpr c) (QExpr d)) where
  defaultCTEColumns _ = ["id", "name", "depth", "path"]
  mkCTERow cols alias =
    let c0 = safeHead "id" cols
        c1 = safeIndex "name" 1 cols
        c2 = safeIndex "depth" 2 cols
        c3 = safeIndex "path" 3 cols
     in CTETuple4 (colExpr alias c0) (colExpr alias c1) (colExpr alias c2) (colExpr alias c3)

instance
  CTERow
    (QExpr a, QExpr b, QExpr c, QExpr d, QExpr e)
    (CTETuple5 (QExpr a) (QExpr b) (QExpr c) (QExpr d) (QExpr e))
  where
  defaultCTEColumns _ = ["col_0", "col_1", "col_2", "col_3", "col_4"]
  mkCTERow cols alias =
    let c0 = safeHead "col_0" cols
        c1 = safeIndex "col_1" 1 cols
        c2 = safeIndex "col_2" 2 cols
        c3 = safeIndex "col_3" 3 cols
        c4 = safeIndex "col_4" 4 cols
     in CTETuple5
          (colExpr alias c0)
          (colExpr alias c1)
          (colExpr alias c2)
          (colExpr alias c3)
          (colExpr alias c4)

-- | Construct a typed 'CTERef' with default column identifiers.
mkCTERef :: forall ret rec. CTERow ret rec => Text -> CTERef rec
mkCTERef name =
  let cols = defaultCTEColumns (Proxy @ret)
   in CTERef
        { cteRefName = name
        , cteRefColumns = cols
        , cteRefMkRecord = mkCTERow cols
        }

-- | Render inner SQL string for CTE definitions.
renderQueryInner :: Projectable a => a -> QueryState -> Text
renderQueryInner res st =
  case qsUpsert st of
    Just upsertSql -> upsertSql
    Nothing ->
      case qsCustomQuery st of
        Just customSql -> customSql
        Nothing ->
          let projections = projectColumns res
              selectClause =
                "SELECT "
                  <> (if null projections then "1" else T.intercalate ", " projections)
              (mFrom, wherePreds) = flattenJoins (qsRelations st) (qsPredicates st)
              mFromWithLaterals = case mFrom of
                Nothing -> case qsLaterals st of
                  [] -> Nothing
                  (l : ls) ->
                    Just $
                      foldl
                        (\acc (LateralBinding a sql) -> LeftJoinLateral acc sql a)
                        (FromTable (lbAlias l) (lbAlias l))
                        ls
                Just baseItem ->
                  Just $
                    foldl
                      (\acc (LateralBinding a sql) -> LeftJoinLateral acc sql a)
                      baseItem
                      (qsLaterals st)
              fromClause = case mFromWithLaterals of
                Nothing -> ""
                Just item -> " FROM " <> renderFromItem item
              whereClause = case wherePreds of
                [] -> ""
                (p : ps) ->
                  let combined = foldl (BinOpExpr "AND") p ps
                   in " WHERE " <> renderExpr combined
              groupByClause = case qsGroupBy st of
                [] -> ""
                cols -> " GROUP BY " <> T.intercalate ", " cols
              havingClause = case qsHaving st of
                [] -> ""
                (h : hs) ->
                  let combined = foldl (BinOpExpr "AND") h hs
                   in " HAVING " <> renderExpr combined
              orderByClause = case qsOrderBy st of
                [] -> ""
                orders -> " ORDER BY " <> T.intercalate ", " orders
           in selectClause <> fromClause <> whereClause <> groupByClause <> havingClause <> orderByClause

-- | Define a standard Common Table Expression (CTE) in the 'Query' monad.
with ::
  forall db ret rec final.
  (CTERow ret rec) =>
  Text ->
  Query db ret ->
  (CTERef rec -> Query db final) ->
  Query db final
with = withMaterialized DefaultMaterialization

-- | Define a CTE with explicit PostgreSQL 12+ materialization controls.
withMaterialized ::
  forall db ret rec final.
  (CTERow ret rec) =>
  Materialization ->
  Text ->
  Query db ret ->
  (CTERef rec -> Query db final) ->
  Query db final
withMaterialized mat name cteQuery mainQuery = Query $ \s ->
  let cteRef = mkCTERef @ret name
      (cteRes, cteSt) = runQuery cteQuery (initialQueryState {qsNextScope = qsNextScope s})
      cteSql = renderQueryInner cteRes cteSt
      binding = CTEBinding name (NonRecursiveCTE mat name cteSql)
      mainStInit =
        s
          { qsNextScope = qsNextScope cteSt
          , qsCTEs = qsCTEs s ++ [binding]
          }
      (finalRes, finalSt) = runQuery (mainQuery cteRef) mainStInit
   in (finalRes, finalSt)

-- | Define a recursive CTE executing base and recursive iterations until convergence.
withRecursive ::
  forall db ret rec final.
  (CTERow ret rec) =>
  Text ->
  (CTERef rec -> Query db ret) ->
  (CTERef rec -> Query db ret) ->
  (CTERef rec -> Query db final) ->
  Query db final
withRecursive name =
  withRecursiveCycle name Nothing

-- | Define a recursive CTE with optional cycle detection (PostgreSQL 14+).
withRecursiveCycle ::
  forall db ret rec final.
  (CTERow ret rec) =>
  Text ->
  Maybe CycleClause ->
  (CTERef rec -> Query db ret) ->
  (CTERef rec -> Query db ret) ->
  (CTERef rec -> Query db final) ->
  Query db final
withRecursiveCycle name mCycle baseCase recCase mainQuery = Query $ \s ->
  let cteRef = mkCTERef @ret name
      (baseRes, baseSt) = runQuery (baseCase cteRef) (initialQueryState {qsNextScope = qsNextScope s})
      baseSql = renderQueryInner baseRes baseSt
      (recRes, recSt) = runQuery (recCase cteRef) (initialQueryState {qsNextScope = qsNextScope baseSt})
      recSql = renderQueryInner recRes recSt
      binding = CTEBinding name (RecursiveCTE name baseSql recSql mCycle)
      mainStInit =
        s
          { qsNextScope = qsNextScope recSt
          , qsCTEs = qsCTEs s ++ [binding]
          }
      (finalRes, finalSt) = runQuery (mainQuery cteRef) mainStInit
   in (finalRes, finalSt)

-- | Render a list of CTE bindings into SQL WITH / WITH RECURSIVE prefix clauses.
renderCTEs :: [CTEBinding] -> Text
renderCTEs = \case
  [] -> ""
  ctes ->
    let isRecursive = any isRec ctes
        prefix = if isRecursive then "WITH RECURSIVE " else "WITH "
        renderedBindings = T.intercalate ",\n" (map renderCTEBinding ctes)
     in prefix <> renderedBindings <> " "
  where
    isRec (CTEBinding _ (RecursiveCTE {})) = True
    isRec _ = False

-- | Render an individual CTE definition.
renderCTEBinding :: CTEBinding -> Text
renderCTEBinding (CTEBinding _ (NonRecursiveCTE mat name sql)) =
  let matStr = case mat of
        DefaultMaterialization -> ""
        Materialized -> "MATERIALIZED "
        NotMaterialized -> "NOT MATERIALIZED "
   in "\"" <> name <> "\" AS " <> matStr <> "(\n  " <> sql <> "\n)"
renderCTEBinding (CTEBinding _ (RecursiveCTE name baseSql recSql mCycle)) =
  let cycleStr = case mCycle of
        Nothing -> ""
        Just (CycleClause c s u) ->
          " CYCLE \"" <> c <> "\" SET \"" <> s <> "\" USING \"" <> u <> "\""
   in "\""
        <> name
        <> "\" AS (\n  "
        <> baseSql
        <> "\n  UNION ALL\n  "
        <> recSql
        <> "\n)"
        <> cycleStr
