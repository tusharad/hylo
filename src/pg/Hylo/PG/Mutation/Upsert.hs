{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.Mutation.Upsert
Description : Type-safe ON CONFLICT upsert algebra and EXCLUDED column referencing
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides atomic PostgreSQL @INSERT ... ON CONFLICT@ synthesis with support for
unique constraint targets, column list targets, DO NOTHING, and DO UPDATE SET
referencing both current and EXCLUDED pseudo-table columns.
-}
module Hylo.PG.Mutation.Upsert
  ( -- * Conflict Targets
    ConflictTarget (..)
  , onConstraint
  , onColumns
  , onConflictDoNothing

    -- * Assignment & Conflict Actions
  , Assignment (..)
  , pattern Assignment
  , Assignable (..)
  , (=:)
  , ConflictAction (..)
  , doNothing
  , doUpdate

    -- * Row Value Serialization
  , InsertValues (..)

    -- * Upsert Action & Combinators
  , UpsertAction (..)
  , insertOnConflict
  , insertOnConflictDoNothing
  , renderUpsertSQL
  , renderUpsertSQLText
  , renderAssignment

    -- * Transactional Guarded Upsert
  , upsert
  ) where

import Control.DeepSeq (NFData (..))
import Data.Foldable (toList)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Hylo.Core.Context (ColDef (..), ColSpec, Ctx (..))
import Hylo.Core.Expr (QExpr (..), ToQExpr (..))
import Hylo.Core.Query (Tx (..), TxMode (..))
import Hylo.Core.Query.Monad (Query (..), QueryState (..))
import Hylo.Core.Table (Table (..), TableSchema (..))
import Hylo.Core.Transaction (BoundParam (..))
import Hylo.Core.TypeError (AssertWritable (..))
import Hylo.SQL.AST (Projectable (..), renderExpr)
import Hylo.SQL.Builder
  ( Dialect (PostgresDialect)
  , SqlBuilderM
  , SqlBuilderState (..)
  , bindPlaceholder
  , buildExpr
  , cleanParamValue
  , inferParamType
  , initialBuilderState
  , runSqlBuilderWithState
  )

-- | Conflict target specification for PostgreSQL ON CONFLICT clause.
data ConflictTarget
  = OnConstraint !Text
  | OnColumns ![Text]
  | OnConflictDoNothing
  deriving (Eq, Show)

instance NFData ConflictTarget where
  rnf = \case
    OnConstraint t -> rnf t
    OnColumns ts -> rnf ts
    OnConflictDoNothing -> ()

-- | Target a named UNIQUE or EXCLUSION constraint.
onConstraint :: Text -> ConflictTarget
onConstraint = OnConstraint

-- | Target specific unique column names.
onColumns :: [Text] -> ConflictTarget
onColumns = OnColumns

-- | Target conflict with no specific columns (DO NOTHING).
onConflictDoNothing :: ConflictTarget
onConflictDoNothing = OnConflictDoNothing

-- | Existential wrapper for a 'QExpr' of any underlying type.
data SomeExpr where
  SomeExpr :: !(QExpr a) -> SomeExpr

instance Show SomeExpr where
  show (SomeExpr e) = show e

instance NFData SomeExpr where
  rnf (SomeExpr e) = rnf e

-- | Single field assignment within an UPDATE SET clause.
data Assignment = AssignmentInternal
  { assignColumn :: !Text
  , assignValue :: !Text
  , assignExpr :: !(Maybe SomeExpr)
  }

pattern Assignment :: Text -> Text -> Assignment
pattern Assignment col val <- AssignmentInternal col val _
  where
    Assignment col val = AssignmentInternal col val Nothing

{-# COMPLETE Assignment #-}

instance Eq Assignment where
  (AssignmentInternal c1 v1 _) == (AssignmentInternal c2 v2 _) = c1 == c2 && v1 == v2

instance Show Assignment where
  show (AssignmentInternal c v _) = "Assignment " ++ show c ++ " " ++ show v

instance NFData Assignment where
  rnf (AssignmentInternal c v e) = rnf c `seq` rnf v `seq` rnf e

-- | Class of types whose column name can be determined for an assignment target.
class Assignable target a | target -> a where
  targetColumnName :: target -> Text

instance Assignable (QExpr a) a where
  targetColumnName = \case
    ColExpr _ c -> c
    _ -> ""

instance Assignable (ColSpec a) a where
  targetColumnName = colName

infix 4 =:

-- | Construct a column assignment from an assignable target and a value expression.
(=:) :: (Assignable target a, ToQExpr val a) => target -> val -> Assignment
(=:) target val =
  let qe = toQExpr val
   in AssignmentInternal (targetColumnName target) (renderExpr qe) (Just (SomeExpr qe))

-- | Conflict resolution action on constraint violation.
data ConflictAction
  = DoNothing
  | DoUpdate ![Assignment]
  deriving (Eq, Show)

instance NFData ConflictAction where
  rnf = \case
    DoNothing -> ()
    DoUpdate as -> rnf as

-- | Conflict action resolving by discarding the conflicting insert without error.
doNothing :: ConflictAction
doNothing = DoNothing

-- | Conflict action updating conflicting row columns.
doUpdate :: [Assignment] -> ConflictAction
doUpdate = DoUpdate

-- | Class specifying how to render inserted row values into column-value pairs.
class Table table => InsertValues (table :: Ctx -> Type) row where
  renderInsertValues :: table 'Meta -> row -> [(Text, Text)]

instance Table table => InsertValues table [(Text, Text)] where
  renderInsertValues _ kvs = kvs

-- | Descriptor for a prepared ON CONFLICT upsert statement.
data UpsertAction (table :: Ctx -> Type) = UpsertAction
  { uaSchema :: !(TableSchema table)
  , uaValues :: ![(Text, Text)]
  , uaTarget :: !ConflictTarget
  , uaAction :: !ConflictAction
  }

instance NFData (UpsertAction table) where
  rnf (UpsertAction (TableSchema tn _) vs tg ac) =
    rnf tn `seq` rnf vs `seq` rnf tg `seq` rnf ac

instance Projectable (UpsertAction table) where
  projectColumns _ = []

-- | Construct a type-safe upsert action against a table schema with conflict handling.
insertOnConflict ::
  (InsertValues table row) =>
  TableSchema table ->
  row ->
  ConflictTarget ->
  (table 'Expr -> table 'Expr -> [Assignment]) ->
  Query db (UpsertAction table)
insertOnConflict schema row target updateFn =
  let current = toTableExpr (tableName schema) (tableMeta schema)
      excluded = toTableExpr "EXCLUDED" (tableMeta schema)
      action = case target of
        OnConflictDoNothing -> DoNothing
        _ -> case updateFn current excluded of
          [] -> DoNothing
          as -> DoUpdate as
      act =
        UpsertAction
          { uaSchema = schema
          , uaValues = renderInsertValues (tableMeta schema) row
          , uaTarget = target
          , uaAction = action
          }
   in Query $ \s ->
        let (upsertSql, params) = renderUpsertSQL act
            s' = s {qsUpsert = Just (upsertSql, params)}
         in (act, s')

-- | Construct an upsert action that performs DO NOTHING on conflict.
insertOnConflictDoNothing ::
  (InsertValues table row) =>
  TableSchema table ->
  row ->
  ConflictTarget ->
  Query db (UpsertAction table)
insertOnConflictDoNothing schema row target =
  insertOnConflict schema row target (\_ _ -> [])

-- | Render an Assignment into SQL text: @"column" = value@.
renderAssignment :: Assignment -> Text
renderAssignment (AssignmentInternal col val _) = "\"" <> col <> "\" = " <> val

-- | Clean single or double surrounding quotes from a raw value string.
cleanVal :: Text -> Text
cleanVal t = case T.uncons (cleanParamValue t) of
  Just ('\'', rest) -> case T.unsnoc rest of
    Just (middle, '\'') -> middle
    _ -> cleanParamValue t
  _ -> cleanParamValue t

-- | Render a full UpsertAction into PostgreSQL SQL text and bound parameters.
renderUpsertSQL :: (Table table) => UpsertAction table -> (Text, [BoundParam])
renderUpsertSQL (UpsertAction schema kvs target action) =
  let tbl = "\"" <> tableName schema <> "\""
      colDefs = tableColDefs (tableMeta schema)
      colTypeMap = Map.fromList [(colName c, colSqlType c) | c <- colDefs]

      -- 1. Parameterize inserted row values
      (valPlaceholders, insertParams) =
        foldl
          ( \(phs, ps) (col, rawVal) ->
              let clean = cleanVal rawVal
                  isDefault = T.toUpper (T.strip clean) == "DEFAULT"
               in if isDefault
                    then (phs ++ ["DEFAULT"], ps)
                    else
                      let nextIdx = length ps + 1
                          cType = fromMaybe (inferParamType clean) (Map.lookup col colTypeMap)
                          p = BoundParam nextIdx clean cType
                          ph = bindPlaceholder PostgresDialect nextIdx
                       in (phs ++ [ph], ps ++ [p])
          )
          ([], [])
          kvs

      insertPart = case kvs of
        [] -> "INSERT INTO " <> tbl <> " DEFAULT VALUES"
        _ ->
          let cols = T.intercalate ", " (map (\(c, _) -> "\"" <> c <> "\"") kvs)
              vals = T.intercalate ", " valPlaceholders
           in "INSERT INTO " <> tbl <> " (" <> cols <> ") VALUES (" <> vals <> ")"

      -- 2. Parameterize DO UPDATE SET expressions
      (conflictPart, allParams) = case target of
        OnConflictDoNothing -> (" ON CONFLICT DO NOTHING", insertParams)
        OnConstraint c ->
          let (actSql, ps) = renderConflictAction action insertParams
           in (" ON CONFLICT ON CONSTRAINT \"" <> c <> "\"" <> actSql, ps)
        OnColumns cs ->
          let cols = T.intercalate ", " (map (\c -> "\"" <> c <> "\"") cs)
              (actSql, ps) = renderConflictAction action insertParams
           in (" ON CONFLICT (" <> cols <> ")" <> actSql, ps)
   in (insertPart <> conflictPart, allParams)

-- | Render the conflict resolution action and accumulate parameters into the state.
renderConflictAction :: ConflictAction -> [BoundParam] -> (Text, [BoundParam])
renderConflictAction DoNothing ps = (" DO NOTHING", ps)
renderConflictAction (DoUpdate []) ps = (" DO NOTHING", ps)
renderConflictAction (DoUpdate assignments) initialParams =
  let initSt =
        (initialBuilderState PostgresDialect)
          { sbsParamCount = length initialParams
          , sbsParams = Seq.fromList initialParams
          }
      act = mapM buildAssignment assignments
      (renderedAssignments, finalSt) = runSqlBuilderWithState initSt act
      finalParams = toList (sbsParams finalSt)
      sql = " DO UPDATE SET " <> T.intercalate ", " renderedAssignments
   in (sql, finalParams)
  where
    buildAssignment :: Assignment -> SqlBuilderM Text
    buildAssignment (AssignmentInternal col val mExpr) = case mExpr of
      Just (SomeExpr qe) -> do
        b <- buildExpr PostgresDialect qe
        pure ("\"" <> col <> "\" = " <> toStrict (toLazyText b))
      Nothing ->
        pure ("\"" <> col <> "\" = " <> val)

-- | Render a full UpsertAction into raw SQL text without bound parameters.
renderUpsertSQLText :: (Table table) => UpsertAction table -> Text
renderUpsertSQLText = fst . renderUpsertSQL

-- | Statically guarded transactional upsert. Fails with [Hylo Error E005] in ReadOnly mode.
upsert ::
  forall table row db (mode :: TxMode).
  (AssertWritable mode, InsertValues table row) =>
  TableSchema table ->
  row ->
  ConflictTarget ->
  (table 'Expr -> table 'Expr -> [Assignment]) ->
  Tx db mode (Query db (UpsertAction table))
upsert schema row target updateFn =
  writableDict (Proxy :: Proxy mode) `seq`
    TxReturn (insertOnConflict schema row target updateFn)
