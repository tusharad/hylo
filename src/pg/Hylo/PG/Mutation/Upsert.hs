{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , renderAssignment

    -- * Transactional Guarded Upsert
  , upsert
  ) where

import Control.DeepSeq (NFData (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core.Context (ColDef (..), ColSpec, Ctx (..))
import Hylo.Core.Expr (QExpr (..), ToQExpr (..))
import Hylo.Core.Query (Tx (..), TxMode (..))
import Hylo.Core.Query.AST (Projectable (..), renderExpr)
import Hylo.Core.Query.Monad (Query (..), QueryState (..))
import Hylo.Core.Table (Table (..), TableSchema (..))
import Hylo.Core.TypeError (AssertWritable (..))

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

-- | Single field assignment within an UPDATE SET clause.
data Assignment = Assignment
  { assignColumn :: !Text
  , assignValue :: !Text
  }
  deriving (Eq, Show)

instance NFData Assignment where
  rnf (Assignment c v) = rnf c `seq` rnf v

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
(=:) target val = Assignment (targetColumnName target) (renderExpr (toQExpr val))

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
        let s' = s {qsUpsert = Just (renderUpsertSQL act)}
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
renderAssignment (Assignment col val) = "\"" <> col <> "\" = " <> val

-- | Render a full UpsertAction into PostgreSQL SQL text.
renderUpsertSQL :: UpsertAction table -> Text
renderUpsertSQL (UpsertAction schema kvs target action) =
  let tbl = "\"" <> tableName schema <> "\""
      insertPart = case kvs of
        [] -> "INSERT INTO " <> tbl <> " DEFAULT VALUES"
        _ ->
          let cols = T.intercalate ", " (map (\(c, _) -> "\"" <> c <> "\"") kvs)
              vals = T.intercalate ", " (map snd kvs)
           in "INSERT INTO " <> tbl <> " (" <> cols <> ") VALUES (" <> vals <> ")"
      conflictPart = case target of
        OnConflictDoNothing -> " ON CONFLICT DO NOTHING"
        OnConstraint c -> " ON CONFLICT ON CONSTRAINT \"" <> c <> "\"" <> renderConflictAction action
        OnColumns cs ->
          let cols = T.intercalate ", " (map (\c -> "\"" <> c <> "\"") cs)
           in " ON CONFLICT (" <> cols <> ")" <> renderConflictAction action
   in insertPart <> conflictPart

renderConflictAction :: ConflictAction -> Text
renderConflictAction = \case
  DoNothing -> " DO NOTHING"
  DoUpdate [] -> " DO NOTHING"
  DoUpdate assignments ->
    " DO UPDATE SET " <> T.intercalate ", " (map renderAssignment assignments)

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
