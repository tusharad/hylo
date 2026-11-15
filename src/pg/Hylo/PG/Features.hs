{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{- |
Module      : Hylo.PG.Features
Description : PostgreSQL version-gated SQL statements and features (PG14–PG18)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

PostgreSQL version-gated statements and expressions guarded by compile-time
'AssertPGMinVersion' type constraints ([Hylo Error E016]).
-}
module Hylo.PG.Features
  ( -- * Version-Gated Statements & Expressions
    uniqueNullsNotDistinct
  , uniqueNullsNotDistinctConstraint
  , mergeInto
  , mergeReturning
  , returningWithOldNew
  , uuidv7
  , uuidv7Tx

    -- * Dictionary Witnesses
  , pg15UniqueNullsNotDistinctDict
  , pg15MergeIntoDict
  , pg17MergeReturningDict
  , pg18ReturningWithOldNewDict
  , pg18UuidV7Dict
  ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.UUID (UUID)
import Hylo.Backend.Capability (Postgres)
import Hylo.Core.Changeset (Changeset (..), ConstraintDeclaration (..))
import Hylo.Core.Expr (QExpr (..))
import Hylo.Core.Transaction (Tx (..), recordMutation)
import Hylo.Core.TypeError
  ( AssertPGMinVersion (..)
  , AssertWritable (..)
  , PGVersion (..)
  , TxMode (..)
  , pgMinVersionDict
  , writableDict
  )

-- | UNIQUE NULLS NOT DISTINCT constraint (requires PostgreSQL 15+).
uniqueNullsNotDistinct ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG15 "UNIQUE NULLS NOT DISTINCT") =>
  Text ->
  Text
uniqueNullsNotDistinct col =
  pgMinVersionDict (Proxy @v) (Proxy @'PG15) (Proxy @"UNIQUE NULLS NOT DISTINCT") `seq`
    ("UNIQUE NULLS NOT DISTINCT (" <> col <> ")")

-- | Declare a UNIQUE NULLS NOT DISTINCT constraint in a Changeset (requires PostgreSQL 15+).
uniqueNullsNotDistinctConstraint ::
  forall (v :: PGVersion) table op.
  (AssertPGMinVersion v 'PG15 "UNIQUE NULLS NOT DISTINCT") =>
  Text ->
  Changeset table op ->
  Changeset table op
uniqueNullsNotDistinctConstraint col cs =
  pgMinVersionDict (Proxy @v) (Proxy @'PG15) (Proxy @"UNIQUE NULLS NOT DISTINCT") `seq`
    let decl = UniqueConstraint col Nothing "must be unique (nulls not distinct)"
     in cs {changesetConstraints = changesetConstraints cs ++ [decl]}

-- | MERGE INTO statement (requires PostgreSQL 15+).
mergeInto ::
  forall (v :: PGVersion) (mode :: TxMode).
  (AssertWritable mode, AssertPGMinVersion v 'PG15 "MERGE INTO") =>
  Text ->
  Tx (Postgres v) mode ()
mergeInto rawSql =
  case writableDict (Proxy :: Proxy mode) of
    () -> case pgMinVersionDict (Proxy @v) (Proxy @'PG15) (Proxy @"MERGE INTO") of
      () -> recordMutation ("MERGE INTO " <> rawSql)

-- | MERGE ... RETURNING statement (requires PostgreSQL 17+).
mergeReturning ::
  forall (v :: PGVersion) (mode :: TxMode).
  (AssertWritable mode, AssertPGMinVersion v 'PG17 "MERGE ... RETURNING") =>
  Text ->
  Tx (Postgres v) mode ()
mergeReturning rawSql =
  case writableDict (Proxy :: Proxy mode) of
    () -> case pgMinVersionDict (Proxy @v) (Proxy @'PG17) (Proxy @"MERGE ... RETURNING") of
      () -> recordMutation ("MERGE ... RETURNING " <> rawSql)

-- | RETURNING WITH (OLD AS old, NEW AS new) clause (requires PostgreSQL 18+).
returningWithOldNew ::
  forall (v :: PGVersion) (mode :: TxMode).
  (AssertWritable mode, AssertPGMinVersion v 'PG18 "RETURNING WITH (OLD AS old, NEW AS new)") =>
  Text ->
  Tx (Postgres v) mode ()
returningWithOldNew rawSql =
  case writableDict (Proxy :: Proxy mode) of
    () -> case pgMinVersionDict (Proxy @v) (Proxy @'PG18) (Proxy @"RETURNING WITH (OLD AS old, NEW AS new)") of
      () -> recordMutation ("RETURNING WITH (OLD AS old, NEW AS new) " <> rawSql)

-- | Server-side uuidv7() SQL expression (requires PostgreSQL 18+).
uuidv7 ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG18 "server-side uuidv7()") =>
  QExpr UUID
uuidv7 =
  pgMinVersionDict (Proxy @v) (Proxy @'PG18) (Proxy @"server-side uuidv7()") `seq`
    LitExpr "uuidv7()"

-- | Server-side uuidv7() within a Tx computation (requires PostgreSQL 18+).
uuidv7Tx ::
  forall (v :: PGVersion) (mode :: TxMode).
  (AssertPGMinVersion v 'PG18 "server-side uuidv7()") =>
  Tx (Postgres v) mode UUID
uuidv7Tx =
  pgMinVersionDict (Proxy @v) (Proxy @'PG18) (Proxy @"server-side uuidv7()") `seq`
    TxAction (Right (errorWithoutStackTrace "uuidv7: evaluated offline"),)

-- | Capability dictionary witness for PG15 UNIQUE NULLS NOT DISTINCT.
pg15UniqueNullsNotDistinctDict ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG15 "UNIQUE NULLS NOT DISTINCT") =>
  Proxy (v :: PGVersion) ->
  ()
pg15UniqueNullsNotDistinctDict p =
  pgMinVersionDict p (Proxy @'PG15) (Proxy @"UNIQUE NULLS NOT DISTINCT")

-- | Capability dictionary witness for PG15 MERGE INTO.
pg15MergeIntoDict ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG15 "MERGE INTO") =>
  Proxy (v :: PGVersion) ->
  ()
pg15MergeIntoDict p =
  pgMinVersionDict p (Proxy @'PG15) (Proxy @"MERGE INTO")

-- | Capability dictionary witness for PG17 MERGE ... RETURNING.
pg17MergeReturningDict ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG17 "MERGE ... RETURNING") =>
  Proxy (v :: PGVersion) ->
  ()
pg17MergeReturningDict p =
  pgMinVersionDict p (Proxy @'PG17) (Proxy @"MERGE ... RETURNING")

-- | Capability dictionary witness for PG18 RETURNING WITH.
pg18ReturningWithOldNewDict ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG18 "RETURNING WITH (OLD AS old, NEW AS new)") =>
  Proxy (v :: PGVersion) ->
  ()
pg18ReturningWithOldNewDict p =
  pgMinVersionDict p (Proxy @'PG18) (Proxy @"RETURNING WITH (OLD AS old, NEW AS new)")

-- | Capability dictionary witness for PG18 uuidv7().
pg18UuidV7Dict ::
  forall (v :: PGVersion).
  (AssertPGMinVersion v 'PG18 "server-side uuidv7()") =>
  Proxy (v :: PGVersion) ->
  ()
pg18UuidV7Dict p =
  pgMinVersionDict p (Proxy @'PG18) (Proxy @"server-side uuidv7()")
