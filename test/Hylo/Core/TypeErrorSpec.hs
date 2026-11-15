{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Avoid restricted function" #-}

{- |
Module      : Hylo.Core.TypeErrorSpec
Description : Specification tests for Epic 3: Closed Type Family Diagnostics (E001–E015)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.TypeErrorSpec
  ( spec
  ) where

import Control.DeepSeq (NFData (..))
import Data.Int (Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Hylo.Core.Context
  ( Col
  , ColDef (..)
  , Ctx (..)
  , PatchField (..)
  , colName
  , colSqlType
  )
import Hylo.Core.Expr (QExpr (..), just, (==.))
import Hylo.Core.Key (Id (..), toId)
import Hylo.Core.Query
  ( Changeset (..)
  , OpKind (..)
  , Postgres
  , Tx (..)
  , TxMode (..)
  , emptyChangeset
  , insert
  )
import Hylo.Core.Table
  ( Table (..)
  , TableSchema
  , colSpecExpr
  , column
  , eraseColDef
  , patchEntry
  , primaryKey
  , tableSchema
  )
import Hylo.Core.TypeError
  ( AssertAcyclic (..)
  , AssertBackendCapability (..)
  , AssertGrouped (..)
  , AssertHasPrimaryKey (..)
  , AssertHasVersion (..)
  , AssertMatchingVectors (..)
  , AssertNoIO (..)
  , AssertPGMinVersion (..)
  , AssertPaginationValid (..)
  , AssertParameterized (..)
  , AssertSafeMigration (..)
  , AssertValidForeignKey (..)
  , PGVersion (..)
  )
import Hylo.SQL.AST (renderExpr)
import Hylo.Test.ShouldNotTypecheck
import Test.Hspec

-- | Test Table A: Document entity
data DocumentT (ctx :: Ctx) = Document
  { id :: !(Col ctx (Id DocumentT Int64))
  , title :: !(Col ctx Text)
  }

deriving instance Eq (DocumentT 'Result)
deriving instance Show (DocumentT 'Result)

instance NFData (DocumentT 'Result) where
  rnf (Document i t) = rnf i `seq` rnf t

instance NFData (DocumentT 'Patch) where
  rnf (Document i t) = i `seq` t `seq` ()

instance Table DocumentT where
  toTableExpr alias (Document docId docTitle) =
    Document (colSpecExpr alias docId) (colSpecExpr alias docTitle)
  tableExprColumns (Document docId docTitle) =
    [renderExpr docId, renderExpr docTitle]
  tableColumnsMeta (Document docId docTitle) =
    [(colName docId, colSqlType docId), (colName docTitle, colSqlType docTitle)]
  tableColDefs (Document docId docTitle) =
    [eraseColDef docId, eraseColDef docTitle]
  patchEntries (Document docId docTitle) (Document pId pTitle) =
    patchEntry docId pId ++ patchEntry docTitle pTitle

-- | Test Table B: User entity
data UserT (ctx :: Ctx) = User
  { id :: !(Col ctx (Id UserT Int64))
  , email :: !(Col ctx Text)
  }

deriving instance Eq (UserT 'Result)
deriving instance Show (UserT 'Result)

instance NFData (UserT 'Result) where
  rnf (User i e) = rnf i `seq` rnf e

docTable :: TableSchema DocumentT
docTable =
  tableSchema "documents" $
    Document
      { id = primaryKey "id" "bigint"
      , title = column "title" "text"
      }

docChangeset :: Changeset DocumentT 'InsertOp
docChangeset =
  emptyChangeset $
    Document
      { id = SetTo (toId 1)
      , title = SetTo "Epic 3 Formal Specification"
      }

spec :: Spec
spec = describe "Epic 3: Closed Type Family Diagnostics" $ do
  describe "3.5 Executable Oracle Specification" $ do
    it "triggers [Hylo Error E001] on cross-table foreign key mismatch" $
      shouldNotTypecheckWith
        "[Hylo Error E001]"
        ( let docOrgId = undefined :: QExpr (Id DocumentT Int64)
              userOrgId = undefined :: QExpr (Id UserT Int64)
           in docOrgId ==. userOrgId
        )

    it "triggers [Hylo Error E002] on nullability mismatch without explicit just" $
      shouldNotTypecheckWith
        "[Hylo Error E002]"
        ( let maybeText = undefined :: QExpr (Maybe Text)
              rawText = undefined :: QExpr Text
           in maybeText ==. rawText
        )

    it "triggers [Hylo Error E005] when invoking insert in ReadOnly mode" $
      shouldNotTypecheckWith
        "[Hylo Error E005]"
        ( let readOnlyTx =
                insert docTable docChangeset ::
                  Tx Postgres 'ReadOnly (DocumentT 'Result)
           in readOnlyTx
        )

  describe "Additional Standardized Diagnostics (E001–E015)" $ do
    it "triggers [Hylo Error E003] on incompatible column types (Int64 vs Text)" $
      shouldNotTypecheckWith
        "[Hylo Error E003]"
        ( let numExpr = undefined :: QExpr Int64
              strExpr = undefined :: QExpr Text
           in numExpr ==. strExpr
        )

    it "permits valid equality comparison between identical phantom keys" $ do
      let k1 = ColExpr "users" "id" :: QExpr (Id UserT Int64)
          k2 = ColExpr "users" "id" :: QExpr (Id UserT Int64)
          cmp = k1 ==. k2
      cmp `shouldBe` BinOpExpr "=" k1 k2

    it "permits valid equality comparison between explicitly lifted nullable expressions" $ do
      let rawExpr = ColExpr "users" "email" :: QExpr Text
          maybeExpr = ColExpr "profiles" "email" :: QExpr (Maybe Text)
          cmp = maybeExpr ==. just rawExpr
      cmp `shouldBe` BinOpExpr "=" maybeExpr (just rawExpr)

    it "triggers [Hylo Error E004] on ungrouped column selection" $
      shouldNotTypecheckWith "[Hylo Error E004]" (groupedDict (Proxy @"name") (Proxy @'False))

    it "permits valid grouped column selection" $
      groupedDict (Proxy @"id") (Proxy @'True) `shouldBe` ()

    it "triggers [Hylo Error E006] on forbidden IO in transaction" $
      shouldNotTypecheckWith "[Hylo Error E006]" (noIODict (Proxy @'True))

    it "permits pure transaction without IO" $
      noIODict (Proxy @'False) `shouldBe` ()

    it "triggers [Hylo Error E007] on mismatched vector dimensions" $
      shouldNotTypecheckWith
        "[Hylo Error E007]"
        ( matchingVectorsDict
            (Proxy @"text-embedding-3")
            (Proxy @1536)
            (Proxy @"text-embedding-3")
            (Proxy @768)
        )

    it "permits matching vector embeddings" $
      matchingVectorsDict
        (Proxy @"text-embedding-3")
        (Proxy @1536)
        (Proxy @"text-embedding-3")
        (Proxy @1536)
        `shouldBe` ()

    it "triggers [Hylo Error E008] on backend capability mismatch" $
      shouldNotTypecheckWith
        "[Hylo Error E008]"
        (backendCapabilityDict (Proxy @"WindowFunctions") (Proxy @"Redis") (Proxy @'False))

    it "permits supported backend capability" $
      backendCapabilityDict (Proxy @"WindowFunctions") (Proxy @"Postgres") (Proxy @'True) `shouldBe` ()

    it "triggers [Hylo Error E009] on missing optimistic lock version" $
      shouldNotTypecheckWith "[Hylo Error E009]" (hasVersionDict (Proxy @"documents") (Proxy @'False))

    it "permits table with version column" $
      hasVersionDict (Proxy @"documents") (Proxy @'True) `shouldBe` ()

    it "triggers [Hylo Error E010] on unsafe raw SQL interpolation" $
      shouldNotTypecheckWith "[Hylo Error E010]" (parameterizedDict (Proxy @'False))

    it "permits parameterized query" $
      parameterizedDict (Proxy @'True) `shouldBe` ()

    it "triggers [Hylo Error E011] on missing primary key specification" $
      shouldNotTypecheckWith "[Hylo Error E011]" (hasPrimaryKeyDict (Proxy @"audit_logs") (Proxy @'False))

    it "permits table with primary key" $
      hasPrimaryKeyDict (Proxy @"users") (Proxy @'True) `shouldBe` ()

    it "triggers [Hylo Error E012] on invalid foreign key reference" $
      shouldNotTypecheckWith
        "[Hylo Error E012]"
        (validForeignKeyDict (Proxy @"author_id") (Proxy @"users") (Proxy @"email") (Proxy @'False))

    it "permits valid foreign key targeting primary key" $
      validForeignKeyDict (Proxy @"author_id") (Proxy @"users") (Proxy @"id") (Proxy @'True) `shouldBe` ()

    it "triggers [Hylo Error E013] on circular graph traversal" $
      shouldNotTypecheckWith "[Hylo Error E013]" (acyclicDict (Proxy @"parent_folder") (Proxy @'True))

    it "permits acyclic graph relationship" $
      acyclicDict (Proxy @"parent_folder") (Proxy @'False) `shouldBe` ()

    it "triggers [Hylo Error E014] on illegal pagination on correlated subquery" $
      shouldNotTypecheckWith "[Hylo Error E014]" (paginationValidDict (Proxy @'False))

    it "permits valid pagination with ordering" $
      paginationValidDict (Proxy @'True) `shouldBe` ()

    it "triggers [Hylo Error E015] on unchecked destructive migration" $
      shouldNotTypecheckWith "[Hylo Error E015]" (safeMigrationDict (Proxy @'True) (Proxy @'False))

    it "permits non-destructive migration" $
      safeMigrationDict (Proxy @'False) (Proxy @'False) `shouldBe` ()

    it "permits destructive migration when force allowed" $
      safeMigrationDict (Proxy @'True) (Proxy @'True) `shouldBe` ()

    it "triggers [Hylo Error E016] on PostgreSQL version mismatch" $
      shouldNotTypecheckWith
        "[Hylo Error E016]"
        (pgMinVersionDict (Proxy @'PG16) (Proxy @'PG18) (Proxy @"server-side uuidv7()"))

    it "permits valid PostgreSQL version requirement" $
      pgMinVersionDict (Proxy @'PG18) (Proxy @'PG18) (Proxy @"server-side uuidv7()") `shouldBe` ()
