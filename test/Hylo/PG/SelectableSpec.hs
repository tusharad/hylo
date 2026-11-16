{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Hylo.PG.SelectableSpec
Description : Test suite for User Story 1: End-to-End Selectable Hydration & Positional Array Decoding
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.PG.SelectableSpec (spec) where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GHC.Float (castDoubleToWord64)
import GHC.Generics (Generic)
import Hylo.Core
  ( Col
  , Ctx (..)
  , Id (..)
  , NullableTable (..)
  , Table (..)
  , TableSchema (..)
  , colSpecExpr
  , column
  , eraseColDef
  , lit
  , primaryKey
  , tableSchema
  , toId
  )
import Hylo.Core.Codec
  ( Codec (..)
  , field
  , runBinaryDecoder
  )
import Hylo.PG.Codec.Binary
  ( Selectable (..)
  , TableCodec (..)
  , boolCodec
  , doubleCodec
  , idCodec
  , int64Codec
  , jsonField
  , runPositional
  , textCodec
  )
import Hylo.PG.Compiler.Lateral (NestedList (..))
import Hylo.PG.Connection (assembleRowBytes)
import Hylo.SQL.AST (Projectable (..), renderExpr)
import Test.Hspec

--------------------------------------------------------------------------------
-- Test Table Definitions
--------------------------------------------------------------------------------

data UserT (ctx :: Ctx) = User
  { id :: !(Col ctx (Id UserT Int64))
  , name :: !(Col ctx Text)
  , active :: !(Col ctx Bool)
  }
  deriving (Generic)

type User = UserT 'Result

userSchema :: TableSchema UserT
userSchema =
  tableSchema "users" $
    User
      { id = primaryKey "id" "bigint"
      , name = column "name" "text"
      , active = column "active" "boolean"
      }

instance Table UserT where
  toTableExpr alias m =
    User
      { id = colSpecExpr alias m.id
      , name = colSpecExpr alias m.name
      , active = colSpecExpr alias m.active
      }
  tableExprColumns u =
    [ renderExpr u.id
    , renderExpr u.name
    , renderExpr u.active
    ]
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.name
    , eraseColDef m.active
    ]

instance Projectable (UserT 'Expr) where
  projectColumns = tableExprColumns

instance TableCodec UserT where
  tableCodec =
    User
      <$> field (.id) (idCodec int64Codec)
      <*> field (.name) textCodec
      <*> field (.active) boolCodec
  tableJsonDecoder =
    User
      <$> jsonField
      <*> jsonField
      <*> jsonField

data ProfileT (ctx :: Ctx) = Profile
  { id :: !(Col ctx (Id ProfileT Int64))
  , bio :: !(Col ctx Text)
  , rating :: !(Col ctx Double)
  }
  deriving (Generic)

type Profile = ProfileT 'Result

profileSchema :: TableSchema ProfileT
profileSchema =
  tableSchema "profiles" $
    Profile
      { id = primaryKey "id" "bigint"
      , bio = column "bio" "text"
      , rating = column "rating" "double precision"
      }

instance Table ProfileT where
  toTableExpr alias m =
    Profile
      { id = colSpecExpr alias m.id
      , bio = colSpecExpr alias m.bio
      , rating = colSpecExpr alias m.rating
      }
  tableExprColumns p =
    [ renderExpr p.id
    , renderExpr p.bio
    , renderExpr p.rating
    ]
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.bio
    , eraseColDef m.rating
    ]

instance Projectable (ProfileT 'Expr) where
  projectColumns = tableExprColumns

instance TableCodec ProfileT where
  tableCodec =
    Profile
      <$> field (.id) (idCodec int64Codec)
      <*> field (.bio) textCodec
      <*> field (.rating) doubleCodec
  tableJsonDecoder =
    Profile
      <$> jsonField
      <*> jsonField
      <*> jsonField

data DocumentT (ctx :: Ctx) = Document
  { id :: !(Col ctx (Id DocumentT Int64))
  , title :: !(Col ctx Text)
  }
  deriving (Generic)

type Document = DocumentT 'Result

docSchema :: TableSchema DocumentT
docSchema =
  tableSchema "documents" $
    Document
      { id = primaryKey "id" "bigint"
      , title = column "title" "text"
      }

instance Table DocumentT where
  toTableExpr alias m =
    Document
      { id = colSpecExpr alias m.id
      , title = colSpecExpr alias m.title
      }
  tableExprColumns d =
    [ renderExpr d.id
    , renderExpr d.title
    ]
  tableColDefs m =
    [ eraseColDef m.id
    , eraseColDef m.title
    ]

instance Projectable (DocumentT 'Expr) where
  projectColumns = tableExprColumns

instance TableCodec DocumentT where
  tableCodec =
    Document
      <$> field (.id) (idCodec int64Codec)
      <*> field (.title) textCodec
  tableJsonDecoder =
    Document
      <$> jsonField
      <*> jsonField

deriving instance Eq (UserT 'Result)
deriving instance Show (UserT 'Result)
deriving instance Eq (ProfileT 'Result)
deriving instance Show (ProfileT 'Result)
deriving instance Eq (DocumentT 'Result)
deriving instance Show (DocumentT 'Result)

--------------------------------------------------------------------------------
-- Helpers & Test Fixtures
--------------------------------------------------------------------------------

decodeSelectable :: Selectable expr res => expr -> ByteString -> Either Text res
decodeSelectable expr = runBinaryDecoder (Codec (const mempty) (selectableDecoder expr))

rawInt64 :: Int64 -> ByteString
rawInt64 n = BSL.toStrict (Builder.toLazyByteString (Builder.int64BE n))

rawText :: Text -> ByteString
rawText = TE.encodeUtf8

rawBool :: Bool -> ByteString
rawBool b = BS.singleton (if b then 1 else 0)

rawDouble :: Double -> ByteString
rawDouble d = BSL.toStrict (Builder.toLazyByteString (Builder.word64BE (castDoubleToWord64 d)))

rawJsonb :: Aeson.Value -> ByteString
rawJsonb val = BS.singleton 1 <> BSL.toStrict (Aeson.encode val)

aliceUser :: User
aliceUser = User (toId 42) "Alice" True

aliceUser1 :: User
aliceUser1 = User (toId 1) "Alice" True

bobUser :: User
bobUser = User (toId 1) "Bob" True

charlieUser :: User
charlieUser = User (toId 2) "Charlie" False

aliceProfile :: Profile
aliceProfile = Profile (toId 10) "Engineer & Hacker" 4.95

principalProfile :: Profile
principalProfile = Profile (toId 99) "Principal Architect" 5.0

bioProfile :: Profile
bioProfile = Profile (toId 10) "Bio text" 4.95

doc1 :: Document
doc1 = Document (toId 101) "Architecture Spec"

doc2 :: Document
doc2 = Document (toId 102) "API Documentation"

simpleDoc1 :: Document
simpleDoc1 = Document (toId 1) "Doc 1"

--------------------------------------------------------------------------------
-- Test Specifications
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "User Story 1: Selectable Hydration & Positional Array Decoding" $ do
  let userExpr = toTableExpr "u" (tableMeta userSchema)
      profileExpr = toTableExpr "p" (tableMeta profileSchema)
      docExpr = toTableExpr "d" (tableMeta docSchema)

  describe "Table Entity Decoding via Selectable" $ do
    it "decodes a hydrated domain record from PostgreSQL binary wire column bytes" $ do
      let rawBytes =
            assembleRowBytes
              [ Just (rawInt64 42)
              , Just (rawText "Alice")
              , Just (rawBool True)
              ]
          decoded = decodeSelectable userExpr rawBytes
      decoded `shouldBe` Right aliceUser

    it "decodes table entity from positional JSON values" $ do
      let vals = [Aeson.Number 42, Aeson.String "Alice", Aeson.Bool True]
          decoded = runPositional (selectablePositionalDecoder userExpr) vals
      decoded `shouldBe` Right aliceUser

  describe "NullableTable Decoding (LEFT JOIN Semantics)" $ do
    it "decodes to Just table when primary key is non-NULL" $ do
      let rawBytes =
            assembleRowBytes
              [ Just (rawInt64 10)
              , Just (rawText "Engineer & Hacker")
              , Just (rawDouble 4.95)
              ]
          decoded = decodeSelectable (NullableTable profileExpr) rawBytes
      decoded `shouldBe` Right (Just aliceProfile)

    it "decodes to Nothing and safely skips table columns when primary key is NULL (-1)" $ do
      let rawBytes =
            assembleRowBytes
              [ Nothing
              , Nothing
              , Nothing
              ]
          decoded = decodeSelectable (NullableTable profileExpr) rawBytes
      decoded `shouldBe` Right Nothing

    it "decodes positional NullableTable to Nothing when first value is Null" $ do
      let vals = [Aeson.Null, Aeson.Null, Aeson.Null]
          decoded = runPositional (selectablePositionalDecoder (NullableTable profileExpr)) vals
      decoded `shouldBe` Right Nothing

    it "decodes positional NullableTable to Just when first value is present" $ do
      let vals = [Aeson.Number 10, Aeson.String "Bio text", Aeson.Number 4.95]
          decoded = runPositional (selectablePositionalDecoder (NullableTable profileExpr)) vals
      decoded `shouldBe` Right (Just bioProfile)

  describe "NestedList Decoding (Positional JSON Array Semantics)" $ do
    let nestedDocs = NestedList "docs" docExpr

    it "decodes empty jsonb array '[]' into empty list []" $ do
      let rawBytes = assembleRowBytes [Just (rawJsonb (Aeson.Array V.empty))]
          decoded = decodeSelectable nestedDocs rawBytes
      decoded `shouldBe` Right []

    it "decodes positional arrays into hydrated child entities without repeating keys" $ do
      let childRows =
            Aeson.Array $
              V.fromList
                [ Aeson.Array (V.fromList [Aeson.Number 101, Aeson.String "Architecture Spec"])
                , Aeson.Array (V.fromList [Aeson.Number 102, Aeson.String "API Documentation"])
                ]
          rawBytes = assembleRowBytes [Just (rawJsonb childRows)]
          decoded = decodeSelectable nestedDocs rawBytes
      decoded
        `shouldBe` Right
          [ doc1
          , doc2
          ]

    it "decodes positional NestedList with empty array into []" $ do
      let vals = [Aeson.Array V.empty]
          decoded = runPositional (selectablePositionalDecoder nestedDocs) vals
      decoded `shouldBe` Right []

  describe "Scalar QExpr Decoding" $ do
    it "decodes scalar QExpr wire value via HasValueCodec" $ do
      let scalarExpr = lit ("custom_tag" :: Text)
          rawBytes = assembleRowBytes [Just (rawText "custom_tag")]
          decoded = decodeSelectable scalarExpr rawBytes
      decoded `shouldBe` Right ("custom_tag" :: Text)

  describe "Composite Tuple Decoding" $ do
    let nestedDocs = NestedList "docs" docExpr

    it "hydrates 2-tuple (User, Maybe Profile)" $ do
      let tupleExpr = (userExpr, NullableTable profileExpr)
          rawBytes =
            assembleRowBytes
              [ Just (rawInt64 1)
              , Just (rawText "Bob")
              , Just (rawBool True)
              , Nothing
              , Nothing
              , Nothing
              ]
          decoded = decodeSelectable tupleExpr rawBytes
      decoded `shouldBe` Right (bobUser, Nothing)

    it
      "hydrates 3-tuple (User, Maybe Profile, [Document]) with matching profile and non-empty documents"
      $ do
        let tupleExpr = (userExpr, NullableTable profileExpr, nestedDocs)
            childRows =
              Aeson.Array $
                V.fromList
                  [ Aeson.Array (V.fromList [Aeson.Number 1, Aeson.String "Doc 1"])
                  ]
            rawBytes =
              assembleRowBytes
                [ Just (rawInt64 1)
                , Just (rawText "Alice")
                , Just (rawBool True)
                , Just (rawInt64 99)
                , Just (rawText "Principal Architect")
                , Just (rawDouble 5.0)
                , Just (rawJsonb childRows)
                ]
            decoded = decodeSelectable tupleExpr rawBytes
        decoded
          `shouldBe` Right
            ( aliceUser1
            , Just principalProfile
            , [simpleDoc1]
            )

    it
      "hydrates 4-tuple (User, Maybe Profile, [Document], Text) with missing profile and empty documents"
      $ do
        let tupleExpr = (userExpr, NullableTable profileExpr, nestedDocs, lit ("admin" :: Text))
            rawBytes =
              assembleRowBytes
                [ Just (rawInt64 2)
                , Just (rawText "Charlie")
                , Just (rawBool False)
                , Nothing
                , Nothing
                , Nothing
                , Just (rawJsonb (Aeson.Array V.empty))
                , Just (rawText "admin")
                ]
            decoded = decodeSelectable tupleExpr rawBytes
        decoded
          `shouldBe` Right
            ( charlieUser
            , Nothing
            , []
            , "admin" :: Text
            )
