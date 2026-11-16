{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Hylo.Test.Properties
Description : Hedgehog SQL Equivalence, Round-Trip, and Invariance Property Tests
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Test.Properties
  ( spec
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as H
import Hylo.Core
  ( PatchField (..)
  , changesetPatch
  , diffRecord
  , dirtyFields
  , toFieldValue
  )
import Hylo.Migrate.Diff (applyDiff, computeCatalogDiff, invertDiff)
import Hylo.PG.Compiler.SQL (compilePostgresQuery)
import Hylo.PG.Search.RRF
  ( computeRRF2
  , isBoundedRRF
  , isMonotonicRRF
  , isSymmetricRRF
  )
import Hylo.SQL.Builder
  ( CompiledQuery (..)
  , Dialect (..)
  , compileQuery
  )
import Hylo.Test.Gen
  ( AccountProfileT (..)
  , accountProfileSchema
  , genAccountProfile
  , genCatalog
  , genExprQueryAST
  , genQueryAST
  , genRRFK
  , genRRFRankPair
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

-- | Complete Hedgehog Property Testing Specification for Hylo.
spec :: Spec
spec = describe "User Story 3: Hedgehog SQL Equivalence Property Testing Suite" $ do
  -- Property 1: Query Rendering Determinism
  describe "Property 1 (Query Rendering Determinism)" $ do
    it
      "compiles identical Table Query AST multiple times with identical SQL and parameters (PostgreSQL)"
      $ hedgehog
      $ do
        q <- H.forAllWith (T.unpack . compiledSQL . compilePostgresQuery) genQueryAST
        let cq1 = compilePostgresQuery q
            cq2 = compilePostgresQuery q
        cq1.compiledSQL === cq2.compiledSQL
        cq1.compiledParams === cq2.compiledParams

    it
      "compiles identical Expression Query AST multiple times with identical SQL and parameters (PostgreSQL)"
      $ hedgehog
      $ do
        q <- H.forAllWith (T.unpack . compiledSQL . compilePostgresQuery) genExprQueryAST
        let cq1 = compilePostgresQuery q
            cq2 = compilePostgresQuery q
        cq1.compiledSQL === cq2.compiledSQL
        cq1.compiledParams === cq2.compiledParams

    it "compiles identical Table Query AST multiple times with identical SQL and parameters (SQLite)" $
      hedgehog $ do
        q <- H.forAllWith (T.unpack . compiledSQL . compileQuery SQLiteDialect) genQueryAST
        let cq1 = compileQuery SQLiteDialect q
            cq2 = compileQuery SQLiteDialect q
        cq1.compiledSQL === cq2.compiledSQL
        cq1.compiledParams === cq2.compiledParams

    it "compiles identical Table Query AST multiple times with identical SQL and parameters (MySQL)" $
      hedgehog $ do
        q <- H.forAllWith (T.unpack . compiledSQL . compileQuery MySQLDialect) genQueryAST
        let cq1 = compileQuery MySQLDialect q
            cq2 = compileQuery MySQLDialect q
        cq1.compiledSQL === cq2.compiledSQL
        cq1.compiledParams === cq2.compiledParams

  -- Property 2: Changeset Dirty Field Invariance
  describe "Property 2 (Changeset Dirty Field Invariance)" $ do
    it "diffing identical records produces zero dirty fields and all Unchanged patch fields" $
      hedgehog $ do
        r <- H.forAll genAccountProfile
        let cs = diffRecord accountProfileSchema r r
            dirty = dirtyFields cs
            patch = changesetPatch cs
        Map.size dirty === 0
        patch.id === Unchanged
        patch.username === Unchanged
        patch.email === Unchanged
        patch.age === Unchanged
        patch.bio === Unchanged
        patch.country === Unchanged
        patch.phone === Unchanged
        patch.role === Unchanged
        patch.score === Unchanged
        patch.active === Unchanged

    it "diffing two generated records correctly marks only modified columns as SetTo" $
      hedgehog $ do
        old <- H.forAll genAccountProfile
        new <- H.forAll genAccountProfile
        let cs = diffRecord accountProfileSchema old new
            dirty = dirtyFields cs
            patch = changesetPatch cs

        -- Check id
        if old.id == new.id
          then do
            patch.id === Unchanged
            H.assert (not (Map.member "id" dirty))
          else do
            patch.id === SetTo new.id
            Map.lookup "id" dirty === Just (toFieldValue new.id)

        -- Check username
        if old.username == new.username
          then do
            patch.username === Unchanged
            H.assert (not (Map.member "username" dirty))
          else do
            patch.username === SetTo new.username
            Map.lookup "username" dirty === Just new.username

        -- Check email
        if old.email == new.email
          then do
            patch.email === Unchanged
            H.assert (not (Map.member "email" dirty))
          else do
            patch.email === SetTo new.email
            Map.lookup "email" dirty === Just new.email

        -- Check age
        if old.age == new.age
          then do
            patch.age === Unchanged
            H.assert (not (Map.member "age" dirty))
          else do
            patch.age === SetTo new.age
            Map.lookup "age" dirty === Just (toFieldValue new.age)

        -- Check bio
        if old.bio == new.bio
          then do
            patch.bio === Unchanged
            H.assert (not (Map.member "bio" dirty))
          else do
            patch.bio === SetTo new.bio
            Map.lookup "bio" dirty === Just new.bio

        -- Check country
        if old.country == new.country
          then do
            patch.country === Unchanged
            H.assert (not (Map.member "country" dirty))
          else do
            patch.country === SetTo new.country
            Map.lookup "country" dirty === Just new.country

        -- Check phone
        if old.phone == new.phone
          then do
            patch.phone === Unchanged
            H.assert (not (Map.member "phone" dirty))
          else do
            patch.phone === SetTo new.phone
            Map.lookup "phone" dirty === Just new.phone

        -- Check role
        if old.role == new.role
          then do
            patch.role === Unchanged
            H.assert (not (Map.member "role" dirty))
          else do
            patch.role === SetTo new.role
            Map.lookup "role" dirty === Just new.role

        -- Check score
        if old.score == new.score
          then do
            patch.score === Unchanged
            H.assert (not (Map.member "score" dirty))
          else do
            patch.score === SetTo new.score
            Map.lookup "score" dirty === Just (toFieldValue new.score)

        -- Check active
        if old.active == new.active
          then do
            patch.active === Unchanged
            H.assert (not (Map.member "active" dirty))
          else do
            patch.active === SetTo new.active
            Map.lookup "active" dirty === Just (toFieldValue new.active)

  -- Property 3: Migration Group Action Inversion
  describe "Property 3 (Migration Group Action Inversion)" $ do
    it "satisfies Law 1 group action: applyDiff (invertDiff diff) (applyDiff diff S1) == S1" $
      hedgehog $ do
        s1 <- H.forAll genCatalog
        s2 <- H.forAll genCatalog
        let diff = computeCatalogDiff s1 s2
            s2Applied = applyDiff diff s1
            s1Restored = applyDiff (invertDiff diff) s2Applied
        s1Restored === s1

    it
      "satisfies diff inversion symmetry: invertDiff (computeCatalogDiff s1 s2) == computeCatalogDiff s2 s1"
      $ hedgehog
      $ do
        s1 <- H.forAll genCatalog
        s2 <- H.forAll genCatalog
        let d1 = computeCatalogDiff s1 s2
            d2 = computeCatalogDiff s2 s1
        invertDiff d1 === d2

  -- Property 4: RRF Math Range Bound
  describe "Property 4 (RRF Math Range Bound)" $ do
    it "strictly honors 0 < RRF <= 2 / (k + 1) for any rank pair and smoothing parameter" $
      hedgehog $ do
        k <- H.forAll genRRFK
        (r1, r2) <- H.forAll genRRFRankPair
        let score = computeRRF2 k r1 r2
            upper = 2.0 / (k + 1.0)
        H.assert (score > 0.0)
        H.assert (score <= upper + 1e-9)
        H.assert (isBoundedRRF k r1 r2)

    it "satisfies RRF symmetry: computeRRF2 k r1 r2 == computeRRF2 k r2 r1" $
      hedgehog $ do
        k <- H.forAll genRRFK
        (r1, r2) <- H.forAll genRRFRankPair
        computeRRF2 k r1 r2 === computeRRF2 k r2 r1
        H.assert (isSymmetricRRF k r1 r2)

    it "satisfies RRF monotonicity: better or equal ranks yield equal or higher RRF scores" $
      hedgehog $ do
        k <- H.forAll genRRFK
        (r1a, r1b) <- H.forAll genRRFRankPair
        (r2a, r2b) <- H.forAll genRRFRankPair
        H.assert (isMonotonicRRF k (r1a, r1b) (r2a, r2b))
