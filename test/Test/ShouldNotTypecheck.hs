{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Test.ShouldNotTypecheck
Description : Test helpers for asserting custom TypeErrors with deferred type errors
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Test.ShouldNotTypecheck
  ( shouldNotTypecheck
  , shouldNotTypecheckWith
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, catch, evaluate)
import Data.List (isInfixOf)
import Test.Hspec (Expectation, expectationFailure)
import qualified "should-not-typecheck" Test.ShouldNotTypecheck as SNT

-- | Standard shouldNotTypecheck from Hackage.
shouldNotTypecheck :: NFData a => (() ~ () => a) -> Expectation
shouldNotTypecheck = SNT.shouldNotTypecheck

{- | Evaluates an expression compiled with deferred type errors,
and asserts that the resulting compiler error contains the expected substring.
-}
shouldNotTypecheckWith :: NFData a => String -> (() ~ () => a) -> Expectation
shouldNotTypecheckWith expectedSubstr expr = do
  res <-
    catch
      (evaluate (force expr) >> pure (Right ()))
      (\(e :: SomeException) -> pure (Left (show e)))
  case res of
    Right () ->
      expectationFailure "Expected expression to fail typechecking, but it succeeded."
    Left msg ->
      if expectedSubstr `isInfixOf` msg
        then pure ()
        else
          expectationFailure $
            "Expected type error to contain substring:\n  "
              ++ show expectedSubstr
              ++ "\nbut caught compiler diagnostic was:\n  "
              ++ msg
