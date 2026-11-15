{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Hylo.Test.ShouldNotTypecheck
Description : Test helpers for asserting custom TypeErrors with deferred type errors
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Test.ShouldNotTypecheck
  ( shouldNotTypecheck
  , shouldNotTypecheckWith
  ) where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (SomeException, catch, evaluate)
import Data.List (isInfixOf)
import Hylo.Core.Error (FieldError (..))
import Hylo.Core.Key (Id (..))
import Hylo.Core.Transaction (Tx (..), TxError (..), TxOp (..), TxState (..))
import Hylo.Core.TypeError (PGVersion)
import Test.Hspec (Expectation, expectationFailure)
import qualified "should-not-typecheck" Test.ShouldNotTypecheck as SNT

instance NFData PGVersion
instance NFData FieldError

instance NFData a => NFData (Id table a) where
  rnf (Id a) = rnf a

instance NFData TxError
instance NFData TxOp

instance NFData TxState where
  rnf (TxState s ops store a ch rh cs) =
    rnf (s, ops, store, a, length ch, length rh, length cs)

instance NFData a => NFData (Tx db mode a) where
  rnf (TxReturn a) = rnf a
  rnf TxEmpty = ()
  rnf (TxFail err) = rnf err
  rnf (TxAction _) = ()

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
