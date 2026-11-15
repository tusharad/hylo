{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Hylo.Core.Error
Description : Core FieldError and validation failure representations
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.Error
  ( FieldError (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Structured field error representing a validation or database constraint failure.
data FieldError = FieldError
  { fieldPath :: !Text
  , errorCode :: !Text
  , message :: !Text
  }
  deriving (Eq, Show, Generic)
