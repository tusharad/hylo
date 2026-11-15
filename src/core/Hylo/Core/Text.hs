{- |
Module      : Hylo.Core.Text
Description : Text and ByteString types and utilities re-exported for core backends
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
-}
module Hylo.Core.Text
  ( ByteString
  , Text
  , decodeUtf8Lenient
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- | Leniently decode a UTF-8 ByteString into Text, replacing invalid sequences with '?'.
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TE.decodeUtf8With (\_ _ -> Just '?')
