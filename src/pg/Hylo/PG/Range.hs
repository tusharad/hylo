{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.PG.Range
Description : PostgreSQL Range types, binary wire codec, and algebraic combinators
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides first-class representation of PostgreSQL range types ('int4range', 'numrange',
'tsrange', 'tstzrange', 'daterange', 'int8range'), lossless binary wire encoding/decoding
via 'rangeCodec', and type-safe query combinators (@>, &&, -|-).
-}
module Hylo.PG.Range
  ( -- * Range Data Types
    RangeBound (..)
  , Range (..)
  , range
  , emptyRange
  , inclusive
  , exclusive
  , unbounded

    -- * Binary Wire Codec
  , rangeCodec
  , encodeRangePayload
  , decodeRangePayload

    -- * Range Literals & Formatting
  , renderRangeText
  , litRange
  , litRangeWith

    -- * Range Operators
  , rangeContains
  , rangeContainsElem
  , rangeContainedBy
  , rangeOverlaps
  , rangeAdjacent
  , rangeLeftOf
  , rangeRightOf

    -- * Symbolic Infix Operators
  , (@>:)
  , (<@:)
  , (&&:)
  , (-|-)
  , (-|-.)
  , (<<:)
  , (>>:)
  ) where

import Control.DeepSeq (NFData)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Hylo.Core.Codec
  ( Codec (..)
  , Decoder (..)
  , ValueCodec
  , makeFramedValueCodec
  )
import Hylo.Core.Expr (QExpr (..), litTyped)

-- | Boundary of a range specification.
data RangeBound a
  = Inclusive !a
  | Exclusive !a
  | Unbounded
  deriving (Eq, Show, Functor, Generic, NFData)

-- | PostgreSQL range representation with inclusive, exclusive, or unbounded endpoints.
data Range a
  = Range !(RangeBound a) !(RangeBound a)
  | EmptyRange
  deriving (Eq, Show, Functor, Generic, NFData)

-- | Construct a bounded or unbounded Range.
range :: RangeBound a -> RangeBound a -> Range a
range = Range

-- | Construct an empty range (SQL @'empty'@).
emptyRange :: Range a
emptyRange = EmptyRange

-- | Construct an inclusive boundary.
inclusive :: a -> RangeBound a
inclusive = Inclusive

-- | Construct an exclusive boundary.
exclusive :: a -> RangeBound a
exclusive = Exclusive

-- | Construct an unbounded boundary.
unbounded :: RangeBound a
unbounded = Unbounded

--------------------------------------------------------------------------------
-- Binary Wire Codec
--------------------------------------------------------------------------------

{- | Encode a Range value into PostgreSQL binary wire format:
1 flag byte (0x01 Empty, 0x02 LB inclusive, 0x04 UB inclusive, 0x08 LB unbounded, 0x10 UB unbounded)
followed by optional Int32BE length-prefixed lower and upper bound payloads.
-}
encodeRangePayload :: ValueCodec a -> Range a -> (Int32, Builder.Builder)
encodeRangePayload _ EmptyRange = (1, Builder.word8 0x01)
encodeRangePayload elemCodec (Range lb ub) =
  let (lbFlag, lbB, lbLen) = case lb of
        Unbounded -> (0x08, mempty, 0)
        Inclusive x ->
          let bs = BSL.toStrict (Builder.toLazyByteString (codecEncoder elemCodec x))
           in (0x02, Builder.byteString bs, BS.length bs)
        Exclusive x ->
          let bs = BSL.toStrict (Builder.toLazyByteString (codecEncoder elemCodec x))
           in (0x00, Builder.byteString bs, BS.length bs)
      (ubFlag, ubB, ubLen) = case ub of
        Unbounded -> (0x10, mempty, 0)
        Inclusive y ->
          let bs = BSL.toStrict (Builder.toLazyByteString (codecEncoder elemCodec y))
           in (0x04, Builder.byteString bs, BS.length bs)
        Exclusive y ->
          let bs = BSL.toStrict (Builder.toLazyByteString (codecEncoder elemCodec y))
           in (0x00, Builder.byteString bs, BS.length bs)
      flag = lbFlag .|. ubFlag
      totalLen = fromIntegral (1 + lbLen + ubLen)
      builder = Builder.word8 flag <> lbB <> ubB
   in (totalLen, builder)

-- | Decode a PostgreSQL binary wire range payload.
decodeRangePayload :: ValueCodec a -> ByteString -> Either Text (Range a)
decodeRangePayload elemCodec bs
  | BS.null bs = Left "Buffer underflow for range: expected at least 1-byte flag"
  | otherwise =
      let flags = BS.index bs 0
          payload = BS.drop 1 bs
          isEmpty = (flags .&. 0x01) /= 0
          lbInc = (flags .&. 0x02) /= 0
          ubInc = (flags .&. 0x04) /= 0
          lbInf = (flags .&. 0x08) /= 0
          ubInf = (flags .&. 0x10) /= 0
       in if isEmpty
            then Right EmptyRange
            else do
              (lb, remAfterLb) <-
                if lbInf
                  then Right (Unbounded, payload)
                  else case runDecoder (codecDecoder elemCodec) payload of
                    Left err -> Left ("Failed to decode range lower bound: " <> err)
                    Right (val, rem1) ->
                      let b = if lbInc then Inclusive val else Exclusive val
                       in Right (b, rem1)
              (ub, remAfterUb) <-
                if ubInf
                  then Right (Unbounded, remAfterLb)
                  else case runDecoder (codecDecoder elemCodec) remAfterLb of
                    Left err -> Left ("Failed to decode range upper bound: " <> err)
                    Right (val, rem2) ->
                      let b = if ubInc then Inclusive val else Exclusive val
                       in Right (b, rem2)
              if not (BS.null remAfterUb)
                then Left ("Trailing bytes in range payload: " <> T.pack (show (BS.length remAfterUb)))
                else Right (Range lb ub)

-- | Symmetrical binary wire codec for PostgreSQL range types.
rangeCodec :: ValueCodec a -> ValueCodec (Range a)
rangeCodec elemCodec =
  makeFramedValueCodec
    (encodeRangePayload elemCodec)
    (decodeRangePayload elemCodec)

--------------------------------------------------------------------------------
-- Range Literals & Formatting
--------------------------------------------------------------------------------

-- | Render a Range into standard PostgreSQL text format (e.g. "[1,10)", "empty", "[,5]").
renderRangeText :: (a -> Text) -> Range a -> Text
renderRangeText _ EmptyRange = "empty"
renderRangeText renderElem (Range lb ub) =
  let (lChar, lText) = case lb of
        Inclusive x -> ("[", renderElem x)
        Exclusive x -> ("(", renderElem x)
        Unbounded -> ("(", "")
      (rChar, rText) = case ub of
        Inclusive y -> ("]", renderElem y)
        Exclusive y -> (")", renderElem y)
        Unbounded -> (")", "")
   in lChar <> lText <> "," <> rText <> rChar

-- | Construct a typed Range expression from a 'Show'-able range, bound as a $1 parameter.
litRange :: Show a => Text -> Range a -> QExpr (Range a)
litRange pgType r = litTyped (renderRangeText (T.pack . show) r) pgType

-- | Construct a typed Range expression using a custom element renderer.
litRangeWith :: (a -> Text) -> Text -> Range a -> QExpr (Range a)
litRangeWith renderElem pgType r = litTyped (renderRangeText renderElem r) pgType

--------------------------------------------------------------------------------
-- Range Operators
--------------------------------------------------------------------------------

-- | Range contains range test (@>). True if the first range contains the second.
rangeContains :: QExpr (Range a) -> QExpr (Range a) -> QExpr Bool
rangeContains = BinOpExpr "@>"

-- | Infix alias for 'rangeContains'.
(@>:) :: QExpr (Range a) -> QExpr (Range a) -> QExpr Bool
(@>:) = rangeContains

infix 4 @>:

-- | Range contains element test (@>). True if range contains element value.
rangeContainsElem :: QExpr (Range a) -> QExpr a -> QExpr Bool
rangeContainsElem = BinOpExpr "@>"

-- | Range contained-by test (<@). True if first range is contained within second.
rangeContainedBy :: QExpr (Range a) -> QExpr (Range a) -> QExpr Bool
rangeContainedBy = BinOpExpr "<@"

-- | Infix alias for 'rangeContainedBy'.
(<@:) :: QExpr (Range a) -> QExpr (Range a) -> QExpr Bool
(<@:) = rangeContainedBy

infix 4 <@:

-- | Range overlaps test (&&). True if ranges share at least one point.
rangeOverlaps :: QExpr (Range a) -> QExpr (Range a) -> QExpr Bool
rangeOverlaps = BinOpExpr "&&"

-- | Infix alias for 'rangeOverlaps'.
(&&:) :: QExpr (Range a) -> QExpr (Range a) -> QExpr Bool
(&&:) = rangeOverlaps

infix 4 &&:

-- | Range adjacent test (-|-). True if ranges are mutually adjacent (share a boundary without overlapping).
rangeAdjacent :: QExpr (Range a) -> QExpr (Range a) -> QExpr Bool
rangeAdjacent = BinOpExpr "-|-"

-- | Infix operator for 'rangeAdjacent'.
(-|-) :: QExpr (Range a) -> QExpr (Range a) -> QExpr Bool
(-|-) = rangeAdjacent

infix 4 -|-

-- | Dotted infix operator for 'rangeAdjacent'.
(-|-.) :: QExpr (Range a) -> QExpr (Range a) -> QExpr Bool
(-|-.) = rangeAdjacent

infix 4 -|-.

-- | Range strictly left-of test (<<). True if first range is strictly to the left of second.
rangeLeftOf :: QExpr (Range a) -> QExpr (Range a) -> QExpr Bool
rangeLeftOf = BinOpExpr "<<"

-- | Infix alias for 'rangeLeftOf'.
(<<:) :: QExpr (Range a) -> QExpr (Range a) -> QExpr Bool
(<<:) = rangeLeftOf

infix 4 <<:

-- | Range strictly right-of test (>>). True if first range is strictly to the right of second.
rangeRightOf :: QExpr (Range a) -> QExpr (Range a) -> QExpr Bool
rangeRightOf = BinOpExpr ">>"

-- | Infix alias for 'rangeRightOf'.
(>>:) :: QExpr (Range a) -> QExpr (Range a) -> QExpr Bool
(>>:) = rangeRightOf

infix 4 >>:
