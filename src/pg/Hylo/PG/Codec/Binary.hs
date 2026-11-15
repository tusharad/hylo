{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Hylo.PG.Codec.Binary
Description : PostgreSQL binary wire protocol codecs (protocol 3.0 / libpq)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

PostgreSQL protocol version 3.0 binary wire codecs for primitive and composite
types, supporting zero-intermediate-string marshalling directly over network byte
buffers.
-}
module Hylo.PG.Codec.Binary
  ( -- * Re-exports from Core Codec
    Codec (..)
  , ValueCodec
  , RowCodec
  , Decoder (..)
  , runBinaryEncoder
  , runBinaryDecoder
  , field
  , fieldWith
  , nullable
  , idCodec
  , dimap
  , lmap
  , rmap
  , pairCodec
  , tripleCodec
  , quadCodec

    -- * Primitive Value Codecs
  , boolCodec
  , int16Codec
  , int32Codec
  , int64Codec
  , intCodec
  , floatCodec
  , doubleCodec
  , textCodec
  , stringCodec
  , byteStringCodec

    -- * Specification Composite Codec
  , compositeCodec

    -- * PRD Column Codec Aliases
  , boolCol
  , int16Col
  , int32Col
  , int64Col
  , intCol
  , floatCol
  , doubleCol
  , textCol
  , stringCol
  , byteStringCol
  , idCol

    -- * PostgreSQL Type OIDs
  , boolOid
  , byteaOid
  , int8Oid
  , int2Oid
  , int4Oid
  , textOid
  , float4Oid
  , float8Oid
  , varcharOid

    -- * Binary Parsing Helpers
  , readInt16BE
  , readInt64BE
  , readWord64BE
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word32, Word64)
import GHC.Float
  ( castDoubleToWord64
  , castFloatToWord32
  , castWord32ToFloat
  , castWord64ToDouble
  )
import Hylo.Core.Codec
  ( Codec (..)
  , Decoder (..)
  , RowCodec
  , ValueCodec
  , dimap
  , field
  , fieldWith
  , idCodec
  , lmap
  , makeFramedFixedCodec
  , makeFramedValueCodec
  , nullable
  , pairCodec
  , quadCodec
  , readInt32BE
  , rmap
  , runBinaryDecoder
  , runBinaryEncoder
  , tripleCodec
  )
import Hylo.Core.Key (Id)

--------------------------------------------------------------------------------
-- PostgreSQL Type OIDs
--------------------------------------------------------------------------------

boolOid, byteaOid, int8Oid, int2Oid, int4Oid, textOid, float4Oid, float8Oid, varcharOid :: Word32
boolOid = 16
byteaOid = 17
int8Oid = 20
int2Oid = 21
int4Oid = 23
textOid = 25
float4Oid = 700
float8Oid = 701
varcharOid = 1043

--------------------------------------------------------------------------------
-- Primitive Value Codecs
--------------------------------------------------------------------------------

-- | Codec for PostgreSQL 'bool' (OID 16).
boolCodec :: ValueCodec Bool
boolCodec =
  makeFramedFixedCodec
    1
    (\b -> Builder.word8 (if b then 1 else 0))
    ( \bs ->
        if BS.null bs
          then Left "Unexpected empty buffer for bool"
          else Right (BS.index bs 0 /= 0)
    )

-- | Codec for PostgreSQL 'int2' (OID 21).
int16Codec :: ValueCodec Int16
int16Codec =
  makeFramedFixedCodec
    2
    Builder.int16BE
    ( \bs ->
        if BS.length bs < 2
          then Left "Buffer too short for int2"
          else Right (readInt16BE bs)
    )

-- | Codec for PostgreSQL 'int4' (OID 23).
int32Codec :: ValueCodec Int32
int32Codec =
  makeFramedFixedCodec
    4
    Builder.int32BE
    ( \bs ->
        if BS.length bs < 4
          then Left "Buffer too short for int4"
          else Right (readInt32BE bs)
    )

-- | Codec for PostgreSQL 'int8' (OID 20).
int64Codec :: ValueCodec Int64
int64Codec =
  makeFramedFixedCodec
    8
    Builder.int64BE
    ( \bs ->
        if BS.length bs < 8
          then Left "Buffer too short for int8"
          else Right (readInt64BE bs)
    )

-- | Codec for Haskell 'Int' mapped to PostgreSQL 'int8'.
intCodec :: ValueCodec Int
intCodec = dimap fromIntegral fromIntegral int64Codec

-- | Codec for PostgreSQL 'float4' (OID 700).
floatCodec :: ValueCodec Float
floatCodec =
  makeFramedFixedCodec
    4
    (Builder.word32BE . castFloatToWord32)
    ( \bs ->
        if BS.length bs < 4
          then Left "Buffer too short for float4"
          else Right (castWord32ToFloat (fromIntegral (readInt32BE bs)))
    )

-- | Codec for PostgreSQL 'float8' (OID 701).
doubleCodec :: ValueCodec Double
doubleCodec =
  makeFramedFixedCodec
    8
    (Builder.word64BE . castDoubleToWord64)
    ( \bs ->
        if BS.length bs < 8
          then Left "Buffer too short for float8"
          else Right (castWord64ToDouble (readWord64BE bs))
    )

-- | Codec for PostgreSQL 'text' (OID 25).
textCodec :: ValueCodec Text
textCodec =
  makeFramedValueCodec
    ( \t ->
        let bs = TE.encodeUtf8 t
         in (fromIntegral (BS.length bs), Builder.byteString bs)
    )
    ( \bs -> case TE.decodeUtf8' bs of
        Left _ -> Left "Invalid UTF-8 byte sequence in text column"
        Right t -> Right t
    )

-- | Codec for Haskell 'String' mapped to PostgreSQL 'text'.
stringCodec :: ValueCodec String
stringCodec = dimap T.pack T.unpack textCodec

-- | Codec for PostgreSQL 'bytea' (OID 17).
byteStringCodec :: ValueCodec ByteString
byteStringCodec =
  makeFramedValueCodec
    (\bs -> (fromIntegral (BS.length bs), Builder.byteString bs))
    Right

--------------------------------------------------------------------------------
-- PRD Aliases
--------------------------------------------------------------------------------

boolCol :: ValueCodec Bool
boolCol = boolCodec

int16Col :: ValueCodec Int16
int16Col = int16Codec

int32Col :: ValueCodec Int32
int32Col = int32Codec

int64Col :: ValueCodec Int64
int64Col = int64Codec

intCol :: ValueCodec Int
intCol = intCodec

floatCol :: ValueCodec Float
floatCol = floatCodec

doubleCol :: ValueCodec Double
doubleCol = doubleCodec

textCol :: ValueCodec Text
textCol = textCodec

stringCol :: ValueCodec String
stringCol = stringCodec

byteStringCol :: ValueCodec ByteString
byteStringCol = byteStringCodec

idCol :: ValueCodec a -> ValueCodec (Id table a)
idCol = idCodec

--------------------------------------------------------------------------------
-- Specification Composite Codec
--------------------------------------------------------------------------------

-- | Composite 4-tuple codec as defined in formal specification Epic 2.
compositeCodec :: Codec (Int64, Double, String, Bool) (Int64, Double, String, Bool)
compositeCodec = quadCodec int64Codec doubleCodec stringCodec boolCodec

--------------------------------------------------------------------------------
-- Binary Byte Decoding Helpers
--------------------------------------------------------------------------------

-- | Decode a 2-byte big-endian signed 16-bit integer.
readInt16BE :: ByteString -> Int16
readInt16BE bs = fromIntegral (readWord16BE bs)

-- | Decode a 2-byte big-endian unsigned 16-bit integer.
readWord16BE :: ByteString -> Word16
readWord16BE bs =
  (fromIntegral (BS.index bs 0) `shiftL` 8)
    .|. fromIntegral (BS.index bs 1)

-- | Decode an 8-byte big-endian signed 64-bit integer.
readInt64BE :: ByteString -> Int64
readInt64BE bs = fromIntegral (readWord64BE bs)

-- | Decode an 8-byte big-endian unsigned 64-bit integer.
readWord64BE :: ByteString -> Word64
readWord64BE bs =
  (fromIntegral (BS.index bs 0) `shiftL` 56)
    .|. (fromIntegral (BS.index bs 1) `shiftL` 48)
    .|. (fromIntegral (BS.index bs 2) `shiftL` 40)
    .|. (fromIntegral (BS.index bs 3) `shiftL` 32)
    .|. (fromIntegral (BS.index bs 4) `shiftL` 24)
    .|. (fromIntegral (BS.index bs 5) `shiftL` 16)
    .|. (fromIntegral (BS.index bs 6) `shiftL` 8)
    .|. fromIntegral (BS.index bs 7)
