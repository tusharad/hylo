{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.Codec.Binary
Description : PostgreSQL binary wire protocol codecs (protocol 3.0 / libpq)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com
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

    -- * Primitive & Domain Value Codecs
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
  , uuidCodec
  , utcTimeCodec
  , dayCodec
  , jsonbCodec
  , jsonCodec
  , embeddingCodec
  , arrayCodec
  , arrayCodecWithOid

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
  , uuidCol
  , utcTimeCol
  , dayCol
  , jsonbCol
  , jsonCol
  , embeddingCol
  , arrayCol
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
  , uuidOid
  , timestamptzOid
  , timestampOid
  , dateOid
  , jsonbOid
  , jsonOid

    -- * Binary Parsing Helpers
  , readInt16BE
  , readInt64BE
  , readWord64BE
  ) where

import qualified Data.Aeson as Aeson
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int16, Int32, Int64)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
  ( Day
  , NominalDiffTime
  , UTCTime (..)
  , addDays
  , addUTCTime
  , diffDays
  , diffUTCTime
  , fromGregorian
  )
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Word (Word16, Word32, Word64)
import GHC.Float
  ( castDoubleToWord64
  , castFloatToWord32
  , castWord32ToFloat
  , castWord64ToDouble
  )
import GHC.TypeLits (KnownNat)
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
import Hylo.PG.Vector (Embedding (..), decodeBinaryVector, encodeBinaryVector)

--------------------------------------------------------------------------------
-- PostgreSQL Type OIDs
--------------------------------------------------------------------------------

boolOid
  , byteaOid
  , int8Oid
  , int2Oid
  , int4Oid
  , textOid
  , float4Oid
  , float8Oid
  , varcharOid
  , uuidOid
  , timestamptzOid
  , timestampOid
  , dateOid
  , jsonbOid
  , jsonOid ::
    Word32
boolOid = 16
byteaOid = 17
int8Oid = 20
int2Oid = 21
int4Oid = 23
textOid = 25
float4Oid = 700
float8Oid = 701
varcharOid = 1043
uuidOid = 2950
timestamptzOid = 1184
timestampOid = 1114
dateOid = 1082
jsonbOid = 3802
jsonOid = 114

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

-- | PostgreSQL epoch date (2000-01-01).
pgEpochDay :: Day
pgEpochDay = fromGregorian 2000 1 1

-- | PostgreSQL epoch timestamp (2000-01-01 00:00:00 UTC).
pgEpochUTCTime :: UTCTime
pgEpochUTCTime = UTCTime pgEpochDay 0

-- | Codec for PostgreSQL 'uuid' (OID 2950, 16-byte binary wire representation).
uuidCodec :: ValueCodec UUID
uuidCodec =
  makeFramedFixedCodec
    16
    (Builder.lazyByteString . UUID.toByteString)
    ( \bs ->
        if BS.length bs < 16
          then Left "Buffer underflow for 16-byte binary UUID"
          else case UUID.fromByteString (BSL.fromStrict bs) of
            Nothing -> Left "Invalid binary UUID payload"
            Just u -> Right u
    )

-- | Codec for PostgreSQL 'timestamptz' (OID 1184, 8-byte signed Int64 microseconds since 2000-01-01).
utcTimeCodec :: ValueCodec UTCTime
utcTimeCodec =
  makeFramedFixedCodec
    8
    ( \t ->
        let diffSec = diffUTCTime t pgEpochUTCTime
            micros = round (toRational diffSec * 1000000) :: Int64
         in Builder.int64BE micros
    )
    ( \bs ->
        if BS.length bs < 8
          then Left "Buffer too short for timestamptz (expected 8 bytes)"
          else
            let micros = readInt64BE bs
                diffSec = fromRational (toInteger micros % 1000000) :: NominalDiffTime
             in Right (addUTCTime diffSec pgEpochUTCTime)
    )

-- | Codec for PostgreSQL 'date' (OID 1082, 4-byte Int32 days since 2000-01-01).
dayCodec :: ValueCodec Day
dayCodec =
  makeFramedFixedCodec
    4
    (\d -> Builder.int32BE (fromIntegral (diffDays d pgEpochDay)))
    ( \bs ->
        if BS.length bs < 4
          then Left "Buffer too short for date (expected 4 bytes)"
          else
            let daysDiff = readInt32BE bs
             in Right (addDays (fromIntegral daysDiff) pgEpochDay)
    )

-- | Codec for PostgreSQL 'jsonb' (OID 3802, 1-byte version header 0x01 followed by UTF-8 JSON text).
jsonbCodec :: ValueCodec Aeson.Value
jsonbCodec =
  makeFramedValueCodec
    ( \val ->
        let bs = BSL.toStrict (Aeson.encode val)
         in (1 + fromIntegral (BS.length bs), Builder.word8 1 <> Builder.byteString bs)
    )
    ( \bs ->
        if BS.null bs
          then Left "Buffer underflow for jsonb (expected at least 1-byte version header)"
          else
            let ver = BS.index bs 0
             in if ver /= 1
                  then Left ("Unsupported jsonb binary wire version: " <> T.pack (show ver))
                  else case Aeson.eitherDecodeStrict' (BS.drop 1 bs) of
                    Left err -> Left ("Failed to decode jsonb payload: " <> T.pack err)
                    Right val -> Right val
    )

-- | Codec for PostgreSQL 'json' (OID 114, UTF-8 JSON text).
jsonCodec :: ValueCodec Aeson.Value
jsonCodec =
  makeFramedValueCodec
    ( \val ->
        let bs = BSL.toStrict (Aeson.encode val)
         in (fromIntegral (BS.length bs), Builder.byteString bs)
    )
    ( \bs -> case Aeson.eitherDecodeStrict' bs of
        Left err -> Left ("Failed to decode json payload: " <> T.pack err)
        Right val -> Right val
    )

-- | Codec for pgvector 'vector' (wrapping 'encodeBinaryVector' and 'decodeBinaryVector').
embeddingCodec ::
  forall model dim.
  (KnownNat dim) =>
  ValueCodec (Embedding model dim)
embeddingCodec =
  makeFramedValueCodec
    ( \emb ->
        let bs = encodeBinaryVector emb
         in (fromIntegral (BS.length bs), Builder.byteString bs)
    )
    ( \bs -> case decodeBinaryVector bs of
        Left err -> Left (T.pack err)
        Right emb -> Right emb
    )

-- | Codec for 1-dimensional PostgreSQL binary arrays with explicit element type OID.
arrayCodecWithOid :: Word32 -> ValueCodec a -> ValueCodec [a]
arrayCodecWithOid elemOid elemCodec =
  makeFramedValueCodec
    ( \xs ->
        let count = fromIntegral (length xs) :: Int32
            elementsBuilder = mconcat (map (codecEncoder elemCodec) xs)
            elementsBytes = BSL.toStrict (Builder.toLazyByteString elementsBuilder)
            (hdrLen, hdrBuilder) =
              if count == 0
                then
                  ( 12
                  , Builder.int32BE 0
                      <> Builder.int32BE 0
                      <> Builder.word32BE elemOid
                  )
                else
                  ( 20
                  , Builder.int32BE 1
                      <> Builder.int32BE 0
                      <> Builder.word32BE elemOid
                      <> Builder.int32BE count
                      <> Builder.int32BE 1
                  )
            totalLen = hdrLen + fromIntegral (BS.length elementsBytes)
         in (totalLen, hdrBuilder <> Builder.byteString elementsBytes)
    )
    ( \bs ->
        if BS.length bs < 12
          then Left "Array binary buffer underflow"
          else
            let ndim = readInt32BE bs
             in if ndim == 0
                  then Right []
                  else
                    if ndim /= 1
                      then Left ("Multi-dimensional arrays are not currently supported: ndim = " <> T.pack (show ndim))
                      else
                        if BS.length bs < 20
                          then Left "Array header underflow for 1-D array"
                          else
                            let dimSize = readInt32BE (BS.drop 12 bs)
                                elemsPayload = BS.drop 20 bs
                                decodeElements 0 rest acc =
                                  if BS.null rest
                                    then Right (reverse acc)
                                    else Left "Trailing unparsed bytes in array payload"
                                decodeElements n rest acc =
                                  case runDecoder (codecDecoder elemCodec) rest of
                                    Left err -> Left err
                                    Right (x, rest') -> decodeElements (n - 1) rest' (x : acc)
                             in decodeElements dimSize elemsPayload []
    )

-- | Codec for 1-dimensional PostgreSQL binary arrays.
arrayCodec :: ValueCodec a -> ValueCodec [a]
arrayCodec = arrayCodecWithOid 0

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

uuidCol :: ValueCodec UUID
uuidCol = uuidCodec

utcTimeCol :: ValueCodec UTCTime
utcTimeCol = utcTimeCodec

dayCol :: ValueCodec Day
dayCol = dayCodec

jsonbCol :: ValueCodec Aeson.Value
jsonbCol = jsonbCodec

jsonCol :: ValueCodec Aeson.Value
jsonCol = jsonCodec

embeddingCol :: (KnownNat dim) => ValueCodec (Embedding model dim)
embeddingCol = embeddingCodec

arrayCol :: ValueCodec a -> ValueCodec [a]
arrayCol = arrayCodec

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
