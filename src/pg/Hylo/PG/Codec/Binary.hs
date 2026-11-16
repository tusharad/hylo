{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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

    -- * Table Codec & Positional JSON Decoding
  , TableCodec (..)
  , PositionalDecoder (..)
  , runPositional
  , jsonField
  , HasValueCodec (..)
  , skipColumn
  , skipColumns
  , Selectable (..)

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
  , rangeCol
  , idCol
  , pgEnumCodec

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

import Control.Monad (forM, replicateM_, void, when)
import qualified Data.Aeson as Aeson
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.Ratio ((%))
import Data.Scientific (floatingOrInteger)
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
import Data.Time.Format (defaultTimeLocale, parseTimeM)
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
  , decodeFail
  , dimap
  , enumCodec
  , field
  , fieldWith
  , getBytes
  , idCodec
  , lmap
  , makeFramedFixedCodec
  , makeFramedValueCodec
  , nullable
  , pairCodec
  , peekBytes
  , quadCodec
  , readInt32BE
  , rmap
  , runBinaryDecoder
  , runBinaryEncoder
  , tripleCodec
  )
import Hylo.Core.Context (Ctx (..))
import Hylo.Core.Expr (QExpr (..))
import Hylo.Core.Key (Id (..))
import Hylo.Core.Nullable (NullableTable (..))
import Hylo.Core.Table (Table (..))
import Hylo.PG.Compiler.Lateral (NestedList (..))
import Hylo.PG.Range (Range (..), rangeCodec)
import Hylo.PG.Vector (Embedding (..), decodeBinaryVector, encodeBinaryVector)
import Hylo.SQL.AST (Projectable (..))
import Text.Read (readMaybe)

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

rangeCol :: ValueCodec a -> ValueCodec (Range a)
rangeCol = rangeCodec

idCol :: ValueCodec a -> ValueCodec (Id table a)
idCol = idCodec

{- | PostgreSQL enum column codec via the text-protocol OID fallback.
PostgreSQL transmits enum values as framed UTF-8 label text regardless of
the enum type's runtime OID, so the label-table codec maps 1:1 onto the wire.
-}
pgEnumCodec :: (Eq a) => [(Text, a)] -> ValueCodec a
pgEnumCodec = enumCodec

--------------------------------------------------------------------------------
-- Table Codec & Positional JSON Array Decoding
--------------------------------------------------------------------------------

-- | Applicative decoder for positional JSON arrays generated by PostgreSQL @jsonb_build_array@.
newtype PositionalDecoder a = PositionalDecoder
  { runPositionalDecoder :: [Aeson.Value] -> Either Text (a, [Aeson.Value])
  }
  deriving (Functor)

instance Applicative PositionalDecoder where
  pure x = PositionalDecoder $ \vals -> Right (x, vals)
  PositionalDecoder f <*> PositionalDecoder a = PositionalDecoder $ \vals ->
    case f vals of
      Left err -> Left err
      Right (g, rest1) -> case a rest1 of
        Left err -> Left err
        Right (x, rest2) -> Right (g x, rest2)

-- | Execute a 'PositionalDecoder' ensuring all positional elements are consumed.
runPositional :: PositionalDecoder a -> [Aeson.Value] -> Either Text a
runPositional (PositionalDecoder dec) vals = case dec vals of
  Left err -> Left err
  Right (x, rest)
    | null rest -> Right x
    | otherwise ->
        Left $
          "Trailing unconsumed JSON fields in positional row ("
            <> T.pack (show (length rest))
            <> " remaining)"

-- | Decode a single positional JSON field into a typed value.
jsonField :: (HasValueCodec a) => PositionalDecoder a
jsonField = PositionalDecoder $ \case
  [] -> Left "Unexpected end of positional JSON array"
  (v : rest) -> case parseJsonValue v of
    Left err -> Left err
    Right x -> Right (x, rest)

-- | Typeclass associating an HKD table with its canonical binary wire RowCodec and positional JSON decoder.
class TableCodec (table :: Ctx -> Type) where
  tableCodec :: RowCodec (table 'Result) (table 'Result)
  tableJsonDecoder :: PositionalDecoder (table 'Result)
  tableJsonDecoder = PositionalDecoder $ \_ -> Left "tableJsonDecoder: not implemented for table"

--------------------------------------------------------------------------------
-- HasValueCodec
--------------------------------------------------------------------------------

-- | Typeclass associating a scalar Haskell type with its PostgreSQL wire and JSON codecs.
class HasValueCodec a where
  hasValueCodec :: ValueCodec a
  parseJsonValue :: Aeson.Value -> Either Text a

instance HasValueCodec Int64 where
  hasValueCodec = int64Codec
  parseJsonValue = \case
    Aeson.Number n -> case floatingOrInteger n of
      Right (i :: Integer) -> Right (fromIntegral i)
      Left (_ :: Double) -> Left "Expected integer for Int64"
    Aeson.String s -> case readMaybe (T.unpack s) of
      Just i -> Right i
      Nothing -> Left ("Cannot parse Int64 from string: " <> s)
    other -> Left ("Expected number or string for Int64, got: " <> T.pack (show other))

instance HasValueCodec Int32 where
  hasValueCodec = int32Codec
  parseJsonValue = \case
    Aeson.Number n -> case floatingOrInteger n of
      Right (i :: Integer) -> Right (fromIntegral i)
      Left (_ :: Double) -> Left "Expected integer for Int32"
    Aeson.String s -> case readMaybe (T.unpack s) of
      Just i -> Right i
      Nothing -> Left ("Cannot parse Int32 from string: " <> s)
    other -> Left ("Expected number or string for Int32, got: " <> T.pack (show other))

instance HasValueCodec Int16 where
  hasValueCodec = int16Codec
  parseJsonValue = \case
    Aeson.Number n -> case floatingOrInteger n of
      Right (i :: Integer) -> Right (fromIntegral i)
      Left (_ :: Double) -> Left "Expected integer for Int16"
    Aeson.String s -> case readMaybe (T.unpack s) of
      Just i -> Right i
      Nothing -> Left ("Cannot parse Int16 from string: " <> s)
    other -> Left ("Expected number or string for Int16, got: " <> T.pack (show other))

instance HasValueCodec Int where
  hasValueCodec = intCodec
  parseJsonValue = \case
    Aeson.Number n -> case floatingOrInteger n of
      Right (i :: Integer) -> Right (fromIntegral i)
      Left (_ :: Double) -> Left "Expected integer for Int"
    Aeson.String s -> case readMaybe (T.unpack s) of
      Just i -> Right i
      Nothing -> Left ("Cannot parse Int from string: " <> s)
    other -> Left ("Expected number or string for Int, got: " <> T.pack (show other))

instance HasValueCodec Text where
  hasValueCodec = textCodec
  parseJsonValue = \case
    Aeson.String s -> Right s
    Aeson.Number n -> Right (T.pack (show n))
    Aeson.Bool b -> Right (if b then "true" else "false")
    other -> Left ("Expected string for Text, got: " <> T.pack (show other))

instance HasValueCodec String where
  hasValueCodec = stringCodec
  parseJsonValue = \case
    Aeson.String s -> Right (T.unpack s)
    Aeson.Number n -> Right (show n)
    Aeson.Bool b -> Right (if b then "true" else "false")
    other -> Left ("Expected string, got: " <> T.pack (show other))

instance HasValueCodec Bool where
  hasValueCodec = boolCodec
  parseJsonValue = \case
    Aeson.Bool b -> Right b
    Aeson.String s -> case T.toLower s of
      "true" -> Right True
      "t" -> Right True
      "1" -> Right True
      "false" -> Right False
      "f" -> Right False
      "0" -> Right False
      _ -> Left ("Cannot parse Bool from string: " <> s)
    Aeson.Number n -> Right (n /= 0)
    other -> Left ("Expected bool, got: " <> T.pack (show other))

instance HasValueCodec Double where
  hasValueCodec = doubleCodec
  parseJsonValue = \case
    Aeson.Number n -> case floatingOrInteger n of
      Right (i :: Integer) -> Right (fromIntegral i)
      Left d -> Right d
    Aeson.String s -> case readMaybe (T.unpack s) of
      Just d -> Right d
      Nothing -> Left ("Cannot parse Double from string: " <> s)
    other -> Left ("Expected number for Double, got: " <> T.pack (show other))

instance HasValueCodec Float where
  hasValueCodec = floatCodec
  parseJsonValue = \case
    Aeson.Number n -> case floatingOrInteger n of
      Right (i :: Integer) -> Right (fromIntegral i)
      Left d -> Right (realToFrac (d :: Double))
    Aeson.String s -> case readMaybe (T.unpack s) of
      Just f -> Right f
      Nothing -> Left ("Cannot parse Float from string: " <> s)
    other -> Left ("Expected number for Float, got: " <> T.pack (show other))

instance HasValueCodec UUID where
  hasValueCodec = uuidCodec
  parseJsonValue = \case
    Aeson.String s -> case UUID.fromText s of
      Just u -> Right u
      Nothing -> Left ("Invalid UUID text: " <> s)
    other -> Left ("Expected string for UUID, got: " <> T.pack (show other))

instance HasValueCodec UTCTime where
  hasValueCodec = utcTimeCodec
  parseJsonValue = \case
    Aeson.String s ->
      let formats =
            [ "%Y-%m-%dT%H:%M:%S%Q%Z"
            , "%Y-%m-%dT%H:%M:%S%Q"
            , "%Y-%m-%d %H:%M:%S%Q%Z"
            , "%Y-%m-%d %H:%M:%S%Q"
            , "%Y-%m-%d"
            ]
          tryParse [] = Left ("Cannot parse UTCTime from string: " <> s)
          tryParse (fmt : fmts) =
            case parseTimeM True defaultTimeLocale fmt (T.unpack s) of
              Just t -> Right t
              Nothing -> tryParse fmts
       in tryParse formats
    other -> Left ("Expected string for UTCTime, got: " <> T.pack (show other))

instance HasValueCodec Day where
  hasValueCodec = dayCodec
  parseJsonValue = \case
    Aeson.String s ->
      case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack s) of
        Just d -> Right d
        Nothing -> Left ("Cannot parse Day from string: " <> s)
    other -> Left ("Expected string for Day, got: " <> T.pack (show other))

instance HasValueCodec Aeson.Value where
  hasValueCodec = jsonbCodec
  parseJsonValue = Right

instance (HasValueCodec a) => HasValueCodec (Id table a) where
  hasValueCodec = idCodec hasValueCodec
  parseJsonValue val = Id <$> parseJsonValue val

instance (HasValueCodec a) => HasValueCodec (Maybe a) where
  hasValueCodec = nullable hasValueCodec
  parseJsonValue = \case
    Aeson.Null -> Right Nothing
    val -> Just <$> parseJsonValue val

instance (HasValueCodec a) => HasValueCodec [a] where
  hasValueCodec = arrayCodec hasValueCodec
  parseJsonValue = \case
    Aeson.Array arr -> mapM parseJsonValue (toList arr)
    Aeson.Null -> Right []
    other -> Left ("Expected array, got: " <> T.pack (show other))

--------------------------------------------------------------------------------
-- Binary Decoding Skip Helpers
--------------------------------------------------------------------------------

-- | Skip a single framed PostgreSQL wire column payload.
skipColumn :: Decoder ()
skipColumn = do
  lenBytes <- getBytes 4
  let len = readInt32BE lenBytes
  when (len > 0) $ void (getBytes (fromIntegral len))

-- | Skip multiple framed PostgreSQL wire column payloads.
skipColumns :: Int -> Decoder ()
skipColumns n = replicateM_ n skipColumn

--------------------------------------------------------------------------------
-- Selectable Typeclass & Instances
--------------------------------------------------------------------------------

-- | Typeclass converting query expression ASTs into hydrated domain row types.
class Selectable expr res | expr -> res where
  selectableDecoder :: expr -> Decoder res
  selectablePositionalDecoder :: expr -> PositionalDecoder res

instance (TableCodec table) => Selectable (table 'Expr) (table 'Result) where
  selectableDecoder _ = codecDecoder (tableCodec :: RowCodec (table 'Result) (table 'Result))
  selectablePositionalDecoder _ = tableJsonDecoder

instance
  (Table table, TableCodec table, Projectable (table 'Expr)) =>
  Selectable (NullableTable (table 'Expr)) (Maybe (table 'Result))
  where
  selectableDecoder (NullableTable expr) = do
    lenBytes <- peekBytes 4
    let pkLen = readInt32BE lenBytes
        cols = projectColumns expr
        colCount = case cols of
          [] -> length (tableExprColumns expr)
          cs -> length cs
    if pkLen == -1
      then do
        skipColumns colCount
        pure Nothing
      else Just <$> codecDecoder (tableCodec :: RowCodec (table 'Result) (table 'Result))

  selectablePositionalDecoder (NullableTable expr) = PositionalDecoder $ \vals -> do
    let cols = projectColumns expr
        colCount = case cols of
          [] -> length (tableExprColumns expr)
          cs -> length cs
    case vals of
      [] -> Left "Unexpected empty positional array for NullableTable"
      (pkVal : _)
        | pkVal == Aeson.Null -> Right (Nothing, drop colCount vals)
        | otherwise -> case runPositionalDecoder (tableJsonDecoder :: PositionalDecoder (table 'Result)) vals of
            Left err -> Left err
            Right (r, rest) -> Right (Just r, rest)

instance (Selectable a res) => Selectable (NestedList a) [res] where
  selectableDecoder (NestedList _ childExpr) = do
    lenBytes <- getBytes 4
    let len = readInt32BE lenBytes
    if len <= 0
      then pure []
      else do
        payload <- getBytes (fromIntegral len)
        if BS.null payload
          then pure []
          else do
            let ver = BS.index payload 0
            if ver /= 1
              then decodeFail ("Unsupported jsonb binary wire version: " <> T.pack (show ver))
              else do
                let jsonBS = BS.drop 1 payload
                if jsonBS == "[]" || BS.null jsonBS
                  then pure []
                  else case Aeson.eitherDecodeStrict' jsonBS of
                    Left err -> decodeFail ("Failed to decode jsonb array: " <> T.pack err)
                    Right (Aeson.Array arr) ->
                      case mapM (decodePositionalRow childExpr) (toList arr) of
                        Left err -> decodeFail err
                        Right rows -> pure rows
                    Right Aeson.Null -> pure []
                    Right _ -> decodeFail "Expected JSON array in jsonb column for NestedList"
    where
      decodePositionalRow child (Aeson.Array rowArr) =
        runPositional (selectablePositionalDecoder child) (toList rowArr)
      decodePositionalRow child otherVal =
        runPositional (selectablePositionalDecoder child) [otherVal]

  selectablePositionalDecoder (NestedList _ childExpr) = PositionalDecoder $ \case
    [] -> Left "Unexpected end of positional array for NestedList"
    (Aeson.Null : rest) -> Right ([], rest)
    (Aeson.Array arr : rest) -> do
      rows <- forM (toList arr) $ \case
        Aeson.Array rowArr -> runPositional (selectablePositionalDecoder childExpr) (toList rowArr)
        otherVal -> runPositional (selectablePositionalDecoder childExpr) [otherVal]
      Right (rows, rest)
    (other : _) -> Left ("Expected JSON array for NestedList, got: " <> T.pack (show other))

instance (HasValueCodec a) => Selectable (QExpr a) a where
  selectableDecoder _ = codecDecoder (hasValueCodec :: ValueCodec a)
  selectablePositionalDecoder _ = jsonField

instance (Selectable a ra, Selectable b rb) => Selectable (a, b) (ra, rb) where
  selectableDecoder (a, b) = (,) <$> selectableDecoder a <*> selectableDecoder b
  selectablePositionalDecoder (a, b) = (,) <$> selectablePositionalDecoder a <*> selectablePositionalDecoder b

instance (Selectable a ra, Selectable b rb, Selectable c rc) => Selectable (a, b, c) (ra, rb, rc) where
  selectableDecoder (a, b, c) = (,,) <$> selectableDecoder a <*> selectableDecoder b <*> selectableDecoder c
  selectablePositionalDecoder (a, b, c) =
    (,,)
      <$> selectablePositionalDecoder a
      <*> selectablePositionalDecoder b
      <*> selectablePositionalDecoder c

instance
  (Selectable a ra, Selectable b rb, Selectable c rc, Selectable d rd) =>
  Selectable (a, b, c, d) (ra, rb, rc, rd)
  where
  selectableDecoder (a, b, c, d) =
    (,,,)
      <$> selectableDecoder a
      <*> selectableDecoder b
      <*> selectableDecoder c
      <*> selectableDecoder d
  selectablePositionalDecoder (a, b, c, d) =
    (,,,)
      <$> selectablePositionalDecoder a
      <*> selectablePositionalDecoder b
      <*> selectablePositionalDecoder c
      <*> selectablePositionalDecoder d

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
