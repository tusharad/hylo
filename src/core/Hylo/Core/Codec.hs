{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

{- |
Module      : Hylo.Core.Codec
Description : Zero-Generics Linear-Time Codec Engine and combinators
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

HyloCodec provides zero-generics, linear-time $O(C)$ Applicative and Profunctor
codecs for binary wire protocol marshalling and value hydration.
-}
module Hylo.Core.Codec
  ( -- * Core Codec Types
    Codec (..)
  , ValueCodec
  , RowCodec
  , Decoder (..)

    -- * Codec Execution
  , runBinaryEncoder
  , runBinaryDecoder

    -- * Decoder Primitives
  , decodeFail
  , getBytes
  , peekBytes
  , getRemainingBytes

    -- * Profunctor & Applicative Combinators
  , dimap
  , lmap
  , rmap
  , field
  , fieldWith

    -- * Framing & Nullability
  , makeFramedValueCodec
  , makeFramedFixedCodec
  , nullable

    -- * Phantom Key Codec
  , idCodec

    -- * Composite Tuple Codecs
  , pairCodec
  , tripleCodec
  , quadCodec

    -- * Binary Int32 Helper
  , readInt32BE
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Hylo.Core.Key (Id (..), unId)

-- | State monad decoder parsing binary buffers with descriptive errors.
newtype Decoder a = Decoder
  { runDecoder :: ByteString -> Either Text (a, ByteString)
  }
  deriving (Functor)

instance Applicative Decoder where
  pure x = Decoder $ \bs -> Right (x, bs)
  Decoder df <*> Decoder da = Decoder $ \bs -> case df bs of
    Left err -> Left err
    Right (f, bs1) -> case da bs1 of
      Left err -> Left err
      Right (a, bs2) -> Right (f a, bs2)

instance Monad Decoder where
  Decoder d >>= k = Decoder $ \bs -> case d bs of
    Left err -> Left err
    Right (x, bs1) -> runDecoder (k x) bs1

instance MonadFail Decoder where
  fail msg = Decoder $ \_ -> Left (T.pack msg)

-- | Abort decoding with a descriptive error message.
decodeFail :: Text -> Decoder a
decodeFail err = Decoder $ \_ -> Left err

-- | Consume exactly 'n' bytes from the input buffer.
getBytes :: Int -> Decoder ByteString
getBytes n
  | n < 0 = decodeFail "Negative byte count requested in getBytes"
  | otherwise = Decoder $ \bs ->
      if BS.length bs < n
        then
          Left $
            "Unexpected end of input: expected "
              <> T.pack (show n)
              <> " bytes, but only "
              <> T.pack (show (BS.length bs))
              <> " available"
        else Right (BS.splitAt n bs)

-- | Inspect the next 'n' bytes without consuming them.
peekBytes :: Int -> Decoder ByteString
peekBytes n
  | n < 0 = decodeFail "Negative byte count requested in peekBytes"
  | otherwise = Decoder $ \bs ->
      if BS.length bs < n
        then
          Left $
            "Unexpected end of input: expected "
              <> T.pack (show n)
              <> " bytes, but only "
              <> T.pack (show (BS.length bs))
              <> " available"
        else Right (BS.take n bs, bs)

-- | Consume all remaining bytes.
getRemainingBytes :: Decoder ByteString
getRemainingBytes = Decoder $ \bs -> Right (bs, BS.empty)

-- | A bidirectional Codec mapping between input type 'i' and output type 'o'.
data Codec i o = Codec
  { codecEncoder :: !(i -> Builder)
  , codecDecoder :: !(Decoder o)
  }

-- | A symmetric codec for a single column value.
type ValueCodec a = Codec a a

-- | An applicative row codec decoding into 'a' from a record of type 'r'.
type RowCodec r a = Codec r a

instance Functor (Codec i) where
  fmap f (Codec enc dec) = Codec enc (fmap f dec)

instance Applicative (Codec i) where
  pure x = Codec (const mempty) (pure x)
  Codec fEnc fDec <*> Codec aEnc aDec =
    Codec (\i -> fEnc i <> aEnc i) (fDec <*> aDec)

-- | Map over both the input and output types of a 'Codec'.
dimap :: (i' -> i) -> (o -> o') -> Codec i o -> Codec i' o'
dimap f g (Codec enc dec) = Codec (enc . f) (fmap g dec)

-- | Contravariantly map the input type of a 'Codec'.
lmap :: (i' -> i) -> Codec i o -> Codec i' o
lmap f (Codec enc dec) = Codec (enc . f) dec

-- | Covariantly map the output type of a 'Codec'.
rmap :: (o -> o') -> Codec i o -> Codec i o'
rmap g (Codec enc dec) = Codec enc (fmap g dec)

-- | Project a record field into an Applicative row codec using a getter.
field :: (r -> a) -> ValueCodec a -> Codec r a
field getter vc = Codec (codecEncoder vc . getter) (codecDecoder vc)

-- | Project an arbitrary input into an Applicative row codec.
fieldWith :: (r -> i) -> Codec i o -> Codec r o
fieldWith getter c = Codec (codecEncoder c . getter) (codecDecoder c)

-- | Encode a value directly into a strict 'ByteString'.
runBinaryEncoder :: Codec i o -> i -> ByteString
runBinaryEncoder codec val =
  BSL.toStrict $ Builder.toLazyByteString (codecEncoder codec val)

-- | Decode a strict 'ByteString' verifying no unparsed bytes remain.
runBinaryDecoder :: Codec i o -> ByteString -> Either Text o
runBinaryDecoder codec bs = case runDecoder (codecDecoder codec) bs of
  Left err -> Left err
  Right (val, rest)
    | BS.null rest -> Right val
    | otherwise ->
        Left $
          "Trailing unparsed bytes ("
            <> T.pack (show (BS.length rest))
            <> " bytes remaining)"

-- | Construct a framed PostgreSQL binary value codec for dynamic-length payloads.
makeFramedValueCodec ::
  -- | Payload length and builder
  (a -> (Int32, Builder)) ->
  -- | Raw payload decoder
  (ByteString -> Either Text a) ->
  ValueCodec a
makeFramedValueCodec rawEnc rawDec =
  Codec
    { codecEncoder = \val ->
        let (len, payload) = rawEnc val
         in Builder.int32BE len <> payload
    , codecDecoder = do
        lenBytes <- getBytes 4
        let len = readInt32BE lenBytes
        if len == -1
          then decodeFail "Unexpected NULL value for non-nullable column"
          else
            if len < -1
              then decodeFail $ "Invalid negative column length: " <> T.pack (show len)
              else do
                payload <- getBytes (fromIntegral len)
                case rawDec payload of
                  Left err -> decodeFail err
                  Right val -> pure val
    }

-- | Construct a framed PostgreSQL binary value codec for fixed-length payloads.
makeFramedFixedCodec ::
  -- | Fixed payload length in bytes
  Int32 ->
  -- | Payload builder
  (a -> Builder) ->
  -- | Raw payload decoder
  (ByteString -> Either Text a) ->
  ValueCodec a
makeFramedFixedCodec fixedLen rawEnc rawDec =
  Codec
    { codecEncoder = \val ->
        Builder.int32BE fixedLen <> rawEnc val
    , codecDecoder = do
        lenBytes <- getBytes 4
        let len = readInt32BE lenBytes
        if len == -1
          then decodeFail "Unexpected NULL value for non-nullable column"
          else
            if len < -1
              then decodeFail $ "Invalid negative column length: " <> T.pack (show len)
              else do
                payload <- getBytes (fromIntegral len)
                case rawDec payload of
                  Left err -> decodeFail err
                  Right val -> pure val
    }

-- | Lift a 'ValueCodec' to handle nullable columns via PostgreSQL libpq -1 marker.
nullable :: ValueCodec a -> ValueCodec (Maybe a)
nullable vc =
  Codec
    { codecEncoder = \case
        Nothing -> Builder.int32BE (-1)
        Just val -> codecEncoder vc val
    , codecDecoder = do
        lenBytes <- peekBytes 4
        let len = readInt32BE lenBytes
        if len == -1
          then do
            _ <- getBytes 4
            pure Nothing
          else
            if len < -1
              then decodeFail $ "Invalid negative column length: " <> T.pack (show len)
              else Just <$> codecDecoder vc
    }

-- | Codec for phantom primary and foreign keys.
idCodec :: ValueCodec a -> ValueCodec (Id table a)
idCodec = dimap unId Id

-- | Pair composite codec.
pairCodec :: ValueCodec a -> ValueCodec b -> Codec (a, b) (a, b)
pairCodec ca cb =
  (,)
    <$> field fst ca
    <*> field snd cb

-- | Triple composite codec.
tripleCodec :: ValueCodec a -> ValueCodec b -> ValueCodec c -> Codec (a, b, c) (a, b, c)
tripleCodec ca cb cc =
  (,,)
    <$> field (\(a, _, _) -> a) ca
    <*> field (\(_, b, _) -> b) cb
    <*> field (\(_, _, c) -> c) cc

-- | Quadruple composite codec.
quadCodec ::
  ValueCodec a ->
  ValueCodec b ->
  ValueCodec c ->
  ValueCodec d ->
  Codec (a, b, c, d) (a, b, c, d)
quadCodec ca cb cc cd =
  (,,,)
    <$> field (\(a, _, _, _) -> a) ca
    <*> field (\(_, b, _, _) -> b) cb
    <*> field (\(_, _, c, _) -> c) cc
    <*> field (\(_, _, _, d) -> d) cd

-- | Decode a 4-byte big-endian signed 32-bit integer.
readInt32BE :: ByteString -> Int32
readInt32BE bs = fromIntegral (readWord32BE bs)

-- | Decode a 4-byte big-endian unsigned 32-bit integer.
readWord32BE :: ByteString -> Word32
readWord32BE bs =
  (fromIntegral (BS.index bs 0) `shiftL` 24)
    .|. (fromIntegral (BS.index bs 1) `shiftL` 16)
    .|. (fromIntegral (BS.index bs 2) `shiftL` 8)
    .|. fromIntegral (BS.index bs 3)
