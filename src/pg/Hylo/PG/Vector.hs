{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Hylo.PG.Vector
Description : Type-Level Model & Dimension-Checked Vector Search (pgvector)
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides compile-time checked AI embedding search backed by PostgreSQL's @pgvector@
extension. Vector models and dimensionalities are phantom-typed, preventing cross-model
and dimension mismatches at compile time with '[Hylo Error E007]'. Includes native binary
wire format encoding/decoding and distance operators:

* '<=>' / 'cosineDistance': Cosine distance
* '<->' / 'l2Distance': L2 Euclidean distance
* '<#>' / 'innerProductDistance': Negative inner product distance
-}
module Hylo.PG.Vector
  ( -- * Type-Level Embedding Representation
    Embedding (..)
  , mkEmbedding
  , embeddingFromList
  , embeddingDim

    -- * Distance Operators & Class
  , VectorDistance (..)
  , (<=>)
  , (<->)
  , (<#>)
  , cosineDistanceExpr
  , l2DistanceExpr
  , innerProductDistanceExpr
  , pureCosineDistance
  , pureL2Distance
  , pureInnerProductDistance

    -- * Binary Wire Codec (pgvector specification)
  , encodeBinaryVector
  , decodeBinaryVector

    -- * Rendering Utilities
  , renderVectorText
  , renderVectorSql

    -- * Indexing Descriptors & Capabilities
  , SupportsVectorSearch
  , VectorIndexType (..)
  , DistanceMetric (..)

    -- * Re-exported Type-Level Primitives
  , AssertMatchingVectors (..)
  , AssertSameEmbedding
  , KnownNat
  , KnownSymbol
  , Nat
  , Symbol
  ) where

import Control.DeepSeq (NFData (..))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word16, Word32)
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Hylo.Core.Expr (QExpr (..), ToQExpr (..))
import Hylo.Core.Query (Postgres)
import Hylo.Core.TypeError (AssertMatchingVectors (..))

-- | Type synonym matching formal specification notation.
type AssertSameEmbedding m1 d1 m2 d2 = AssertMatchingVectors m1 d1 m2 d2

-- | Phantom-typed vector wrapper tracking embedding model name and vector dimensionality.
newtype Embedding (model :: Symbol) (dim :: Nat) = Embedding
  { unEmbedding :: Vector Float
  }
  deriving (Eq, Show, Generic)

instance NFData (Embedding model dim) where
  rnf (Embedding v) = Vector.foldl' (\() f -> rnf f) () v

instance (KnownNat dim) => ToQExpr (Embedding model dim) (Embedding model dim) where
  toQExpr vec = LitExpr (renderVectorSql vec)

-- | Safely construct an 'Embedding', ensuring the input vector matches the type-level dimension.
mkEmbedding :: forall model dim. (KnownNat dim) => Vector Float -> Maybe (Embedding model dim)
mkEmbedding v
  | fromIntegral (Vector.length v) == natVal (Proxy @dim) = Just (Embedding v)
  | otherwise = Nothing

-- | Construct an 'Embedding' from a list of floats if the list length matches @dim@.
embeddingFromList :: forall model dim. (KnownNat dim) => [Float] -> Maybe (Embedding model dim)
embeddingFromList xs = mkEmbedding (Vector.fromList xs)

-- | Read the type-level dimension of an 'Embedding'.
embeddingDim :: forall model dim. (KnownNat dim) => Embedding model dim -> Integer
embeddingDim _ = toInteger (natVal (Proxy @dim))

-- | Typeclass abstracting vector distance computation across SQL 'QExpr' ASTs and pure 'Embedding' values.
class VectorDistance a b res | a b -> res where
  -- | Cosine distance metric: @1.0 - (u · v) / (||u|| * ||v||)@.
  cosineDistance :: a -> b -> res

  -- | L2 Euclidean distance metric: @sqrt(sum((u_i - v_i)^2))@.
  l2Distance :: a -> b -> res

  -- | Negative inner product distance metric: @-(u · v)@.
  innerProductDistance :: a -> b -> res

instance
  (AssertMatchingVectors m1 d1 m2 d2) =>
  VectorDistance (QExpr (Embedding m1 d1)) (QExpr (Embedding m2 d2)) (QExpr Double)
  where
  cosineDistance v1 v2 =
    matchingVectorsDict (Proxy @m1) (Proxy @d1) (Proxy @m2) (Proxy @d2) `seq`
      BinOpExpr "<=>" v1 v2
  l2Distance v1 v2 =
    matchingVectorsDict (Proxy @m1) (Proxy @d1) (Proxy @m2) (Proxy @d2) `seq`
      BinOpExpr "<->" v1 v2
  innerProductDistance v1 v2 =
    matchingVectorsDict (Proxy @m1) (Proxy @d1) (Proxy @m2) (Proxy @d2) `seq`
      BinOpExpr "<#>" v1 v2

instance
  (AssertMatchingVectors m1 d1 m2 d2) =>
  VectorDistance (Embedding m1 d1) (Embedding m2 d2) Double
  where
  cosineDistance v1 v2 =
    matchingVectorsDict (Proxy @m1) (Proxy @d1) (Proxy @m2) (Proxy @d2) `seq`
      pureCosineDistance v1 v2
  l2Distance v1 v2 =
    matchingVectorsDict (Proxy @m1) (Proxy @d1) (Proxy @m2) (Proxy @d2) `seq`
      pureL2Distance v1 v2
  innerProductDistance v1 v2 =
    matchingVectorsDict (Proxy @m1) (Proxy @d1) (Proxy @m2) (Proxy @d2) `seq`
      pureInnerProductDistance v1 v2

infix 4 <=>

-- | Infix alias for 'cosineDistance'.
(<=>) :: (VectorDistance a b res) => a -> b -> res
(<=>) = cosineDistance

infix 4 <->

-- | Infix alias for 'l2Distance'.
(<->) :: (VectorDistance a b res) => a -> b -> res
(<->) = l2Distance

infix 4 <#>

-- | Infix alias for 'innerProductDistance'.
(<#>) :: (VectorDistance a b res) => a -> b -> res
(<#>) = innerProductDistance

-- | Explicitly typed SQL expression variant for 'cosineDistance'.
cosineDistanceExpr ::
  forall m1 d1 m2 d2.
  (AssertMatchingVectors m1 d1 m2 d2) =>
  QExpr (Embedding m1 d1) ->
  QExpr (Embedding m2 d2) ->
  QExpr Double
cosineDistanceExpr = cosineDistance

-- | Explicitly typed SQL expression variant for 'l2Distance'.
l2DistanceExpr ::
  forall m1 d1 m2 d2.
  (AssertMatchingVectors m1 d1 m2 d2) =>
  QExpr (Embedding m1 d1) ->
  QExpr (Embedding m2 d2) ->
  QExpr Double
l2DistanceExpr = l2Distance

-- | Explicitly typed SQL expression variant for 'innerProductDistance'.
innerProductDistanceExpr ::
  forall m1 d1 m2 d2.
  (AssertMatchingVectors m1 d1 m2 d2) =>
  QExpr (Embedding m1 d1) ->
  QExpr (Embedding m2 d2) ->
  QExpr Double
innerProductDistanceExpr = innerProductDistance

-- | Pure mathematical cosine distance computation.
pureCosineDistance :: Embedding m1 d1 -> Embedding m2 d2 -> Double
pureCosineDistance (Embedding u) (Embedding v)
  | u == v = 0.0
  | otherwise =
      let dot = Vector.sum (Vector.zipWith (*) u v)
          normU = sqrt (Vector.sum (Vector.map (\x -> x * x) u))
          normV = sqrt (Vector.sum (Vector.map (\x -> x * x) v))
          denom = normU * normV
       in if denom == 0.0
            then 0.0
            else
              let sim = fromRational (toRational (dot / denom))
                  dist = 1.0 - sim
               in max 0.0 dist

-- | Pure mathematical L2 Euclidean distance computation.
pureL2Distance :: Embedding m1 d1 -> Embedding m2 d2 -> Double
pureL2Distance (Embedding u) (Embedding v)
  | u == v = 0.0
  | otherwise =
      let diffs = Vector.zipWith (-) u v
          sumSq = Vector.sum (Vector.map (\x -> x * x) diffs)
       in sqrt (fromRational (toRational sumSq))

-- | Pure mathematical negative inner product distance computation matching pgvector semantics.
pureInnerProductDistance :: Embedding m1 d1 -> Embedding m2 d2 -> Double
pureInnerProductDistance (Embedding u) (Embedding v) =
  let dot = Vector.sum (Vector.zipWith (*) u v)
   in -fromRational (toRational dot)

{- | Encode an 'Embedding' directly into pgvector's binary wire format:
@[uint16 dim (big-endian)] [uint16 unused (0x0000)] [dim * float32 (IEEE 754 big-endian)]@
-}
encodeBinaryVector :: Embedding model dim -> ByteString
encodeBinaryVector (Embedding vec) =
  let dimInt = fromIntegral (Vector.length vec) :: Word16
      header =
        [ fromIntegral (dimInt `shiftR` 8)
        , fromIntegral (dimInt .&. 0xFF)
        , 0x00
        , 0x00
        ]
      packFloat32 f =
        let w = castFloatToWord32 f
         in [ fromIntegral (w `shiftR` 24)
            , fromIntegral ((w `shiftR` 16) .&. 0xFF)
            , fromIntegral ((w `shiftR` 8) .&. 0xFF)
            , fromIntegral (w .&. 0xFF)
            ]
      payload = concatMap packFloat32 (Vector.toList vec)
   in BS.pack (header ++ payload)

-- | Decode a pgvector binary wire buffer into a typed 'Embedding'.
decodeBinaryVector ::
  forall model dim.
  (KnownNat dim) =>
  ByteString ->
  Either String (Embedding model dim)
decodeBinaryVector bs
  | BS.length bs < 4 = Left "Vector binary buffer underflow: header requires at least 4 bytes"
  | otherwise =
      let b0 = fromIntegral (BS.index bs 0) :: Word16
          b1 = fromIntegral (BS.index bs 1) :: Word16
          dim = (b0 `shiftL` 8) .|. b1
          expectedDim = fromIntegral (natVal (Proxy @dim)) :: Word16
       in if dim /= expectedDim
            then
              Left
                ( "Vector dimension mismatch: expected "
                    ++ show expectedDim
                    ++ " but received "
                    ++ show dim
                )
            else
              let expectedLen = 4 + fromIntegral dim * 4
               in if BS.length bs /= expectedLen
                    then
                      Left
                        ( "Vector binary payload length mismatch: expected "
                            ++ show expectedLen
                            ++ " bytes but received "
                            ++ show (BS.length bs)
                        )
                    else
                      let payload = BS.drop 4 bs
                          parseFloats :: Int -> ByteString -> Either String [Float]
                          parseFloats 0 _ = Right []
                          parseFloats n remBytes
                            | BS.length remBytes < 4 = Left "Premature end of float stream"
                            | otherwise = do
                                let w0 = fromIntegral (BS.index remBytes 0) :: Word32
                                    w1 = fromIntegral (BS.index remBytes 1) :: Word32
                                    w2 = fromIntegral (BS.index remBytes 2) :: Word32
                                    w3 = fromIntegral (BS.index remBytes 3) :: Word32
                                    w = (w0 `shiftL` 24) .|. (w1 `shiftL` 16) .|. (w2 `shiftL` 8) .|. w3
                                    fl = castWord32ToFloat w
                                rest <- parseFloats (n - 1) (BS.drop 4 remBytes)
                                Right (fl : rest)
                       in case parseFloats (fromIntegral dim) payload of
                            Left err -> Left err
                            Right fls -> Right (Embedding (Vector.fromList fls))

-- | Render an 'Embedding' as text array literal: @[1.0, 2.0, 3.0]@.
renderVectorText :: Embedding model dim -> Text
renderVectorText (Embedding v) =
  "[" <> T.intercalate ", " (map (T.pack . show) (Vector.toList v)) <> "]"

-- | Render an 'Embedding' as PostgreSQL typed vector expression: @'[1.0, 2.0, 3.0]'::vector(3)@.
renderVectorSql :: forall model dim. (KnownNat dim) => Embedding model dim -> Text
renderVectorSql vec =
  "'" <> renderVectorText vec <> "'::vector(" <> T.pack (show (natVal (Proxy @dim))) <> ")"

-- | Backend capability marker for database engines supporting vector similarity search.
class SupportsVectorSearch db

instance SupportsVectorSearch Postgres

-- | Vector index structures supported by pgvector.
data VectorIndexType
  = HNSW
  | IVFFlat
  deriving (Eq, Show, Generic)

instance NFData VectorIndexType

-- | Distance metrics supported for pgvector index indexing.
data DistanceMetric
  = CosineMetric
  | L2Metric
  | InnerProductMetric
  deriving (Eq, Show, Generic)

instance NFData DistanceMetric
