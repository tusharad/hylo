{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Hylo.PG.Search.RRF
Description : Reciprocal Rank Fusion (RRF) mathematics, scoring formulas & ranking laws
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides pure mathematical implementations and formal invariant validators for
Reciprocal Rank Fusion (RRF). RRF fuses distinct ranking signals (e.g. lexical BM25
and dense vector cosine similarity) into a single unified score without requiring
unstable score calibration or normalization:

\[
RRF_k(d) = \sum_{m \in \text{Modalities}} \frac{w_m}{k + \text{rank}_m(d)}
\]

Formal Laws:
1. **Monotonicity**: If item d1 ranks equal or better than item d2 across all modalities,
   its fused RRF score is greater than or equal to d2's.
2. **Score Range Bound**: For \(M\) modalities and smoothing constant \(k > 0\),
   \(0 < RRF_k(d) \le \frac{M}{k + 1}\).
3. **Symmetry**: Modality ordering is commutative; swapping lexical and vector ranks
   preserves identical scores and orderings.
-}
module Hylo.PG.Search.RRF
  ( -- * Pure RRF Mathematical Formulas
    computeRRF
  , computeRRF2
  , weightedRRF
  , rrfUpperBound

    -- * RRF Configuration
  , RRFConfig (..)
  , defaultRRFConfig

    -- * Invariant Property Verifiers
  , isMonotonicRRF
  , isBoundedRRF
  , isSymmetricRRF
  ) where

import Control.DeepSeq (NFData (..))
import GHC.Generics (Generic)

-- | Configuration parameters for Reciprocal Rank Fusion.
data RRFConfig = RRFConfig
  { rrfK :: !Int
  -- ^ Smoothing constant (typically 60).
  , rrfLimit :: !Int
  -- ^ Maximum number of fused results to return.
  , rrfCandidateLimit :: !(Maybe Int)
  -- ^ Maximum candidates pulled from each individual modality index.
  }
  deriving (Eq, Show, Generic)

instance NFData RRFConfig

-- | Default RRF configuration with standard k=60 and 20 results.
defaultRRFConfig :: RRFConfig
defaultRRFConfig =
  RRFConfig
    { rrfK = 60
    , rrfLimit = 20
    , rrfCandidateLimit = Just 100
    }

-- | Compute pure Reciprocal Rank Fusion score over an arbitrary number of 1-based ranks.
computeRRF :: (Fractional a) => a -> [Int] -> a
computeRRF _ [] = 0.0
computeRRF k ranks = sum [1.0 / (k + fromIntegral r) | r <- ranks, r > 0]

-- | Specialized 2-modality RRF calculation (e.g. lexical and vector).
computeRRF2 :: Double -> Int -> Int -> Double
computeRRF2 k r1 r2 =
  let s1 = if r1 > 0 then 1.0 / (k + fromIntegral r1) else 0.0
      s2 = if r2 > 0 then 1.0 / (k + fromIntegral r2) else 0.0
   in s1 + s2

-- | Compute weighted Reciprocal Rank Fusion score given (weight, 1-based rank) pairs.
weightedRRF :: (Fractional a) => a -> [(a, Int)] -> a
weightedRRF _ [] = 0.0
weightedRRF k scoredRanks =
  sum [w / (k + fromIntegral r) | (w, r) <- scoredRanks, r > 0]

-- | Theoretical maximum RRF score achievable when an item ranks 1st in all @m@ modalities.
rrfUpperBound :: (Fractional a) => a -> Int -> a
rrfUpperBound k m = fromIntegral m / (k + 1.0)

{- | Invariant 1: Monotonicity verifier.
Given ranks (r1a, r1b) for item 1 and (r2a, r2b) for item 2:
if r1a <= r2a and r1b <= r2b (better or equal ranks), then RRF(item 1) >= RRF(item 2).
-}
isMonotonicRRF :: Double -> (Int, Int) -> (Int, Int) -> Bool
isMonotonicRRF k (r1a, r1b) (r2a, r2b) =
  let score1 = computeRRF2 k r1a r1b
      score2 = computeRRF2 k r2a r2b
   in not (r1a <= r2a && r1b <= r2b) || (score1 >= score2 - 1e-9)

{- | Invariant 2: Boundedness verifier.
For strictly positive ranks, 0 < RRF_k <= 2 / (k + 1).
-}
isBoundedRRF :: Double -> Int -> Int -> Bool
isBoundedRRF k r1 r2
  | r1 <= 0 || r2 <= 0 || k <= 0 = True
  | otherwise =
      let score = computeRRF2 k r1 r2
          upperLimit = rrfUpperBound k 2
       in score > 0.0 && score <= upperLimit + 1e-9

{- | Invariant 3: Symmetry verifier.
RRF_k(r1, r2) == RRF_k(r2, r1).
-}
isSymmetricRRF :: Double -> Int -> Int -> Bool
isSymmetricRRF k r1 r2 =
  let s12 = computeRRF2 k r1 r2
      s21 = computeRRF2 k r2 r1
   in abs (s12 - s21) < 1e-9
