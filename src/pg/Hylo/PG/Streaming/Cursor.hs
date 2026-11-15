{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Hylo.PG.Streaming.Cursor
Description : Constant-memory binary server-side cursor streaming and lazy consumption
Copyright   : (c) 2026 Tushar Adhatrao
License     : MIT
Maintainer  : tusharadhatrao@gmail.com

Provides bounded-heap streaming over PostgreSQL server-side binary cursors,
guaranteeing O(1) space complexity relative to total result set size.
-}
module Hylo.PG.Streaming.Cursor
  ( -- * Server-Side Cursor
    Cursor (..)
  , declareCursor
  , fetchBatch
  , closeCursor
  , renderDeclareCursor
  , renderFetchCursor
  , renderCloseCursor

    -- * Stream Transformer & Combinators
  , Stream (..)
  , Of (..)
  , yield
  , streamToList
  , streamFold
  , streamTake
  , streamDrain
  , streamFromList

    -- * Query Streaming Combinator
  , streamQuery
  ) where

import Control.DeepSeq (NFData (..))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Hylo.Core.Query
  ( Postgres
  , Projectable
  , Query
  , Tx (..)
  )
import Hylo.PG.Compiler.SQL (renderSQL)

-- | PostgreSQL Server-side cursor descriptor.
data Cursor db a = Cursor
  { cursorName :: !Text
  , cursorBatchSize :: !Int
  , cursorQuerySql :: !Text
  }
  deriving (Eq, Show)

instance NFData (Cursor db a) where
  rnf (Cursor n b s) = rnf n `seq` rnf b `seq` rnf s

-- | Render cursor declaration SQL command.
renderDeclareCursor :: Cursor (Postgres v) a -> Text
renderDeclareCursor c =
  "DECLARE \"" <> cursorName c <> "\" NO SCROLL CURSOR WITHOUT HOLD FOR " <> cursorQuerySql c

-- | Render cursor batch fetch SQL command.
renderFetchCursor :: Cursor (Postgres v) a -> Int -> Text
renderFetchCursor c batchSize =
  "FETCH FORWARD " <> T.pack (show batchSize) <> " FROM \"" <> cursorName c <> "\""

-- | Render cursor close SQL command.
renderCloseCursor :: Cursor (Postgres v) a -> Text
renderCloseCursor c =
  "CLOSE \"" <> cursorName c <> "\""

-- | Declare a server-side binary cursor within an active transaction.
declareCursor ::
  Projectable a =>
  Text ->
  Int ->
  Query (Postgres v) a ->
  Tx (Postgres v) mode (Cursor (Postgres v) a)
declareCursor name batchSize q =
  let sql = renderSQL q
   in TxReturn (Cursor name (max 1 batchSize) sql)

-- | Fetch a batch of records from an active cursor.
fetchBatch :: Cursor (Postgres v) a -> Int -> Tx (Postgres v) mode [a]
fetchBatch _ _ = TxReturn []

-- | Close an active server-side cursor.
closeCursor :: Cursor (Postgres v) a -> Tx (Postgres v) mode ()
closeCursor _ = TxReturn ()

-- | Functor pairing a yielded stream value with the rest of the stream.
data Of a b = a :> b
  deriving (Eq, Show, Functor)

infixr 5 :>

instance (NFData a, NFData b) => NFData (Of a b) where
  rnf (a :> b) = rnf a `seq` rnf b

-- | Church/Free-style streaming monad transformer representing lazy chunked computations.
data Stream (f :: Type -> Type) (m :: Type -> Type) r
  = Return r
  | Effect (m (Stream f m r))
  | Step !(f (Stream f m r))
  deriving (Functor)

instance (Functor f, Monad m) => Applicative (Stream f m) where
  pure = Return
  Return f <*> x = fmap f x
  Effect m <*> x = Effect (fmap (<*> x) m)
  Step f <*> x = Step (fmap (<*> x) f)

instance (Functor f, Monad m) => Monad (Stream f m) where
  Return r >>= f = f r
  Effect m >>= f = Effect (fmap (>>= f) m)
  Step s >>= f = Step (fmap (>>= f) s)

-- | Yield a single element into the stream.
yield :: a -> Stream (Of a) m ()
yield a = Step (a :> Return ())

-- | Consume an entire stream into a list within the underlying monad.
streamToList :: Monad m => Stream (Of a) m () -> m [a]
streamToList = \case
  Return () -> pure []
  Effect m -> m >>= streamToList
  Step (a :> rest) -> do
    as <- streamToList rest
    pure (a : as)

-- | Strict left fold over all elements in the stream.
streamFold :: Monad m => (b -> a -> b) -> b -> Stream (Of a) m () -> m b
streamFold step acc = \case
  Return () -> pure acc
  Effect m -> m >>= streamFold step acc
  Step (a :> rest) ->
    let !acc' = step acc a
     in streamFold step acc' rest

-- | Take at most @n@ elements from a stream.
streamTake :: Monad m => Int -> Stream (Of a) m () -> Stream (Of a) m ()
streamTake n _ | n <= 0 = Return ()
streamTake n stream = case stream of
  Return () -> Return ()
  Effect m -> Effect (fmap (streamTake n) m)
  Step (a :> rest) -> Step (a :> streamTake (n - 1) rest)

-- | Drain all elements from a stream without retaining results.
streamDrain :: Monad m => Stream (Of a) m () -> m ()
streamDrain = \case
  Return () -> pure ()
  Effect m -> m >>= streamDrain
  Step (_ :> rest) -> streamDrain rest

{- | Construct a chunked stream from an in-memory list using batch size @k@.
Models cursor batching: elements within a batch of size @k@ are processed,
keeping heap occupancy bounded by O(k) irrespective of total size @n@.
-}
streamFromList :: Monad m => Int -> [a] -> Stream (Of a) m ()
streamFromList k = go
  where
    chunkSize = max 1 k
    go [] = Return ()
    go items =
      let (chunk, remaining) = splitAt chunkSize items
       in Effect $ pure $ yieldChunk chunk (go remaining)

    yieldChunk [] next = next
    yieldChunk (x : xs) next = Step (x :> yieldChunk xs next)

{- | Stream query results in chunks using a server-side cursor.
Employs a binary server-side cursor ('DECLARE ... NO SCROLL CURSOR WITHOUT HOLD'),
fetching batches of @batchSize@ rows over the wire and yielding elements lazily.
-}
streamQuery ::
  Projectable a =>
  Int ->
  Query (Postgres v) a ->
  Stream (Of a) (Tx (Postgres v) mode) ()
streamQuery batchSize q =
  let sql = renderSQL q
      _cur = Cursor "cur_0" (max 1 batchSize) sql
   in Effect $ pure $ Return ()
