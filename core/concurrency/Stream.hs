module Stream (
  Stream,
  StreamMessage (..),
  new,
  readNext,
  writeItem,
  end,
  pushError,
  consume,
  consumeMaybe,
  toArray,
  fromArray,
) where

import Array (Array)
import Array qualified
import Basics
import Channel (Channel)
import Channel qualified
import Maybe (Maybe (..))
import Maybe qualified as Maybe
import Task (Task)
import Task qualified
import Text (Text)


-- | Message wrapper that signals stream state
data StreamMessage value
  = Item value -- Regular item
  | EndOfStream -- Stream has ended normally
  | StreamError Text -- Error occurred
  deriving (Show, Eq)


-- | Stream is a Channel of StreamMessages
newtype Stream value = Stream (Channel (StreamMessage value))
  deriving (Show)


-- | Create a new stream
new :: forall value error. Task error (Stream value)
new =
  Channel.new
    |> Task.map Stream


-- | Read next item from stream (blocks if empty)
-- Returns Nothing when stream ends, throws error if stream errors
readNext :: Stream value -> Task Text (Maybe value)
readNext (Stream stream) = do
  msg <- Channel.read stream
  case msg of
    Item value -> Task.yield (Just value)
    EndOfStream -> Task.yield Nothing
    StreamError err -> Task.throw err


-- | Write an item to the stream
writeItem :: value -> Stream value -> Task error Unit
writeItem item (Stream stream) = do
  let message = Item item
  Channel.write message stream


-- | Signal end of stream
end :: Stream value -> Task error Unit
end (Stream stream) = do
  Channel.write EndOfStream stream


-- | Signal error in stream
pushError :: Text -> Stream value -> Task error Unit
pushError err (Stream stream) = do
  Channel.write (StreamError err) stream


-- | Consume stream with a fold function
consume ::
  forall accumulator value.
  (accumulator -> value -> Task Text accumulator) ->
  accumulator ->
  Stream value ->
  Task Text accumulator
consume folder initial stream = do
  let loop accumulator = do
        maybeItem <- readNext stream
        case maybeItem of
          Nothing -> Task.yield accumulator
          Just item -> do
            nextAccumulator <- folder accumulator item
            loop nextAccumulator
      {-# INLINE loop #-}
  loop initial


-- | Consume stream with a fold function, returning Nothing if stream is empty
consumeMaybe ::
  forall accumulator value.
  (accumulator -> value -> Task Text accumulator) ->
  accumulator ->
  Stream value ->
  Task Text (Maybe accumulator)
consumeMaybe folder initial stream = do
  let loop maybeAccumulator = do
        maybeItem <- readNext stream
        case maybeItem of
          Nothing -> Task.yield maybeAccumulator
          Just item -> do
            let currentAccumulator = Maybe.withDefault initial maybeAccumulator
            nextAccumulator <- folder currentAccumulator item
            loop (Just nextAccumulator)
      {-# INLINE loop #-}
  loop Nothing


-- | Convert stream to array
toArray :: Stream value -> Task Text (Array value)
toArray stream = do
  consume (\accumulator item -> Task.yield (Array.push item accumulator)) Array.empty stream


-- | Create a stream from an array
-- All items from the array will be written to the stream, followed by an end signal
fromArray :: Array value -> Task error (Stream value)
fromArray array = do
  stream <- new
  array
    |> Task.forEach (\item -> writeItem item stream)
  end stream
  Task.yield stream
