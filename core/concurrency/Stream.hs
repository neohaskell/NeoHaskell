module Stream (
  Stream,
  StreamMessage (..),
  new,
  readNext,
  writeItem,
  endStream,
  errorStream,
  consumeStream,
  streamToArray,
) where

import Array (Array)
import Array qualified
import Basics
import Channel (Channel)
import Channel qualified
import Maybe (Maybe (..))
import Task (Task)
import Task qualified
import Text (Text)
import ToText (Show (..))


-- | Message wrapper that signals stream state
data StreamMessage value
  = Item value -- Regular item
  | EndOfStream -- Stream has ended normally
  | StreamError Text -- Error occurred
  deriving (Show, Eq)


-- | Stream is a Channel of StreamMessages
newtype Stream value = Stream (Channel (StreamMessage value))


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
endStream :: Stream value -> Task error Unit
endStream (Stream stream) = do
  Channel.write EndOfStream stream


-- | Signal error in stream
errorStream :: Text -> Stream value -> Task error Unit
errorStream err (Stream stream) = do
  Channel.write (StreamError err) stream


-- | Consume stream with a fold function
consumeStream ::
  forall accumulator value.
  (accumulator -> value -> Task Text accumulator) ->
  accumulator ->
  Stream value ->
  Task Text accumulator
consumeStream folder initial stream = do
  let loop accumulator = do
        maybeItem <- readNext stream |> Task.mapError identity
        case maybeItem of
          Nothing -> Task.yield accumulator
          Just item -> do
            nextAccumulator <- folder accumulator item
            loop nextAccumulator
      {-# INLINE loop #-}
  loop initial


-- | Convert stream to array
streamToArray :: Stream value -> Task Text (Array value)
streamToArray stream = do
  consumeStream (\accumulator item -> Task.yield (Array.push item accumulator)) Array.empty stream
