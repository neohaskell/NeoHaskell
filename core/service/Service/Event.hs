module Service.Event (
  Event (..),
  StreamId (..),
  StreamPosition (..),
) where

import Core


data Event = Event
  { id :: Text, -- FIXME: Use Uuid
    streamId :: StreamId,
    position :: StreamPosition, -- Local position in stream

    -- | Global position across all streams. Nothing when event is created
    -- but not yet persisted to the global stream; Just when assigned a
    -- position in the global event ordering. Used for global event queries
    -- and maintaining causal ordering across different streams.
    globalPosition :: Maybe StreamPosition
  }
  deriving (Eq, Show, Ord, Generic)


newtype StreamId = StreamId Text
  deriving (Eq, Show, Ord, Generic)


newtype StreamPosition = StreamPosition (Positive Int)
  deriving (Eq, Show, Ord, Generic)
