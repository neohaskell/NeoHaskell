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
    globalPosition :: Maybe StreamPosition
  }
  deriving (Eq, Show, Ord, Generic)


newtype StreamId = StreamId Text
  deriving (Eq, Show, Ord, Generic)


newtype StreamPosition = StreamPosition (Positive Int)
  deriving (Eq, Show, Ord, Generic)
