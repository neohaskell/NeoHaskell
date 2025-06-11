module Service.Event (
  Event (..),
  StreamId (..),
  StreamPosition (..),
  InsertionEvent (..),
) where

import Core


data Event = Event
  { id :: Uuid,
    streamId :: Uuid,
    entityId :: Uuid,
    localPosition :: StreamPosition,
    globalPosition :: StreamPosition
  }
  deriving (Eq, Show, Ord, Generic)


data InsertionEvent = InsertionEvent
  { id :: Uuid,
    streamId :: Uuid,
    entityId :: Uuid,
    localPosition :: StreamPosition
  }
  deriving (Eq, Show, Ord, Generic)


newtype StreamId = StreamId Text
  deriving (Eq, Show, Ord, Generic)


newtype StreamPosition = StreamPosition (Natural Int)
  deriving (Eq, Show, Ord, Generic)
