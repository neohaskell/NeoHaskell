module Service.Event (
  Event (..),
  InsertionEvent (..),
  StreamId (..),
  StreamPosition (..),
  EntityId (..),
) where

import Core


newtype StreamId = StreamId Uuid
  deriving (Eq, Show, Ord, Generic)


newtype StreamPosition = StreamPosition Int
  deriving (Eq, Show, Ord, Generic)


newtype EntityId = EntityId Uuid
  deriving (Eq, Show, Ord, Generic)


data Event = Event
  { id :: Uuid,
    streamId :: StreamId,
    entityId :: EntityId,
    localPosition :: StreamPosition,
    globalPosition :: StreamPosition
  }
  deriving (Eq, Show, Ord, Generic)


data InsertionEvent = InsertionEvent
  { id :: Uuid,
    streamId :: StreamId,
    entityId :: EntityId,
    localPosition :: StreamPosition
  }
  deriving (Eq, Show, Ord, Generic)
