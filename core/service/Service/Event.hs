module Service.Event (
  Event (..),
  InsertionEvent (..),
  StreamId (..),
  StreamPosition (..),
  EntityId (..),
  fromInsertionEvent,
) where

import Core
import Service.Event.EntityId (EntityId (..))
import Service.Event.StreamId (StreamId (..))
import Service.Event.StreamPosition (StreamPosition (..))


data Event = Event
  { streamId :: StreamId,
    entityId :: EntityId,
    localPosition :: StreamPosition,
    globalPosition :: StreamPosition
  }
  deriving (Eq, Show, Ord, Generic)


data InsertionEvent eventType = InsertionEvent
  { streamId :: StreamId,
    entityId :: EntityId,
    localPosition :: StreamPosition,
    insertions :: Array (Insertion eventType)
  }
  deriving (Eq, Show, Ord, Generic)


data EventMetadata = EventMetadata
  { relatedUserSub :: Maybe Text,
    correlationId :: Maybe Text,
    causationId :: Maybe Text,
    createdAt :: DateTime,
    localPosition :: Maybe StreamPosition,
    globalPosition :: Maybe StreamPosition
  }
  deriving (Eq, Show, Ord, Generic)


data Insertion eventType = Insertion
  { id :: Uuid,
    event :: eventType,
    metadata :: EventMetadata
  }
  deriving (Eq, Show, Ord, Generic)


-- | Convert an insertion event to an event with a global position.
fromInsertionEvent :: StreamPosition -> InsertionEvent -> Event
fromInsertionEvent globalPosition event =
  Event
    { id = event.id,
      streamId = event.streamId,
      entityId = event.entityId,
      localPosition = event.localPosition,
      globalPosition
    }
