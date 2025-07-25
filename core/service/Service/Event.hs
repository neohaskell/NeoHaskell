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
