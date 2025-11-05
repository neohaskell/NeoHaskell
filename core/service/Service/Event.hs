module Service.Event (
  Event (..),
  InsertionPayload (..),
  StreamId (..),
  StreamPosition (..),
  EntityName (..),
  InsertionType (..),
  Insertion (..),
  InsertionSuccess (..),
  InsertionFailure (..),
  payloadFromEvents,
) where

import Core
import Service.Event.EntityName (EntityName (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId (StreamId (..))
import Service.Event.StreamPosition (StreamPosition (..))
import Task qualified
import Uuid qualified


data Event = Event
  { id :: Uuid,
    streamId :: StreamId,
    entityName :: EntityName,
    localPosition :: StreamPosition,
    globalPosition :: StreamPosition
  }
  deriving (Eq, Show, Ord, Generic)


data InsertionType
  = StreamCreation
  | InsertAfter StreamPosition
  | ExistingStream
  | AnyStreamState
  deriving (Eq, Show, Ord, Generic)


data InsertionPayload eventType = InsertionPayload
  { streamId :: StreamId,
    entityName :: EntityName,
    insertionType :: InsertionType,
    insertions :: Array (Insertion eventType)
  }
  deriving (Eq, Show, Ord, Generic)


eventToInsertion :: eventType -> Task _ (Insertion eventType)
eventToInsertion event = do
  id <- Uuid.generate
  metadata <- EventMetadata.new
  Task.yield
    Insertion
      { id,
        event,
        metadata
      }


payloadFromEvents :: EntityName -> StreamId -> Array eventType -> Task _ (InsertionPayload eventType)
payloadFromEvents entityName streamId events = do
  insertions <- events |> Task.mapArray eventToInsertion
  let insertionType = AnyStreamState
  Task.yield
    InsertionPayload
      { streamId,
        entityName,
        insertions,
        insertionType
      }


data Insertion eventType = Insertion
  { id :: Uuid,
    event :: eventType,
    metadata :: EventMetadata
  }
  deriving (Eq, Show, Ord, Generic)


data InsertionSuccess = InsertionSuccess
  { globalPosition :: StreamPosition,
    localPosition :: StreamPosition
  }
  deriving (Eq, Show, Ord, Generic)


data InsertionFailure
  = ConsistencyCheckFailed
  | InsertionFailed
  | PayloadTooLarge
  deriving (Eq, Show, Ord, Generic)
