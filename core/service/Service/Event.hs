module Service.Event (
  Event (..),
  InsertionPayload (..),
  StreamId (..),
  ToStreamId (..),
  StreamPosition (..),
  EntityName (..),
  InsertionType (..),
  Insertion (..),
  InsertionSuccess (..),
  InsertionFailure (..),
  payloadFromEvents,
) where

import Array (Array)
import Basics
import Json qualified
import Service.Event.EntityName (EntityName (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId (StreamId (..), ToStreamId (..))
import Service.Event.StreamPosition (StreamPosition (..))
import Task (Task (..))
import Task qualified
import Text (Text)
import Uuid (Uuid)
import Uuid qualified


data Event eventType = Event
  { entityName :: EntityName,
    streamId :: StreamId,
    event :: eventType,
    metadata :: EventMetadata
  }
  deriving (Eq, Show, Ord, Generic)


instance (Json.FromJSON eventType) => Json.FromJSON (Event eventType)


instance (Json.ToJSON eventType) => Json.ToJSON (Event eventType)


data InsertionType
  = StreamCreation
  | InsertAfter StreamPosition
  | ExistingStream
  | AnyStreamState
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON InsertionType


data InsertionPayload eventType = InsertionPayload
  { streamId :: StreamId,
    entityName :: EntityName,
    insertionType :: InsertionType,
    insertions :: Array (Insertion eventType)
  }
  deriving (Eq, Show, Ord, Generic)


instance (Json.ToJSON eventType) => Json.ToJSON (InsertionPayload eventType)


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


instance (Json.ToJSON eventType) => Json.ToJSON (Insertion eventType)


data InsertionSuccess = InsertionSuccess
  { globalPosition :: StreamPosition,
    localPosition :: StreamPosition
  }
  deriving (Eq, Show, Ord, Generic)


data InsertionFailure
  = ConsistencyCheckFailed
  | InsertionFailed Text
  | PayloadTooLarge
  | EmptyPayload
  deriving (Eq, Show, Ord, Generic)
