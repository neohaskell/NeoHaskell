module Service.Event (
  Event (..),
  InsertionPayload (..),
  StreamId (..),
  StreamPosition (..),
  EntityName (..),
  fromInsertionPayload,
  InsertionType (..),
  InsertionSuccess (..),
  InsertionFailure (..),
) where

import Core
import Maybe qualified
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamId (StreamId (..))
import Service.Event.StreamPosition (StreamPosition (..))


data Event = Event
  { streamId :: StreamId,
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
