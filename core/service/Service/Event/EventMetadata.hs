module Service.Event.EventMetadata (
  EventMetadata (..),
  new,
) where

import Core
import DateTime qualified
import Json (FromJSON, ToJSON)
import Service.Event.StreamPosition (StreamPosition)
import Task qualified
import Uuid qualified


data EventMetadata = EventMetadata
  { eventId :: Uuid,
    relatedUserSub :: Maybe Text,
    correlationId :: Maybe Text,
    causationId :: Maybe Text,
    createdAt :: DateTime,
    localPosition :: Maybe StreamPosition,
    globalPosition :: Maybe StreamPosition
  }
  deriving (Eq, Show, Ord, Generic)


instance ToJSON EventMetadata


instance FromJSON EventMetadata


new :: Task _ EventMetadata
new = do
  id <- Uuid.generate
  now <- DateTime.now
  Task.yield
    EventMetadata
      { eventId = id,
        relatedUserSub = Nothing,
        correlationId = Nothing,
        causationId = Nothing,
        createdAt = now,
        localPosition = Nothing,
        globalPosition = Nothing
      }