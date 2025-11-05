module Service.Event.EventMetadata (
  EventMetadata (..),
  new,
) where

import Core
import DateTime qualified
import Service.Event.StreamPosition (StreamPosition)
import Task qualified


data EventMetadata = EventMetadata
  { relatedUserSub :: Maybe Text,
    correlationId :: Maybe Text,
    causationId :: Maybe Text,
    createdAt :: DateTime,
    localPosition :: Maybe StreamPosition,
    globalPosition :: Maybe StreamPosition
  }
  deriving (Eq, Show, Ord, Generic)


new :: Task _ EventMetadata
new = do
  now <- DateTime.now
  Task.yield
    EventMetadata
      { relatedUserSub = Nothing,
        correlationId = Nothing,
        causationId = Nothing,
        createdAt = now,
        localPosition = Nothing,
        globalPosition = Nothing
      }