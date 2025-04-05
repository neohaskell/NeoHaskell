module Service.Event (
  Event,
  StreamId (..),
) where

import Core


data Event payload = Event
  { id :: Uuid,
    streamId :: StreamId,
    positionId :: Int,
    payload :: payload
  }
  deriving (Eq, Show, Ord, Generic)


newtype StreamId = StreamId Text
  deriving (Eq, Show, Ord, Generic)
