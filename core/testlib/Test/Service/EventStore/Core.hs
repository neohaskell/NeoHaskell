module Test.Service.EventStore.Core (MyEvent (..), newInsertion) where

import Core
import Json qualified
import Service.Event qualified as Event
import Service.Event.EventMetadata qualified as EventMetadata
import Task qualified
import Uuid qualified


data MyEvent
  = MyEvent
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON MyEvent


instance Json.FromJSON MyEvent


instance Default MyEvent where
  def = MyEvent


newInsertion :: Int -> Task _ (Event.Insertion MyEvent)
newInsertion index = do
  id <- Uuid.generate
  newMetadata <- EventMetadata.new
  let event = MyEvent
  let localPosition =
        fromIntegral index
          |> Event.StreamPosition
          |> Just
  let metadata =
        newMetadata
          { EventMetadata.localPosition = localPosition,
            EventMetadata.eventId = id
          }
  Task.yield
    Event.Insertion
      { id = id,
        event = event,
        metadata = metadata
      }