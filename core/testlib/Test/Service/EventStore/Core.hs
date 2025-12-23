module Test.Service.EventStore.Core (CartEvent (..), newInsertion) where

import Core
import Json qualified
import Service.Event qualified as Event
import Service.Event.EventMetadata qualified as EventMetadata
import Task qualified
import Uuid qualified


-- | Example domain events for a shopping cart
-- Note: The Event instance is defined in Test.Service.Command.Core
-- where EntityOf CartEvent = CartEntity is also defined
data CartEvent
  = CartCreated {entityId :: Uuid}
  | ItemAdded {entityId :: Uuid, itemId :: Uuid, amount :: Int}
  | ItemRemoved {entityId :: Uuid, itemId :: Uuid}
  | CartCheckedOut {entityId :: Uuid}
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON CartEvent


instance Json.FromJSON CartEvent


instance Default CartEvent where
  def = CartCreated {entityId = def}


newInsertion :: Int -> Task _ (Event.Insertion CartEvent)
newInsertion index = do
  id <- Uuid.generate
  entityUuid <- Uuid.generate
  newMetadata <- EventMetadata.new
  let event = ItemAdded {entityId = entityUuid, itemId = id, amount = 10} -- Simple default event for tests
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