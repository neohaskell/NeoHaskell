module Service.TestHelpers
  ( insertTestEvent,
    insertTypedEvent,
  )
where

import Core
import Json qualified
import Service.Event (Insertion (..), InsertionPayload (..))
import Service.Event.EntityName (EntityName)
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore (..))
import Task qualified
import Uuid qualified


-- | Helper to insert a test event into the event store.
insertTestEvent :: EventStore Json.Value -> EntityName -> Task Text Unit
insertTestEvent eventStore entityName = do
  eventId <- Uuid.generate
  streamId <- StreamId.new
  metadata <- EventMetadata.new

  let insertion =
        Insertion
          { id = eventId,
            event = Json.encode (),
            metadata = metadata
          }

  let payload =
        InsertionPayload
          { streamId = streamId,
            entityName = entityName,
            insertionType = AnyStreamState,
            insertions = [insertion]
          }

  eventStore.insert payload
    |> Task.mapError toText
    |> discard


-- | Helper to insert a typed event into the event store.
insertTypedEvent ::
  forall event.
  (Json.ToJSON event) =>
  EventStore Json.Value ->
  EntityName ->
  Uuid ->
  event ->
  Task Text Unit
insertTypedEvent eventStore entityName streamUuid event = do
  eventId <- Uuid.generate
  let streamId = StreamId.fromText (Uuid.toText streamUuid)
  metadata <- EventMetadata.new

  let insertion =
        Insertion
          { id = eventId,
            event = Json.encode event,
            metadata = metadata
          }

  let payload =
        InsertionPayload
          { streamId = streamId,
            entityName = entityName,
            insertionType = AnyStreamState,
            insertions = [insertion]
          }

  eventStore.insert payload
    |> Task.mapError toText
    |> discard
