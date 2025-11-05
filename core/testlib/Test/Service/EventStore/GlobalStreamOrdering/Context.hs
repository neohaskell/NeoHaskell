module Test.Service.EventStore.GlobalStreamOrdering.Context (Context (..), initialize) where

import Array qualified
import Core
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata qualified as EventMetadata
import Service.EventStore (EventStore (..))
import Task qualified
import Test.Service.EventStore.Core (MyEvent (..))
import Uuid qualified


data Context = Context
  { streamCount :: Int,
    eventsPerStream :: Int,
    eventStreams :: Array (Array Event),
    store :: EventStore MyEvent
  }


initialize :: Task Text (EventStore MyEvent) -> Int -> Task Text Context
initialize newStore streamCount = do
  store <- newStore
  -- Create multiple streams with events
  let getStreamIds :: Task Text (Array (Event.EntityName, Event.StreamId))
      getStreamIds = do
        Array.fromLinkedList [0 .. streamCount - 1]
          |> Task.mapArray \_ -> do
            entityName <- Uuid.generate |> Task.map toText
            streamId <- Uuid.generate
            (Event.EntityName entityName, Event.StreamId streamId) |> Task.yield

  streamIds <- getStreamIds

  -- Create events for each stream (2 events per stream for testing)
  let eventsPerStream = 2 :: Int
  let getAllEvents :: Task Text (Array (Event.InsertionPayload MyEvent))
      getAllEvents =
        streamIds |> Task.mapArray \(entityName, streamId) -> do
          insertions <-
            Array.fromLinkedList [0 .. eventsPerStream - 1]
              |> Task.mapArray \eventIndex -> do
                id <- Uuid.generate
                newMetadata <- EventMetadata.new
                let event = MyEvent
                let localPosition = fromIntegral eventIndex |> Event.StreamPosition |> Just
                let metadata = newMetadata {EventMetadata.localPosition = localPosition}
                Task.yield
                  Event.Insertion
                    { id = id,
                      event = event,
                      metadata = metadata
                    }
          Task.yield
            Event.InsertionPayload
              { streamId = streamId,
                entityName = entityName,
                insertionType = Event.AnyStreamState,
                insertions
              }

  allEvents <- getAllEvents

  -- Append all events to their respective streams
  allEvents
    |> Task.forEach \event ->
      event
        |> store.insert
        |> Task.mapError toText
        |> discard

  -- Test readAllStreamEvents for each individual stream
  eventStreams <-
    streamIds
      |> Task.mapArray
        ( \(entityName, streamId) ->
            store.readAllStreamEvents entityName streamId
              |> Task.mapError toText
        )

  Task.yield
    Context
      { streamCount,
        eventsPerStream,
        eventStreams,
        store
      }
