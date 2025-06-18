module Test.Service.EventStore.GlobalStreamOrdering.Context (Context (..), initialize) where

import Array qualified
import Core
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore (EventStore (..))
import Task qualified
import ToText (toText)
import Uuid qualified


data Context = Context
  { streamCount :: Int,
    eventsPerStream :: Int,
    eventStreams :: Array (Array Event),
    store :: EventStore
  }


initialize :: Task Text EventStore -> Int -> Task Text Context
initialize newStore streamCount = do
  store <- newStore
  -- Create multiple streams with events
  let getStreamIds :: Task Text (Array (Event.EntityId, Event.StreamId))
      getStreamIds =
        Array.fromLinkedList [0 .. streamCount - 1]
          |> Task.mapArray \_ -> do
            entityId <- Uuid.generate
            streamId <- Uuid.generate
            (Event.EntityId entityId, Event.StreamId streamId) |> Task.yield

  -- Create events for each stream (2 events per stream for testing)
  let eventsPerStream = 2 :: Int
  let getAllEvents :: Task Text (Array Event.InsertionEvent)
      getAllEvents = do
        streamIds <- getStreamIds
        foo <-
          streamIds |> Task.mapArray \(entityId, streamId) ->
            Array.fromLinkedList [0 .. eventsPerStream - 1]
              |> Task.mapArray \eventIndex -> do
                id <- Uuid.generate
                Event.InsertionEvent
                  { id = id,
                    streamId = streamId,
                    entityId = entityId,
                    localPosition = Event.StreamPosition eventIndex
                  }
                  |> Task.yield
        Array.flatten foo |> Task.yield

  streamIds <- getStreamIds
  allEvents <- getAllEvents

  -- Append all events to their respective streams
  allEvents
    |> Task.mapArray
      ( \event ->
          event
            |> store.appendToStream
            |> Task.mapError toText
      )
    |> discard

  -- Test readAllStreamEvents for each individual stream
  eventStreams <-
    streamIds
      |> Task.mapArray
        ( \(entityId, streamId) ->
            store.readAllStreamEvents entityId streamId
              |> Task.mapError toText
        )

  Task.yield
    Context
      { streamCount,
        eventsPerStream,
        eventStreams,
        store
      }
