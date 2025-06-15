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
  let getStreamIds :: Task Text (Array Event.StreamId)
      getStreamIds =
        Array.fromLinkedList [0 .. streamCount - 1]
          |> Task.mapArray \_ -> do
            id <- Uuid.generate
            Event.StreamId id |> Task.yield

  -- Create events for each stream (2 events per stream for testing)
  let eventsPerStream = 2 :: Int
  let getAllEvents :: Task Text (Array Event.InsertionEvent)
      getAllEvents = do
        streamIds <- getStreamIds
        foo <-
          streamIds |> Task.mapArray \streamId ->
            Array.fromLinkedList [0 .. eventsPerStream - 1]
              |> Task.mapArray \eventIndex -> do
                id <- Uuid.generate
                entityId <- Uuid.generate
                Event.InsertionEvent
                  { id = id,
                    streamId = streamId,
                    entityId = Event.EntityId entityId,
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
        ( \streamId ->
            streamId
              |> store.readAllStreamEvents
              |> Task.mapError toText
        )

  Task.yield
    Context
      { streamCount,
        eventsPerStream = eventsPerStream,
        eventStreams,
        store
      }
