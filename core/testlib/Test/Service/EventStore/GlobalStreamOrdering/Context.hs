module Test.Service.EventStore.GlobalStreamOrdering.Context (Context (..), initialize) where

import Array qualified
import Core
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore (EventStore (..))
import Task qualified
import ToText (toText)


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
  let streamIds :: Array Event.StreamId
      streamIds = do
        Array.initialize streamCount \index -> do
          Event.StreamId [fmt|stream-#{index}|]

  -- Create events for each stream (2 events per stream for testing)
  let eventsPerStream = 2
  let allEvents :: Array Event
      allEvents = do
        streamIds |> Array.flatMap \streamId -> do
          Array.initialize eventsPerStream \eventIndex -> do
            let streamIdText = case streamId of Event.StreamId text -> text
            Event
              { id = [fmt|event-#{streamIdText}-#{eventIndex}|],
                streamId = streamId,
                position = Event.StreamPosition (Positive eventIndex),
                globalPosition = Nothing
              }

  -- Append all events to their respective streams
  allEvents
    |> Task.mapArray
      ( \event ->
          event
            |> store.appendToStream event.streamId event.position
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

  pure
    Context
      { streamCount,
        eventsPerStream,
        eventStreams,
        store
      }
