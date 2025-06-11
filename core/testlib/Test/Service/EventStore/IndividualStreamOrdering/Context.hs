module Test.Service.EventStore.IndividualStreamOrdering.Context (
  Context (..),
  initialize,
) where

import Array qualified
import Core
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import ToText (toText)


data Context = Context
  { eventCount :: Int,
    streamId :: Event.StreamId,
    store :: EventStore,
    generatedEvents :: Array Event,
    positions :: Array Event.StreamPosition
  }


initialize :: Task Text EventStore -> Int -> Task Text Context
initialize newStore eventCount = do
  store <- newStore
  let streamId = Event.StreamId "test-stream"
  let generatedEvents = Array.initialize eventCount \index -> do
        let position = Event.StreamPosition (Natural index)
        let id = [fmt|event-#{index}|]
        Event {id, streamId, position, globalPosition = Nothing}
  -- Append all events sequentially
  let appendEvents :: Array Event -> Task EventStore.Error (Array Event.StreamPosition)
      appendEvents events = do
        events |> Task.mapArray \event -> do
          result <- event |> store.appendToStream streamId event.position
          Task.yield result.localPosition

  positions <- generatedEvents |> appendEvents |> Task.mapError toText

  return Context {eventCount, streamId, store, generatedEvents, positions}
