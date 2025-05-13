module Service.EventStore.InMemorySpec where

import Array qualified
import Core
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test


spec :: Spec
spec = do
  describe "InMemoryEventStore" do
    it "should maintain total ordering per stream with 1M events" do
      -- Create a new event store
      store <- InMemory.new |> runTask

      -- Create a stream ID for testing
      let streamId = Event.StreamId "test-stream"

      -- Generate 1M events with sequential positions
      let generatedEvents :: Array Event
          generatedEvents = do
            let count = 1_000_000
            Array.initialize count \index -> do
              let position = Event.StreamPosition (Positive index)
              let id = [fmt|event-{index}|]
              Event {id, streamId, position}

      -- Append all events sequentially
      let appendEvents :: Array Event -> Task EventStore.Error (Array Event.StreamPosition)
          appendEvents events = do
            events |> Task.mapArray \event -> do
              let expectedPosition = event.position
              store.appendToStream streamId expectedPosition event

      positions <- appendEvents generatedEvents |> runTask
      -- Read back all events to verify
      let expectedCount = Array.length generatedEvents
      events <-
        store.readStreamForwardFrom streamId (Event.StreamPosition 0) (EventStore.Limit (Positive expectedCount)) |> runTask

      -- let isCorrectCount = Array.length events == expectedCount
      Array.length events |> shouldBe expectedCount

      -- let isCorrectOrder = events |> Array.map (\v -> v.position) |> (==) positions
      events |> Array.map (\v -> v.position) |> shouldBe positions

      -- let isCorrectContent = events == generatedEvents
      events |> shouldBe generatedEvents
