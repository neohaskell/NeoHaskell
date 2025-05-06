module Service.EventStore.InMemorySpec where

import Array (Array (..))
import Array qualified
import Core
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test


spec :: Spec
spec = do
  describe "InMemoryEventStore" do
    it "should be able to append an event to a stream" do
      panic "not implemented"

    it "should maintain total ordering per stream with 1M events" do
      -- Create a new event store
      store <- InMemory.new

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
      let appendEvents :: Array Event -> Task _ (Array Event.StreamPosition)
          appendEvents events = do
            events |> Task.mapArray \event -> do
              let expectedPosition = event.position
              store.appendToStream streamId expectedPosition event

      -- Read back all events to verify
      let verifyEvents :: Array Event -> Array Event.StreamPosition -> Task _ Bool
          verifyEvents originalEvents positions = do
            let expectedCount = Array.length originalEvents
            events <- store.readStreamForwardFrom streamId (Event.StreamPosition 0) expectedCount
            let isCorrectCount = Array.length events == expectedCount
            let isCorrectOrder = events |> Array.map (.position) |> (==) positions
            let isCorrectContent = events == originalEvents
            Task.yield (isCorrectCount && isCorrectOrder && isCorrectContent)

      -- Run the test
      positions <- appendEvents generatedEvents
      isCorrect <- verifyEvents generatedEvents positions |> runTask
      isCorrect |> shouldBe True
