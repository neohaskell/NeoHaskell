module Service.EventStore.InMemorySpec where

import Array qualified
import Core
import GHC.IO qualified as GHC
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test


testStreamOrderingSequential :: Int -> GHC.IO Unit
testStreamOrderingSequential count = do
  -- Create a new event store
  store <- InMemory.new |> runTask

  -- Create a stream ID for testing
  let streamId = Event.StreamId "test-stream"

  -- Generate events with sequential positions
  let generatedEvents :: Array Event
      generatedEvents = do
        Array.initialize count \index -> do
          let position = Event.StreamPosition (Positive index)
          let id = [fmt|event-{index}|]
          Event {id, streamId, position, globalPosition = Nothing}

  -- Append all events sequentially
  let appendEvents :: Array Event -> Task EventStore.Error (Array Event.StreamPosition)
      appendEvents events = do
        events |> Task.mapArray \event -> do
          let expectedPosition = event.position
          result <- event |> store.appendToStream streamId expectedPosition
          Task.yield result.localPosition

  positions <- appendEvents generatedEvents |> runTask
  -- Read back all events to verify
  let expectedCount = Array.length generatedEvents
  events <-
    store.readStreamForwardFrom streamId (Event.StreamPosition 0) (EventStore.Limit (Positive expectedCount))
      |> runTask
  -- Verify the results
  Array.length events
    |> shouldBe expectedCount

  events
    |> Array.map (\v -> v.position)
    |> shouldBe positions

  events
    |> shouldBe generatedEvents


spec :: Spec
spec = do
  describe "InMemoryEventStore" do
    it "should maintain total ordering per stream with 10 events" do
      testStreamOrderingSequential 10

    it "should maintain total ordering per stream with 100 events" do
      testStreamOrderingSequential 100

    it "should maintain total ordering per stream with 1000 events" do
      testStreamOrderingSequential 1000
