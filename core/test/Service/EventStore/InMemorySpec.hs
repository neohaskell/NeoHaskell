module Service.EventStore.InMemorySpec where

import Array qualified
import AsyncIO qualified
import Console qualified
import Core
import GHC.IO qualified as GHC
import Maybe qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test
import ToText (toText)


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


testOptimisticConcurrency :: GHC.IO Unit
testOptimisticConcurrency = do
  -- Create a new event store
  store <- InMemory.new |> runTask

  -- Create a stream ID for testing
  let streamId = Event.StreamId "test-stream"

  -- First, append an initial event to position 0
  let initialEvent =
        Event
          { id = "event-0",
            streamId = streamId,
            position = Event.StreamPosition 0,
            globalPosition = Nothing
          }

  initialEvent
    |> store.appendToStream streamId (Event.StreamPosition 0)
    |> runTask

  -- Create two identical events both expecting to append at position 1
  let event1 =
        Event
          { id = "event-1-writer1",
            streamId = streamId,
            position = Event.StreamPosition 1,
            globalPosition = Nothing
          }

  let event2 =
        Event
          { id = "event-1-writer2",
            streamId = streamId,
            position = Event.StreamPosition 1,
            globalPosition = Nothing
          }

  -- Prepare concurrent operations
  let append1 =
        event1
          |> store.appendToStream streamId (Event.StreamPosition 1)
          |> Task.mapError toText
          |> Task.runResult
          |> AsyncIO.run

  let append2 =
        event2
          |> store.appendToStream streamId (Event.StreamPosition 1)
          |> Task.mapError toText
          |> Task.runResult
          |> AsyncIO.run

  -- Run both operations concurrently
  thread1 <- append1
  thread2 <- append2

  (_, result) <-
    [thread1, thread2]
      |> Array.fromLinkedList
      |> AsyncIO.waitAnyCancel

  let resultText = toPrettyText result

  Console.print [fmt|Result: {resultText}|]
    |> runTask @Text

  -- Wait for both to complete
  -- result1 <- AsyncIO.waitFor thread1
  -- result2 <- AsyncIO.waitFor thread2

  -- let result1Text = toPrettyText result1
  -- let result2Text = toPrettyText result2

  -- Console.print [fmt|Result 1: {result1Text}|]
  --   |> runTask @Text

  -- Console.print [fmt|Result 2: {result2Text}|]
  --   |> runTask @Text

  -- -- Verify that exactly one operation succeeded and one failed
  -- [result1, result2]
  --   |> Array.fromLinkedList
  --   |> Array.takeIf Result.isOk
  --   |> Array.length
  --   |> shouldBe 1

  -- Read back all events to verify
  events <-
    store.readStreamForwardFrom streamId (Event.StreamPosition 0) (EventStore.Limit (Positive 10))
      |> runTask

  let eventsText = toPrettyText events

  Console.print [fmt|Read events: {eventsText}|]
    |> runTask @Text

  -- We should have exactly 2 events (initial + one successful append)
  events
    |> Array.length
    |> shouldBe 2

  -- The first event should be our initial event
  events
    |> Array.get 0
    |> Maybe.map (\event -> event.id)
    |> Maybe.withDefault ("No event found")
    |> shouldBe "event-0"

  -- The second event should be one of our concurrent events
  events
    |> Array.get 1
    |> Maybe.map (\event -> event.id)
    |> Maybe.withDefault ("No event found")
    |> shouldStartWith "event-1-writer"


spec :: Spec
spec = do
  describe "InMemoryEventStore" do
    describe "ability to maintain total ordering per stream" do
      it "handles 10 events" do
        testStreamOrderingSequential 10

      it "handles 100 events" do
        testStreamOrderingSequential 100

      it "handles 1000 events" do
        testStreamOrderingSequential 1000

      it "handles 10000 events" do
        testStreamOrderingSequential 10000

      xit "handles 100000 events" do
        testStreamOrderingSequential 100000

      xit "handles 1000000 events" do
        testStreamOrderingSequential 1000000

    describe "ability to enforce optimistic concurrency control" do
      it "should handle well two concurrent operations" do
        testOptimisticConcurrency
