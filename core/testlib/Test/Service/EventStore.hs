module Test.Service.EventStore where

import Array qualified
import AsyncTask qualified
import Core
import Maybe qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import ToText (toText)


testStreamOrderingSequential :: EventStore -> Int -> Task _ Unit
testStreamOrderingSequential store count = do
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

  positions <- appendEvents generatedEvents
  -- Read back all events to verify
  let expectedCount = Array.length generatedEvents
  events <-
    store.readStreamForwardFrom streamId (Event.StreamPosition 0) (EventStore.Limit (Positive expectedCount))

  -- Verify the results
  Array.length events
    |> shouldBe expectedCount

  events
    |> Array.map (\v -> v.position)
    |> shouldBe positions

  events
    |> shouldBe generatedEvents


testOptimisticConcurrency :: EventStore -> Task _ Unit
testOptimisticConcurrency store = do
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
    |> Task.mapError toText

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
          |> AsyncTask.run

  let append2 =
        event2
          |> store.appendToStream streamId (Event.StreamPosition 1)
          |> Task.mapError toText
          |> AsyncTask.run

  -- Run both operations concurrently
  thread1 <- append1
  thread2 <- append2

  [thread1, thread2]
    |> Array.fromLinkedList
    |> AsyncTask.waitAnyCancel
    |> discard

  -- Read back all events to verify
  events <-
    store.readStreamForwardFrom streamId (Event.StreamPosition 0) (EventStore.Limit (Positive 10))
      |> Task.mapError toText

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


testGlobalStreamOrdering :: EventStore -> Int -> Task _ Unit
testGlobalStreamOrdering store streamCount = do
  -- Create multiple streams with events
  let streamIds :: Array Event.StreamId
      streamIds = do
        Array.initialize streamCount \index -> do
          Event.StreamId [fmt|stream-{index}|]

  -- Create events for each stream (2 events per stream for testing)
  let eventsPerStream = 2
  let allEvents :: Array Event
      allEvents = do
        streamIds |> Array.flatMap \streamId -> do
          Array.initialize eventsPerStream \eventIndex -> do
            let streamIdText = case streamId of Event.StreamId text -> text
            Event
              { id = [fmt|event-{streamIdText}-{eventIndex}|],
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
  streamEvents <-
    streamIds
      |> Task.mapArray
        ( \streamId ->
            streamId
              |> store.readAllStreamEvents
              |> Task.mapError toText
        )

  streamEvents
    |> Array.length
    |> shouldBe
      eventsPerStream

  -- Verify events are ordered correctly within the stream
  streamEvents
    |> Array.indexed
    |> Task.mapArray
      ( \(index, eventStream) ->
          eventStream
            |> Task.mapArray \event -> do
              event.position
                |> shouldBe (Event.StreamPosition (Positive index))
      )

  -- Test readAllEventsForwardFrom starting from global position 0
  let expectedTotalEvents = streamCount * eventsPerStream
  allGlobalEvents <-
    store.readAllEventsForwardFrom (Event.StreamPosition 0) (EventStore.Limit (Positive expectedTotalEvents))
      |> Task.mapError toText

  -- Verify we got all events from the global stream
  allGlobalEvents
    |> Array.length
    |> shouldBe expectedTotalEvents

  -- Verify that events have been assigned global positions
  allGlobalEvents |> Task.mapArray \event -> do
    case event.globalPosition of
      Nothing ->
        fail "Event should have a global position assigned"
      Just globalPos ->
        globalPos |> shouldSatisfy (\pos -> pos >= Event.StreamPosition 0)

  -- Test reading global events from a specific position
  let midPoint = expectedTotalEvents // 2
  laterGlobalEvents <-
    store.readAllEventsForwardFrom
      (Event.StreamPosition (Positive midPoint))
      (EventStore.Limit (Positive expectedTotalEvents))
      |> Task.mapError toText

  -- Should get roughly half the events (or fewer, depending on implementation)
  laterGlobalEvents
    |> Array.length
    |> shouldSatisfy (\count -> count <= expectedTotalEvents - midPoint)

  -- Verify global ordering consistency - events should be in chronological order
  Task.unless ((Array.length allGlobalEvents) <= 1) do
    let eventPairs =
          allGlobalEvents
            |> Array.zip (Array.drop 1 allGlobalEvents)

    let matchPositions :: (Event, Event) -> Task _ Unit
        matchPositions (earlier, later) =
          case (earlier.globalPosition, later.globalPosition) of
            (Just earlierPos, Just laterPos) ->
              earlierPos |> shouldSatisfy (\pos -> pos <= laterPos)
            _ ->
              fail "Event should have a global position assigned"

    eventPairs
      |> Task.mapArray matchPositions
      |> discard
