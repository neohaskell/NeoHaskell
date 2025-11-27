module Test.Service.EventStore.IndividualStreamOrdering.Spec where

import Array qualified
import Core
import Maybe qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata (EventMetadata (..))
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Stream qualified
import Task qualified
import Test
import Test.Service.EventStore.Core (CartEvent)
import Test.Service.EventStore.IndividualStreamOrdering.Context qualified as Context


spec :: Task Text (EventStore CartEvent) -> Spec Unit
spec newStore = do
  describe "Individual Stream Ordering" do
    specWithCount newStore 10

    whenEnvVar "TEST_EVENT_COUNT" do
      specWithCount newStore 100
      specWithCount newStore 1000

    whenEnvVar "TEST_EVENT_COUNT_HIGH" do
      specWithCount newStore 10000
      specWithCount newStore 100000
      specWithCount newStore 1000000


specWithCount :: Task Text (EventStore CartEvent) -> Int -> Spec Unit
specWithCount newStore eventCount = do
  describe [fmt|testing with #{toText eventCount} events|] do
    beforeAll (Context.initialize newStore eventCount) do
      it "has the correct number of events" \context -> do
        let startPosition =
              Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount)
        streamMessages <-
          context.store.readStreamForwardFrom context.entityName context.streamId startPosition limit
            |> Task.mapError toText
            |> Task.andThen Stream.toArray
        let events = EventStore.collectStreamEvents streamMessages
        Array.length events
          |> fromIntegral
          |> shouldBe context.eventCount

      it "has the correct order" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount)
        streamMessages <-
          context.store.readStreamForwardFrom context.entityName context.streamId startPosition limit
            |> Task.mapError toText
            |> Task.andThen Stream.toArray
        let events = EventStore.collectStreamEvents streamMessages
        events
          |> Array.map
            ( \event ->
                event.metadata.localPosition
                  |> Maybe.getOrDie
            )
          |> shouldBe context.positions

      it "has all the events" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount)
        streamMessages <-
          context.store.readStreamForwardFrom context.entityName context.streamId startPosition limit
            |> Task.mapError toText
            |> Task.andThen Stream.toArray
        let events = EventStore.collectStreamEvents streamMessages

        -- Validate the important properties without hardcoding global positions
        -- since global positions are assigned dynamically based on append order
        Array.length events |> fromIntegral |> shouldBe context.eventCount

        -- Pair up actual events with expected insertions for validation
        let eventPairs = context.allInsertions |> Array.zip events

        -- Each event should have the correct core properties
        eventPairs |> Task.forEach \(insertion, event) -> do
          -- Validate event ID, entity ID, stream ID, and local position
          event.metadata.eventId |> shouldBe insertion.id
          event.entityName |> shouldBe context.entityName
          event.streamId |> shouldBe context.streamId
          (event.metadata.localPosition)
            |> shouldBe insertion.metadata.localPosition

      it "has global positions in strictly increasing order" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount)
        streamMessages <-
          context.store.readStreamForwardFrom context.entityName context.streamId startPosition limit
            |> Task.mapError toText
            |> Task.andThen Stream.toArray
        let events = EventStore.collectStreamEvents streamMessages

        -- Global positions should be strictly increasing
        let globalPositions = events |> Array.map (\e -> e.metadata.globalPosition |> Maybe.getOrDie)
        globalPositions |> shouldHaveIncreasingOrder

        -- Each event should have the correct entity ID
        events |> Task.forEach \event -> do
          event.entityName |> shouldBe context.entityName

        -- Each event should have the correct stream ID
        events |> Task.forEach \event -> do
          event.streamId |> shouldBe context.streamId

        -- Should have the expected number of events
        Array.length events |> fromIntegral |> shouldBe context.eventCount

      it "reads from middle position and gets remaining events" \context -> do
        let halfwayPoint = (fromIntegral context.eventCount) // 2
        let startPosition = Event.StreamPosition (fromIntegral halfwayPoint)
        let limit = EventStore.Limit (context.eventCount) -- Large enough to get all remaining events
        streamMessages <-
          context.store.readStreamForwardFrom context.entityName context.streamId startPosition limit
            |> Task.mapError toText
            |> Task.andThen Stream.toArray
        let eventsFromMiddle = EventStore.collectStreamEvents streamMessages

        -- Should read exactly the second half of events
        let expectedRemainingCount = (fromIntegral context.eventCount) - halfwayPoint
        Array.length eventsFromMiddle |> shouldBe expectedRemainingCount

        -- All events should have local positions >= startPosition
        eventsFromMiddle |> Task.forEach \event -> do
          event.metadata.localPosition |> Maybe.getOrDie |> shouldBeGreaterThanOrEqual startPosition

        -- Global positions should still be strictly increasing
        let globalPositions = eventsFromMiddle |> Array.map (\e -> e.metadata.globalPosition |> Maybe.getOrDie)
        globalPositions |> shouldHaveIncreasingOrder

        -- Each event should have the correct entity ID
        eventsFromMiddle |> Task.forEach \event -> do
          event.entityName |> shouldBe context.entityName

        -- Each event should have the correct stream ID
        eventsFromMiddle |> Task.forEach \event -> do
          event.streamId |> shouldBe context.streamId

      it "reads backward from middle position with correct count" \context -> do
        let halfwayPoint = (fromIntegral context.eventCount) // 2
        let readFromPosition = Event.StreamPosition (halfwayPoint + 1 |> fromIntegral) -- Read from position after halfway point
        let limit = EventStore.Limit (context.eventCount) -- Large enough to get all events before position
        streamMessages <-
          context.store.readStreamBackwardFrom context.entityName context.streamId readFromPosition limit
            |> Task.mapError toText
            |> Task.andThen Stream.toArray
        let eventsBackward = EventStore.collectStreamEvents streamMessages

        -- Test expects exactly half the events (not including the readFromPosition)
        -- For 10 events (0-9), reading backward from position 6 with < gives us positions 0-5 (6 events)
        let expectedBackwardCount = halfwayPoint + 1 -- positions 0 through halfwayPoint
        Array.length eventsBackward |> shouldBe expectedBackwardCount

        -- All events should have local positions < readFromPosition (strict less than)
        eventsBackward |> Task.forEach \event -> do
          event.metadata.localPosition |> Maybe.getOrDie |> shouldBeLessThan readFromPosition

        -- Global positions should be in decreasing order (backward reading)
        let globalPositions = eventsBackward |> Array.map (\e -> e.metadata.globalPosition |> Maybe.getOrDie)
        globalPositions |> shouldHaveDecreasingOrder

        -- Each event should have the correct entity ID
        eventsBackward |> Task.forEach \event -> do
          event.entityName |> shouldBe context.entityName

        -- Each event should have the correct stream ID
        eventsBackward |> Task.forEach \event -> do
          event.streamId |> shouldBe context.streamId

        -- Verify we get the expected range of events (positions 0 through halfwayPoint)
        let expectedPositions = Array.fromLinkedList [0 .. halfwayPoint] |> Array.map (fromIntegral .> Event.StreamPosition)
        let actualPositions = eventsBackward |> Array.map (\e -> e.metadata.localPosition |> Maybe.getOrDie) |> Array.reverse
        actualPositions |> shouldBe expectedPositions

      it "reads backward from the end of stream with all events" \context -> do
        -- To read from the end, we need a position higher than the last event position
        let lastEventPosition = Event.StreamPosition (context.eventCount - 1 |> fromIntegral)
        let endPosition = Event.StreamPosition (context.eventCount) -- One past the last event
        let limit = EventStore.Limit (context.eventCount)

        streamMessages <-
          context.store.readStreamBackwardFrom context.entityName context.streamId endPosition limit
            |> Task.mapError toText
            |> Task.andThen Stream.toArray
        let allEventsBackward = EventStore.collectStreamEvents streamMessages

        -- Should read all events in the stream
        Array.length allEventsBackward |> fromIntegral |> shouldBe context.eventCount

        -- All events should have local positions < endPosition (strict less than)
        allEventsBackward |> Task.forEach \event -> do
          event.metadata.localPosition |> Maybe.getOrDie |> shouldBeLessThan endPosition

        -- Global positions should be in decreasing order (backward reading)
        let globalPositions = allEventsBackward |> Array.map (\e -> e.metadata.globalPosition |> Maybe.getOrDie)
        globalPositions |> shouldHaveDecreasingOrder

        -- Each event should have the correct entity ID
        allEventsBackward |> Task.forEach \event -> do
          event.entityName |> shouldBe context.entityName

        -- Each event should have the correct stream ID
        allEventsBackward |> Task.forEach \event -> do
          event.streamId |> shouldBe context.streamId

        -- Verify we get all events in reverse order (positions eventCount-1 down to 0)
        let expectedPositions = Array.fromLinkedList [context.eventCount - 1, context.eventCount - 2 .. 0] |> Array.map Event.StreamPosition
        let actualPositions = allEventsBackward |> Array.map (\e -> e.metadata.localPosition |> Maybe.getOrDie)
        actualPositions |> shouldBe expectedPositions

        -- First event should be the last one written (highest local position)
        case Array.get 0 allEventsBackward of
          Nothing ->
            fail "Expected to find first event in backward read from end"
          Just firstEvent -> do
            firstEvent.metadata.localPosition |> Maybe.getOrDie |> shouldBe lastEventPosition

        -- Last event should be the first one written (position 0)
        case Array.get (Array.length allEventsBackward - 1) allEventsBackward of
          Nothing ->
            fail "Expected to find last event in backward read from end"
          Just lastEvent -> do
            lastEvent.metadata.localPosition |> Maybe.getOrDie |> shouldBe (Event.StreamPosition 0)
