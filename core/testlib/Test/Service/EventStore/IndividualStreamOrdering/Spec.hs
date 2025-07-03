module Test.Service.EventStore.IndividualStreamOrdering.Spec where

import Array qualified
import Core
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.IndividualStreamOrdering.Context qualified as Context


spec :: Task Text EventStore -> Spec Unit
spec newStore = do
  describe "Individual Stream Ordering" do
    specWithCount newStore 10

    whenEnvVar "TEST_EVENT_COUNT" do
      specWithCount newStore 100
      specWithCount newStore 1000
      specWithCount newStore 10000
      specWithCount newStore 100000
      specWithCount newStore 1000000


specWithCount :: Task Text EventStore -> Int -> Spec Unit
specWithCount newStore eventCount = do
  describe [fmt|testing with #{toText eventCount} events|] do
    beforeAll (Context.initialize newStore eventCount) do
      it "has the correct number of events" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount)
        events <-
          context.store.readStreamForwardFrom context.entityId context.streamId startPosition limit
            |> Task.mapError toText
        Array.length events
          |> shouldBe context.eventCount

      it "has the correct order" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount)
        events <-
          context.store.readStreamForwardFrom context.entityId context.streamId startPosition limit
            |> Task.mapError toText
        events
          |> Array.map (\v -> v.localPosition)
          |> shouldBe context.positions

      it "has all the events" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount)
        events <-
          context.store.readStreamForwardFrom context.entityId context.streamId startPosition limit
            |> Task.mapError toText

        -- Validate the important properties without hardcoding global positions
        -- since global positions are assigned dynamically based on append order
        Array.length events |> shouldBe context.eventCount

        -- Pair up actual events with expected events for validation
        let eventPairs = Array.zip events context.generatedEvents

        -- Each event should have the correct core properties
        eventPairs |> Task.forEach \(event, expectedEvent) -> do
          -- Validate event ID, entity ID, stream ID, and local position
          event.id |> shouldBe expectedEvent.id
          event.entityId |> shouldBe expectedEvent.entityId
          event.streamId |> shouldBe expectedEvent.streamId
          event.localPosition |> shouldBe expectedEvent.localPosition

      it "has global positions in strictly increasing order (C# test scenario)" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount)
        events <-
          context.store.readStreamForwardFrom context.entityId context.streamId startPosition limit
            |> Task.mapError toText

        -- Global positions should be strictly increasing
        let globalPositions = events |> Array.map (\e -> e.globalPosition)
        globalPositions |> shouldHaveIncreasingOrder

        -- Each event should have the correct entity ID
        events |> Task.forEach \event -> do
          event.entityId |> shouldBe context.entityId

        -- Each event should have the correct stream ID
        events |> Task.forEach \event -> do
          event.streamId |> shouldBe context.streamId

        -- Should have the expected number of events
        Array.length events |> shouldBe context.eventCount
