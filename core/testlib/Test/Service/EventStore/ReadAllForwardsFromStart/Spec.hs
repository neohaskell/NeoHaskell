module Test.Service.EventStore.ReadAllForwardsFromStart.Spec where

import Array qualified
import Core
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.ReadAllForwardsFromStart.Context qualified as Context
import ToText (toText)


spec :: Task Text EventStore -> Spec Unit
spec newStore = do
  describe "Read All Forwards From Start" do
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
      it "reads all events from start in order" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount * 2) -- Double the limit since we have two entities
        events <-
          context.store.readAllEventsForwardFrom startPosition limit
            |> Task.mapError toText

        Array.length events
          |> shouldBe (context.eventCount * 2)

      it "has all events in order" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount * 2)
        events <-
          context.store.readAllEventsForwardFrom startPosition limit
            |> Task.mapError toText

        let positions = events |> Array.map (\e -> e.globalPosition)
        positions |> shouldHaveIncreasingOrder

      it "has all events from entity1 in order" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount * 2)

        events <-
          context.store.readStreamForwardFrom context.entity1Id context.streamId startPosition limit
            |> Task.mapError toText

        let eventsFromEntity = events |> Array.takeIf (\event -> event.entityId == context.entity1Id)
        eventsFromEntity
          |> Array.length
          |> shouldBe context.eventCount
