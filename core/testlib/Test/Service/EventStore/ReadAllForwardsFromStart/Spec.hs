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
      it "reads all events from start in order" \ctx -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (ctx.eventCount * 2) -- Double the limit since we have two entities
        events <-
          ctx.store.readStreamForwardFrom ctx.streamId startPosition limit
            |> Task.mapError toText

        -- Check that we got the expected number of events
        Array.length events
          |> shouldBe (ctx.eventCount * 2)

        -- Check that events are in order by global position
        let positions = events |> Array.map (\e -> e.globalPosition)
        positions |> shouldHaveIncreasingOrder

        -- Check that we got the correct number of events from our entity
        let eventsFromEntity = events |> Array.takeIf (\e -> e.entityId == ctx.entityId)
        Array.length eventsFromEntity
          |> shouldBe ctx.eventCount
