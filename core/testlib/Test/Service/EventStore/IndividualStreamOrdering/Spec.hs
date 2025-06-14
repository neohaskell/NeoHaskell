module Test.Service.EventStore.IndividualStreamOrdering.Spec where

import Array qualified
import Core
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.IndividualStreamOrdering.Context qualified as Context
import ToText (toText)


spec :: Task Text EventStore -> Spec Unit
spec newStore = do
  describe "Individual Stream Ordering" do
    specWithCount newStore 10
    specWithCount newStore 100
    specWithCount newStore 1000
    specWithCount newStore 10000

    whenEnvVar "TEST_EVENT_COUNT" do
      specWithCount newStore 100000
      specWithCount newStore 1000000


specWithCount :: Task Text EventStore -> Int -> Spec Unit
specWithCount newStore eventCount = do
  describe [fmt|testing with #{toText eventCount} events|] do
    beforeAll (Context.initialize newStore eventCount) do
      it "has the correct number of events" \ctx -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (Natural ctx.eventCount)
        events <-
          ctx.store.readStreamForwardFrom ctx.streamId startPosition limit
            |> Task.mapError toText
        Array.length events
          |> shouldBe ctx.eventCount

      it "has the correct order" \ctx -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (Natural ctx.eventCount)
        events <-
          ctx.store.readStreamForwardFrom ctx.streamId startPosition limit
            |> Task.mapError toText
        events
          |> Array.map (\v -> v.localPosition)
          |> shouldBe ctx.positions

      it "has all the events" \ctx -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (Natural ctx.eventCount)
        events <-
          ctx.store.readStreamForwardFrom ctx.streamId startPosition limit
            |> Task.mapError toText
        let expectedEvents = ctx.generatedEvents |> Array.map (Event.fromInsertionEvent (Event.StreamPosition 0))
        events
          |> shouldBe expectedEvents
