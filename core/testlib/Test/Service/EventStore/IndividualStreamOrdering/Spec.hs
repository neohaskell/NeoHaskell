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
    beforeAll (Context.initialize newStore 10) do
      it "has the correct number of events" \ctx -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (Positive ctx.eventCount)
        events <-
          ctx.store.readStreamForwardFrom ctx.streamId startPosition limit
            |> Task.mapError toText
        Array.length events
          |> shouldBe ctx.eventCount

      it "has the correct positions" \ctx -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (Positive ctx.eventCount)
        events <-
          ctx.store.readStreamForwardFrom ctx.streamId startPosition limit
            |> Task.mapError toText
        events
          |> Array.map (\v -> v.position)
          |> shouldBe ctx.positions

      it "has all the events" \ctx -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (Positive ctx.eventCount)
        events <-
          ctx.store.readStreamForwardFrom ctx.streamId startPosition limit
            |> Task.mapError toText
        events
          |> shouldBe ctx.generatedEvents
