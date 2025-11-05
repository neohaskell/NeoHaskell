module Test.Service.EventStore.GlobalStreamOrdering.Spec (spec) where

import Array qualified
import Core
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata (EventMetadata (..))
import Service.EventStore (EventStore (..))
import Service.EventStore qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.Core (MyEvent)
import Test.Service.EventStore.GlobalStreamOrdering.Context (Context (..))
import Test.Service.EventStore.GlobalStreamOrdering.Context qualified as Context


spec :: Task Text (EventStore MyEvent) -> Spec Unit
spec newStore = do
  describe "Global Stream Ordering" do
    beforeAll (Context.initialize newStore 10) do
      it "has the correct number of streams" \context -> do
        context.eventStreams
          |> Array.length
          |> shouldBe context.streamCount

      it "has the correct number of events per stream" \context -> do
        let shouldHaveCorrectNumberOfEvents eventStream = do
              eventStream
                |> Array.length
                |> shouldBe context.eventsPerStream
        context.eventStreams |> Task.forEach shouldHaveCorrectNumberOfEvents

      it "has the events correctly ordered within the stream" \context -> do
        let shouldMatchPosition (index, event) = do
              event.metadata.localPosition
                |> shouldBe (Event.StreamPosition (fromIntegral index) |> Just)
        let shouldHaveCorrectOrdering eventStream = do
              eventStream
                |> Array.indexed
                |> Task.forEach shouldMatchPosition
        context.eventStreams
          |> Task.forEach shouldHaveCorrectOrdering

      it "has the correct number of events globally" \context -> do
        let expectedTotalEvents = context.streamCount * context.eventsPerStream |> fromIntegral
        let limit = EventStore.Limit (expectedTotalEvents)
        allGlobalEvents <-
          context.store.readAllEventsForwardFrom (Event.StreamPosition 0) limit
            |> Task.mapError toText
        allGlobalEvents
          |> Array.length
          |> shouldBe (context.streamCount * context.eventsPerStream)

      it "has all events with assigned global positions" \context -> do
        let expectedTotalEvents = context.streamCount * context.eventsPerStream |> fromIntegral
        allGlobalEvents <-
          context.store.readAllEventsForwardFrom (Event.StreamPosition 0) (EventStore.Limit (expectedTotalEvents))
            |> Task.mapError toText
        allGlobalEvents |> Task.forEach \event -> do
          event.metadata.globalPosition |> shouldSatisfy (\pos -> pos >= (Event.StreamPosition 0 |> Just))

      it "can read events from a specific position" \context -> do
        let expectedTotalEvents = context.streamCount * context.eventsPerStream
        let midPoint = expectedTotalEvents // 2
        laterGlobalEvents <-
          context.store.readAllEventsForwardFrom
            (Event.StreamPosition (fromIntegral midPoint))
            (EventStore.Limit (expectedTotalEvents |> fromIntegral))
            |> Task.mapError toText
        laterGlobalEvents
          |> Array.length
          |> shouldSatisfy (\count -> count <= expectedTotalEvents - midPoint)

      it "has the events globally ordered" \context -> do
        let expectedTotalEvents = context.streamCount * context.eventsPerStream |> fromIntegral
        allGlobalEvents <-
          context.store.readAllEventsForwardFrom (Event.StreamPosition 0) (EventStore.Limit (expectedTotalEvents))
            |> Task.mapError toText
        Task.unless ((Array.length allGlobalEvents) <= 1) do
          let eventPairs =
                allGlobalEvents
                  |> Array.zip (Array.drop 1 allGlobalEvents)

          let matchPositions :: (Event MyEvent, Event MyEvent) -> Task _ Unit
              matchPositions (earlier, later) =
                earlier.metadata.globalPosition |> shouldSatisfy (\pos -> pos <= later.metadata.globalPosition)

          eventPairs
            |> Task.mapArray matchPositions
            |> discard
