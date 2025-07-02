module Test.Service.EventStore.ReadAllBackwardsFromEnd.Spec where

import Array qualified
import Core
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.ReadAllBackwardsFromEnd.Context qualified as Context


spec :: Task Text EventStore -> Spec Unit
spec newStore = do
  describe "Read All Backwards From End" do
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
      it "reads all events from end in reverse order" \context -> do
        let limit = EventStore.Limit (context.eventCount * 2) -- Double the limit since we have two entities
        events <-
          context.store.readAllEventsBackwardFrom context.maxGlobalPosition limit
            |> Task.mapError toText

        Array.length events
          |> shouldBe (context.eventCount * 2)

      it "has all events in decreasing order" \context -> do
        let limit = EventStore.Limit (context.eventCount * 2)
        events <-
          context.store.readAllEventsBackwardFrom context.maxGlobalPosition limit
            |> Task.mapError toText

        let positions = events |> Array.map (\e -> e.globalPosition)
        positions |> shouldHaveDecreasingOrder

      it "has all events from entity1 in reverse order" \context -> do
        let limit = EventStore.Limit (context.eventCount * 2)
        events <-
          context.store.readAllEventsBackwardFrom context.maxGlobalPosition limit
            |> Task.mapError toText

        let eventsFromEntity1 = events |> Array.takeIf (\event -> event.entityId == context.entity1Id)
        eventsFromEntity1
          |> Array.length
          |> shouldBe context.eventCount

        -- Events should be in reverse local position order within the entity
        let localPositions = eventsFromEntity1 |> Array.map (\e -> e.localPosition)
        localPositions |> shouldHaveDecreasingOrder

      it "supports partial reads with resumption from global position (backwards)" \context -> do
        let totalEvents = context.eventCount * 2
        let batchSize = 3 -- Small batch to force multiple reads

        -- Read all events in batches backward, resuming from previous position
        let readInBatchesBackward :: Event.StreamPosition -> Array Event.Event -> Task Text (Array Event.Event)
            readInBatchesBackward currentPosition accumulatedEvents = do
              batch <-
                context.store.readAllEventsBackwardFrom currentPosition (EventStore.Limit batchSize)
                  |> Task.mapError toText

              case Array.length batch of
                0 ->
                  -- No more events, return accumulated
                  Task.yield accumulatedEvents
                batchLength -> do
                  let newAccumulated = Array.append accumulatedEvents batch
                  case Array.get (batchLength - 1) batch of
                    Just lastEvent -> do
                      -- Resume from position before the last read event
                      let Event.StreamPosition lastPos = lastEvent.globalPosition
                      if lastPos > 0
                        then do
                          let nextPosition = Event.StreamPosition (lastPos - 1)
                          readInBatchesBackward nextPosition newAccumulated
                        else -- Reached position 0, no more events to read
                          Task.yield newAccumulated
                    Nothing ->
                      -- This should never happen if batchLength > 0
                      Task.yield newAccumulated

        allEventsBatched <- readInBatchesBackward context.maxGlobalPosition Array.empty

        -- Compare with single read to ensure no events lost/duplicated
        allEventsSingle <-
          context.store.readAllEventsBackwardFrom context.maxGlobalPosition (EventStore.Limit totalEvents)
            |> Task.mapError toText

        -- Should have same number of events
        Array.length allEventsBatched
          |> shouldBe (Array.length allEventsSingle)

        -- Should have exact same events in same order
        allEventsBatched
          |> shouldBe allEventsSingle

        -- Global positions should be strictly decreasing across all batches
        let globalPositions = allEventsBatched |> Array.map (\e -> e.globalPosition)
        globalPositions |> shouldHaveDecreasingOrder

      it "can read from middle position backward" \context -> do
        let Event.StreamPosition maxPos = context.maxGlobalPosition
        let midPosition = Event.StreamPosition (maxPos // 2)
        let limit = EventStore.Limit (context.eventCount * 2)

        eventsFromMid <-
          context.store.readAllEventsBackwardFrom midPosition limit
            |> Task.mapError toText

        -- Should have events with positions <= midPosition
        eventsFromMid |> Task.forEach \event -> do
          event.globalPosition |> shouldBeLessThanOrEqual midPosition

        -- Should be in decreasing order
        let positions = eventsFromMid |> Array.map (\e -> e.globalPosition)
        positions |> shouldHaveDecreasingOrder

      it "reading from position 0 returns empty array" \context -> do
        let limit = EventStore.Limit 10
        events <-
          context.store.readAllEventsBackwardFrom (Event.StreamPosition 0) limit
            |> Task.mapError toText

        events
          |> Array.length
          |> shouldBe 1 -- Should contain only the event at position 0
