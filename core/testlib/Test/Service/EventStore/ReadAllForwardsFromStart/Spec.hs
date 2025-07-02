module Test.Service.EventStore.ReadAllForwardsFromStart.Spec where

import Array qualified
import Core
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.ReadAllForwardsFromStart.Context qualified as Context


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

      it "supports partial reads with resumption from global position" \context -> do
        let totalEvents = context.eventCount * 2
        let batchSize = 3 -- Small batch to force multiple reads

        -- Read all events in batches, resuming from last position
        let readInBatches :: Event.StreamPosition -> Array Event.Event -> Task Text (Array Event.Event)
            readInBatches currentPosition accumulatedEvents = do
              batch <-
                context.store.readAllEventsForwardFrom currentPosition (EventStore.Limit batchSize)
                  |> Task.mapError toText

              case Array.length batch of
                0 ->
                  -- No more events, return accumulated
                  Task.yield accumulatedEvents
                batchLength -> do
                  let newAccumulated = Array.append accumulatedEvents batch
                  case Array.get (batchLength - 1) batch of
                    Just lastEvent -> do
                      -- Resume from next position after last read event
                      let Event.StreamPosition lastPos = lastEvent.globalPosition
                      let nextPosition = Event.StreamPosition (lastPos + 1)
                      readInBatches nextPosition newAccumulated
                    Nothing ->
                      -- This should never happen if batchLength > 0
                      Task.yield newAccumulated

        allEventsBatched <- readInBatches (Event.StreamPosition 0) Array.empty

        -- Compare with single read to ensure no events lost/duplicated
        allEventsSingle <-
          context.store.readAllEventsForwardFrom (Event.StreamPosition 0) (EventStore.Limit totalEvents)
            |> Task.mapError toText

        -- Should have same number of events
        Array.length allEventsBatched
          |> shouldBe (Array.length allEventsSingle)

        -- Should have exact same events in same order
        allEventsBatched
          |> shouldBe allEventsSingle

        -- Global positions should be strictly increasing across all batches
        let globalPositions = allEventsBatched |> Array.map (\e -> e.globalPosition)
        globalPositions |> shouldHaveIncreasingOrder

      it "reads from arbitrary middle position with correct skip count" \context -> do
        -- First, find the global position of the 5th event (arbitrary middle position)
        let skipCount = 5
        let limit = EventStore.Limit skipCount
        firstEvents <-
          context.store.readAllEventsForwardFrom (Event.StreamPosition 0) limit
            |> Task.mapError toText

        case Array.get (skipCount - 1) firstEvents of
          Nothing ->
            fail "Expected to find the 5th event for skip test"
          Just fifthEvent -> do
            let startPosition = fifthEvent.globalPosition
            let Event.StreamPosition startPos = startPosition
            let readFromPosition = Event.StreamPosition (startPos + 1) -- Read from after the 5th event

            -- Read remaining events from that position
            let remainingLimit = EventStore.Limit (context.eventCount * 2)
            remainingEvents <-
              context.store.readAllEventsForwardFrom readFromPosition remainingLimit
                |> Task.mapError toText

            -- Should get exactly (totalEvents - skipCount) events
            let expectedRemaining = (context.eventCount * 2) - skipCount -- Removed the -1 since position+1 logic was wrong
            Array.length remainingEvents
              |> shouldBe expectedRemaining

            -- All events should have positions > startPosition
            remainingEvents |> Task.forEach \event -> do
              event.globalPosition |> shouldBeGreaterThan startPosition

            -- Events should still be in increasing order
            let positions = remainingEvents |> Array.map (\e -> e.globalPosition)
            positions |> shouldHaveIncreasingOrder
