module Test.Service.EventStore.ReadAllForwardsFromStart.Spec where

import Array qualified
import Core
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.ReadAllForwardsFromStart.Context qualified as Context
import Uuid qualified


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
            let expectedRemaining = (context.eventCount * 2) - skipCount
            Array.length remainingEvents
              |> shouldBe expectedRemaining

            -- All events should have positions > startPosition
            remainingEvents |> Task.forEach \event -> do
              event.globalPosition |> shouldBeGreaterThan startPosition

            -- Events should still be in increasing order
            let positions = remainingEvents |> Array.map (\e -> e.globalPosition)
            positions |> shouldHaveIncreasingOrder

      it "filters events by single entity ID from start" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount * 2)
        let entityFilter = Array.fromLinkedList [context.entity1Id]

        filteredEvents <-
          context.store.readAllEventsForwardFromFiltered startPosition limit entityFilter
            |> Task.mapError toText

        -- Should only contain events from entity1
        filteredEvents |> Task.forEach \event -> do
          event.entityId |> shouldBe context.entity1Id

        -- Should have exactly eventCount events (all from entity1)
        Array.length filteredEvents
          |> shouldBe context.eventCount

        -- Should be in increasing global position order
        let positions = filteredEvents |> Array.map (\e -> e.globalPosition)
        positions |> shouldHaveIncreasingOrder

      it "filters events by multiple entity IDs from start" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount * 2)
        let entityFilter = Array.fromLinkedList [context.entity1Id, context.entity2Id]

        filteredEvents <-
          context.store.readAllEventsForwardFromFiltered startPosition limit entityFilter
            |> Task.mapError toText

        -- Should contain events from both entities
        let entity1Events = filteredEvents |> Array.takeIf (\e -> e.entityId == context.entity1Id)
        let entity2Events = filteredEvents |> Array.takeIf (\e -> e.entityId == context.entity2Id)

        Array.length entity1Events |> shouldBe context.eventCount
        Array.length entity2Events |> shouldBe context.eventCount

        -- Total should be all events
        Array.length filteredEvents
          |> shouldBe (context.eventCount * 2)

        -- Should be in increasing global position order
        let positions = filteredEvents |> Array.map (\e -> e.globalPosition)
        positions |> shouldHaveIncreasingOrder

      it "filters events by entity ID from middle position" \context -> do
        -- Start from 5th event position
        let skipCount = 5
        let firstLimit = EventStore.Limit skipCount
        firstEvents <-
          context.store.readAllEventsForwardFrom (Event.StreamPosition 0) firstLimit
            |> Task.mapError toText

        case Array.get (skipCount - 1) firstEvents of
          Nothing ->
            fail "Expected to find the 5th event for position test"
          Just fifthEvent -> do
            let startPosition = fifthEvent.globalPosition
            let Event.StreamPosition startPos = startPosition
            let readFromPosition = Event.StreamPosition (startPos + 1)
            let entityFilter = Array.fromLinkedList [context.entity1Id]
            let limit = EventStore.Limit (context.eventCount * 2)

            filteredEvents <-
              context.store.readAllEventsForwardFromFiltered readFromPosition limit entityFilter
                |> Task.mapError toText

            -- Should only contain events from entity1
            filteredEvents |> Task.forEach \event -> do
              event.entityId |> shouldBe context.entity1Id

            -- All events should have position > startPosition
            filteredEvents |> Task.forEach \event -> do
              event.globalPosition |> shouldBeGreaterThan startPosition

            -- Should be in increasing global position order
            let positions = filteredEvents |> Array.map (\e -> e.globalPosition)
            positions |> shouldHaveIncreasingOrder

      it "returns empty array when filtering by non-existent entity" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit 100
        nonExistentEntityId <- Uuid.generate |> Task.map Event.EntityId
        let entityFilter = Array.fromLinkedList [nonExistentEntityId]

        filteredEvents <-
          context.store.readAllEventsForwardFromFiltered startPosition limit entityFilter
            |> Task.mapError toText

        Array.length filteredEvents
          |> shouldBe 0

      it "respects limit when filtering by entity" \context -> do
        let startPosition = Event.StreamPosition 0
        let smallLimit = EventStore.Limit 3 -- Small limit to test
        let entityFilter = Array.fromLinkedList [context.entity1Id, context.entity2Id]

        filteredEvents <-
          context.store.readAllEventsForwardFromFiltered startPosition smallLimit entityFilter
            |> Task.mapError toText

        -- Should return exactly 3 events (the limit)
        Array.length filteredEvents
          |> shouldBe 3

        -- All events should be from our target entities
        filteredEvents |> Task.forEach \event -> do
          let isFromTargetEntity = (event.entityId == context.entity1Id) || (event.entityId == context.entity2Id)
          isFromTargetEntity |> shouldBe True

      it "maintains global order when filtering mixed entities" \context -> do
        let startPosition = Event.StreamPosition 0
        let limit = EventStore.Limit (context.eventCount * 2)
        let entityFilter = Array.fromLinkedList [context.entity1Id, context.entity2Id]

        -- Get all events without filtering for comparison
        allEvents <-
          context.store.readAllEventsForwardFrom startPosition limit
            |> Task.mapError toText

        -- Get filtered events
        filteredEvents <-
          context.store.readAllEventsForwardFromFiltered startPosition limit entityFilter
            |> Task.mapError toText

        -- Filtered events should be exactly the same as all events (since we're filtering by both entities)
        filteredEvents |> shouldBe allEvents

        -- Should maintain global position ordering
        let positions = filteredEvents |> Array.map (\e -> e.globalPosition)
        positions |> shouldHaveIncreasingOrder
