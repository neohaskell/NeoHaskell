module Test.Service.EventStore.LocalPositionStamping.Spec where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Maybe qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata (EventMetadata (..))
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.Core (MyEvent (..))
import Uuid qualified


spec :: Task Text (EventStore MyEvent) -> Spec Unit
spec newStore = do
  describe "Local Position Stamping" do
    it "stamps local positions when using payloadFromEvents helper" \_ -> do
      store <- newStore
      let entityName = Event.EntityName "TestEntity"
      streamId <- Uuid.generate |> Task.map Event.StreamId

      -- Create events using the payloadFromEvents helper (which sets localPosition to Nothing)
      let events = Array.fromLinkedList [MyEvent, MyEvent, MyEvent]
      payload <- Event.payloadFromEvents entityName streamId events

      -- Verify that insertions don't have local positions set (this is the input state)
      payload.insertions |> Task.forEach \insertion -> do
        insertion.metadata.localPosition |> shouldBe Nothing

      -- Insert the events
      _result <- store.insert payload |> Task.mapError toText

      -- Read back the events
      let startPosition = Event.StreamPosition 0
      let limit = EventStore.Limit 10
      storedEvents <-
        store.readStreamForwardFrom entityName streamId startPosition limit
          |> Task.mapError toText

      -- Verify we got 3 events back
      Array.length storedEvents |> shouldBe 3

      -- THIS IS THE BUG: Events should have local positions stamped by the store
      -- Currently they will have Nothing because fromInsertionPayload only stamps globalPosition
      storedEvents
        |> Array.indexed
        |> Task.forEach \(index, event) -> do
          case event.metadata.localPosition of
            Nothing ->
              fail [fmt|Event at index #{toText index} has no local position - store should stamp it!|]
            Just (Event.StreamPosition pos) ->
              pos |> shouldBe (fromIntegral index)

    it "stamps local positions for events read from subscriptions" \_ -> do
      store <- newStore
      let entityName = Event.EntityName "TestEntity"
      streamId <- Uuid.generate |> Task.map Event.StreamId

      -- Set up a subscription to capture events
      capturedEvents <- ConcurrentVar.containing Array.empty
      let handler event = do
            capturedEvents |> ConcurrentVar.modify (Array.push event)

      _subscriptionId <- store.subscribeToStreamEvents entityName streamId handler |> Task.mapError toText

      -- Create and insert events using payloadFromEvents
      let events = Array.fromLinkedList [MyEvent, MyEvent]
      payload <- Event.payloadFromEvents entityName streamId events
      _result <- store.insert payload |> Task.mapError toText

      -- Wait a bit for async notification
      AsyncTask.sleep 100 |> Task.mapError (\_ -> "timeout")

      -- Check captured events from subscription
      captured <- ConcurrentVar.peek capturedEvents
      Array.length captured |> shouldBe 2

      -- THIS IS THE BUG: Subscribers should receive events with local positions
      captured
        |> Array.indexed
        |> Task.forEach \(index, event) -> do
          case event.metadata.localPosition of
            Nothing ->
              fail [fmt|Subscriber received event at index #{toText index} with no local position!|]
            Just (Event.StreamPosition pos) ->
              pos |> shouldBe (fromIntegral index)

    it "derives local positions from stream length at insert time" \_ -> do
      store <- newStore
      let entityName = Event.EntityName "TestEntity"
      streamId <- Uuid.generate |> Task.map Event.StreamId

      -- Insert first batch
      let firstBatch = Array.fromLinkedList [MyEvent, MyEvent]
      payload1 <- Event.payloadFromEvents entityName streamId firstBatch
      _result1 <- store.insert payload1 |> Task.mapError toText

      -- Insert second batch
      let secondBatch = Array.fromLinkedList [MyEvent, MyEvent, MyEvent]
      payload2 <- Event.payloadFromEvents entityName streamId secondBatch
      _result2 <- store.insert payload2 |> Task.mapError toText

      -- Read all events
      allEvents <- store.readAllStreamEvents entityName streamId |> Task.mapError toText
      Array.length allEvents |> shouldBe 5

      -- Verify local positions are sequential from 0 to 4
      let expectedPositions =
            Array.fromLinkedList [0 :: Int, 1, 2, 3, 4]
              |> Array.map (fromIntegral .> Event.StreamPosition)

      let actualPositions =
            allEvents
              |> Array.map
                ( \event ->
                    event.metadata.localPosition
                      |> Maybe.withDefault (Event.StreamPosition (-1))
                )

      actualPositions |> shouldBe expectedPositions
