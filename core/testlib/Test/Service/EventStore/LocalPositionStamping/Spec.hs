module Test.Service.EventStore.LocalPositionStamping.Spec where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Maybe qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Stream qualified
import Task qualified
import Test
import Test.Service.EventStore.Core (BankAccountEvent (..))
import Uuid qualified


spec :: Task Text (EventStore BankAccountEvent) -> Spec Unit
spec newStore = do
  describe "Local Position Stamping" do
    it "stamps local positions when using payloadFromEvents helper" \_ -> do
      store <- newStore
      let entityName = Event.EntityName "TestEntity"
      streamId <- StreamId.new

      -- Create events using the payloadFromEvents helper (which sets localPosition to Nothing)
      let events =
            Array.fromLinkedList [AccountOpened {initialBalance = 1000}, MoneyDeposited {amount = 10}, MoneyWithdrawn {amount = 5}]
      payload <- Event.payloadFromEvents entityName streamId events

      -- Verify that insertions don't have local positions set (this is the input state)
      payload.insertions |> Task.forEach \insertion -> do
        insertion.metadata.localPosition |> shouldBe Nothing

      -- Insert the events
      _result <- store.insert payload |> Task.mapError toText

      -- Read back the events
      let startPosition = Event.StreamPosition 0
      let limit = EventStore.Limit 10
      streamMessages <-
        store.readStreamForwardFrom entityName streamId startPosition limit
          |> Task.mapError toText
          |> Task.andThen Stream.toArray

      let storedEvents = EventStore.collectStreamEvents streamMessages

      -- Verify we got 3 events back
      Array.length storedEvents |> shouldBe 3

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
      streamId <- StreamId.new

      -- Set up a subscription to capture events
      capturedEvents <- ConcurrentVar.containing Array.empty
      let handler event = do
            capturedEvents |> ConcurrentVar.modify (Array.push event)

      _subscriptionId <- store.subscribeToStreamEvents entityName streamId handler |> Task.mapError toText

      -- Create and insert events using payloadFromEvents
      let events = Array.fromLinkedList [AccountOpened {initialBalance = 2000}, MoneyDeposited {amount = 15}]
      payload <- Event.payloadFromEvents entityName streamId events
      _result <- store.insert payload |> Task.mapError toText

      -- Wait longer for async notifications to complete (increased from 100ms to 200ms to reduce flakiness)
      AsyncTask.sleep 200

      -- Check captured events from subscription
      captured <- ConcurrentVar.peek capturedEvents
      Array.length captured |> shouldBe 2

      -- First verify all events have local positions
      captured |> Task.forEach \event -> do
        case event.metadata.localPosition of
          Nothing -> fail "Subscriber received event with no local position!"
          Just _ -> Task.yield unit

      -- Check we received both position 0 and position 1 (in any order)
      let hasPosition pos =
            captured
              |> Array.takeIf
                ( \event ->
                    case event.metadata.localPosition of
                      Just (Event.StreamPosition p) -> p == pos
                      Nothing -> False
                )
              |> Array.length
              |> (== 1)

      Task.unless (hasPosition 0) do
        fail "Expected to receive event with local position 0"

      Task.unless (hasPosition 1) do
        fail "Expected to receive event with local position 1"

    it "derives local positions from stream length at insert time" \_ -> do
      store <- newStore
      let entityName = Event.EntityName "TestEntity"
      streamId <- StreamId.new

      -- Insert first batch
      let firstBatch = Array.fromLinkedList [AccountOpened {initialBalance = 500}, MoneyDeposited {amount = 100}]
      payload1 <- Event.payloadFromEvents entityName streamId firstBatch
      _result1 <- store.insert payload1 |> Task.mapError toText

      -- Insert second batch
      let secondBatch = Array.fromLinkedList [MoneyDeposited {amount = 50}, MoneyWithdrawn {amount = 25}, MoneyDeposited {amount = 75}]
      payload2 <- Event.payloadFromEvents entityName streamId secondBatch
      _result2 <- store.insert payload2 |> Task.mapError toText

      -- Read all events
      streamMessages <- store.readAllStreamEvents entityName streamId |> Task.mapError toText |> Task.andThen Stream.toArray
      let allEvents = EventStore.collectStreamEvents streamMessages
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

    it "preserves caller-provided local positions when explicitly set" \_ -> do
      store <- newStore
      let entityName = Event.EntityName "TestEntity"
      streamId <- StreamId.new

      -- Create first insertion with explicit local position
      id1 <- Uuid.generate
      metadata1 <- EventMetadata.new
      let insertion1 =
            Event.Insertion
              { id = id1,
                event = AccountOpened {initialBalance = 3000},
                metadata = metadata1 {EventMetadata.localPosition = Just (Event.StreamPosition 0)}
              }

      let payload1 =
            Event.InsertionPayload
              { streamId,
                entityName,
                insertionType = Event.StreamCreation,
                insertions = Array.fromLinkedList [insertion1]
              }

      _result1 <- store.insert payload1 |> Task.mapError toText

      -- Create second insertion with explicit local position
      id2 <- Uuid.generate
      metadata2 <- EventMetadata.new
      let insertion2 =
            Event.Insertion
              { id = id2,
                event = MoneyDeposited {amount = 10},
                metadata = metadata2 {EventMetadata.localPosition = Just (Event.StreamPosition 1)}
              }

      let payload2 =
            Event.InsertionPayload
              { streamId,
                entityName,
                insertionType = Event.ExistingStream,
                insertions = Array.fromLinkedList [insertion2]
              }

      _result2 <- store.insert payload2 |> Task.mapError toText

      -- Read back and verify positions were preserved
      streamMessages <- store.readAllStreamEvents entityName streamId |> Task.mapError toText |> Task.andThen Stream.toArray
      let allEvents = EventStore.collectStreamEvents streamMessages
      Array.length allEvents |> shouldBe 2

      -- Both events should have their explicitly-set local positions
      case Array.get 0 allEvents of
        Nothing -> fail "Expected first event"
        Just event1 ->
          event1.metadata.localPosition |> shouldBe (Just (Event.StreamPosition 0))

      case Array.get 1 allEvents of
        Nothing -> fail "Expected second event"
        Just event2 ->
          event2.metadata.localPosition |> shouldBe (Just (Event.StreamPosition 1))

    it "auto-assigns sequential positions when localPosition is Nothing" \_ -> do
      store <- newStore
      let entityName = Event.EntityName "TestEntity"
      streamId <- StreamId.new

      -- Create insertions with NO local position (Nothing)
      id1 <- Uuid.generate
      metadata1 <- EventMetadata.new
      let insertion1 =
            Event.Insertion
              { id = id1,
                event = MoneyDeposited {amount = 10},
                metadata = metadata1 {EventMetadata.localPosition = Nothing}
              }

      id2 <- Uuid.generate
      metadata2 <- EventMetadata.new
      let insertion2 =
            Event.Insertion
              { id = id2,
                event = MoneyDeposited {amount = 20},
                metadata = metadata2 {EventMetadata.localPosition = Nothing}
              }

      let payload =
            Event.InsertionPayload
              { streamId,
                entityName,
                insertionType = Event.AnyStreamState,
                insertions = Array.fromLinkedList [insertion1, insertion2]
              }

      _result <- store.insert payload |> Task.mapError toText

      -- Read back events
      streamMessages <- store.readAllStreamEvents entityName streamId |> Task.mapError toText |> Task.andThen Stream.toArray
      let allEvents = EventStore.collectStreamEvents streamMessages
      Array.length allEvents |> shouldBe 2

      -- Events should have auto-assigned sequential positions
      case Array.get 0 allEvents of
        Nothing -> fail "Expected first event"
        Just event1 ->
          event1.metadata.localPosition |> shouldBe (Just (Event.StreamPosition 0))

      case Array.get 1 allEvents of
        Nothing -> fail "Expected second event"
        Just event2 ->
          event2.metadata.localPosition |> shouldBe (Just (Event.StreamPosition 1))
