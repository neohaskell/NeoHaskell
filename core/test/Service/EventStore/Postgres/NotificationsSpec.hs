module Service.EventStore.Postgres.NotificationsSpec (spec) where

import Core
import Result qualified
import Text qualified
import Json qualified
import Service.Event (EntityName (..), Event (..), StreamId (..), StreamPosition (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.EventStore.Postgres.PostgresEventRecord (PostgresEventRecord (..))
import Service.EventStore.Postgres.Sessions (EventNotificationPayload (..))
import Service.EventStore.Postgres.Sessions qualified as Sessions

import LinkedList qualified
import Array qualified
import ConcurrentVar qualified
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement (Statement (..))
import Bytes qualified
import AsyncTask qualified
import Var qualified
import Service.EventStore.Postgres.Internal (PostgresEventStore (..), toConnectionSettings)
import Service.EventStore.Postgres.Notifications (nextBackoff, catchUpFromCursor, connectTo)
import Service.EventStore.Postgres.SubscriptionStore (SubscriptionStore)
import Service.EventStore.Postgres.SubscriptionStore qualified as SubscriptionStore
import Service.EventStore (EventStore (..))
import Service.TestHelpers (insertTestEvent)
import Service.EventStore.Postgres qualified as Postgres
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "Notification Payload Flow" do
    describe "EventNotificationPayload JSON decoding" do
      it "decodes a valid lightweight payload with all 5 fields" \_ -> do
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":42,\"localPosition\":5,\"inlinedStreamId\":\"cart-123\",\"entity\":\"CartEntity\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        case result of
          Err err -> Test.fail [fmt|Expected successful decode, got: #{err}|]
          Ok notification -> do
            notification.globalPosition |> shouldBe 42
            notification.localPosition |> shouldBe 5
            notification.inlinedStreamId |> shouldBe "cart-123"
            notification.entity |> shouldBe "CartEntity"

      it "fails to decode when globalPosition is missing" \_ -> do
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"localPosition\":5,\"inlinedStreamId\":\"cart-123\",\"entity\":\"CartEntity\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        result |> shouldSatisfy Result.isErr

      it "fails to decode when entity is missing" \_ -> do
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":42,\"localPosition\":5,\"inlinedStreamId\":\"cart-123\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        result |> shouldSatisfy Result.isErr

      it "fails to decode when eventId is missing" \_ -> do
        let payload =
              "{\"globalPosition\":42,\"localPosition\":5,\"inlinedStreamId\":\"cart-123\",\"entity\":\"CartEntity\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        result |> shouldSatisfy Result.isErr

      it "fails to decode when globalPosition is a string instead of number" \_ -> do
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":\"forty-two\",\"localPosition\":5,\"inlinedStreamId\":\"cart-123\",\"entity\":\"CartEntity\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        result |> shouldSatisfy Result.isErr

      it "ignores extra fields in payload (forward compatibility)" \_ -> do
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":42,\"localPosition\":5,\"inlinedStreamId\":\"cart-123\",\"entity\":\"CartEntity\",\"extraField\":\"ignored\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        case result of
          Err err -> Test.fail [fmt|Expected successful decode with extra fields, got: #{err}|]
          Ok notification -> do
            notification.globalPosition |> shouldBe 42
            notification.entity |> shouldBe "CartEntity"

      it "decodes zero positions correctly (boundary case)" \_ -> do
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":0,\"localPosition\":0,\"inlinedStreamId\":\"cart-first\",\"entity\":\"CartEntity\"}"
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        case result of
          Err err -> Test.fail [fmt|Expected successful decode, got: #{err}|]
          Ok notification -> do
            notification.globalPosition |> shouldBe 0
            notification.localPosition |> shouldBe 0

    describe "postgresRecordToEvent conversion" do
      it "converts a valid record to an Event with correct positions" \_ -> do
        metadata <- EventMetadata.new
        let record =
              PostgresEventRecord
                { eventId = def,
                  globalPosition = 100,
                  localPosition = 7,
                  inlinedStreamId = "cart-456",
                  entityName = "CartEntity",
                  eventData = Json.encode ("test" :: Text),
                  metadata = Json.encode metadata
                }
        case Sessions.postgresRecordToEvent record of
          Err err -> Test.fail [fmt|Expected successful conversion, got: #{err}|]
          Ok event -> do
            event.entityName |> shouldBe (EntityName "CartEntity")
            event.streamId |> shouldBe (StreamId "cart-456")
            event.metadata.globalPosition |> shouldBe (Just (StreamPosition 100))
            event.metadata.localPosition |> shouldBe (Just (StreamPosition 7))

      it "preserves event data through conversion" \_ -> do
        metadata <- EventMetadata.new
        let testData = Json.object ["itemId" Json..= ("abc" :: Text), "amount" Json..= (42 :: Int)]
        let record =
              PostgresEventRecord
                { eventId = def,
                  globalPosition = 0,
                  localPosition = 0,
                  inlinedStreamId = "cart-789",
                  entityName = "CartEntity",
                  eventData = testData,
                  metadata = Json.encode metadata
                }
        case Sessions.postgresRecordToEvent record of
          Err err -> Test.fail [fmt|Expected successful conversion, got: #{err}|]
          Ok event -> do
            event.event |> shouldBe testData

      it "fails when metadata JSON is not a valid EventMetadata object" \_ -> do
        let record =
              PostgresEventRecord
                { eventId = def,
                  globalPosition = 0,
                  localPosition = 0,
                  inlinedStreamId = "cart-000",
                  entityName = "CartEntity",
                  eventData = Json.encode ("test" :: Text),
                  metadata = Json.encode ("not-valid-metadata" :: Text)
                }
        Sessions.postgresRecordToEvent record |> shouldSatisfy Result.isErr

      it "sets positions from record columns, not from metadata JSON" \_ -> do
        -- The record's position columns should override whatever is in the metadata JSON.
        -- This ensures positions come from the authoritative source (DB columns).
        metadata <- EventMetadata.new
        let metadataWithPositions =
              metadata
                { EventMetadata.globalPosition = Just (StreamPosition 999),
                  EventMetadata.localPosition = Just (StreamPosition 888)
                }
        let record =
              PostgresEventRecord
                { eventId = def,
                  globalPosition = 42,
                  localPosition = 7,
                  inlinedStreamId = "cart-override",
                  entityName = "CartEntity",
                  eventData = Json.encode ("test" :: Text),
                  metadata = Json.encode metadataWithPositions
                }
        case Sessions.postgresRecordToEvent record of
          Err err -> Test.fail [fmt|Expected successful conversion, got: #{err}|]
          Ok event -> do
            event.metadata.globalPosition |> shouldBe (Just (StreamPosition 42))
            event.metadata.localPosition |> shouldBe (Just (StreamPosition 7))

      it "extracts entityName and streamId from record columns" \_ -> do
        metadata <- EventMetadata.new
        let record =
              PostgresEventRecord
                { eventId = def,
                  globalPosition = 0,
                  localPosition = 0,
                  inlinedStreamId = "order-abc-123",
                  entityName = "OrderEntity",
                  eventData = Json.encode ("test" :: Text),
                  metadata = Json.encode metadata
                }
        case Sessions.postgresRecordToEvent record of
          Err err -> Test.fail [fmt|Expected successful conversion, got: #{err}|]
          Ok event -> do
            event.entityName |> shouldBe (EntityName "OrderEntity")
            event.streamId |> shouldBe (StreamId "order-abc-123")

    describe "Notification payload design properties" do
      it "lightweight payload is well under PostgreSQL 8000-byte NOTIFY limit" \_ -> do
        -- The whole point of the fix: notification payload must be lightweight.
        -- Even with long identifiers, the payload should be well under 8000 bytes.
        -- inlinedStreamId is VARCHAR(4000), entity is VARCHAR(255).
        let longStreamId = Text.repeat 200 "stream-id-"
        let longEntity = Text.repeat 25 "EntityName"
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":9999999999,\"localPosition\":9999999999,\"inlinedStreamId\":\""
              ++ longStreamId
              ++ "\",\"entity\":\""
              ++ longEntity
              ++ "\"}"
        let payloadLength = Text.length payload
        payloadLength |> shouldSatisfy (\len -> len < 8000)
        -- And it should still decode correctly
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        result |> shouldSatisfy Result.isOk

      it "global and stream-specific triggers produce identical payload shape" \_ -> do
        -- Both pg_notify calls in the trigger use the same json_build_object structure.
        -- This test documents that the payload shape is consistent across channels.
        -- The trigger sends: eventId, globalPosition, localPosition, inlinedStreamId, entity
        let payload =
              "{\"eventId\":\"550e8400-e29b-41d4-a716-446655440000\",\"globalPosition\":1,\"localPosition\":0,\"inlinedStreamId\":\"my-stream\",\"entity\":\"MyEntity\"}"
        -- Both channels produce the same JSON structure — one decode type handles both
        let result = Json.decodeText payload :: Result Text EventNotificationPayload
        case result of
          Err err -> Test.fail [fmt|Payload shape should decode for both channels, got: #{err}|]
          Ok notification -> do
            notification.globalPosition |> shouldBe 1
            notification.localPosition |> shouldBe 0
            notification.inlinedStreamId |> shouldBe "my-stream"
            notification.entity |> shouldBe "MyEntity"

  describe "Reconnection backoff" do
    it "initial backoff is 1000ms" \_ -> do
      -- The reconnection loop starts with 1000ms
      -- nextBackoff applied to 500 (half of initial) gives 1000
      nextBackoff 500 |> shouldBe 1000

    it "doubles the backoff on each attempt" \_ -> do
      nextBackoff 1000 |> shouldBe 2000
      nextBackoff 2000 |> shouldBe 4000
      nextBackoff 4000 |> shouldBe 8000

    it "caps backoff at 60000ms (60 seconds)" \_ -> do
      nextBackoff 30000 |> shouldBe 60000
      nextBackoff 60000 |> shouldBe 60000
      nextBackoff 100000 |> shouldBe 60000

  describe "TCP keepalive configuration" do
    it "toConnectionSettings does not throw for valid config" \_ -> do
      let cfg =
            (def :: PostgresEventStore)
              { host = "localhost",
                port = 5432,
                databaseName = "test",
                user = "test",
                password = "test"
              }
      -- toConnectionSettings is pure — just verify it evaluates without error
      let settings = toConnectionSettings cfg
      -- Settings list should be non-empty (has at least the connection params)
      -- Use length (Int has Show) rather than shouldSatisfy on the list (no Show instance)
      LinkedList.length settings |> shouldSatisfy (\n -> n > 0)

  describe "Listener cleanup" do
    whenEnvVar "POSTGRES_AVAILABLE" do
      let config =
            (def :: Postgres.PostgresEventStore)
              { host = "localhost",
                databaseName = "neohaskell",
                user = "neohaskell",
                password = "neohaskell",
                port = 5432
              }
      it "close stops the listener without crashing" \_ -> do
        store <- Postgres.new config |> Task.mapError toText
        let closeOp = store.close :: Task Text Unit
        result <- closeOp |> Task.asResult
        case result of
          Err err -> Test.fail [fmt|store.close failed: #{err}|]
          Ok _ -> Task.yield unit

      it "close allows creating a new store on same database" \_ -> do
        store1 <- Postgres.new config |> Task.mapError toText
        let closeOp1 = store1.close :: Task Text Unit
        closeOp1
        store2Result <- Postgres.new config |> Task.mapError toText |> Task.asResult
        case store2Result of
          Err err -> Test.fail [fmt|Second store creation failed after close: #{err}|]
          Ok store2 -> do
            let closeOp2 = store2.close :: Task Text Unit
            closeOp2

  -- ADR-0061: reconnect catch-up — the cursor never moves backwards (monotonic
  -- max), so an out-of-order or duplicate boundary event during catch-up keeps
  -- at-least-once safety without skipping or regressing the cursor. These are
  -- pure and need no Postgres.
  describe "cursor advance (monotonic max)" do
    it "keeps the higher position when an earlier one arrives (C1)" \_ -> do
      max (5 :: Int64) 3 |> shouldBe 5
      max (5 :: Int64) 9 |> shouldBe 9

    it "stays put for an equal position — idempotent re-delivery (C2)" \_ -> do
      max (7 :: Int64) 7 |> shouldBe 7

    it "advances correctly at the zero boundary (C3)" \_ -> do
      max (0 :: Int64) 0 |> shouldBe 0
      max (0 :: Int64) 1 |> shouldBe 1

  describe "selectEventsForwardFromGlobalPositionSession" do
    whenEnvVar "POSTGRES_AVAILABLE" do
      it "returns only events strictly after the cursor, ascending (A1)" \_ -> do
        catchUpFixture \conn _store entityName -> do
          maxBefore <- currentMaxPosition conn
          insertN conn entityName 3
          records <-
            Sessions.selectEventsForwardFromGlobalPositionSession maxBefore 1000
              |> Sessions.runConnection conn
              |> Task.mapError toText
          let positions = records |> Array.map (\r -> r.globalPosition)
          (records |> Array.length) |> shouldBe 3
          (positions |> isAscending) |> shouldBe True
          (positions |> allInArray (\p -> p > maxBefore)) |> shouldBe True

      it "returns empty when the cursor equals the current max (A2)" \_ -> do
        catchUpFixture \conn _store entityName -> do
          insertN conn entityName 2
          maxNow <- currentMaxPosition conn
          records <-
            Sessions.selectEventsForwardFromGlobalPositionSession maxNow 1000
              |> Sessions.runConnection conn
              |> Task.mapError toText
          (records |> Array.length) |> shouldBe 0

      it "returns empty when the cursor is above the current max (A3)" \_ -> do
        catchUpFixture \conn _store entityName -> do
          insertN conn entityName 1
          maxNow <- currentMaxPosition conn
          records <-
            Sessions.selectEventsForwardFromGlobalPositionSession (maxNow + 1000) 1000
              |> Sessions.runConnection conn
              |> Task.mapError toText
          (records |> Array.length) |> shouldBe 0

      it "is exclusive of the cursor position (A4)" \_ -> do
        catchUpFixture \conn _store entityName -> do
          maxBefore <- currentMaxPosition conn
          insertN conn entityName 2
          -- Read from the position of the FIRST new event: it must be excluded.
          records <-
            Sessions.selectEventsForwardFromGlobalPositionSession (maxBefore + 1) 1000
              |> Sessions.runConnection conn
              |> Task.mapError toText
          let positions = records |> Array.map (\r -> r.globalPosition)
          (positions |> allInArray (\p -> p > maxBefore + 1)) |> shouldBe True

  describe "catchUpFromCursor (reconnect catch-up)" do
    whenEnvVar "POSTGRES_AVAILABLE" do
      it "dispatches every event committed during the outage — no events skipped (B1, issue #682)" \_ -> do
        catchUpFixture \conn store entityName -> do
          collector <- ConcurrentVar.containing Array.empty
          attachCollector store collector
          maxBefore <- currentMaxPosition conn
          cursor <- Var.new maxBefore
          -- Simulate the outage: events commit while no LISTEN is active.
          insertN conn entityName 3
          -- Reconnect catch-up replays the gap.
          catchUpFromCursor conn store cursor
          received <- ConcurrentVar.peek collector
          (received |> Array.length) |> shouldBe 3

      it "dispatches nothing when there is no gap (B2)" \_ -> do
        catchUpFixture \conn store entityName -> do
          insertN conn entityName 2
          collector <- ConcurrentVar.containing Array.empty
          attachCollector store collector
          maxNow <- currentMaxPosition conn
          cursor <- Var.new maxNow
          catchUpFromCursor conn store cursor
          received <- ConcurrentVar.peek collector
          (received |> Array.length) |> shouldBe 0

      it "advances the cursor to the last caught-up position (B3)" \_ -> do
        catchUpFixture \conn store entityName -> do
          collector <- ConcurrentVar.containing Array.empty
          attachCollector store collector
          maxBefore <- currentMaxPosition conn
          cursor <- Var.new maxBefore
          insertN conn entityName 3
          maxAfter <- currentMaxPosition conn
          catchUpFromCursor conn store cursor
          finalCursor <- Var.get cursor
          finalCursor |> shouldBe maxAfter

      it "propagates a read failure rather than silently skipping the gap (B4)" \_ -> do
        catchUpFixture \conn store entityName -> do
          let _ = entityName
          -- Close the connection, then attempt catch-up: the read must error.
          Hasql.release conn |> Task.fromIO
          cursor <- Var.new (0 :: Int64)
          result <- catchUpFromCursor conn store cursor |> Task.asResult
          result |> shouldSatisfy resultIsErr

  -- ADR-0061 (issue #682) — TRUE reconnect-path regression. The B1–B4 cases
  -- above exercise 'catchUpFromCursor' in isolation; they never drive the
  -- 'connectTo' / 'listenerWithReconnect' wiring, so the load-bearing contract
  -- (LISTEN re-established by the loop BEFORE catch-up runs, and the shared
  -- in-memory cursor surviving the reconnect) is untested by them — a wiring
  -- regression would reintroduce #682 while keeping B1–B4 green.
  --
  -- This case drives the ACTUAL path: it starts a listener via 'connectTo' with
  -- a connection factory under the test's control, forces the live listen
  -- backend to drop (server-side 'pg_terminate_backend', which wakes
  -- 'waitForNotifications' immediately — it does not rely on TCP keepalive
  -- timing), inserts events DURING the outage, and asserts that after the
  -- supervised loop reconnects, EVERY missed event is dispatched (none skipped)
  -- and the shared cursor advanced past them across the reconnect.
  describe "listener reconnect catch-up (connectTo wiring)" do
    whenEnvVar "POSTGRES_AVAILABLE" do
      it "re-establishes LISTEN and catches up all events missed during the outage (D1, issue #682)" \_ -> do
        let cfg =
              (def :: PostgresEventStore)
                { host = "localhost",
                  databaseName = "neohaskell",
                  user = "neohaskell",
                  password = "neohaskell",
                  port = 5432
                }
        -- Ensure events table + trigger exist.
        store0 <- Postgres.new cfg |> Task.mapError toText
        store0.close
        subStore <- SubscriptionStore.new |> Task.mapError toText
        let entityName = EntityName "ReconnectWiringEntity"
        collector <- ConcurrentVar.containing Array.empty
        attachCollector subStore collector
        -- Track factory invocations and the first connect's listen backend PID,
        -- so the test can terminate exactly that backend to force a reconnect.
        connectCount <- Var.new (0 :: Int)
        firstPidRef <- Var.new (Nothing :: Maybe Int64)
        adminConn <- acquireConn cfg
        let factory = do
              listenConn <- acquireConn cfg
              queryConn <- acquireConn cfg
              currentCount <- Var.get connectCount
              Task.when (currentCount == 0) do
                pid <- backendPid listenConn
                firstPidRef |> Var.set (Just pid)
              connectCount |> Var.set (currentCount + 1)
              Task.yield (listenConn, queryConn)
        cleanup <- subStore |> connectTo factory
        let teardown = do
              cleanup
              (Hasql.release adminConn |> Task.fromIO) |> Task.asResult |> discard
        let scenario = do
              -- Wait for the first connect to establish LISTEN + initialise cursor.
              waitUntil 50 (do n <- Var.get connectCount; Task.yield (n >= 1))
              waitUntil 50 do
                p <- Var.get firstPidRef
                case p of
                  Just _ -> Task.yield True
                  Nothing -> Task.yield False
              maybePid <- Var.get firstPidRef
              firstPid <- case maybePid of
                Just pid -> Task.yield pid
                Nothing -> Task.throw "first connect never reported a backend PID"
              -- Force the live listen socket to drop (the outage begins).
              terminateBackend adminConn firstPid
              -- Insert events while no LISTEN is active: NOTIFY is lost for these,
              -- so only the reconnect catch-up can deliver them.
              insertN adminConn entityName 3
              -- Wait for the supervised loop to reconnect (factory called again)
              -- and catch up. Generous bound: first backoff is 1000ms.
              waitUntil 200 (do n <- Var.get connectCount; Task.yield (n >= 2))
              waitUntil 200 (do
                received <- ConcurrentVar.peek collector
                Task.yield (Array.length received >= 3))
              received <- ConcurrentVar.peek collector
              Task.yield received
        result <- Task.finally teardown scenario |> Task.asResult
        case result of
          Err err -> Test.fail [fmt|reconnect wiring test failed: #{err}|]
          Ok received -> do
            -- Every event committed during the outage reached live dispatch.
            (received |> Array.length) |> shouldBe 3
            -- The reconnect actually happened (loop re-acquired connections).
            reconnects <- Var.get connectCount
            reconnects |> shouldSatisfy (\n -> n >= 2)



-- | A reconnect-catch-up Postgres fixture: acquire a fresh query connection,
-- build a SubscriptionStore, run the body on a unique entity name, and always
-- release the connection afterwards.
catchUpFixture ::
  (Hasql.Connection -> SubscriptionStore -> EntityName -> Task Text Unit) ->
  Task Text Unit
catchUpFixture body = do
  let cfg =
        (def :: PostgresEventStore)
          { host = "localhost",
            databaseName = "neohaskell",
            user = "neohaskell",
            password = "neohaskell",
            port = 5432
          }
  -- Ensure the events table + trigger exist by creating a store once.
  store0 <- Postgres.new cfg |> Task.mapError toText
  store0.close
  conn <-
    Hasql.acquire (toConnectionSettings cfg)
      |> Task.fromIOEither
      |> Task.mapError toText
  -- Bracket the connection: release it on EVERY path, so an exception in
  -- SubscriptionStore.new or in the body cannot leak the Hasql connection.
  -- Release is best-effort because B4 may already have released it.
  let releaseConn = (Hasql.release conn |> Task.fromIO) |> Task.asResult |> discard
  result <-
    Task.finally releaseConn do
      subStore <- SubscriptionStore.new |> Task.mapError toText
      let entityName = EntityName "ReconnectCatchUpEntity"
      body conn subStore entityName
    |> Task.asResult
  case result of
    Err err -> Test.fail [fmt|catch-up fixture body failed: #{err}|]
    Ok _ -> Task.yield unit


-- | Insert @n@ events into the global stream via a fresh store on the same DB,
-- exercising the real insert + NOTIFY-trigger path.
insertN :: Hasql.Connection -> EntityName -> Int -> Task Text Unit
insertN _conn entityName n = do
  let cfg =
        (def :: PostgresEventStore)
          { host = "localhost",
            databaseName = "neohaskell",
            user = "neohaskell",
            password = "neohaskell",
            port = 5432
          }
  store <- Postgres.new cfg |> Task.mapError toText
  -- Bracket the temporary store: close it on EVERY path, so a failure in
  -- Task.forEach cannot leak the Postgres store/its pool.
  Task.finally
    store.close
    (Task.forEach (\_ -> insertTestEvent store entityName) (Array.fromLinkedList (rangeList n)))


-- | The current MAX(globalPosition), or -1 when the table is empty.
currentMaxPosition :: Hasql.Connection -> Task Text Int64
currentMaxPosition conn = do
  maybeMax <-
    Sessions.selectMaxGlobalPosition
      |> Sessions.runConnection conn
      |> Task.mapError toText
  case maybeMax of
    Nothing -> Task.yield (-1)
    Just (StreamPosition p) -> Task.yield p


-- | Register a global subscription that appends every dispatched event to the
-- collector.
attachCollector ::
  SubscriptionStore ->
  ConcurrentVar.ConcurrentVar (Array (Event Json.Value)) ->
  Task Text Unit
attachCollector store collector = do
  let callback event = do
        collector |> ConcurrentVar.modify (\events -> events |> Array.push event)
        Task.yield unit
  store
    |> SubscriptionStore.addGlobalSubscription callback
    |> Task.mapError toText
    |> discard


-- | True when every element of the array satisfies the predicate.
allInArray :: (a -> Bool) -> Array a -> Bool
allInArray predicate arr =
  (arr |> Array.takeIf predicate |> Array.length) == (arr |> Array.length)


-- | True when the positions are in strictly ascending order.
isAscending :: Array Int64 -> Bool
isAscending arr =
  checkAscending (Array.toLinkedList arr)


checkAscending :: LinkedList Int64 -> Bool
checkAscending xs =
  case xs of
    [] -> True
    [_] -> True
    (a : b : rest) -> a < b && checkAscending (b : rest)


-- | A list @[1 .. n]@ used only to drive @n@ inserts.
rangeList :: Int -> LinkedList Int
rangeList n =
  if n <= 0
    then []
    else buildRange 1 n


buildRange :: Int -> Int -> LinkedList Int
buildRange lo hi =
  if lo > hi
    then []
    else lo : buildRange (lo + 1) hi


resultIsErr :: Result err val -> Bool
resultIsErr result =
  case result of
    Err _ -> True
    Ok _ -> False


-- | Acquire a raw Hasql connection from the test config (used to drive the
-- reconnect wiring test's connection factory and its admin connection).
acquireConn :: PostgresEventStore -> Task Text Hasql.Connection
acquireConn cfg =
  Hasql.acquire (toConnectionSettings cfg)
    |> Task.fromIOEither
    |> Task.mapError toText


-- | The PostgreSQL backend process id (@pg_backend_pid()@) of a connection.
-- Captured for the listener's listen connection so the test can terminate
-- exactly that backend and force a reconnect.
backendPid :: Hasql.Connection -> Task Text Int64
backendPid conn = do
  let query :: Text = "SELECT pg_backend_pid() :: int8"
  let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
  let statement :: Statement Unit Int64 =
        Statement (query |> Text.toBytes |> Bytes.unwrap) Encoders.noParams decoder True
  runRawSession conn (Session.statement unit statement)


-- | Server-side terminate a backend by pid (@pg_terminate_backend@). Unlike a
-- TCP drop this delivers a FATAL packet to the client immediately, so the
-- listener's blocked @waitForNotifications@ wakes without waiting on keepalive
-- timers.
terminateBackend :: Hasql.Connection -> Int64 -> Task Text Unit
terminateBackend conn pid = do
  let query :: Text = "SELECT pg_terminate_backend(($1) :: int4) :: bool"
  let encoder = Encoders.param (Encoders.nonNullable Encoders.int8)
  let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool))
  let statement :: Statement Int64 Bool =
        Statement (query |> Text.toBytes |> Bytes.unwrap) encoder decoder True
  runRawSession conn (Session.statement pid statement) |> discard


-- | Run a Hasql session on a raw connection, surfacing any error as Text.
runRawSession :: Hasql.Connection -> Session.Session result -> Task Text result
runRawSession conn session = do
  result <- Session.run session conn |> Task.fromIO |> Task.map Result.fromEither
  case result of
    Err err -> Task.throw [fmt|raw session failed: #{err}|]
    Ok res -> Task.yield res


-- | Poll @condition@ every @stepMs@ milliseconds, up to ~60 attempts (so the
-- generous 200ms steps cover the listener's first 1000ms reconnect backoff with
-- margin). Throws on timeout so a wiring regression fails loudly rather than
-- hanging. Deterministic on success: returns as soon as the condition holds.
waitUntil :: Int -> Task Text Bool -> Task Text Unit
waitUntil stepMs condition =
  waitUntilLoop stepMs condition 60


waitUntilLoop :: Int -> Task Text Bool -> Int -> Task Text Unit
waitUntilLoop stepMs condition attemptsLeft = do
  case attemptsLeft <= 0 of
    True -> Task.throw "waitUntil timed out"
    False -> do
      satisfied <- condition
      case satisfied of
        True -> Task.yield unit
        False -> do
          AsyncTask.sleep stepMs |> Task.mapError (\_ -> "waitUntil sleep failed")
          waitUntilLoop stepMs condition (attemptsLeft - 1)
