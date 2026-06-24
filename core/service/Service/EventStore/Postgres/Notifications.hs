module Service.EventStore.Postgres.Notifications (
  connectTo,
  nextBackoff,
  subscribeToStream,
  catchUpFromCursor,
) where

import Array qualified
import AsyncTask qualified
import Bytes qualified
import Core hiding (Var)
import Data.ByteString qualified
import Hasql.Connection qualified as Hasql
import Hasql.Notifications qualified as HasqlNotifications
import Json qualified
import Log qualified
import Maybe (Maybe (..))
import Service.Event (Event (..), StreamPosition (StreamPosition))
import Service.EventStore.Postgres.PostgresEventRecord (PostgresEventRecord (globalPosition))
import Service.EventStore.Postgres.Sessions qualified as Sessions
import Service.EventStore.Postgres.SubscriptionStore (SubscriptionStore)
import Service.EventStore.Postgres.SubscriptionStore qualified as SubscriptionStore
import Task qualified
import Var (Var)
import Var qualified


connectTo ::
  Task Text (Hasql.Connection, Hasql.Connection) ->
  SubscriptionStore ->
  Task Text (Task Text Unit)
connectTo acquireConnections store = do
  shutdownRef <- (Var.new False :: Task Text (Var Bool))
  currentConnectionsRef <- (Var.new (Maybe.Nothing :: Maybe (Hasql.Connection, Hasql.Connection)) :: Task Text (Var (Maybe (Hasql.Connection, Hasql.Connection))))
  -- ADR-0061: last-processed globalPosition, shared across reconnects for the
  -- life of this listener task. Initialised to the current max on the first
  -- connect ("subscribe from now"); advanced by the live handler and by the
  -- reconnect catch-up. The DB is the durable cursor — this Var is transient.
  lastProcessedRef <- (Var.new (0 :: Int64) :: Task Text (Var Int64))
  initialisedRef <- (Var.new False :: Task Text (Var Bool))
  let listenerWithReconnect backoffMs = do
        isShutdown <- Var.get shutdownRef
        Task.unless isShutdown do
          result <- Task.asResultSafe do
            (listenConnection, queryConnection) <- acquireConnections
            let releaseConns = do
                  currentConnectionsRef |> Var.set Maybe.Nothing
                  Hasql.release listenConnection |> Task.fromIO
                  Hasql.release queryConnection |> Task.fromIO
            Task.finally
              releaseConns
              do
                currentConnectionsRef |> Var.set (Maybe.Just (listenConnection, queryConnection))
                let channelToListen = HasqlNotifications.toPgIdentifier "global"
                HasqlNotifications.listen listenConnection channelToListen
                  |> Task.fromIO
                  |> discard
                Log.info "LISTEN/NOTIFY listener started"
                  |> Task.ignoreError
                -- LOAD-BEARING ORDERING (ADR-0061): the catch-up below runs
                -- AFTER LISTEN is active and BEFORE the live wait. LISTEN being
                -- active first guarantees that an event committed during the
                -- catch-up read is also delivered live (at-least-once overlap,
                -- never a gap). On the very first connect we instead initialise
                -- the cursor to the current max ("from now") and skip catch-up.
                -- Do NOT move this catch-up before the LISTEN call.
                initialiseOrCatchUp queryConnection store lastProcessedRef initialisedRef
                listenConnection
                  |> HasqlNotifications.waitForNotifications (handler queryConnection store lastProcessedRef)
                  |> Task.fromIO
          case result of
            Ok _ -> do
              Log.critical "LISTEN/NOTIFY listener returned unexpectedly. Reconnecting..."
                |> Task.ignoreError
              AsyncTask.sleep backoffMs
              listenerWithReconnect (nextBackoff backoffMs)
            Err err -> do
              Log.critical [fmt|LISTEN/NOTIFY listener crashed: #{err}. Reconnecting in #{backoffMs}ms...|]
                |> Task.ignoreError
              AsyncTask.sleep backoffMs
              listenerWithReconnect (nextBackoff backoffMs)
  asyncTask <- listenerWithReconnect 1000 |> AsyncTask.run
  let cleanup = do
        shutdownRef |> Var.set True
        asyncTask |> AsyncTask.cancel
        maybeConns <- Var.get currentConnectionsRef
        case maybeConns of
          Maybe.Nothing -> Task.yield unit
          Maybe.Just (listenConn, queryConn) -> do
            Hasql.release listenConn |> Task.fromIO
            Hasql.release queryConn |> Task.fromIO
            currentConnectionsRef |> Var.set Maybe.Nothing
  Task.yield cleanup


subscribeToStream ::
  Hasql.Connection ->
  StreamId ->
  Task Text Unit
subscribeToStream connection streamId = do
  let channelToListen = HasqlNotifications.toPgIdentifier (toText streamId)
  HasqlNotifications.listen connection channelToListen
    |> Task.fromIO
    |> discard


handler ::
  Hasql.Connection ->
  SubscriptionStore ->
  Var Int64 ->
  Data.ByteString.ByteString ->
  Data.ByteString.ByteString ->
  IO ()
handler queryConnection store lastProcessedRef _channelName payloadLegacyBytes = do
  result <- processNotification queryConnection store lastProcessedRef payloadLegacyBytes |> Task.runResult
  case result of
    Err err ->
      ((Log.warn [fmt|#{err}|] |> Task.ignoreError) :: Task Text Unit) |> Task.runOrPanic
    Ok _ ->
      pass


processNotification ::
  Hasql.Connection ->
  SubscriptionStore ->
  Var Int64 ->
  Data.ByteString.ByteString ->
  Task Text Unit
processNotification queryConnection store lastProcessedRef payloadLegacyBytes = do
  Log.withScope [("component", "Notifications")] do
    notification <- decodeNotification payloadLegacyBytes
    event <- fetchFullEvent queryConnection notification.globalPosition
    Log.withScope [("component", "Notifications"), ("streamId", toText event.streamId)] do
      store |> SubscriptionStore.dispatch event.streamId event |> Task.mapError toText
      -- ADR-0061: advance the shared cursor so a later reconnect catch-up knows
      -- where live dispatch reached.
      advanceCursor lastProcessedRef notification.globalPosition
      Log.debug "Event dispatched from notification" |> Task.ignoreError


decodeNotification ::
  Data.ByteString.ByteString ->
  Task Text Sessions.EventNotificationPayload
decodeNotification payloadLegacyBytes = do
  let result =
        payloadLegacyBytes
          |> Bytes.fromLegacy
          |> Json.decodeBytes :: Result Text Sessions.EventNotificationPayload
  case result of
    Err err -> Task.throw [fmt|Notification decode failed: #{err}|]
    Ok notification -> Task.yield notification


fetchFullEvent ::
  Hasql.Connection ->
  Int64 ->
  Task Text (Event Json.Value)
fetchFullEvent queryConnection globalPosition = do
  maybeRecord <-
    Sessions.selectEventByGlobalPositionSession globalPosition
      |> Sessions.runConnection queryConnection
      |> Task.mapError toText
  record <- case maybeRecord of
    Nothing -> Task.throw [fmt|Event not found for globalPosition #{globalPosition}|]
    Just r -> Task.yield r
  case Sessions.postgresRecordToEvent record of
    Err err -> Task.throw [fmt|Event record decode failed: #{err}|]
    Ok event -> Task.yield event


nextBackoff :: Int -> Int
nextBackoff backoffMs =
  min 60000 (backoffMs * 2)


-- | On the first connect, initialise the cursor to the current max
-- globalPosition ("subscribe from now") and skip catch-up — a cold listener
-- inherits the same semantics it always had. On every later connect (a
-- reconnect), replay the gap via 'catchUpFromCursor'. The first-connect branch
-- is what stops the listener from replaying the entire history on startup.
initialiseOrCatchUp ::
  Hasql.Connection ->
  SubscriptionStore ->
  Var Int64 ->
  Var Bool ->
  Task Text Unit
initialiseOrCatchUp queryConnection store lastProcessedRef initialisedRef = do
  initialised <- Var.get initialisedRef
  case initialised of
    False -> do
      maybeMax <-
        Sessions.selectMaxGlobalPosition
          |> Sessions.runConnection queryConnection
          |> Task.mapError toText
      case maybeMax of
        Maybe.Nothing -> lastProcessedRef |> Var.set 0
        Maybe.Just (StreamPosition maxPos) -> lastProcessedRef |> Var.set maxPos
      initialisedRef |> Var.set True
    True ->
      catchUpFromCursor queryConnection store lastProcessedRef


-- | Catch-up batch size: events read per forward page during a reconnect
-- catch-up. Matches the ADR-0059 query-rebuild chunk size (1000) so the two
-- forward-replay paths page consistently and the catch-up read stays bounded
-- regardless of how long the listener was offline.
catchUpBatchSize :: Int64
catchUpBatchSize = 1000


-- | Replay every event committed after the last-processed @globalPosition@
-- through the shared 'SubscriptionStore', advancing the cursor as it goes.
-- Invoked on each listener reconnect, AFTER @LISTEN@ is re-established and
-- BEFORE the live wait, so the gap created while the listen socket was down
-- reaches live dispatch (ADR-0061). The dispatch sink is identical to the
-- live handler's, so there is one sink and one read implementation.
--
-- The read is BOUNDED: rather than pulling every row after the cursor into one
-- 'Array' (which a long outage could blow up), it reads in pages of
-- 'catchUpBatchSize', dispatches each page, advances the cursor to the last
-- 'globalPosition' in the page (via 'dispatchCaughtUpRecord'), and repeats until
-- a page returns fewer rows than the limit (the gap is drained). Paging is
-- monotonic in @globalPosition@, so at-least-once/idempotency are preserved and
-- ordering is unchanged.
catchUpFromCursor ::
  Hasql.Connection ->
  SubscriptionStore ->
  Var Int64 ->
  Task Text Unit
catchUpFromCursor queryConnection store lastProcessedRef = do
  cursor <- Var.get lastProcessedRef
  records <-
    Sessions.selectEventsForwardFromGlobalPositionSession cursor catchUpBatchSize
      |> Sessions.runConnection queryConnection
      |> Task.mapError toText
  records |> Task.forEach (dispatchCaughtUpRecord store lastProcessedRef)
  -- A full page means there may be more rows after it: 'dispatchCaughtUpRecord'
  -- has already advanced the cursor past this page, so loop to drain the rest.
  -- A short page means the gap is exhausted and catch-up is done.
  Task.when (Array.length records == fromIntegral catchUpBatchSize) do
    catchUpFromCursor queryConnection store lastProcessedRef


-- | Decode a caught-up record, dispatch it through the same sink the live
-- handler uses, and advance the cursor (monotonic max). A decode failure is
-- thrown so the supervised reconnect loop retries the gap rather than
-- silently skipping it.
dispatchCaughtUpRecord ::
  SubscriptionStore ->
  Var Int64 ->
  PostgresEventRecord ->
  Task Text Unit
dispatchCaughtUpRecord store lastProcessedRef record = do
  case Sessions.postgresRecordToEvent record of
    Err err ->
      Task.throw [fmt|Catch-up event decode failed: #{err}|]
    Ok event -> do
      store
        |> SubscriptionStore.dispatch event.streamId event
        |> Task.mapError toText
      advanceCursor lastProcessedRef record.globalPosition


-- | Advance the cursor to @max current position@, keeping it monotonic so a
-- duplicate or out-of-order boundary event never moves it backwards.
advanceCursor :: Var Int64 -> Int64 -> Task Text Unit
advanceCursor lastProcessedRef position = do
  current <- Var.get lastProcessedRef
  lastProcessedRef |> Var.set (max current position)
