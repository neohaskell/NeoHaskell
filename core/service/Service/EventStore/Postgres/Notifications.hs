module Service.EventStore.Postgres.Notifications (
  connectTo,
  nextBackoff,
  subscribeToStream,
) where

import AsyncTask qualified
import Bytes qualified
import Core
import Data.ByteString qualified
import Hasql.Connection qualified as Hasql
import Hasql.Notifications qualified as HasqlNotifications
import Json qualified
import Log qualified
import Service.Event (Event (..))
import Service.EventStore.Postgres.Sessions qualified as Sessions
import Service.EventStore.Postgres.SubscriptionStore (SubscriptionStore)
import Service.EventStore.Postgres.SubscriptionStore qualified as SubscriptionStore
import Task qualified


connectTo ::
  Task Text (Hasql.Connection, Hasql.Connection) ->
  SubscriptionStore ->
  Task Text Unit
connectTo acquireConnections store = do
  let listenerWithReconnect backoffMs = do
        result <- Task.asResultSafe do
          (listenConnection, queryConnection) <- acquireConnections
          let channelToListen = HasqlNotifications.toPgIdentifier "global"
          HasqlNotifications.listen listenConnection channelToListen
            |> Task.fromIO
            |> discard
          Log.info "LISTEN/NOTIFY listener started"
            |> Task.ignoreError
          listenConnection
            |> HasqlNotifications.waitForNotifications (handler queryConnection store)
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
  listenerWithReconnect 1000
    |> AsyncTask.run
    |> discard


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
  Data.ByteString.ByteString ->
  Data.ByteString.ByteString ->
  IO ()
handler queryConnection store _channelName payloadLegacyBytes = do
  result <- processNotification queryConnection store payloadLegacyBytes |> Task.runResult
  case result of
    Err err ->
      ((Log.warn [fmt|#{err}|] |> Task.ignoreError) :: Task Text Unit) |> Task.runOrPanic
    Ok _ ->
      pass


processNotification ::
  Hasql.Connection ->
  SubscriptionStore ->
  Data.ByteString.ByteString ->
  Task Text Unit
processNotification queryConnection store payloadLegacyBytes = do
  Log.withScope [("component", "Notifications")] do
    notification <- decodeNotification payloadLegacyBytes
    event <- fetchFullEvent queryConnection notification.globalPosition
    Log.withScope [("component", "Notifications"), ("streamId", toText event.streamId)] do
      store |> SubscriptionStore.dispatch event.streamId event |> Task.mapError toText
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
