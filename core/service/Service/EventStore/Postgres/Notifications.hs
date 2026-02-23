module Service.EventStore.Postgres.Notifications (
  connectTo,
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
  Hasql.Connection ->
  Sessions.Connection ->
  SubscriptionStore ->
  Task Text Unit
connectTo connection pool store = do
  let channelToListen = HasqlNotifications.toPgIdentifier "global"
  HasqlNotifications.listen connection channelToListen
    |> Task.fromIO
    |> discard
  Log.info "LISTEN/NOTIFY listener started"
    |> Task.ignoreError
  connection
    |> HasqlNotifications.waitForNotifications (handler pool store)
    |> Task.fromIO
    |> AsyncTask.run
    |> Task.andThen \_ -> do
      Log.critical "LISTEN/NOTIFY listener exited unexpectedly"
        |> Task.ignoreError
      Task.yield ()
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
  Sessions.Connection ->
  SubscriptionStore ->
  Data.ByteString.ByteString ->
  Data.ByteString.ByteString ->
  IO ()
handler pool store _channelName payloadLegacyBytes = do
  result <- processNotification pool store payloadLegacyBytes |> Task.runResult
  case result of
    Err err ->
      ((Log.warn [fmt|[Notifications] #{err}|] |> Task.ignoreError) :: Task Text Unit) |> Task.runOrPanic
    Ok _ ->
      ((Log.debug "[Notifications] Event dispatched from notification" |> Task.ignoreError) :: Task Text Unit) |> Task.runOrPanic


processNotification ::
  Sessions.Connection ->
  SubscriptionStore ->
  Data.ByteString.ByteString ->
  Task Text Unit
processNotification pool store payloadLegacyBytes = do
  notification <- decodeNotification payloadLegacyBytes
  event <- fetchFullEvent pool notification.globalPosition
  store |> SubscriptionStore.dispatch event.streamId event |> Task.mapError toText


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
  Sessions.Connection ->
  Int64 ->
  Task Text (Event Json.Value)
fetchFullEvent pool globalPosition = do
  maybeRecord <-
    Sessions.selectEventByGlobalPositionSession globalPosition
      |> Sessions.run pool
      |> Task.mapError toText
  record <- case maybeRecord of
    Nothing -> Task.throw [fmt|Event not found for globalPosition #{globalPosition}|]
    Just r -> Task.yield r
  case Sessions.postgresRecordToEvent record of
    Err err -> Task.throw [fmt|Event record decode failed: #{err}|]
    Ok event -> Task.yield event
