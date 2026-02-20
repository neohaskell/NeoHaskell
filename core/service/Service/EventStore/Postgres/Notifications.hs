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
  let notificationResult =
        payloadLegacyBytes
          |> Bytes.fromLegacy
          |> Json.decodeBytes :: Result Text Sessions.EventNotificationPayload
  case notificationResult of
    Err decodeErr -> do
      let msg = [fmt|[Notifications] Notification decode failed: #{decodeErr}|]
      ((Log.warn msg |> Task.ignoreError) :: Task Text Unit) |> Task.runOrPanic
    Ok notification -> do
      let notificationGlobalPosition = notification.globalPosition
      -- Fetch full event from database using the connection pool
      fetchResult <-
        Sessions.selectEventByGlobalPositionSession notificationGlobalPosition
          |> Sessions.run pool
          |> Task.mapError toText
          |> Task.runResult
      case fetchResult of
        Err fetchErr -> do
          let msg = [fmt|[Notifications] DB fetch failed for globalPosition #{notificationGlobalPosition}: #{fetchErr}|]
          ((Log.warn msg |> Task.ignoreError) :: Task Text Unit) |> Task.runOrPanic
        Ok maybeRecord -> do
          case maybeRecord of
            Nothing -> do
              let msg = [fmt|[Notifications] Event not found for globalPosition #{notificationGlobalPosition}|]
              ((Log.warn msg |> Task.ignoreError) :: Task Text Unit) |> Task.runOrPanic
            Just record -> do
              case Sessions.postgresRecordToEvent record of
                Err decodeErr -> do
                  let msg = [fmt|[Notifications] Event decode failed: #{decodeErr}|]
                  ((Log.warn msg |> Task.ignoreError) :: Task Text Unit) |> Task.runOrPanic
                Ok event -> do
                  let eventStreamId = event.streamId
                  result <-
                    store
                      |> SubscriptionStore.dispatch eventStreamId event
                      |> Task.runResult
                  case result of
                    Err dispatchErr -> do
                      let msg = [fmt|[Notifications] Dispatch failed for stream #{eventStreamId}: #{dispatchErr}|]
                      ((Log.warn msg |> Task.ignoreError) :: Task Text Unit) |> Task.runOrPanic
                    Ok _ -> do
                      ((Log.debug [fmt|Event dispatched from notification for stream #{eventStreamId}|] |> Task.ignoreError) :: Task Text Unit) |> Task.runOrPanic
