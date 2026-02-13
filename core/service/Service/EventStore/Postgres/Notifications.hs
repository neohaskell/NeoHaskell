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
import Result qualified
import Service.Event (Event (..))
import Service.EventStore.Postgres.Sessions qualified as Sessions
import Service.EventStore.Postgres.SubscriptionStore (SubscriptionStore)
import Service.EventStore.Postgres.SubscriptionStore qualified as SubscriptionStore
import Task qualified


connectTo ::
  Hasql.Connection ->
  SubscriptionStore ->
  Task Text Unit
connectTo connection store = do
  let channelToListen = HasqlNotifications.toPgIdentifier "global"
  HasqlNotifications.listen connection channelToListen
    |> Task.fromIO
    |> discard
  Log.info "LISTEN/NOTIFY listener started"
    |> Task.ignoreError
  connection
    |> HasqlNotifications.waitForNotifications (handler store)
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
  SubscriptionStore ->
  Data.ByteString.ByteString ->
  Data.ByteString.ByteString ->
  IO ()
handler store _channelName payloadLegacyBytes = do
  let decodingResult =
        payloadLegacyBytes
          |> Bytes.fromLegacy
          |> Json.decodeBytes
          |> Result.andThen Sessions.insertionRecordToEvent
  case decodingResult of
    Err decodeErr -> do
      -- Event decoding failed - log and continue
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
          -- Dispatch error - log and continue
          let msg = [fmt|[Notifications] Dispatch failed for stream #{eventStreamId}: #{dispatchErr}|]
          ((Log.warn msg |> Task.ignoreError) :: Task Text Unit) |> Task.runOrPanic
        Ok _ ->
          pass
