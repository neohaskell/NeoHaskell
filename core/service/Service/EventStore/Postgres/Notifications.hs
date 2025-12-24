module Service.EventStore.Postgres.Notifications (
  connectTo,
  subscribeToStream,
) where

import AsyncTask qualified
import Bytes qualified
import Core
import Data.ByteString qualified
import Data.Text.IO qualified as GHC
import Hasql.Connection qualified as Hasql
import Hasql.Notifications qualified as HasqlNotifications
import Json qualified
import Result qualified
import Service.Event (Event (..))
import Service.EventStore.Postgres.Sessions qualified as Sessions
import Service.EventStore.Postgres.SubscriptionStore (SubscriptionStore)
import Service.EventStore.Postgres.SubscriptionStore qualified as SubscriptionStore
import Task qualified


connectTo ::
  forall eventType.
  (Json.FromJSON eventType) =>
  Hasql.Connection ->
  SubscriptionStore eventType ->
  Task Text Unit
connectTo connection store = do
  let channelToListen = HasqlNotifications.toPgIdentifier "global"
  HasqlNotifications.listen connection channelToListen
    |> Task.fromIO
    |> discard
  connection
    |> HasqlNotifications.waitForNotifications (handler store)
    |> Task.fromIO
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
  forall eventType.
  (Json.FromJSON eventType) =>
  SubscriptionStore eventType ->
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
    Err err -> do
      -- FIXME: Implement proper logging here
      GHC.putStrLn (err)
    Ok event -> do
      result <-
        store
          |> SubscriptionStore.dispatch event.streamId event
          |> Task.runResult
      case result of
        Err err -> do
          -- FIXME: Implement proper logging here
          GHC.putStrLn (toText err)
        Ok _ -> pass
