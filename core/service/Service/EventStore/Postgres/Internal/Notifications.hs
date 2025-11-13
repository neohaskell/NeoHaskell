module Service.EventStore.Postgres.Internal.Notifications where

import Bytes qualified
import Core
import Data.ByteString qualified
import Data.Text.IO qualified as GHC
import Hasql.Notifications qualified as HasqlNotifications
import Json qualified
import Service.Event (Event)
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Postgres.Internal.Sessions qualified as Sessions
import Service.EventStore.Postgres.Internal.SubscriptionStore (SubscriptionStore)
import Service.EventStore.Postgres.Internal.SubscriptionStore qualified as SubscriptionStore
import Task qualified
import Text qualified


connectTo ::
  forall eventType.
  (Json.FromJSON eventType) =>
  Sessions.Connection ->
  SubscriptionStore eventType ->
  Task Text Unit
connectTo conn store =
  case conn of
    Sessions.MockConnection ->
      pass
    Sessions.Connection connection -> do
      HasqlNotifications.waitForNotifications (handler store) connection |> Task.fromIO
      Task.yield (panic "lol")


handler ::
  forall eventType.
  (Json.Decodable eventType) =>
  SubscriptionStore eventType ->
  Data.ByteString.ByteString ->
  Data.ByteString.ByteString ->
  IO ()
handler store streamIdLegacyBytes payloadLegacyBytes = do
  let streamId =
        streamIdLegacyBytes
          |> Bytes.fromLegacy
          |> Text.fromBytes
          |> StreamId.fromText
  let decodingResult =
        payloadLegacyBytes
          |> Bytes.fromLegacy
          |> Json.decodeBytes @(Event eventType)
  case decodingResult of
    Err err -> do
      -- FIXME: Implement proper logging here
      GHC.putStrLn (err)
    Ok payload -> do
      result <-
        store
          |> SubscriptionStore.dispatch streamId payload
          |> Task.runResult
      case result of
        Err err -> do
          -- FIXME: Implement proper logging here
          GHC.putStrLn (toText err)
        Ok _ -> pass
