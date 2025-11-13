module Service.EventStore.Postgres.Internal.Notifications where

import Core
import Data.ByteString qualified
import Hasql.Notifications qualified as HasqlNotifications
import Service.EventStore.Postgres.Internal.Sessions qualified as Sessions
import Service.EventStore.Postgres.Internal.SubscriptionStore (SubscriptionStore)
import Task qualified


connectTo :: Sessions.Connection -> SubscriptionStore eventType -> Task Text Unit
connectTo conn _store =
  case conn of
    Sessions.MockConnection ->
      pass
    Sessions.Connection connection -> do
      HasqlNotifications.waitForNotifications adaptHandler connection |> Task.fromIO
      Task.yield (panic "lol")


adaptHandler :: Data.ByteString.ByteString -> Data.ByteString.ByteString -> IO ()
adaptHandler _ _ = panic "adaptHandler"
