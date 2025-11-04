module Service.EventStore.Postgres.Internal (
  Config (..),
  Connection (..),
  Ops (..),
  new,
  defaultOps,
) where

import Core
import Hasql.Connection qualified as Hasql
import Hasql.Connection.Setting qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection qualified as ConnectionSettingConnection
import Hasql.Connection.Setting.Connection.Param qualified as Param
import Service.Event
import Service.EventStore.Core
import Task qualified


data Config = Config
  { host :: Text,
    databaseName :: Text,
    user :: Text,
    password :: Text,
    port :: Int
  }
  deriving (Eq, Ord, Show)


toConnectionSettings :: Config -> LinkedList ConnectionSetting.Setting
toConnectionSettings cfg = do
  let params =
        ConnectionSettingConnection.params
          [ Param.host cfg.host,
            Param.port (fromIntegral cfg.port),
            Param.dbname cfg.databaseName,
            Param.user cfg.user,
            Param.password cfg.password
          ]
  [params |> ConnectionSetting.connection]


data Connection
  = Connection Hasql.Connection
  | MockConnection


data Ops = Ops
  { acquire :: Task Text Connection
  }


defaultOps :: Config -> Ops
defaultOps cfg = do
  let acquireImpl = do
        connection <-
          cfg
            |> toConnectionSettings
            |> Hasql.acquire
            |> Task.fromIOEither
            |> Task.mapError toText
        Task.yield (Connection connection)
  Ops
    { acquire = acquireImpl
    }


new :: Ops -> Config -> Task Text EventStore
new ops _ = do
  _ <- ops.acquire
  let eventStore =
        EventStore
          { appendToStream = appendToStreamImpl,
            readStreamForwardFrom = readStreamForwardFromImpl,
            readStreamBackwardFrom = readStreamBackwardFromImpl,
            readAllStreamEvents = readAllStreamEventsImpl,
            readAllEventsForwardFrom = readAllEventsForwardFromImpl,
            readAllEventsBackwardFrom = readAllEventsBackwardFromImpl,
            readAllEventsForwardFromFiltered = readAllEventsForwardFromFilteredImpl,
            readAllEventsBackwardFromFiltered = readAllEventsBackwardFromFilteredImpl,
            subscribeToAllEvents = subscribeToAllEventsImpl,
            subscribeToAllEventsFromPosition = subscribeToAllEventsFromPositionImpl,
            subscribeToAllEventsFromStart = subscribeToAllEventsFromStartImpl,
            subscribeToEntityEvents = subscribeToEntityEventsImpl,
            subscribeToStreamEvents = subscribeToStreamEventsImpl,
            unsubscribe = unsubscribeImpl,
            truncateStream = truncateStreamImpl
          }
  Task.yield eventStore


appendToStreamImpl :: InsertionEvent -> Task Error Event
appendToStreamImpl _ = panic "Postgres.appendToStreamImpl - Not implemented yet" |> Task.yield


readStreamForwardFromImpl :: EntityId -> StreamId -> StreamPosition -> Limit -> Task Error (Array Event)
readStreamForwardFromImpl _ _ _ _ = panic "Postgres.readStreamForwardFromImpl - Not implemented yet" |> Task.yield


readStreamBackwardFromImpl :: EntityId -> StreamId -> StreamPosition -> Limit -> Task Error (Array Event)
readStreamBackwardFromImpl _ _ _ _ = panic "Postgres.readStreamBackwardFromImpl - Not implemented yet" |> Task.yield


readAllStreamEventsImpl :: EntityId -> StreamId -> Task Error (Array Event)
readAllStreamEventsImpl _ _ = panic "Postgres.readAllStreamEventsImpl - Not implemented yet" |> Task.yield


readAllEventsForwardFromImpl :: StreamPosition -> Limit -> Task Error (Array Event)
readAllEventsForwardFromImpl _ _ = panic "Postgres.readAllEventsForwardFromImpl - Not implemented yet" |> Task.yield


readAllEventsBackwardFromImpl :: StreamPosition -> Limit -> Task Error (Array Event)
readAllEventsBackwardFromImpl _ _ = panic "Postgres.readAllEventsBackwardFromImpl - Not implemented yet" |> Task.yield


readAllEventsForwardFromFilteredImpl :: StreamPosition -> Limit -> Array EntityId -> Task Error (Array Event)
readAllEventsForwardFromFilteredImpl _ _ _ = panic "Postgres.readAllEventsForwardFromFilteredImpl - Not implemented yet" |> Task.yield


readAllEventsBackwardFromFilteredImpl :: StreamPosition -> Limit -> Array EntityId -> Task Error (Array Event)
readAllEventsBackwardFromFilteredImpl _ _ _ = panic "Postgres.readAllEventsBackwardFromFilteredImpl - Not implemented yet" |> Task.yield


subscribeToAllEventsImpl :: (Event -> Task Error Unit) -> Task Error SubscriptionId
subscribeToAllEventsImpl _ = panic "Postgres.subscribeToAllEventsImpl - Not implemented yet" |> Task.yield


subscribeToAllEventsFromPositionImpl :: StreamPosition -> (Event -> Task Error Unit) -> Task Error SubscriptionId
subscribeToAllEventsFromPositionImpl _ _ = panic "Postgres.subscribeToAllEventsFromPositionImpl - Not implemented yet" |> Task.yield


subscribeToAllEventsFromStartImpl :: (Event -> Task Error Unit) -> Task Error SubscriptionId
subscribeToAllEventsFromStartImpl _ = panic "Postgres.subscribeToAllEventsFromStartImpl - Not implemented yet" |> Task.yield


subscribeToEntityEventsImpl :: EntityId -> (Event -> Task Error Unit) -> Task Error SubscriptionId
subscribeToEntityEventsImpl _ _ = panic "Postgres.subscribeToEntityEventsImpl - Not implemented yet" |> Task.yield


subscribeToStreamEventsImpl :: EntityId -> StreamId -> (Event -> Task Error Unit) -> Task Error SubscriptionId
subscribeToStreamEventsImpl _ _ _ = panic "Postgres.subscribeToStreamEventsImpl - Not implemented yet" |> Task.yield


unsubscribeImpl :: SubscriptionId -> Task Error Unit
unsubscribeImpl _ = panic "Postgres.unsubscribeImpl - Not implemented yet" |> Task.yield


truncateStreamImpl :: EntityId -> StreamId -> StreamPosition -> Task Error Unit
truncateStreamImpl _ _ _ = panic "Postgres.truncateStreamImpl - Not implemented yet" |> Task.yield