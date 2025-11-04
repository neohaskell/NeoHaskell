module Service.EventStore.Postgres (
  Config (..),
  new,
) where

import Core
import Service.Event
import Service.EventStore.Core
import Task qualified


data Config = Config
  { host :: Text,
    databaseName :: Text,
    user :: Text,
    password :: Text
  }
  deriving (Eq, Ord, Show)


new :: Config -> Task Error EventStore
new _ = do
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
appendToStreamImpl = panic "Postgres.appendToStreamImpl - Not implemented yet"


readStreamForwardFromImpl :: EntityId -> StreamId -> StreamPosition -> Limit -> Task Error (Array Event)
readStreamForwardFromImpl = panic "Postgres.readStreamForwardFromImpl - Not implemented yet"


readStreamBackwardFromImpl :: EntityId -> StreamId -> StreamPosition -> Limit -> Task Error (Array Event)
readStreamBackwardFromImpl = panic "Postgres.readStreamBackwardFromImpl - Not implemented yet"


readAllStreamEventsImpl :: EntityId -> StreamId -> Task Error (Array Event)
readAllStreamEventsImpl = panic "Postgres.readAllStreamEventsImpl - Not implemented yet"


readAllEventsForwardFromImpl :: StreamPosition -> Limit -> Task Error (Array Event)
readAllEventsForwardFromImpl = panic "Postgres.readAllEventsForwardFromImpl - Not implemented yet"


readAllEventsBackwardFromImpl :: StreamPosition -> Limit -> Task Error (Array Event)
readAllEventsBackwardFromImpl = panic "Postgres.readAllEventsBackwardFromImpl - Not implemented yet"


readAllEventsForwardFromFilteredImpl :: StreamPosition -> Limit -> Array EntityId -> Task Error (Array Event)
readAllEventsForwardFromFilteredImpl = panic "Postgres.readAllEventsForwardFromFilteredImpl - Not implemented yet"


readAllEventsBackwardFromFilteredImpl :: StreamPosition -> Limit -> Array EntityId -> Task Error (Array Event)
readAllEventsBackwardFromFilteredImpl = panic "Postgres.readAllEventsBackwardFromFilteredImpl - Not implemented yet"


subscribeToAllEventsImpl :: (Event -> Task Error Unit) -> Task Error SubscriptionId
subscribeToAllEventsImpl = panic "Postgres.subscribeToAllEventsImpl - Not implemented yet"


subscribeToAllEventsFromPositionImpl :: StreamPosition -> (Event -> Task Error Unit) -> Task Error SubscriptionId
subscribeToAllEventsFromPositionImpl = panic "Postgres.subscribeToAllEventsFromPositionImpl - Not implemented yet"


subscribeToAllEventsFromStartImpl :: (Event -> Task Error Unit) -> Task Error SubscriptionId
subscribeToAllEventsFromStartImpl = panic "Postgres.subscribeToAllEventsFromStartImpl - Not implemented yet"


subscribeToEntityEventsImpl :: EntityId -> (Event -> Task Error Unit) -> Task Error SubscriptionId
subscribeToEntityEventsImpl = panic "Postgres.subscribeToEntityEventsImpl - Not implemented yet"


subscribeToStreamEventsImpl :: EntityId -> StreamId -> (Event -> Task Error Unit) -> Task Error SubscriptionId
subscribeToStreamEventsImpl = panic "Postgres.subscribeToStreamEventsImpl - Not implemented yet"


unsubscribeImpl :: SubscriptionId -> Task Error Unit
unsubscribeImpl = panic "Postgres.unsubscribeImpl - Not implemented yet"


truncateStreamImpl :: EntityId -> StreamId -> StreamPosition -> Task Error Unit
truncateStreamImpl = panic "truncateStreamImpl - not implemented"