module Service.EventStore.Postgres (
  new,
) where

import Core
import Service.EventStore.Core
import Task qualified


new :: Task Error EventStore
new = do
  let eventStore =
        EventStore
          { appendToStream = panic "appendToStream: Not implemented yet",
            readStreamForwardFrom = panic "readStreamForwardFrom: Not implemented yet",
            readStreamBackwardFrom = panic "readStreamBackwardFrom: Not implemented yet",
            readAllStreamEvents = panic "readAllStreamEvents: Not implemented yet",
            readAllEventsForwardFrom = panic "readAllEventsForwardFrom: Not implemented yet",
            readAllEventsBackwardFrom = panic "readAllEventsBackwardFrom: Not implemented yet",
            readAllEventsForwardFromFiltered = panic "readAllEventsForwardFromFiltered: Not implemented yet",
            readAllEventsBackwardFromFiltered = panic "readAllEventsBackwardFromFiltered: Not implemented yet",
            subscribeToAllEvents = panic "subscribeToAllEvents: Not implemented yet",
            subscribeToAllEventsFromPosition = panic "subscribeToAllEventsFromPosition: Not implemented yet",
            subscribeToAllEventsFromStart = panic "subscribeToAllEventsFromStart: Not implemented yet",
            subscribeToEntityEvents = panic "subscribeToEntityEvents: Not implemented yet",
            subscribeToStreamEvents = panic "subscribeToStreamEvents: Not implemented yet",
            unsubscribe = panic "unsubscribe: Not implemented yet"
          }
  Task.yield eventStore
