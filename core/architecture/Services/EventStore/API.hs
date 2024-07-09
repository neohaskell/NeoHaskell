module Services.EventStore.API (EventStore) where

import Core


type EventStore event =
  Record
    '[ "dispatch" := (event -> IO Unit)
     ]