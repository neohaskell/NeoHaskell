module Services.EventStore (EventStore) where

import Core


type EventStore event =
  '[ "register" := (event -> IO Unit)
   ]
