module Services.EventStore.InMemory (create, destroy) where

import Channel qualified
import Core
import Services.EventStore (EventStore)


create :: IO (EventStore event)
create = do
  eventChannel <- Channel.new
  print "Creating in-memory event store"
  pure
    ANON
      { dispatch = \(event) -> do
          print "Registering event"
          eventChannel |> Channel.write event
          pure unit
      }


destroy :: EventStore event -> IO Unit
destroy _ = do
  print "Destroying in-memory event store"
  pure unit