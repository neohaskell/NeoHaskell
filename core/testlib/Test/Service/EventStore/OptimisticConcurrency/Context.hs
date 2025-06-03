module Test.Service.EventStore.OptimisticConcurrency.Context (
  Context (..),
  initialize,
) where

import Core
import Service.Event qualified as Event
import Service.EventStore (EventStore)


data Context = Context
  { store :: EventStore,
    streamId :: Event.StreamId
  }


initialize :: Task Text EventStore -> Task Text Context
initialize newStore = do
  store <- newStore
  let streamId = Event.StreamId "test-stream"
  pure Context {store, streamId}
