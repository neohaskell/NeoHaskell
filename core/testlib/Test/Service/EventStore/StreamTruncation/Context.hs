module Test.Service.EventStore.StreamTruncation.Context (
  Context (..),
  initialize,
) where

import Core
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Task qualified
import Uuid qualified


data Context = Context
  { store :: EventStore,
    streamId :: Event.StreamId
  }


initialize :: Task Text EventStore -> Task Text Context
initialize newStore = do
  store <- newStore
  streamId <- Uuid.generate |> Task.map Event.StreamId
  pure Context {store, streamId}
