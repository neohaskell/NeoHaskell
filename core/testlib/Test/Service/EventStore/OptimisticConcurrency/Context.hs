module Test.Service.EventStore.OptimisticConcurrency.Context (
  Context (..),
  initialize,
) where

import Core
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Task qualified
import Test.Service.EventStore.Core (MyEvent)
import Uuid qualified


data Context = Context
  { store :: EventStore MyEvent,
    streamId :: Event.StreamId
  }


initialize :: Task Text (EventStore MyEvent) -> Task Text Context
initialize newStore = do
  store <- newStore
  streamId <- Uuid.generate |> Task.map Event.StreamId
  pure Context {store, streamId}
