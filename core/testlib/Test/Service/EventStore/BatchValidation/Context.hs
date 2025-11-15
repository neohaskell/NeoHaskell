module Test.Service.EventStore.BatchValidation.Context (
  Context (..),
  initialize,
) where

import Core
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Task qualified
import Test.Service.EventStore.Core (MyEvent)


data Context = Context
  { store :: EventStore MyEvent,
    streamId :: Event.StreamId
  }


initialize :: Task Text (EventStore MyEvent) -> Task Text Context
initialize newStore = do
  store <- newStore
  streamId <- StreamId.new
  Task.yield Context {store, streamId}
