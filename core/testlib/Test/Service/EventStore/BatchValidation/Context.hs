module Test.Service.EventStore.BatchValidation.Context (
  Context (..),
  initialize,
) where

import Core
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Task qualified
import Test.Service.EventStore.Core (CartEvent)


data Context = Context
  { store :: EventStore CartEvent,
    streamId :: Event.StreamId
  }


initialize :: Task Text (EventStore CartEvent) -> Task Text Context
initialize newStore = do
  store <- newStore
  streamId <- StreamId.new
  Task.yield Context {store, streamId}
