module Test.Service.EventStore.BatchValidation.Context (
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
  streamIdUuid <- Uuid.generate
  let streamId = Event.StreamId streamIdUuid
  Task.yield Context {store, streamId}
