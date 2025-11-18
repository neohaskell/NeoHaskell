module Test.Service.EventStore.StreamTruncation.Context (
  Context (..),
  initialize,
) where

import Core
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Test.Service.EventStore.Core (BankAccountEvent)


data Context = Context
  { store :: EventStore BankAccountEvent,
    streamId :: Event.StreamId
  }


initialize :: Task Text (EventStore BankAccountEvent) -> Task Text Context
initialize newStore = do
  store <- newStore
  streamId <- StreamId.new
  pure Context {store, streamId}
