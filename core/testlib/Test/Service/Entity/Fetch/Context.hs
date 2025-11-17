module Test.Service.Entity.Fetch.Context (
  Context (..),
  initialize,
) where

import Core
import Service.Entity.Core (EntityReducer)
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Test.Service.Entity.Core (BankAccountEvent, BankAccountState)


data Context = Context
  { store :: EventStore BankAccountEvent,
    reducer :: EntityReducer BankAccountState BankAccountEvent,
    streamId :: Event.StreamId,
    entityName :: Event.EntityName
  }


initialize ::
  Task Text (EventStore BankAccountEvent) ->
  Task Text (EntityReducer BankAccountState BankAccountEvent) ->
  Task Text Context
initialize newStore newReducer = do
  store <- newStore
  reducer <- newReducer
  streamId <- StreamId.new
  let entityName = Event.EntityName "BankAccount"
  pure Context {store, reducer, streamId, entityName}
