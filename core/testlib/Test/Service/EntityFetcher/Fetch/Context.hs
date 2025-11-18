module Test.Service.EntityFetcher.Fetch.Context (
  Context (..),
  initialize,
) where

import Core
import Service.EntityFetcher.Core (EntityFetcher)
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Test.Service.EntityFetcher.Core (BankAccountEvent, BankAccountState)


data Context = Context
  { store :: EventStore BankAccountEvent,
    fetcher :: EntityFetcher BankAccountState BankAccountEvent,
    streamId :: Event.StreamId,
    entityName :: Event.EntityName
  }


initialize ::
  Task Text (EventStore BankAccountEvent, EntityFetcher BankAccountState BankAccountEvent) ->
  Task Text Context
initialize newStoreAndFetcher = do
  (store, fetcher) <- newStoreAndFetcher
  streamId <- StreamId.new
  let entityName = Event.EntityName "BankAccount"
  pure Context {store, fetcher, streamId, entityName}
