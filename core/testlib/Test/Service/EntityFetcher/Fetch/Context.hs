module Test.Service.EntityFetcher.Fetch.Context (
  Context (..),
  initialize,
) where

import Core
import Service.EntityFetcher.Core (EntityFetcher)
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Test.Service.EntityFetcher.Core (CartEvent, CartState)


data Context = Context
  { store :: EventStore CartEvent,
    fetcher :: EntityFetcher CartState CartEvent,
    streamId :: Event.StreamId,
    entityName :: Event.EntityName
  }


initialize ::
  Task Text (EventStore CartEvent, EntityFetcher CartState CartEvent) ->
  Task Text Context
initialize newStoreAndFetcher = do
  (store, fetcher) <- newStoreAndFetcher
  streamId <- StreamId.new
  let entityName = Event.EntityName "Cart"
  pure Context {store, fetcher, streamId, entityName}
