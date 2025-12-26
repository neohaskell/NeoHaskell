module Test.Service.EntityFetcher.FetchWithCache.Context (
  Context (..),
  initialize,
) where

import Core
import Service.EntityFetcher.Core (EntityFetcher)
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Service.SnapshotCache.Core (SnapshotCache)
import Test.Service.EntityFetcher.Core (CartEvent, CartState)


data Context = Context
  { store :: EventStore CartEvent,
    fetcher :: EntityFetcher CartState CartEvent,
    cache :: SnapshotCache CartState,
    streamId :: Event.StreamId,
    entityName :: Event.EntityName
  }


initialize ::
  Task Text (EventStore CartEvent, EntityFetcher CartState CartEvent, SnapshotCache CartState) ->
  Task Text Context
initialize newStoreAndFetcherAndCache = do
  (store, fetcher, cache) <- newStoreAndFetcherAndCache
  streamId <- StreamId.new
  let entityName = Event.EntityName "Cart"
  pure Context {store, fetcher, cache, streamId, entityName}
