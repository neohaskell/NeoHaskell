module Test.Service.SnapshotCache.InMemory.Context (
  Context (..),
  initialize,
) where

import Core
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.SnapshotCache.Core (SnapshotCache)
import Test.Service.SnapshotCache.Core (CartState)


data Context = Context
  { cache :: SnapshotCache CartState,
    streamId :: Event.StreamId,
    entityName :: Event.EntityName
  }


initialize ::
  Task Text (SnapshotCache CartState) ->
  Task Text Context
initialize newCache = do
  cache <- newCache
  streamId <- StreamId.new
  let entityName = Event.EntityName "Cart"
  pure Context {cache, streamId, entityName}
