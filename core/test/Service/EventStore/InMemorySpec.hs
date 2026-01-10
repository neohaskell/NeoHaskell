module Service.EventStore.InMemorySpec where

import Core
import Service.EventStore.Core qualified as EventStoreCore
import Service.EventStore.InMemory qualified as InMemory
import Service.SnapshotCache.InMemory qualified as SnapshotCacheInMemory
import Task qualified
import Test
import Test.Service.EntityFetcher qualified as EntityFetcher
import Test.Service.EntityFetcher.Core qualified as EntityFetcherCore
import Test.Service.EventStore qualified as EventStore


spec :: Spec Unit
spec = do
  describe "InMemoryEventStore" do
    let newStore = InMemory.new |> Task.map (EventStoreCore.castEventStore @EntityFetcherCore.CartEvent) |> Task.mapError toText
    EventStore.spec newStore

    let newStoreAndFetcher = do
          store <- newStore
          fetcher <- EntityFetcherCore.newFetcher store |> Task.mapError toText
          Task.yield (store, fetcher)

    EntityFetcher.spec newStoreAndFetcher

    let newStoreAndFetcherAndCache = do
          store <- newStore
          cache <- SnapshotCacheInMemory.new |> Task.mapError toText
          fetcher <- EntityFetcherCore.newFetcherWithCache store cache |> Task.mapError toText
          Task.yield (store, fetcher, cache)

    EntityFetcher.specWithCache newStoreAndFetcherAndCache
