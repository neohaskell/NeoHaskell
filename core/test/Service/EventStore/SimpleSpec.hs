module Service.EventStore.SimpleSpec where

import Core
import Maybe qualified
import "nhcore" Path qualified
import Service.EventStore.Core qualified as EventStoreCore
import Service.EventStore.Simple (SimpleEventStore (..))
import Service.EventStore.Simple qualified as Simple
import Service.SnapshotCache.InMemory qualified as SnapshotCacheInMemory
import Task qualified
import Test
import Test.Service.EntityFetcher qualified as EntityFetcher
import Test.Service.EntityFetcher.Core qualified as EntityFetcherCore
import Test.Service.EventStore qualified as EventStore


spec :: Spec Unit
spec = do
  describe "SimpleEventStore (non-persistent)" do
    let config =
          SimpleEventStore
            { basePath = Path.fromText ".neo/test-events" |> Maybe.getOrDie,
              persistent = False
            }
    let newStore = Simple.new config |> Task.map (EventStoreCore.castEventStore @EntityFetcherCore.CartEvent) |> Task.mapError toText
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
