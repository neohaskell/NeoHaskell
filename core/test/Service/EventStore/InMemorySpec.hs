module Service.EventStore.InMemorySpec where

import Core
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test
import Test.Service.EntityFetcher qualified as EntityFetcher
import Test.Service.EntityFetcher.Core qualified as EntityFetcherCore
import Test.Service.EventStore qualified as EventStore


spec :: Spec Unit
spec = do
  describe "InMemoryEventStore" do
    let newStore = InMemory.new @EntityFetcherCore.CartEvent |> Task.mapError toText
    EventStore.spec newStore

    let newStoreAndFetcher = do
          store <- newStore
          fetcher <- EntityFetcherCore.newFetcher store |> Task.mapError toText
          Task.yield (store, fetcher)

    EntityFetcher.spec newStoreAndFetcher
