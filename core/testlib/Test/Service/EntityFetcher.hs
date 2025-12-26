module Test.Service.EntityFetcher where

import Core
import Service.EntityFetcher.Core (EntityFetcher)
import Service.EventStore.Core (EventStore)
import Service.SnapshotCache.Core (SnapshotCache)
import Test
import Test.Service.EntityFetcher.Core (CartEvent, CartState)
import Test.Service.EntityFetcher.Fetch.Spec qualified as Fetch
import Test.Service.EntityFetcher.FetchWithCache.Spec qualified as FetchWithCache


spec ::
  Task Text (EventStore CartEvent, EntityFetcher CartState CartEvent) -> Spec Unit
spec newStoreAndFetcher = do
  describe "Entity Fetcher Specification Tests" do
    Fetch.spec newStoreAndFetcher


specWithCache ::
  Task Text (EventStore CartEvent, EntityFetcher CartState CartEvent, SnapshotCache CartState) ->
  Spec Unit
specWithCache newStoreAndFetcherAndCache = do
  describe "Entity Fetcher with Cache Specification Tests" do
    FetchWithCache.spec newStoreAndFetcherAndCache
