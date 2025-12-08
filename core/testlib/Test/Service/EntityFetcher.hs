module Test.Service.EntityFetcher where

import Core
import Service.EntityFetcher.Core (EntityFetcher)
import Service.EventStore.Core (EventStore)
import Test
import Test.Service.EntityFetcher.Core (CartEvent, CartState)
import Test.Service.EntityFetcher.Fetch.Spec qualified as Fetch


spec ::
  Task Text (EventStore CartEvent, EntityFetcher CartState CartEvent) -> Spec Unit
spec newStoreAndFetcher = do
  describe "Entity Fetcher Specification Tests" do
    Fetch.spec newStoreAndFetcher
