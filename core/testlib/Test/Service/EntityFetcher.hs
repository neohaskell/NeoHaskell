module Test.Service.EntityFetcher where

import Core
import Service.EntityFetcher.Core (EntityFetcher)
import Service.EventStore.Core (EventStore)
import Test
import Test.Service.EntityFetcher.Core (BankAccountEvent, BankAccountState)
import Test.Service.EntityFetcher.Fetch.Spec qualified as Fetch


spec ::
  Task Text (EventStore BankAccountEvent, EntityFetcher BankAccountState BankAccountEvent) -> Spec Unit
spec newStoreAndFetcher = do
  describe "Entity Fetcher Specification Tests" do
    Fetch.spec newStoreAndFetcher
