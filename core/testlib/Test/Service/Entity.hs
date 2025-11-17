module Test.Service.Entity where

import Core
import Service.Entity.Core (EntityReducer)
import Service.EventStore.Core (EventStore)
import Test
import Test.Service.Entity.Core (BankAccountEvent, BankAccountState)
import Test.Service.Entity.Fetch.Spec qualified as Fetch


spec ::
  Task Text (EventStore BankAccountEvent) -> Task Text (EntityReducer BankAccountState BankAccountEvent) -> Spec Unit
spec newStore newReducer = do
  describe "Entity Reducer Specification Tests" do
    Fetch.spec newStore newReducer
