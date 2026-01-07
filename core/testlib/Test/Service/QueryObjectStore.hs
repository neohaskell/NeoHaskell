module Test.Service.QueryObjectStore where

import Core
import Service.QueryObjectStore.Core (QueryObjectStore)
import Test
import Test.Service.QueryObjectStore.Core (TestQuery)
import Test.Service.QueryObjectStore.InMemory.Spec qualified as InMemory


spec :: Task Text (QueryObjectStore TestQuery) -> Spec Unit
spec newStore = do
  describe "QueryObjectStore Specification Tests" do
    InMemory.spec newStore
