module Test.Service.EventStore where

import Core
import Service.EventStore.Core (EventStore)
import Test
-- import Test.Service.EventStore.GlobalStreamOrdering.Spec qualified as GlobalStreamOrdering
-- import Test.Service.EventStore.IndividualStreamOrdering.Spec qualified as IndividualStreamOrdering
-- import Test.Service.EventStore.OptimisticConcurrency.Spec qualified as OptimisticConcurrency
import Test.Service.EventStore.ReadAllForwardsFromStart.Spec qualified as ReadAllForwardsFromStart


spec :: Task Text EventStore -> Spec Unit
spec newStore = do
  describe "Event Store Specification Tests" do
    ReadAllForwardsFromStart.spec newStore

-- IndividualStreamOrdering.spec newStore
-- OptimisticConcurrency.spec newStore
-- GlobalStreamOrdering.spec newStore
