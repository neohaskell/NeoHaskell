module Test.Service.EventStore where

import Core
import Service.EventStore.Core (EventStore)
import Test
import Test.Service.EventStore.BatchValidation.Spec qualified as BatchValidation
import Test.Service.EventStore.Core (CartEvent)
import Test.Service.EventStore.GlobalStreamOrdering.Spec qualified as GlobalStreamOrdering
import Test.Service.EventStore.IndividualStreamOrdering.Spec qualified as IndividualStreamOrdering
import Test.Service.EventStore.LocalPositionStamping.Spec qualified as LocalPositionStamping
import Test.Service.EventStore.OptimisticConcurrency.Spec qualified as OptimisticConcurrency
import Test.Service.EventStore.ReadAllBackwardsFromEnd.Spec qualified as ReadAllBackwardsFromEnd
import Test.Service.EventStore.ReadAllForwardsFromStart.Spec qualified as ReadAllForwardsFromStart
import Test.Service.EventStore.StreamTruncation.Spec qualified as StreamTruncation
import Test.Service.EventStore.Subscriptions.SimpleSpec qualified as SimpleSubscriptions
import Test.Service.EventStore.Subscriptions.Spec qualified as Subscriptions


spec :: Task Text (EventStore CartEvent) -> Spec Unit
spec newStore = do
  describe "Event Store Specification Tests" do
    ReadAllForwardsFromStart.spec newStore
    ReadAllBackwardsFromEnd.spec newStore
    IndividualStreamOrdering.spec newStore
    GlobalStreamOrdering.spec newStore
    OptimisticConcurrency.spec newStore
    StreamTruncation.spec newStore
    SimpleSubscriptions.spec newStore
    Subscriptions.spec newStore
    BatchValidation.spec newStore
    LocalPositionStamping.spec newStore
