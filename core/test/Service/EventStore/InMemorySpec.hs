module Service.EventStore.InMemorySpec where

import Core
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test
import Test.Service.EventStore
import Test.Service.EventStore.GlobalStreamOrdering.Spec qualified as GlobalStreamOrdering
import Test.Service.EventStore.IndividualStreamOrdering.Spec qualified as IndividualStreamOrdering
import ToText (toText)


spec :: Spec Unit
spec = do
  describe "InMemoryEventStore" do
    IndividualStreamOrdering.spec (InMemory.new |> Task.mapError toText)

    describe "ability to enforce optimistic concurrency control" do
      it "should handle well two concurrent operations" \_ -> do
        store <- InMemory.new |> Task.mapError toText
        testOptimisticConcurrency store

    GlobalStreamOrdering.spec (InMemory.new |> Task.mapError toText)
