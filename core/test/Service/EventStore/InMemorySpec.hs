module Service.EventStore.InMemorySpec where

import Core
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test
import Test.Service.EventStore
import ToText (toText)


spec :: Spec
spec = do
  describe "InMemoryEventStore" do
    describe "ability to maintain total ordering per stream" do
      it "handles 10 events" do
        store <- InMemory.new
        testStreamOrderingSequential store 10

      it "handles 100 events" do
        store <- InMemory.new
        testStreamOrderingSequential store 100

      it "handles 1000 events" do
        store <- InMemory.new
        testStreamOrderingSequential store 1000

      it "handles 10000 events" do
        store <- InMemory.new
        testStreamOrderingSequential store 10000

      it "handles 100000 events" do
        pending "This test is only run manually"
        store <- InMemory.new
        testStreamOrderingSequential store 100000

      it "handles 1000000 events" do
        pending "This test is only run manually"
        store <- InMemory.new
        testStreamOrderingSequential store 1000000

    describe "ability to enforce optimistic concurrency control" do
      it "should handle well two concurrent operations" do
        store <- InMemory.new |> Task.mapError toText
        testOptimisticConcurrency store

    describe "ability to retain an 'all event' stream" do
      it "should retain global ordering" do
        store <- InMemory.new |> Task.mapError toText
        testGlobalStreamOrdering store 10
