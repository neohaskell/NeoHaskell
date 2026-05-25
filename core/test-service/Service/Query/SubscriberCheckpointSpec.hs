{-# OPTIONS_GHC -Wno-unused-imports #-}
module Service.Query.SubscriberCheckpointSpec where

import Core
import Test.Spec (Spec, describe, it, pending)

spec :: Spec Unit
spec = do
  describe "Service.Query.Subscriber" do
    describe "rebuildFrom" do
      it "Start from position 0 with no checkpoints — all events processed, all queries updated" \_ -> do
        pending "not implemented"

      it "Start from position 100 with checkpoints for all queries — only events at position >= 101 processed" \_ -> do
        pending "not implemented"

      it "EventStore returns empty stream immediately — returns Unit with 0 events replayed" \_ -> do
        pending "not implemented"

      it "Position equals current head of event store — returns Unit, no events processed" \_ -> do
        pending "not implemented"

      it "Exactly one chunk (1000 events) — all 1000 events processed, progress logged" \_ -> do
        pending "not implemented"

      it "EventStore returns an error on readAllEventsForwardFrom — Task returns the error" \_ -> do
        pending "not implemented"

      it "A QueryUpdater throws during processing — error logged for that query, other queries continue" \_ -> do
        pending "not implemented"

      it "rebuildFrom at P then rebuildFrom at P+1 processes at most 1 new event — idempotency" \_ -> do
        pending "not implemented"

  describe "Service.Query.Subscriber readiness" do
    describe "subscriber.ready" do
      it "Rebuild completes successfully — ready is True" \_ -> do
        pending "not implemented"

      it "No queries registered — ready is True immediately" \_ -> do
        pending "not implemented"

      it "Rebuild in progress — ready is False" \_ -> do
        pending "not implemented"

      it "Rebuild completes with 0 events — ready is True" \_ -> do
        pending "not implemented"

      it "Rebuild task crashes — ready stays False, error logged" \_ -> do
        pending "not implemented"

      it "ready transitions from False to True exactly once — monotonic" \_ -> do
        pending "not implemented"
