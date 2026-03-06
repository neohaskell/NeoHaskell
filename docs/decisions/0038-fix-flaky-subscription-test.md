# ADR-0038: Fix Flaky Subscription Test Ordering Assertion

## Status

Accepted

## Context

The test `handles high-frequency event publishing without data loss` in `core/testlib/Test/Service/EventStore/Subscriptions/Spec.hs` (lines 369-416) is flaky under concurrent load. It inserts 50 events rapidly and then asserts that the subscriber received them in strict index order:

```haskell
received
  |> Array.indexed
  |> Task.forEach
    ( \(index, event) -> do
        event.metadata.localPosition
          |> Maybe.getOrDie
          |> shouldBe (Event.StreamPosition (index |> fromIntegral))
    )
```

This assertion fails intermittently because it conflates two separate guarantees: **completeness** (all 50 events arrived) and **delivery order** (they arrived in insertion order). The EventStore only guarantees the former.

### Root Cause

`notifySubscribers` in `core/service/Service/EventStore/Simple.hs` (lines 594-609) dispatches subscriber callbacks in parallel:

```haskell
notifySubscribers :: StreamStore -> Event Json.Value -> Task Never Unit
notifySubscribers store event = do
  allSubscriptions <- ConcurrentVar.peek store.subscriptions
  let relevantSubscriptions =
        allSubscriptions
          |> Map.entries
          |> Array.takeIf (\(_, subscription) -> shouldNotify subscription.subscriptionType event)
  let notificationTasks :: Array (Task Text Unit) =
        relevantSubscriptions
          |> Array.map (\(_, subscription) -> notifySubscriber subscription event)
  notificationTasks |> AsyncTask.runAllIgnoringErrors
```

`AsyncTask.runAllIgnoringErrors` runs all notification tasks concurrently. The subscriber callback does:

```haskell
let collectEvent event = receivedEvents |> ConcurrentVar.modify (Array.push event)
```

`ConcurrentVar.modify` is atomic per call, but when multiple concurrent threads each call it, the push order is non-deterministic. Thread scheduling determines which event lands at index 0, 1, 2, and so on. Under light load the test passes because the threads happen to finish in insertion order. Under heavier load or on slower CI machines, they don't.

## Decision

Replace the strict index-based ordering assertion with a position-set verification. Instead of checking that `received[i].localPosition == i`, collect all `localPosition` values, sort them, and assert they equal the expected sequence `[StreamPosition 0 .. StreamPosition 49]`.

```haskell
-- Verify events are complete (all positions present, no gaps or duplicates)
receivedPositions <-
  received
    |> Array.map (\event -> event.metadata.localPosition |> Maybe.getOrDie)
    |> Array.toLinkedList
    |> GhcList.sort
    |> Array.fromLinkedList
    |> Task.yield
let expectedPositions =
      Array.range 0 (rapidEventCount - 1)
        |> Array.map (\i -> Event.StreamPosition (i |> fromIntegral))
receivedPositions |> shouldBe expectedPositions
```

This validates that every expected `StreamPosition` was received exactly once, without requiring any particular delivery order.

## Consequences

### Positive

1. **Test is no longer flaky.** The assertion matches what the EventStore actually guarantees: every inserted event is delivered to every subscriber, with no duplicates and no gaps.

2. **Zero data loss is still verified.** Sorting and comparing the full position set catches missing events, duplicate deliveries, and wrong-stream events just as well as the index-based check did.

3. **Contract is now explicit.** The EventStore guarantees **durability** and **completeness**. It does not guarantee subscriber delivery order. This ADR records that distinction so future contributors don't re-introduce an ordering assumption.

### Negative

None. Delivery order to subscribers is not part of the EventStore contract, so removing the ordering assertion does not weaken any meaningful guarantee.

### Clarification on EventStore Guarantees

| Guarantee | Provided? | Notes |
|-----------|-----------|-------|
| Durability (events persisted) | Yes | Events are written before `insert` returns |
| Completeness (all events delivered) | Yes | Every subscriber receives every matching event |
| Delivery order to subscribers | No | `AsyncTask.runAllIgnoringErrors` is parallel; order is non-deterministic |
| Stream ordering (localPosition) | Yes | `localPosition` values are assigned monotonically at insert time |

## References

- [GitHub Issue #238](https://github.com/neohaskell/NeoHaskell/issues/238) — Original flaky test report
- [Subscriptions/Spec.hs](../../core/testlib/Test/Service/EventStore/Subscriptions/Spec.hs) — Flaky test at lines 369-416
- [Simple.hs](../../core/service/Service/EventStore/Simple.hs) — `notifySubscribers` at lines 594-609
