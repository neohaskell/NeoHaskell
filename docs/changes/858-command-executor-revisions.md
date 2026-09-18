# Change 858: Preserve command decision concurrency semantics

Fix `CommandExecutor` so command decisions retain the stream revision observed by `EntityFetcher` when they reach the event store. `acceptExisting` will append against that fetched revision, `acceptNew` will keep the stream-creation precondition, and retries will refetch both aggregate state and revision before re-deciding. `acceptAny` and explicit `acceptAfter` semantics remain unchanged.

```yaml spec
issue: issue#858
kind: bug
touches: [commands, event-store]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

The public signatures do not change. The fix is internal to `CommandExecutor`; the event-store insertion types already express the required contract.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | `acceptExisting` reaches the event store with the revision fetched for the command | `hspec:nhcore-test-service:core/testlib/Test/Service/CommandHandler/Execute/Spec.hs#binds acceptExisting to the fetched stream revision` | unit | none |
| C2 | A consistency conflict refetches fresh state and revision, re-decides, and appends with the fresh precondition rather than the stale payload | `hspec:nhcore-test-service:core/testlib/Test/Service/CommandHandler/Execute/Spec.hs#records the exact insertion precondition after refetch` | integration | postgres:real |
| C3 | Concurrent `StreamCreation` appends allow exactly one durable creation | `hspec:nhcore-test-service:core/testlib/Test/Service/EventStore/OptimisticConcurrency/Spec.hs#allows only one concurrent StreamCreation and persists one creation fact` | integration | postgres:real |
| C4 | `AnyStreamState` remains an unconditional append | `hspec:nhcore-test-service:core/testlib/Test/Service/EventStore/OptimisticConcurrency/Spec.hs#persists both AnyStreamState events in durable order` | integration | postgres:real |

## User impact

Existing applications need no migration. Commands that update an entity through `acceptExisting` will now fail and retry atomically when another writer changes the stream, while `acceptNew` prevents duplicate stream creation at the PostgreSQL boundary. `acceptAny` remains deliberately unconditional.

## ADR

Not required — this is an internal correctness fix with no public signature, dependency, capability, or extension-point change.
