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
| C2 | Concurrent `acceptExisting` commands refetch the fresh revision, re-decide from fresh state, and persist sequential derived values rather than a duplicated stale payload | `hspec:nhcore-test-service:core/testlib/Test/Service/CommandHandler/Execute/Spec.hs#acceptExisting commands refetch and re-decide with fresh state` | integration | postgres:real |
| C3 | Concurrent `acceptNew` commands preserve `StreamCreation` and allow exactly one durable creation | `hspec:nhcore-test-service:core/testlib/Test/Service/CommandHandler/Execute/Spec.hs#acceptNew commands preserve StreamCreation under a PostgreSQL race` | integration | postgres:real |
| C4 | Concurrent `acceptAny` commands preserve `AnyStreamState` and remain unconditional appends | `hspec:nhcore-test-service:core/testlib/Test/Service/CommandHandler/Execute/Spec.hs#acceptAny commands preserve unconditional appends` | integration | postgres:real |

## User impact

Existing applications need no migration. Commands that update an entity through `acceptExisting` will now fail and retry atomically when another writer changes the stream, while `acceptNew` prevents duplicate stream creation at the PostgreSQL boundary. `acceptAny` remains deliberately unconditional.

## ADR

Not required — this is an internal correctness fix with no public signature, dependency, capability, or extension-point change.
