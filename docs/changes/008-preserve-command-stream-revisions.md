# Change 008: Preserve command stream revisions

Fix `CommandExecutor` so decisions keep their atomic stream preconditions at the event-store boundary and retry from freshly fetched aggregate state and revision after a consistency conflict.

```yaml spec
issue: issue#858
kind: bug
touches: [commands, entities, event-store]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

This change corrects internal execution semantics without changing the public API.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | `acceptExisting` binds its append to the revision fetched for the decision instead of using `AnyStreamState` | `CommandHandler Execute Specification Tests` "binds acceptExisting to the fetched stream revision" | unit |
| C2 | Two concurrent history-derived `acceptExisting` commands persist sequential values, and the losing stale payload is never appended unchanged | `CommandHandler Execute Specification Tests` "re-decides concurrent acceptExisting commands from the winning revision" against PostgreSQL | integration |
| C3 | Concurrent `acceptNew` commands for one stream durably create exactly one stream | `CommandHandler Execute Specification Tests` "allows exactly one concurrent acceptNew creation" against PostgreSQL | integration |
| C4 | `acceptAny` remains an unconditional append | `CommandHandler Execute Specification Tests` "keeps acceptAny unconditional" against PostgreSQL | integration |
| C5 | A consistency retry refetches both aggregate state and expected stream position before rebuilding the insertion payload | `CommandHandler Execute Specification Tests` "refreshes state and stream position before retrying" | unit |

## User impact

Commands using `acceptExisting` and `acceptNew` regain their documented atomic semantics under concurrent PostgreSQL writes. Existing APIs and call sites require no migration. The testbed behavior is unchanged except that stale history-derived event payloads can no longer be persisted during races.

## ADR

Not required — no trigger (breaking / new-dependency / new-capability / new-extension-point all false).
