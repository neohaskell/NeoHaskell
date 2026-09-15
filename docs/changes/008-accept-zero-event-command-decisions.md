# Change 008: Accept zero-event command decisions

Treat an accepted command decision with no events as a successful idempotent no-op after resolving and validating its target stream, without calling the event store.

```yaml spec
issue: issue#859
kind: bug
touches: [commands, event-store, testlib]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | An accepted zero-event decision for an existing stream returns `CommandAccepted` with zero appended events | `CommandHandler Execute Specification Tests` "accepts a zero-event decision for an existing stream" | integration |
| C2 | An accepted zero-event decision does not invoke event-store insertion | `CommandHandler Execute Specification Tests` "does not insert an accepted zero-event decision" | unit |
| C3 | `acceptAny []` succeeds when the command resolves a valid stream | `CommandHandler Execute Specification Tests` "accepts an acceptAny zero-event decision with a resolved stream" | integration |
| C4 | `acceptExisting []` still rejects when the target entity does not exist | `CommandHandler Execute Specification Tests` "rejects an acceptExisting zero-event decision for a missing entity" | integration |
| C5 | An accepted zero-event decision without a resolvable stream ID returns a bounded explicit failure | `CommandHandler Execute Specification Tests` "fails a zero-event decision without a resolvable stream" | integration |
| C6 | Simple and PostgreSQL-backed execution accept zero-event decisions without changing durable event counts | `CommandHandler zero-event backend integration` "keeps Simple and PostgreSQL event counts unchanged" | integration |
| C7 | Repeating an idempotent command succeeds with zero appended events and leaves the durable event count unchanged | `CommandHandler Execute Specification Tests` "accepts a repeated idempotent command without appending events" | integration |

## User impact

Idempotent commands can report successful convergence when aggregate state shows that no new event is needed. No public API changes or migration are required.

## ADR

Not required — no trigger (breaking / new-dependency / new-capability / new-extension-point all false).
