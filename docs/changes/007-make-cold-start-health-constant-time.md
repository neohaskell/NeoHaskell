# Change 007: Make cold-start health constant-time without losing replayed events

A service with a non-empty event store currently performs a synchronous replay
from `StreamPosition 0` inside `Subscriber.start` before Warp binds its port.
Container probes therefore see connection-refused for time proportional to the
whole event log and eventually crash-loop a healthy revision. Make live
subscription registration and read-model catch-up non-blocking and gap-free,
page the rebuild past 1,000 events, keep `/ready` truthful until every query is
caught up, and remove the per-event Postgres pool-construction multiplier that
makes replay disproportionately slow.

```yaml spec
issue: issue#731
kind: bug
touches: [event-store, queries, entities, http-transport, service-wiring, testbed, ci-cd, website, governance-docs]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

No exported Haskell signature changes. This change fulfills the existing
ADR-0059 HTTP contract: `/health` becomes reachable independently of replay
size, while `/ready` remains unavailable until the read side is genuinely at
head.

```diff signatures
```

## Criteria

C1 is the committed-red reproduction: the current `Subscriber.start` waits for
the event-store catch-up task and therefore cannot return while that task is
blocked. The remaining criteria pin the load-bearing ordering: paging first,
then register-first overlap with exactly-once ordered delivery, then truthful
readiness.

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | Starting the query subscriber returns within a fixed startup budget even while historical catch-up remains blocked, so transport binding is not proportional to event count | `Service.Query.Subscriber.ReadinessSpec` "returns from start before historical catch-up completes" | unit |
| C2 | A rebuild processes every matching event beyond the configured 1,000-event page, advances pages without a boundary skip or duplicate, and routes each event only to updaters registered for its entity | `Service.Query.Subscriber.ReadinessSpec` "replays more than chunkSize exactly once and routes by entity" | integration |
| C3 | Subscription registers before replay; events appended while replay is active are delivered exactly once in increasing `globalPosition`, and inbox overflow falls back to positional catch-up rather than dropping an event | `Test.Service.EventStore.Subscriptions.Spec` "register-first replay overlap is ordered and gap-free" | integration |
| C4 | Simple and Postgres use the same inclusive position contract, so replay/resume neither skips nor reapplies the boundary event | `Test.Service.EventStore.Subscriptions.Spec` "position resume is consistent across backends" | integration |
| C5 | `/ready` is 503 before any rebuild work starts, remains 503 while any registered query is rebuilding or retrying, becomes 200 only after all queries reach head, and a checkpoint-write/rebuild failure cannot pin an unobservable `Rebuilding` state forever | `Service.Query.Subscriber.ReadinessSpec` "aggregate readiness covers every query from start through retry" | integration |
| C6 | Catch-up reads the global log once rather than once per query, uses entity-filtered reads, and entity reconstruction reuses the configured snapshot cache rather than re-fetching complete stream history for every updater/event pair | `Service.Query.Subscriber.ReadinessSpec` "multi-query rebuild performs one entity-routed pass" | integration |
| C7 | The Postgres event store constructs one long-lived pool at store creation, checks connections out and returns them with bracketed cleanup for each operation, and does not construct/destroy a pool per replayed event | `Service.EventStore.PostgresSpec` "replay operations reuse the store pool and release checked-out connections" | integration |
| C8 | Rebuild progress emits `events_replayed`, `lag_from_head`, and `duration_seconds` at page boundaries and completion, including structured failure context | `Service.Query.Subscriber.ReadinessSpec` "emits bounded replay progress and failure telemetry" | unit |
| C9 | With real Postgres, `/health` answers within the startup budget while `/ready` is 503 during replay; health-to-bind latency remains flat across seeded logs of 1k, 10k, and 100k events, and `/ready` becomes 200 only after the final event is queryable | `testbed/tests/scenarios/cold-start-readiness.hurl` | acceptance |
| C10 | Postgres-gated query/event-store suites execute in CI instead of collapsing to pending because `POSTGRES_AVAILABLE` is absent | `Service.QueryObjectStore.PostgresSpec` "state round-trips under the threaded query name" | integration |

## User impact

**Runtime:** container liveness no longer scales with event-store size. Operators
can probe `/health` for process liveness and `/ready` for traffic readiness
without widening a grace period as the store grows. Replay remains ordered and
complete when live events overlap startup; fixing the bind delay must not trade
a loud crash-loop for silent projection loss.

**Performance:** rebuild changes from one full-log pass per query plus
per-operation pool construction to one paged, entity-routed pass over a reused
Postgres pool. Progress is visible through the ADR-0059 field names rather than
a silent multi-minute gap.

**CI:** `.github/workflows/test.yml` exports `POSTGRES_AVAILABLE=true` for the
Postgres-backed suites so the concurrency and pool regressions execute on every
substantive PR.

**Deployment documentation:** restore the deployment guide and lead with a
`startupProbe`/readiness configuration, including explicit
`periodSeconds × failureThreshold` arithmetic and the distinction between
`/health` and `/ready`.

**Deliberately deferred:** production checkpoint-store wiring and query-state
migration remain a separate data-migration-shaped change. They must ship with
the stale-hash deletion and field-shape hash fixes. SIGTERM cancellation remains
blocked by #662; outbound-integration recovery and the missing
`X-Query-Status` client contract remain separate changes. None is required to
make port binding constant-time and replay gap-free.

## ADR

No new ADR trigger. The implementation repairs the already-decided behavior in
[ADR-0059](../decisions/0059-async-query-rebuild-with-persistent-checkpoints.md)
and restores the connection model promised by
[ADR-0060](../decisions/0060-postgres-pool-budget.md); it does not add or remove
public signatures, dependencies, capabilities, or extension points.
