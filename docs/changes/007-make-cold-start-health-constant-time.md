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

The replay module needs the registry's entity names before reading so it can
make one filtered pass instead of discovering entities by scanning the whole
log. Add a read-only accessor; existing construction and lookup interfaces are
unchanged. The HTTP contract fulfills ADR-0059: `/health` becomes reachable
independently of replay size, while `/ready` remains unavailable until the read
side is genuinely at head.

```diff signatures
+ Service.Query.Registry: registeredEntityNames :: QueryRegistry -> Array EntityName
```

## Criteria

C1 is the committed-red reproduction: the current `Subscriber.start` waits for
the event-store catch-up task and therefore cannot return while that task is
blocked. The remaining criteria pin the load-bearing ordering: paging first,
then register-first overlap with exactly-once ordered delivery, then truthful
readiness.

The positional inbox has a framework-owned capacity of 1,000 events. It is not
a user-facing tuning knob: overflow records the monotonic high-water position
and switches to paged event-store catch-up. Retry policy is also framework-owned:
transient event-store/checkpoint failures retry three times with 100ms, 500ms,
and 2s delays; deterministic updater failures become `Failed` immediately. A
later explicit rebuild invocation always resets `Failed` to `Rebuilding` and
may recover.

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Starting the query subscriber returns within 250ms even while historical catch-up remains blocked, so transport binding is not proportional to event count | `hspec:nhcore-test-service:core/test-service/Service/Query/Subscriber/ReadinessSpec.hs#returns from start before historical catch-up completes` | unit | none |
| C2 | A rebuild processes every matching event beyond the configured 1,000-event page, advances pages without a boundary skip or duplicate, and routes each event only to updaters registered for its entity | `hspec:nhcore-test-service:core/test-service/Service/Query/Subscriber/ReadinessSpec.hs#replays more than chunkSize exactly once and routes by entity` | unit | none |
| C3 | Subscription registers before replay; events appended while replay is active are delivered exactly once in increasing `globalPosition`. A 1,001-event overlap exceeds the 1,000-event inbox and recovers positionally rather than dropping an event | `hspec:nhcore-test-service:core/testlib/Test/Service/EventStore/Subscriptions/Spec.hs#register-first replay overlap is ordered and gap-free after inbox overflow` | integration | postgres:real |
| C4 | Simple and Postgres use the same inclusive start-position contract. Resume callers pass `checkpoint + 1`, so neither backend skips nor reapplies the checkpoint boundary | `hspec:nhcore-test-service:core/testlib/Test/Service/EventStore/Subscriptions/Spec.hs#inclusive start position is consistent across backends` | integration | postgres:real |
| C5 | Every registered query is `Rebuilding` before background work starts; `/ready` is 503 while any query is rebuilding/retrying and 200 only after all reach head. Transient read/checkpoint failures use the fixed three-delay retry policy, updater failures become visible `Failed`, and a later explicit rebuild resets failure state | `hspec:nhcore-test-service:core/test-service/Service/Query/Subscriber/ReadinessSpec.hs#aggregate readiness covers every query from start through bounded retry`<br>`hspec:nhcore-test-service:core/test/Service/Transport/WebSpec.hs#ready stays unavailable until all queries reach head` | unit | none |
| C6 | Catch-up reads the filtered global log once rather than once per query and resolves entity routing before invoking updaters | `hspec:nhcore-test-service:core/test-service/Service/Query/Subscriber/ReadinessSpec.hs#multi-query rebuild performs one entity-filtered pass` | unit | none |
| C7 | Automatic query wiring uses `EntityFetcher.newWithCache`; repeated updates for one entity reuse the snapshot rather than re-reading complete stream history for every updater/event pair | `hspec:nhcore-test-service:core/test-service/Service/Query/DefinitionSpec.hs#automatic query wiring reuses entity snapshots during replay` | unit | none |
| C8 | The Postgres event store constructs one long-lived pool at store creation, uses `HasqlPool.use` for operation checkout/return, and releases the pool exactly once on close rather than constructing/destroying one per operation | `hspec:nhcore-test-service:core/test/Service/EventStore/PostgresSpec.hs#operations reuse the store-owned pool until close` | unit | none |
| C9 | Page and completion logs expose `events_replayed`, `lag_from_head`, and `duration_seconds`; failure logs include query name and position but no event payload, SQL, connection string, or credential | `hurl:testbed/tests/scenarios/cold-start-readiness.hurl` | acceptance | http:real |
| C10 | With real Postgres, `/health` answers within 5s while `/ready` is 503 during replay. Across separately seeded 1k, 10k, and 100k logs, maximum minus minimum health-to-bind time is at most 2s. The 1k run also proves `/ready` becomes 200 and its final projection includes the last event | `hurl:testbed/tests/scenarios/cold-start-readiness.hurl` | acceptance | http:real |
| C11 | Postgres-gated query/event-store suites execute in CI instead of collapsing to pending because `POSTGRES_AVAILABLE` is absent | `hspec:nhcore-test-service:core/test-service/Service/QueryObjectStore/PostgresSpec.hs#state round-trips under the threaded query name` | integration | postgres:real |

## User impact

**Runtime:** container liveness no longer scales with event-store size. Operators
can probe `/health` for process liveness and `/ready` for traffic readiness
without widening a grace period as the store grows. Replay remains ordered and
complete when live events overlap startup; fixing the bind delay must not trade
a loud crash-loop for silent projection loss.

**Performance:** rebuild changes from one full-log pass per query plus
per-operation pool construction to one paged, entity-filtered pass over a
reused Postgres pool. Entity snapshots prevent repeated full-stream fetches.
Progress is visible through the ADR-0059 field names rather than a silent
multi-minute gap.

**Public Haskell surface:** additive only. `Service.Query.Registry` exposes
`registeredEntityNames :: QueryRegistry -> Array EntityName`; existing callers
need no migration.

**CI:** `.github/workflows/test.yml` exports `POSTGRES_AVAILABLE=true` for the
Postgres-backed suites so the concurrency and pool regressions execute on every
substantive PR.

**Deployment documentation:** restore the deployment guide and lead with a
`startupProbe`/readiness configuration, including explicit
`periodSeconds × failureThreshold` arithmetic and the distinction between
`/health` and `/ready`.

**Deliberately deferred:** production checkpoint-store wiring and query-state
migration remain tracked by #854/#855/#666. SIGTERM cancellation remains #662;
outbound-integration recovery is #856; the missing `X-Query-Status` contract is
#664; Neon scale-to-zero support is #857. None is required to make port binding
constant-time and in-process replay/live overlap gap-free.

## ADR

No new ADR trigger. The implementation repairs the already-decided behavior in
[ADR-0059](../decisions/0059-async-query-rebuild-with-persistent-checkpoints.md)
and restores the connection model promised by
[ADR-0060](../decisions/0060-postgres-pool-budget.md); it adds one read-only
registry accessor but introduces no breaking signature, dependency, capability,
or extension point.
