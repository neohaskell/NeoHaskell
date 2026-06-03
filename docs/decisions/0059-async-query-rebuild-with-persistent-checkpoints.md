# ADR-0059: Async Query Rebuild with Persistent Checkpoints

> Issue: [#650 — Query rebuild blocks HTTP readiness on every restart](https://github.com/neohaskell/NeoHaskell/issues/650)

## Status

Proposed

## Context

### Current State

`Service.Query.Subscriber.rebuildAll` is invoked synchronously from
`Application.run` (`core/service/Service/Application.hs` lines 1154–1161)
before any transport binds. It reads the entire event store from
`StreamPosition 0` with `Limit 9223372036854775807` (Int64 max) and
funnels every event through every registered `QueryUpdater`. The only
`QueryObjectStore` implementation today is in-memory, so the read-model
state starts empty on every restart. There is no persistent cursor, no
snapshot, and no way to skip work the previous process already did.

ADR-0007 already flags this in its Related Work section as item #4
("Checkpoint persistence"). This ADR is that follow-up.

### Use Cases

- **Rolling deploys on fly.io / Kubernetes** — the new machine must
  pass a readiness probe within seconds, even when the event log
  contains millions of events. Today readiness is `O(eventCount)`.
- **Crash recovery and machine restarts** — a transient OOM or a
  serverless Postgres warmup should not turn into a multi-minute window
  where the load balancer sees a dead port.
- **Shipping one-line code fixes** — a no-op deploy should not force a
  full read-model re-derivation. Persisting state with its position
  turns that into a few cents of Postgres reads instead of a full
  replay.
- **Adding a query to an existing service** — only the newly-added
  query should replay from `StreamPosition 0`. Pre-existing queries
  continue from their stored position.

### Design Goals

1. **HTTP `/health` is constant-time** regardless of event count, so
   liveness probes succeed the moment the process binds.
2. **`/ready` reflects actual catch-up state**, so traffic is only
   routed to a machine when its read models are current.
3. **Per-query positions**, so adding or evolving one query does not
   penalise the others.
4. **Persistent `QueryObjectStore` backend**, so a restart does not
   discard work the previous process already paid for.
5. **Chunked reads with progress logging**, so a slow rebuild is
   observable rather than silent.
6. **Schema-evolution safety**, so a `KnownHash` mismatch forces a
   full replay of only the affected query — never silent corruption.
7. **Default API stays Jess-friendly** — existing apps must keep
   working without learning new vocabulary. The async path and the
   readiness gate are framework-provided defaults.

### GitHub Issue

- [#650: Query rebuild blocks HTTP readiness on every restart — no checkpoint, no persistent QueryObjectStore](https://github.com/neohaskell/NeoHaskell/issues/650)

## Decision drivers

- **Boot time on warm restart must be constant**, not linear in event
  count. Anything else fails the rolling-deploy use case.
- **The default `QueryObjectStoreConfig` in `Service.Application` stays
  in-memory** — adding Postgres must be a one-line opt-in, not a
  required dependency for hello-world apps.
- **The persisted position and the persisted state must commit
  together** — otherwise a crash between the two creates either a
  duplicate apply (if position is written first) or a lost update (if
  state is written first). Both are observable as read-model drift.
  The fix is to keep them in the *same row* — precedent: ADR-0006
  `Snapshot { state, position }`.
- **Readiness is a first-class concept** — `/health` (liveness, per
  ADR-0025) and `/ready` (readiness) are separate endpoints. The
  framework owns both. Jess never writes either one by hand.
- **One readiness flag per query, plus an aggregate** — global
  readiness is `all queries are ready`. A slow query does not have
  to block the rest.

## Considered options

### Option 1 — Persistent `QueryObjectStore.Postgres` with embedded position + async rebuild (chosen)

Layered set of small changes:

1. New `Service.QueryObjectStore.Postgres` backend (table
   `query_object_store` keyed by `(query_name, instance_uuid)`, JSON
   column for the serialised query state, `position` and `query_hash`
   columns in the same row — same shape as `Snapshot { state, position }`
   in ADR-0006).
2. `atomicUpdate` is a single
   `INSERT ... ON CONFLICT (query_name, instance_uuid) DO UPDATE`
   that writes state and position together. One transaction, one
   table, no two-table coupling.
3. `Subscriber.rebuildAll` is split into `rebuildFrom` (per-query,
   resumable, chunked) and `rebuildAllAsync` which spawns the work via
   `AsyncTask.run` and flips `subscriber.readiness` when done.
4. `Application.run` becomes non-blocking on rebuild — transports
   bind immediately, `/health` is 200 (ADR-0025), `/ready` waits on
   `subscriber.readiness`. Per-query endpoints respect per-query
   readiness with response header `X-Query-Status: rebuilding`.
5. Chunked reads (default `Limit 1000` per page) with progress logging
   and observability counters.
6. `KnownHash` mismatch (`deriveQuery`-derived) triggers a full replay
   of the affected query only — rows with the stale hash are deleted
   at startup, that query replays from `StreamPosition 0`.

### Option 2 — Synchronous rebuild + Postgres `QueryObjectStore` only

Persist the object store but keep the synchronous rebuild and the
single global cursor.

- Rejected: still re-reads the full event history on every restart
  because there is no per-query position. Strictly worse than option 1
  on boot time; only marginally better on memory pressure.

### Option 3 — Async rebuild + InMemory only

Spin off `rebuildAll` to an async task, but keep the in-memory store
and no persisted position.

- Rejected: unblocks startup but `/ready` stays 503 for the full
  rebuild window on every restart. Acceptable only for tiny event
  stores — useless at scale.

### Option 4 — Snapshot the in-memory store to JSONL periodically

Mirror `SimpleEventStore`'s persistent JSONL mode.

- Rejected: recovery semantics are weaker than transactional Postgres
  (torn writes, no `INSERT … ON CONFLICT`, no per-query atomicity) and
  gain nothing for users who already run Postgres for the event store.

### Option 5 — Eager hydration on first read instead of at startup

Defer all replay until the first `GET /queries/{name}` call hits.

- Rejected: pushes the cost to an unbounded first-request latency.
  Breaks the 50k req/s budget the framework targets, and the first
  caller becomes the unlucky one.

### Option 6 — Drop rebuild entirely; treat queries as live-only

- Rejected: silently breaks the ADR-0007 consistency guarantee. Every
  event emitted before the current process started would be lost from
  the read model.

| Option | Verdict | Reason |
|--------|---------|--------|
| 1. Postgres store with embedded position + async | **Chosen** | Only design that satisfies all seven goals. |
| 2. Postgres store, sync rebuild | Rejected | Still O(events) on boot. |
| 3. Async rebuild, InMemory store | Rejected | `/ready` still flaps on every restart. |
| 4. JSONL snapshots | Rejected | Weaker recovery semantics than Postgres. |
| 5. Lazy hydration | Rejected | Unbounded first-request latency. |
| 6. Live-only queries | Rejected | Silently loses pre-existing events. |

## Decision outcome

Adopt Option 1. The implementation is layered — each piece compiles and
ships on its own, and each is independently testable.

### 1. Persistent `QueryObjectStore.Postgres`

New module `Service.QueryObjectStore.Postgres` provides a Hasql-backed
`QueryObjectStore` implementation. Position lives **inside** the
state row (precedent: ADR-0006 `Snapshot { state, position }`) so the
state write and the position write commit together by construction.
Schema:

```sql
CREATE TABLE query_object_store (
  query_name        TEXT NOT NULL,
  instance_uuid     UUID NOT NULL,
  query_hash        TEXT NOT NULL,           -- KnownHash-derived, used for schema evolution
  position          BIGINT NOT NULL,         -- StreamPosition reached at this state
  state_json        JSONB NOT NULL,
  updated_at        TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (query_name, instance_uuid)
);
```

`atomicUpdate` becomes a single
`INSERT ... ON CONFLICT (query_name, instance_uuid) DO UPDATE`
statement using Hasql's typed `Statement` API (string concatenation is
unrepresentable — see ADR-0027 / EventStore.Postgres precedent). One
transaction, no two-table coupling.

Serialization to `state_json` runs on the per-event hot path. Any
persisted query state must implement `toEncoding` directly — either
via `Generic`-derived `deriveJSON` or hand-written `toEncoding` — to
write straight into the encoding `Builder` without materialising an
intermediate `Value` tree. A `toJSON`-only instance is rejected at
compile time by a `QueryStateSerializable` constraint on
`useQueryObjectStore`.

**Data classification.** `state_json` inherits the data-classification
properties of its source events; this ADR does not introduce a new PII
surface (the same data already lives in `eventstore.events` JSONB
under ADR-0004). Encryption-at-rest, if required, is a Postgres-
deployment concern at the cluster/tablespace layer, not an
application-schema one. Queries that *project* sensitive fields out of
events should drop them explicitly in the `QueryUpdater` —
documenting this in the `useQueryObjectStore` haddock is in-scope for
phase 10.

### 2. Startup resume

For each registered query, on startup the framework runs:

```sql
SELECT min(position)
FROM query_object_store
WHERE query_name = ?
  AND query_hash = ?
```

The minimum position across that query's rows is the resume point —
the subscriber starts there. Rows whose `query_hash` does not match
the current `KnownHash`-derived hash for that query are treated as
garbage and deleted; that query then replays from `StreamPosition 0`.
Schema evolution is therefore explicit and per-query: a hash change
forces a full replay of exactly that query, nothing else.

### 3. Async rebuild + readiness

`Service.Query.Subscriber.QuerySubscriber` gains a new
`ConcurrentVar` carrying per-query readiness:

```haskell
data Readiness
  = Rebuilding
  | Ready
  deriving (Eq, Show)
```

`Application.run` replaces the synchronous call site with an async
spawn, and transports bind immediately. `/health` (ADR-0025) keeps its
existing meaning — the process is up — and is unaffected by rebuild
state. `/ready` is the new endpoint that gates on
`subscriber.readiness`.

**Rebuild timeout.** Each query's rebuild runs under a configurable
per-query timeout (default `5 minutes`, settable via
`RebuildOptions { timeout :: Duration }`). On timeout or updater
exception, the readiness state flips to a third constructor:

```haskell
data Readiness
  = Rebuilding
  | Ready
  | Failed Text     -- reason: timeout, updater exception, hash-replay failure
  deriving (Eq, Show)
```

`Failed` is a terminal state for that query — `/ready` reports it
distinctly so the orchestrator stops flapping, and the per-query
endpoint returns `503` with `X-Query-Status: failed`. Recovery
requires operator intervention (fix the updater, then restart).
Updater exceptions are logged at `WARN` with the offending event
position, the query name, and a truncated payload digest.

### 4. Chunked reads + progress

`Subscriber.rebuildFrom` reads events in pages of
`Limit 1000` (configurable via `RebuildOptions`), logs progress every
page, and emits the three observability counters
(`events_replayed`, `lag_from_head`, `duration_seconds`) per query.

### 5. Module placement

```text
core/service/
  Service/
    Query/
      Subscriber.hs               -- modified: rebuildFrom, readiness
    QueryObjectStore/
      Postgres.hs                 -- new: Hasql-backed implementation (state + position in one row)
    Transport/
      Web/
        Readiness.hs              -- new: /ready handler (/health stays in existing module)
```

Follows the established flat structure with one level of nesting for
implementation variants. No `Service.Query.Checkpoint` module — the
position lives inside `QueryObjectStore` rows, so there is nothing
separate to put behind a trait.

### Performance testing

A dedicated benchmark suite exercises the new design under the
conditions that motivated it. Each suite below names its assertion
shape; floor numbers are filled in during benchmarking (phase 5).

1. **HTTP-bind latency** (Hurl + `time_total`): `/health` returns
   `200` within X ms of process start, *flat* across event counts
   1k / 10k / 100k.
2. **Cold-start replay throughput** (`tasty-bench` or whatever
   harness `EventStore` already uses): events/sec replayed at chunk
   size `1000` through one trivial updater, reported as a single
   number.
3. **Warm-restart latency**: with `query_object_store` already at
   head, `/ready` flips to `200` within X ms regardless of N. Proves
   "skip already-processed work" actually skips.
4. **Chunked-read memory bound**: heap stays under M bytes during a
   100k-event replay. Proves chunking, not just termination.
5. **`atomicUpdate` contention** (`criterion` or equivalent): K
   concurrent updates to the same `(query_name, instance_uuid)` —
   throughput curve reported at K = 1, 10, 100.
6. **Per-query selective replay**: hash mismatch on 1 of K queries
   replays only that one; other queries' rebuild times statistically
   indistinguishable from the no-mismatch baseline.
7. **Catch-up during live events**: replay running concurrently with
   live appends → effective catch-up rate ≥ append rate at steady
   state.
8. **Idempotent replay (property)**: replaying the same N events
   twice produces byte-identical state and an identical final
   position.

Suites match the rigor expected of `EventStore.Postgres`. Phase 8
(test spec) checks for an existing `tasty-bench` harness under
`core/test-service/`; if absent, adding a minimal harness is
in-scope for this ADR's implementation.

### Concurrency & correctness testing

Async rebuild + live subscription + persistent store creates concrete
hazards. Each `H#` below has a named test counterpart.

- **H1 — Replay racing live subscription.** Async replay catches up
  to position `P` while the live subscriber is already processing
  events `≥ P`; the same event can hit `atomicUpdate` from two
  threads in indeterminate order.
  - Test: race test seeds the EventStore at positions `1..1000`,
    starts replay, injects a live event near the boundary; final
    state is byte-equal to a "replay only, no live" reference.

- **H2 — Lost write via `ON CONFLICT DO UPDATE`.** Two concurrent
  updates with positions `(Pₐ, P_b)` where `Pₐ > P_b` — naïve
  last-writer-wins overwrites the higher position with the lower
  one.
  - Test: property test asserts `position` is monotonically
    non-decreasing per `(query_name, instance_uuid)` under any
    random interleaving of `N = 100` concurrent updates.
  - Resolution: `atomicUpdate` uses CAS-on-position semantics — the
    SQL `DO UPDATE` fires only `WHERE query_object_store.position < EXCLUDED.position`.

- **H3 — Crash mid-update.** Process killed between event fetch and
  Postgres commit; after restart, replay must converge.
  - Test: crash injection (`Task.throw` or `pg_terminate_backend`)
    at three points — pre-write, mid-transaction,
    post-write-pre-ack. Restart and verify state converges to the
    expected baseline.

- **H4 — Readiness flag visibility.** `subscriber.readiness` flips
  to `Ready` before the last `query_object_store` write is durable
  → a request lands and reads stale or missing state.
  - Test: assert no query read can succeed before its last write is
    observable via a *fresh* Postgres connection (no caching).

- **H5 — Hash-mismatch mid-flight.** Hash changes for query `Q`; we
  delete `Q`'s rows and replay from `0` while a concurrent live
  event arrives for `Q`.
  - Test: trigger a hash change while live events are arriving for
    that query; final state equals "fresh replay from 0 with all
    events"; other queries' positions are unchanged.

- **H6 — Chunk-boundary tearing.** Chunked read at `Limit 1000`;
  causally-linked events that span chunks must still produce
  identical state to a single-chunk read.
  - Test: property test —
    `replay (events₁ ++ events₂) ≡ replay events₁ ; replay events₂`
    from the same starting position, for any split point.

- **H7 — Init ordering.** Live subscription started before all
  queries registered → events silently dropped for unregistered
  queries.
  - Test: register a query *after* `Application.run` has started →
    either rejected with a clear error or accepted with full replay
    from `0`. No silent drops, asserted by a sum projection.

- **H8 — AsyncTask cancellation on shutdown.** SIGTERM during
  rebuild → `AsyncTask` cancelled; persisted position must be safe
  (no partial commits past it).
  - Test: Hurl scenario — SIGTERM mid-rebuild, restart; replay
    resumes from the persisted position; no events double-counted
    (asserted via a sum-style query).

- **H9 — Multi-writer (future-proofing only).** Out of MVP scope,
  but the design must not preclude it.
  - Test: assert the contract — `atomicUpdate` with a stale
    `expectedPosition` rejects rather than overwrites. Keeps the
    door open for HA without designing for it now.

Race orchestration follows the barrier pattern used in
`core/test/Service/EventStore/` (verified during phase 8). Crash
injection helpers under `core/testlib/Test/Service/` — added
in-scope if missing.

## Public API

The framework already provides `useQueryObjectStore`, `withQuery`, and
the `/health` endpoint (ADR-0025). The new surface area is minimal:
the `Postgres` constructor for `QueryObjectStore`, the framework-owned
`/ready` endpoint (which is on by default), and the response header
`X-Query-Status: rebuilding` on query endpoints during rebuild.

### `Service.QueryObjectStore.Postgres`

```haskell
module Service.QueryObjectStore.Postgres (
  PostgresQueryObjectStoreConfig (..),
) where

import Service.QueryObjectStore.Core (QueryObjectStoreConfig (..))
import Text (Text)


data PostgresQueryObjectStoreConfig = PostgresQueryObjectStoreConfig
  { host :: Text,
    databaseName :: Text,
    user :: Text,
    password :: Text,
    port :: Int
  }
  deriving (Eq, Ord, Show)


instance QueryObjectStoreConfig PostgresQueryObjectStoreConfig where
  createQueryObjectStore config = do
    -- See Service.EventStore.Postgres.Internal for the pool pattern.
    pool <- acquirePool config
    Task.yield
      QueryObjectStore
        { get = getFromPool pool,
          atomicUpdate = atomicUpdateInPool pool,
          getAll = getAllFromPool pool
        }
```

### Application builder

```haskell
app :: Application
app =
  Application.new
    |> Application.withEventStore postgresEventStoreConfig
    |> Application.useQueryObjectStore postgresConfig
    |> Application.useReadinessEndpoint
    |> Application.withQuery @UserOrders
    |> Application.withService userService
```

`useQueryObjectStore postgresConfig` is the entire opt-in for
persistent state with embedded position — there is no separate
checkpoint builder. `useReadinessEndpoint` is on by default; the
explicit form is shown so Jess sees one autocomplete entry that
covers the readiness contract.

### Readiness endpoint contract

```text
GET /health    → 200 {"status":"ok"}           (ADR-0025; unchanged)

GET /ready     → 200 {"status":"ready"}        (when overallReadiness == Ready)
               → 503 {"status":"rebuilding", "queries":[{name,lag}]}

GET /queries/{name}
               → 200 [...]                     (when readinessOf name == Ready)
               → 503 {"status":"rebuilding"}   with header X-Query-Status: rebuilding
               → 503 {"status":"failed","reason":"..."}  with header X-Query-Status: failed
```

`/health` (ADR-0025) is unchanged — the process is alive. `/ready` is
the new probe that reflects subscriber catch-up. Per-query degraded
mode is a header (`X-Query-Status: rebuilding`) on the existing query
endpoint, not a separate URL.

### Example: handling a rebuilding query in client code

Jess never writes this — it is what the framework returns. Sample
response body:

```json
{
  "status": "rebuilding",
  "queries": [
    {"name": "user-orders", "lag": 42091, "position": 1234567}
  ]
}
```

## Consequences

### Positive

1. **HTTP liveness is constant-time on warm restart.** `/health`
   succeeds the moment the process binds; rebuild work is invisible
   to liveness probes (ADR-0025 semantics preserved).
2. **Rolling deploys stop flapping.** Load balancers route to
   machines only when `subscriber.readiness == Ready`, observed via
   `/ready`.
3. **Persistent state survives restarts.** A no-op deploy reads zero
   events from the EventStore for queries that were caught up before
   the restart.
4. **Position is embedded in the persisted state record** (precedent:
   ADR-0006 `Snapshot`). Serverless-ready — the DB row IS the cache,
   no separate cursor to synchronise.
5. **Per-query isolation.** Adding `OrderSummary` to a service with a
   caught-up `UserOrders` only replays the new query.
6. **Schema evolution is safe and explicit.** A `KnownHash` change
   forces a full replay for exactly that query, leaving the others
   untouched.
7. **Observable rebuild progress.** `events_replayed`,
   `lag_from_head`, `duration_seconds` are emitted per query — a slow
   rebuild becomes a measurement rather than a guess.
8. **Default API stays Jess-clean.** Apps that do not opt into
   Postgres keep working unchanged; the only visible difference is
   that `/health` returns immediately and `/ready` is a new probe
   they can ignore until they need it.
9. **Aligns with existing precedents.** Same trait + Postgres-impl
   pattern as ADR-0006 (snapshot cache, same `state + position`
   shape) and ADR-0004 (EventStore); readiness contract extends
   ADR-0025 (`/health`).

### Negative

1. **One new table.** `query_object_store` must be migrated into the
   user's Postgres schema. The `EventStore.Postgres` migration
   pattern is the precedent — the framework runs the migration on
   startup.
2. **Larger surface for `QueryObjectStore` implementations.** Future
   backends (Redis, DynamoDB) must support an atomic
   compare-and-swap on `(state, position)` per row, or accept a
   documented at-least-once apply semantics.
3. **Per-query readiness adds one `ConcurrentVar` per registered
   query.** Negligible memory cost (a handful of bytes per query)
   but it is a new concurrency primitive on the hot path of every
   event delivery. The 50k req/s budget is not affected because
   reads only touch `ConcurrentVar.peek`.
4. **`X-Query-Status: rebuilding` is a new client contract.** Hurl
   smoke tests and acceptance tests need to learn the header. JSON
   503 body is documented but new.
5. **Async rebuild surfaces a class of bug that synchronous rebuild
   hid** — a buggy `QueryUpdater` no longer crashes the boot, so its
   failure must be surfaced through structured logs and the rebuild
   counter rather than the process exit code.

### Risks

| Risk | Mitigation |
|------|------------|
| `INSERT ... ON CONFLICT DO UPDATE` with CAS-on-position semantics is a non-trivial Hasql `Statement`. | Single statement using Hasql's typed `Statement` API — same pattern as `EventStore.Postgres` already uses for insert + notify. Reviewed in phase 4 (security) and phase 5 (perf). |
| `KnownHash` mismatch path could be exploited to force expensive replays. | Hash is derived at compile time by `deriveQuery`; not user-controlled at runtime. Replay is per-query, not global, and progress is observable. |
| Async rebuild masks updater failures. | Mandatory structured-log + counter on updater failure; `/ready` body lists per-query lag so a stuck query is visible. |
| Long catch-up windows on first deploy of a Postgres-backed store. | Acceptable — first deploy is the one time per service that O(eventCount) work is unavoidable. Progress logging makes it observable; the async path keeps `/health` honest throughout. |
| Hash mismatch + crash mid-rebuild could leave row stale. | State and position are written in the same row, so a crash mid-rebuild simply resumes from the last successfully-committed position; CAS-on-position prevents regression. |

### Mitigations

- Migration script for the new table follows the existing
  `EventStore.Postgres` precedent (idempotent `CREATE TABLE IF NOT
  EXISTS`, runs on startup).
- Default `useQueryObjectStore` falls back to in-memory, so the
  framework does not become harder to spin up locally.
- Hurl acceptance tests in `testbed/` add coverage for `/ready` and
  the `X-Query-Status: rebuilding` header on a query endpoint hit
  during rebuild.
- ADR-0007 status section is updated to cross-reference this ADR.

## References

- [#650: Query rebuild blocks HTTP readiness on every restart](https://github.com/neohaskell/NeoHaskell/issues/650)
- [ADR-0007: Queries (Read Models)](0007-queries-read-models.md) — original CQRS read-model design; §"Application Startup Sequence" and §"Related Work" item 4.
- [ADR-0006: Entity Snapshot Cache](0006-entity-snapshot-cache.md) — precedent for the `state + position` in-one-row shape.
- [ADR-0004: EventStore Abstraction](0004-eventstore-abstraction.md) — Postgres backend template.
- [ADR-0025: Auto Health Endpoint for WebTransport Apps](0025-auto-health-endpoint.md) — `/health` precedent; `/ready` extends it.
- [ADR-0027: PostgreSQL Connection Pool Health for Serverless Databases](0027-postgres-pool-health.md) — readiness-signal precedent; pool config to inherit.
- [core/service/Service/Query/Subscriber.hs](../../core/service/Service/Query/Subscriber.hs) — current `rebuildAll`.
- [core/service/Service/Application.hs](../../core/service/Service/Application.hs) (lines 1154–1161) — synchronous call site.
- [core/service/Service/QueryObjectStore/Core.hs](../../core/service/Service/QueryObjectStore/Core.hs) — trait that needs a Postgres impl.
- [core/service/Service/EventStore/Postgres/Internal.hs](../../core/service/Service/EventStore/Postgres/Internal.hs) — template for the new Postgres backend.

---

## Implementation notes

- **`withoutReadinessEndpoint` is deferred.** Decision C of this ADR prescribed
  a public builder `Application.withoutReadinessEndpoint` for opt-out. During
  PR review the maintainer called YAGNI on that opt-out and the function was
  removed. `/ready` is always on with no opt-out today. A follow-up issue will
  track adding the opt-out only if a real deployment surfaces the need.
