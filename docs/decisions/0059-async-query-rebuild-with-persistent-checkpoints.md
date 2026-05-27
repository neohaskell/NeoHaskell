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
  full read-model re-derivation. Persisting the checkpoint and the
  store turns that into a few cents of Postgres reads instead of a
  full replay.
- **Adding a query to an existing service** — only the newly-added
  query should replay from `StreamPosition 0`. Pre-existing queries
  continue from their stored checkpoint.

### Design Goals

1. **HTTP `/healthz` is constant-time** regardless of event count, so
   liveness probes succeed the moment the process binds.
2. **`/readyz` reflects actual catch-up state**, so traffic is only
   routed to a machine when its read models are current.
3. **Per-query checkpoints**, so adding or evolving one query does not
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
- **Checkpoint writes must be transactional with the object write** —
  otherwise a crash between the two creates either a duplicate apply
  (if checkpoint is written first) or a lost update (if object is
  written first). Both are observable as read-model drift.
- **Readiness is a first-class concept** — `/healthz` (liveness) and
  `/readyz` (readiness) are separate endpoints. The framework owns
  both. Jess never writes either one by hand.
- **One readiness flag per query, plus an aggregate** — global
  readiness is `all queries are ready`. A slow query does not have
  to block the rest.

## Considered options

### Option 1 — Persistent `QueryObjectStore.Postgres` + per-query checkpoint + async rebuild (chosen)

Layered set of small changes:

1. New `Service.QueryObjectStore.Postgres` backend (table
   `query_object_store` keyed by `(query_name, instance_uuid)`, JSON
   column for the serialised query).
2. New `Service.Query.Checkpoint` module + table `query_checkpoint`
   keyed by `query_name`, updated transactionally in the same
   statement batch as the object write inside `atomicUpdate`.
3. `Subscriber.rebuildAll` is split into `rebuildFrom` (per-query,
   resumable, chunked) and `rebuildAllAsync` which spawns the work via
   `AsyncTask.run` and flips `subscriber.readiness` when done.
4. `Application.run` becomes non-blocking on rebuild — transports
   bind immediately, `/healthz` is 200, `/readyz` waits on
   `subscriber.readiness`. Per-query endpoints respect per-query
   readiness with `X-Query-Status: rebuilding` + 503.
5. Chunked reads (default `Limit 1000` per page) with progress logging
   and observability counters.
6. `KnownHash` mismatch (`deriveQuery`-derived) triggers a full replay
   of the affected query only — checkpoint for that query is cleared
   at startup.

### Option 2 — Synchronous rebuild + Postgres `QueryObjectStore` only

Persist the object store but keep the synchronous rebuild and the
single global cursor.

- Rejected: still re-reads the full event history on every restart
  because there is no checkpoint. Strictly worse than option 1 on boot
  time; only marginally better on memory pressure.

### Option 3 — Async rebuild + InMemory only

Spin off `rebuildAll` to an async task, but keep the in-memory store
and no checkpoint.

- Rejected: unblocks startup but `/readyz` stays 503 for the full
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
| 1. Postgres store + per-query checkpoint + async | **Chosen** | Only design that satisfies all seven goals. |
| 2. Postgres store, sync rebuild | Rejected | Still O(events) on boot. |
| 3. Async rebuild, InMemory store | Rejected | `/readyz` still flaps on every restart. |
| 4. JSONL snapshots | Rejected | Weaker recovery semantics than Postgres. |
| 5. Lazy hydration | Rejected | Unbounded first-request latency. |
| 6. Live-only queries | Rejected | Silently loses pre-existing events. |

## Decision outcome

Adopt Option 1. The implementation is layered — each piece compiles and
ships on its own, and each is independently testable.

### 1. Persistent `QueryObjectStore.Postgres`

New module `Service.QueryObjectStore.Postgres` provides a Hasql-backed
`QueryObjectStore` implementation. Schema:

```sql
CREATE TABLE query_object_store (
  query_name        TEXT NOT NULL,
  instance_uuid     UUID NOT NULL,
  payload           JSONB NOT NULL,
  PRIMARY KEY (query_name, instance_uuid)
);
```

`atomicUpdate` becomes a single `INSERT … ON CONFLICT DO UPDATE`
statement using Hasql's typed `Statement` API (string concatenation is
unrepresentable — see ADR-0027 / EventStore.Postgres precedent). The
`QueryObjectStoreConfig` typeclass already exposes the right entry
point; the new module just adds another instance.

### 2. Per-query checkpoint

New module `Service.Query.Checkpoint` and table:

```sql
CREATE TABLE query_checkpoint (
  query_name        TEXT PRIMARY KEY,
  last_position     BIGINT NOT NULL,
  known_hash        TEXT NOT NULL
);
```

The checkpoint is updated in the same Hasql transaction as the object
write. The `known_hash` column stores the `KnownHash` derived by
`deriveQuery`; a mismatch at startup means schema evolved and the row
is deleted, forcing a full replay for that query only.

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
spawn, and transports bind immediately. The readiness endpoints
(`/healthz`, `/readyz`) are added to the WebTransport route table
alongside the existing `/health` (ADR-0025).

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
      Checkpoint.hs               -- new: checkpoint trait + types
      Checkpoint/
        InMemory.hs               -- default impl (mirrors InMemory QueryObjectStore)
        Postgres.hs               -- transactional impl, sharing the pool
      Subscriber.hs               -- modified: rebuildFrom, readiness
    QueryObjectStore/
      Postgres.hs                 -- new: Hasql-backed implementation
  Service/
    Transport/
      Web/
        Readiness.hs              -- new: /healthz + /readyz handlers
```

Follows the established flat structure with one level of nesting for
implementation variants.

## Public API

The framework already provides `useQueryObjectStore`, `withQuery`, and
the `/health` endpoint. The new surface area is minimal: the
`Postgres` constructor, an opt-in `useQueryCheckpoint`, and the
readiness gate (which is on by default).

### `Service.QueryObjectStore.Postgres`

```haskell
module Service.QueryObjectStore.Postgres (
  PostgresQueryObjectStoreConfig (..),
) where

import Service.QueryObjectStore.Core (QueryObjectStoreConfig (..))

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

### `Service.Query.Checkpoint`

```haskell
module Service.Query.Checkpoint (
  Checkpoint (..),
  CheckpointStore (..),
  CheckpointStoreConfig (..),
  Error (..),
) where

import Service.Event.StreamPosition (StreamPosition)
import Text (Text)

data Checkpoint = Checkpoint
  { queryName :: Text,
    lastPosition :: StreamPosition,
    knownHash :: Text
  }
  deriving (Eq, Show)

data Error
  = StorageError Text
  | SerializationError Text
  deriving (Eq, Show)

data CheckpointStore = CheckpointStore
  { get :: Text -> Task Error (Maybe Checkpoint),
    set :: Checkpoint -> Task Error Unit,
    delete :: Text -> Task Error Unit,
    getAll :: Task Error (Array Checkpoint)
  }

class CheckpointStoreConfig config where
  createCheckpointStore :: config -> Task Text CheckpointStore
```

### `Service.Query.Subscriber` (new entry points)

```haskell
-- | Spawn the rebuild on an async task. Returns immediately.
-- Transports are free to bind before the inner Task completes.
rebuildAllAsync :: QuerySubscriber -> Task Text (AsyncTask Unit)

-- | Resume a single query from its checkpoint (or from 0 on hash mismatch).
-- Page size defaults to 1000 events.
rebuildFrom ::
  QuerySubscriber ->
  Text ->            -- queryName
  StreamPosition ->  -- start position (exclusive)
  Task Text Unit

-- | Inspect readiness. Used by /readyz and per-query endpoints.
readinessOf :: QuerySubscriber -> Text -> Task Text Readiness
overallReadiness :: QuerySubscriber -> Task Text Readiness
```

### Application builder

```haskell
app :: Application
app =
  Application.new
    |> Application.withEventStore postgresEventStoreConfig
    |> Application.useQueryObjectStore postgresQueryObjectStoreConfig
    |> Application.useQueryCheckpoint postgresCheckpointConfig
    |> Application.withQuery @UserOrders
    |> Application.withService userService
```

`useQueryCheckpoint` is opt-in. When absent, the framework falls back
to an `InMemory` checkpoint store — same boot semantics as today
(full replay on every restart) but the async path still applies, so
`/healthz` is constant-time regardless.

### Readiness endpoint contract

```text
GET /healthz   → 200 {"status":"alive"}        (always, while the process is up)
GET /readyz    → 200 {"status":"ready"}        (when overallReadiness == Ready)
               → 503 {"status":"rebuilding", "queries":[{name,lag}]}

GET /queries/{name}
               → 200 [...]                     (when readinessOf name == Ready)
               → 503 {"status":"rebuilding"}   with header X-Query-Status: rebuilding
```

`/health` (ADR-0025) keeps its current behaviour for backwards
compatibility; `/healthz` and `/readyz` are the new probe pair.

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

1. **HTTP readiness is constant-time on warm restart.** `/healthz`
   succeeds the moment the process binds; rebuild work is invisible to
   liveness probes.
2. **Rolling deploys stop flapping.** Load balancers only route to
   machines whose `subscriber.readiness == Ready`.
3. **Persistent state survives restarts.** A no-op deploy reads zero
   events from the EventStore for queries that were caught up before
   the restart.
4. **Per-query isolation.** Adding `OrderSummary` to a service with a
   caught-up `UserOrders` only replays the new query.
5. **Schema evolution is safe and explicit.** A `KnownHash` change
   forces a full replay for exactly that query, leaving the others
   untouched.
6. **Observable rebuild progress.** `events_replayed`,
   `lag_from_head`, `duration_seconds` are emitted per query — a slow
   rebuild becomes a measurement rather than a guess.
7. **Default API stays Jess-clean.** Apps that do not opt into
   Postgres or a custom checkpoint store keep working unchanged; the
   only visible difference is that `/healthz` returns immediately.
8. **Aligns with existing precedents.** Same trait + Postgres-impl
   pattern as ADR-0006 (snapshot cache) and ADR-0004 (EventStore);
   readiness contract mirrors ADR-0027 (pool health) and ADR-0025
   (health endpoint).

### Negative

1. **Two new tables.** `query_object_store` and `query_checkpoint`
   must be migrated into the user's Postgres schema. The
   `EventStore.Postgres` migration pattern is the precedent — the
   framework runs the migration on startup.
2. **Larger surface for `QueryObjectStore` implementations.** Future
   backends (Redis, DynamoDB) must provide transactional semantics
   spanning both the object write and the checkpoint write, or accept
   a documented at-least-once apply semantics for the seam between
   them.
3. **Per-query readiness adds one `ConcurrentVar` per registered
   query.** Negligible memory cost (a handful of bytes per query) but
   it is a new concurrency primitive on the hot path of every event
   delivery. The 50k req/s budget is not affected because reads only
   touch `ConcurrentVar.peek`.
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
| Transactional `INSERT … ON CONFLICT DO UPDATE` on `query_object_store` + `query_checkpoint` is a non-trivial Hasql `Statement`. Splitting writes risks data loss. | Single statement using a Hasql `Pipeline` or a `Session` wrapping both — same pattern as `EventStore.Postgres` already uses for insert + notify. Reviewed in phase 4 (security) and phase 5 (perf). |
| `KnownHash` mismatch path could be exploited to force expensive replays. | Hash is derived at compile time by `deriveQuery`; not user-controlled at runtime. Replay is per-query, not global, and progress is observable. |
| Async rebuild masks updater failures. | Mandatory structured-log + counter on updater failure; `/readyz` body lists per-query lag so a stuck query is visible. |
| Long catch-up windows on first deploy of a Postgres-backed store. | Acceptable — first deploy is the one time per service that O(eventCount) work is unavoidable. Progress logging makes it observable; the async path keeps `/healthz` honest throughout. |
| Hash mismatch + crash mid-rebuild could leave checkpoint stale. | Checkpoint is only ever updated transactionally with the object write — a crash mid-rebuild simply resumes from the last successfully-committed position. |

### Mitigations

- Migration script for the two new tables follows the existing
  `EventStore.Postgres` precedent (idempotent `CREATE TABLE IF NOT
  EXISTS`, runs on startup).
- Default `useQueryCheckpoint` falls back to in-memory, so the
  framework does not become harder to spin up locally.
- Hurl acceptance tests in `testbed/` add coverage for `/healthz`,
  `/readyz`, and the `X-Query-Status: rebuilding` header on a query
  endpoint hit during rebuild.
- ADR-0007 status section is updated to cross-reference this ADR.

## References

- [#650: Query rebuild blocks HTTP readiness on every restart](https://github.com/neohaskell/NeoHaskell/issues/650)
- [ADR-0007: Queries (Read Models)](0007-queries-read-models.md) — original CQRS read-model design; §"Application Startup Sequence" and §"Related Work" item 4.
- [ADR-0006: Entity Snapshot Cache](0006-entity-snapshot-cache.md) — precedent for the trait + persisted-position pattern.
- [ADR-0004: EventStore Abstraction](0004-eventstore-abstraction.md) — Postgres backend template.
- [ADR-0025: Auto Health Endpoint for WebTransport Apps](0025-auto-health-endpoint.md) — `/health` precedent; `/healthz` + `/readyz` extend it.
- [ADR-0027: PostgreSQL Connection Pool Health for Serverless Databases](0027-postgres-pool-health.md) — readiness-signal precedent; pool config to inherit.
- [core/service/Service/Query/Subscriber.hs](../../core/service/Service/Query/Subscriber.hs) — current `rebuildAll`.
- [core/service/Service/Application.hs](../../core/service/Service/Application.hs) (lines 1154–1161) — synchronous call site.
- [core/service/Service/QueryObjectStore/Core.hs](../../core/service/Service/QueryObjectStore/Core.hs) — trait that needs a Postgres impl.
- [core/service/Service/EventStore/Postgres/Internal.hs](../../core/service/Service/EventStore/Postgres/Internal.hs) — template for the new Postgres backend.
