# ADR-0059: Asynchronous Query Rebuild with Checkpoint Persistence

## Status

Proposed

## Context

### Current State

ADR-0007 introduced Queries (read models) with an `InMemoryQueryObjectStore`. The
`QuerySubscriber` performs a synchronous full rebuild on every application start
— it reads every event in the event store from position 0 and sequentially
feeds each one through every registered `QueryUpdater`. This rebuild runs on the
main startup task in `Application.run`, blocking transport binding until it
completes.

ADR-0007 §"Application Startup Sequence" explicitly flags this:

> **Design note**: Full rebuild on startup ensures consistency but may be slow
> for large event stores. Future work may add incremental rebuilds or checkpoint
> persistence.

and lists "Checkpoint persistence: Persist last-processed event position to
avoid full rebuilds" as Related Work item #4.

The problem compounds across deployment workflows: rolling deploys, blue/green
cutovers, fly.io machine restarts, kubectl rollouts, and crash recovery all pay
`O(eventCount)` on every restart before the HTTP server can bind. A service with
millions of events takes minutes to become ready, during which load balancers
see a dead port and either fail health checks or buffer client requests.

Additionally, the rebuild is `Θ(N·M)` where N is the number of registered
queries and M is the number of events — entirely serial — and the underlying
`EntityFetcher` may re-replay per-entity streams if the `QueryUpdater` is
implemented via `EntityFetcher.fetch`.

### Use Cases

- **Deploy with no state change.** Shipping a one-line code fix forces the new
  process to re-derive read-model state from the full event log, even though no
  events were added.

- **Crash recovery.** A process restart after a transient failure pays the full
  replay cost before serving traffic.

- **Adding a new query to an existing application.** A newly-registered query
  must replay the entire event history to build its initial state. Existing
  queries should not be penalised — they already have current state.

- **Schema change on one query.** Changing a query's serialization format
  invalidates its checkpoint. Only that query should replay; others continue
  from their stored positions.

- **Load balancer integration.** Kubernetes readiness probes, AWS ALB target
  group health checks, and fly.io health checks all need reliable signals. Today
  the HTTP port binds only after rebuild, so there is no way to signal "alive
  but not ready."

### Design Goals

1. **HTTP server binds before rebuild completes.** The main startup task must
   never block on query state derivation. Transports must bind immediately so
   liveness probes succeed.

2. **Cold restart is free when state exists.** If a persistent
   `QueryObjectStore` holds valid checkpointed state, no events should be
   replayed on restart.

3. **Per-query, not global.** Checkpoints, readiness, and rebuild progress are
   tracked per-query. A newly-added query with no checkpoint replays from 0;
   existing queries skip already-processed events.

4. **Schema-change safety.** When a query's serialization format changes
   (detected via `KnownHash`), the checkpoint is invalidated for that query
   only, and it replays from 0. Other queries are unaffected.

5. **Framework absorbs the complexity.** Jess writes
   `Application.withQueryObjectStore PostgresQueryObjectStoreConfig` and the
   framework handles checkpoints, async rebuild, readiness, and per-query
   routing. No new knobs, no new vocabulary.

6. **Observable.** Rebuild progress and lag must be surfaced through the
   existing `Log` module so operators can distinguish "slow rebuild" from
   "stuck process."

### GitHub Issue

- [#650: Query rebuild blocks HTTP readiness on every restart — no checkpoint,
  no persistent QueryObjectStore](https://github.com/neohaskell/NeoHaskell/issues/650)

## Decision

### 1. Persistent QueryObjectStore Backend (Postgres)

Add `Service.QueryObjectStore.Postgres` following the ADR-0006
`SnapshotCache.InMemory` → `SnapshotCache.Postgres` precedent.

The `QueryObjectStore` trait already exposes `atomicUpdate :: UUID -> (Maybe
query -> Maybe query) -> Task Error Unit`, which maps naturally onto a
transactional `INSERT … ON CONFLICT DO UPDATE` in Postgres.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Postgres `queryobjectstore` table with JSON column | **Chosen** | Mirrors ADR-0006 pattern; `atomicUpdate` is a natural fit for `INSERT … ON CONFLICT`; users who already run Postgres for the event store get persistence with no new infrastructure. |
| JSONL file per query | Rejected | Recovery semantics weaker than transactional Postgres; gains nothing for users who already run Postgres. |
| Redis / external KV | Rejected | Adds a new dependency; overkill for the MVP. |

The table schema:

```sql
CREATE TABLE queryobjectstore (
  query_name  TEXT NOT NULL,
  instance_id UUID NOT NULL,
  payload     JSONB NOT NULL,
  PRIMARY KEY (query_name, instance_id)
);
```

The config type:

```haskell
data PostgresQueryObjectStoreConfig = PostgresQueryObjectStoreConfig
  { connectionString :: Text
  }

instance QueryObjectStoreConfig PostgresQueryObjectStoreConfig where
  createQueryObjectStore ::
    (Json.FromJSON query, Json.ToJSON query) =>
    PostgresQueryObjectStoreConfig ->
    Task Text (QueryObjectStore query)
```

### 2. Per-Query Checkpoint Table

Introduce `Service.Query.Checkpoint` — a small table separate from the
QueryObjectStore so the subscriber can read all checkpoints in one query without
touching the payload JSON.

Schema:

```sql
CREATE TABLE query_checkpoint (
  query_name           TEXT NOT NULL PRIMARY KEY,
  last_global_position BIGINT NOT NULL
);
```

The Haskell module:

```haskell
module Service.Query.Checkpoint (
  CheckpointStore (..),
  new,
  getMinPosition,
  getPositions,
  setPosition,
) where

data CheckpointStore = CheckpointStore
  { getMinPosition :: Task Text (Maybe StreamPosition)
  , getPositions :: Task Text (Map Text StreamPosition)
  , setPosition :: Text -> StreamPosition -> Task Text Unit
  }
```

**Startup logic** (per-query, not global):

```haskell
checkpoints <- Checkpoint.getPositions store
let rebuildFrom queryName =
      case Map.lookup queryName checkpoints of
        Just pos -> StreamPosition (pos + 1)
        Nothing  -> StreamPosition 0
```

The subscriber iterates events once and routes each event to the subset of
queries whose checkpoint is behind that event's position. No updater runs on an
already-processed event.

### 3. Hash-Based Schema-Change Detection

The `Query` typeclass carries a `KnownHash "QueryName"` instance (generated by
`deriveQuery`). The checkpoint table gains an optional `schema_hash` column.
When the hash at startup differs from the stored hash, the checkpoint is
invalidated for that query:

```haskell
data CheckpointState
  = CheckpointValid StreamPosition
  | CheckpointStale      -- hash mismatch → replay from 0
  | CheckpointAbsent     -- first time this query runs

resolveCheckpoint ::
  KnownHash queryName =>
  CheckpointStore ->
  Task Text CheckpointState
```

### 4. Chunked Rebuild Read

Replace `Limit 9223372036854775807` (Int64 max) with a chunked read loop:

```haskell
rebuildFrom :: QuerySubscriber -> StreamPosition -> Task Text Unit
rebuildFrom subscriber startPos =
  Stream.unfoldM startPos readNextChunk
    |> Stream.consume processChunk
  where
    chunkSize = Limit 1000
    readNextChunk pos = do
      events <- subscriber.eventStore.readAllEventsForwardFrom pos chunkSize
      case Array.length events of
        0 -> Task.yield Stream.Done
        _ -> Task.yield (Stream.Yield events (pos + Array.length events))
```

Three reasons:
- Bounds memory pressure on the EventStore driver and the Haskell heap.
- Natural backpressure for slow updaters.
- Enables progress reporting (`"X / Y events replayed, ETA …"`).

### 5. Async Rebuild off the Main Startup Task

`Subscriber.rebuildAll` becomes background work, gated on a readiness signal:

```haskell
-- In Application.runWithResolved, replace the synchronous rebuild block with:
case hasQueries of
  True -> do
    AsyncTask.run do
      Subscriber.rebuildAll subscriber
      ConcurrentVar.write True subscriber.ready
      Subscriber.start subscriber
    pass
  False -> pass
-- Continue to transport startup immediately
```

Two readiness contracts:

- `/health` (liveness, already exists per ADR-0025) returns 200 as soon as the
  process is up. Unchanged.
- `/ready` (new) returns 200 when `subscriber.ready` is `True`. Load balancers
  route traffic only after readiness flips.

Per-query readiness is the natural refinement: each query flips its own flag as
its checkpoint catches up to the head. Query endpoints
(`GET /queries/{name}`) return 503 with `X-Query-Status: rebuilding` until that
query's flag is set.

### 6. Readiness Endpoint

The WebTransport gains a readiness endpoint exposed via the existing health
check infrastructure:

```haskell
-- In the WebTransport module:
data ReadinessState
  = Ready
  | NotReady Text  -- reason, e.g. "query rebuild in progress"

-- The /ready endpoint consults subscriber.ready
```

This is configurable via the existing `HealthCheckConfig`, extended with:

```haskell
data HealthCheckConfig = HealthCheckConfig
  { healthPath :: Text        -- default: "health"
  , readinessPath :: Text     -- default: "ready"
  }
```

### 7. Observability

Three counters emitted through the existing `Log` module:

- `query.rebuild.events_replayed{queryName}` — monotonic counter, incremented
  per chunk.
- `query.rebuild.lag_from_head{queryName}` — gauge in events, derived from
  `maxPosition - checkpointPosition`.
- `query.rebuild.duration_seconds{queryName}` — logged on rebuild completion.

These are logged at `Info` level during rebuild and at `Notice` on completion.
No new telemetry library — `Log.info` / `Log.notice` with the existing
structured log format are sufficient.

### 8. Module Structure

```text
core/service/
  Service/
    Query/
      Checkpoint.hs         -- CheckpointStore trait + Postgres implementation
      Subscriber.hs         -- Modified: rebuildFrom, chunked read, readiness
    QueryObjectStore/
      Core.hs               -- Unchanged trait
      InMemory.hs           -- Unchanged
      Postgres.hs           -- NEW: PostgresQueryObjectStoreConfig + implementation
    Application.hs          -- Modified: async rebuild, readiness wiring
```

### 9. Public API

```haskell
-- The user-facing change is exactly one line:
app =
  Application.new
    |> Application.withEventStore postgresConfig
    |> Application.withTransport WebTransport.server
    |> Application.withService cartService
    |> Application.withQuery @CartSummary
    |> Application.withQueryObjectStore PostgresQueryObjectStoreConfig
        { connectionString = "…" }
```

Everything else — async rebuild, checkpointing, chunked reads, readiness
endpoints, per-query routing, observability — is absorbed by the framework. Jess
does not see any of it.

The existing `InMemoryQueryObjectStoreConfig` remains the default when
`withQueryObjectStore` is not called. Existing applications continue to work
unchanged, with the same synchronous rebuild behaviour.

## Consequences

### Positive

1. **HTTP binds immediately.** The main startup task never blocks on query
   state. Deployments that previously took minutes of dead-port time now take
   seconds.

2. **Cold restart is free.** When a persistent `QueryObjectStore` holds valid
   checkpointed state, zero events are replayed on restart. `rebuildFrom`
   queries the event store, finds the checkpoint catches up to the head, and
   returns immediately.

3. **Per-query independence.** Adding a new query, or changing one query's
   schema, does not force other queries to replay.

4. **Framework defaults for Jess.** `Application.withQueryObjectStore
   PostgresQueryObjectStoreConfig { connectionString = "…" }` is the entire API
   surface. Async rebuild, chunked reads, checkpointing, readiness endpoints,
   and per-query routing are all framework internals. Jess never sees
   `CheckpointStore`, `StreamPosition`, `rebuildFrom`, or `AsyncTask.run` — she
   gets autocomplete through to `PostgresQueryObjectStoreConfig` and ships.

5. **Observable without new tools.** Rebuild progress and lag are emitted
   through the existing `Log` module. Operators can `grep` structured logs; no
   new dashboard or metrics stack is required.

6. **Load balancer integration.** `/health` returns 200 immediately. `/ready`
   returns 200 after rebuild. Kubernetes, AWS ALB, and fly.io health checks work
   without custom scripts.

### Negative

1. **Schema migration burden.** The Postgres `QueryObjectStore` and
   `query_checkpoint` tables must be created. This follows the pattern
   established by the existing Postgres `EventStore` migrations but is a new
   surface for operational errors.

2. **Increased startup complexity.** The startup sequence gains an
   `AsyncTask.run` branch with a readiness state machine. Debugging startup
   issues now involves checking both the main task and the background rebuild
   task.

3. **Per-query routing during rebuild.** Routing each event to the subset of
   queries whose checkpoint is behind that event's position adds a
   `Map Text StreamPosition` lookup per event. This is `O(Q)` where Q is the
   number of registered queries (typically < 10) — negligible for the rebuild
   path, but a small constant-factor cost that did not exist before.

### Risks

1. **Postgres connection during rebuild.** The rebuild task holds a Postgres
   connection for the duration of the event replay. For very large event stores,
   this could be a long-lived connection; connection pool exhaustion is possible
   if the pool size is too small. Mitigation: the chunked read loop (1000 events
   per page) naturally yields between chunks, but the connection remains open.
   The Postgres pool should be sized with headroom for one rebuild connection.

2. **Readiness inversion.** If the rebuild task crashes silently (exception
   caught by `AsyncTask`), `subscriber.ready` is never set to `True` and
   `/ready` returns 503 indefinitely. Mitigation: the rebuild task logs the
   crash at `Error` level; the `/ready` endpoint includes the reason in the
   response body so operators can diagnose a stuck readiness state.

3. **Checkpoint drift.** If an event is deleted or truncated from the event
   store after a checkpoint is written, `rebuildFrom checkpoint + 1` may read
   past the truncation point. Mitigation: the `StreamPosition` arithmetic is
   bounded by the event store's current head; the chunked read loop detects
   end-of-stream and stops naturally.

4. **Hash false-positive.** If `KnownHash` produces a different hash for the
   same query type across compilations (e.g., due to a dependency version bump
   that changes derived instances), the checkpoint is invalidated unnecessarily.
   Mitigation: `KnownHash` is stable across compilations for a given type
   definition; a hash change means the type changed.

### Mitigations

- Connection pool sizing guidance added to the ADR-0059 docs.
- Rebuild crash logged at `Error` with the exception message.
- Chunked read loop bounds memory and connection duration.
- `KnownHash` stability is a property of the TH derivation and is tested in CI.

## Future Work

1. **Snapshot the QueryObjectStore periodically.** Periodic snapshots could
   reduce the rebuild window further by checkpointing not just the position but
   the full query state. Unnecessary for the MVP since the persistent
   `QueryObjectStore` already survives restarts.

2. **TTL on checkpoints.** A configurable TTL could force a full rebuild
   periodically (e.g., weekly) as a safety net against checkpoint drift or
   undetected corruption.

3. **Rebuild cancellation.** If the application shuts down during rebuild, the
   rebuild task should detect the shutdown signal and stop gracefully, writing
   its partial checkpoint so the next start can resume.

## References

- [#650: Query rebuild blocks HTTP readiness on every restart — no checkpoint, no persistent QueryObjectStore](https://github.com/neohaskell/NeoHaskell/issues/650)
- [ADR-0007: Queries (Read Models)](0007-queries-read-models.md) — §"Application Startup Sequence" and §"Related Work" item 4
- [ADR-0006: Entity Snapshot Cache](0006-entity-snapshot-cache.md) — direct precedent for the `Snapshot { state, position }` + cache-aware fetcher pattern; also the `InMemory` → `Postgres` trait pattern
- [#250](https://github.com/neohaskell/NeoHaskell/issues/250) — at-least-once delivery via `IntegrationStore` position tracking. Same shape (persisted cursor, resume on restart).
- [core/service/Service/Query/Subscriber.hs](../../core/service/Service/Query/Subscriber.hs) — current `rebuildAll`
- [core/service/Service/Application.hs](../../core/service/Service/Application.hs) — current synchronous call site
- [core/service/Service/QueryObjectStore/Core.hs](../../core/service/Service/QueryObjectStore/Core.hs) — trait that needs a Postgres impl
- [ADR-0025: Auto Health Endpoint](0025-auto-health-endpoint.md) — existing `/health` endpoint that this ADR extends with `/ready`
