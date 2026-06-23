# ADR-0061: Replay from Last globalPosition on LISTEN/NOTIFY Listener Reconnect

> Issue: [#682 — WI-3: Replay from last globalPosition on LISTEN/NOTIFY listener reconnect](https://github.com/neohaskell/NeoHaskell/issues/682)

## Status

Proposed

## Context

### Current State

The Postgres EventStore drives **live** projections and outbound
integrations through a single supervised `LISTEN/NOTIFY` listener.
`Service.EventStore.Postgres.Notifications.connectTo`
(`core/service/Service/EventStore/Postgres/Notifications.hs` lines
25–76) opens a `(listenConnection, queryConnection)` pair, issues
`LISTEN "global"`, and blocks in `waitForNotifications`. Every `NOTIFY`
carries a lightweight payload; the handler reads the full event by its
`globalPosition` and pushes it into `SubscriptionStore.dispatch`, which
fans it out to every registered global / entity / stream subscription.

When the listen connection dies (Flexible Server maintenance restart,
failover, transient network drop), `listenerWithReconnect` (lines
32–64) catches the failure, sleeps an exponential backoff, re-acquires
the connection pair, and **re-issues `LISTEN`**. That is the whole
reconnect path — it performs **no catch-up read**.

`NOTIFY` is not durable. Postgres delivers a notification only to
sessions that are actively `LISTEN`ing at the moment the transaction
commits. Any event whose row was inserted (and whose trigger fired
`NOTIFY`) during the window between the connection dying and the new
`LISTEN` taking effect is **never delivered to the new session**. The
event is durably stored in the events table, so historical reads and a
fresh subscription's catch-up still see it — but **live dispatch
silently skips it**. Projections built from live dispatch drift; an
outbound integration that should have reacted never fires. Nothing logs
an error, because from the listener's point of view nothing went wrong.

The fix already exists for a different entry point. New subscriptions
created via `subscribeToAllEventsFromPositionImpl`
(`core/service/Service/EventStore/Postgres/Internal.hs` lines 667–728)
run a `catchUp` loop that reads every event forward from a starting
`globalPosition` and feeds it through the subscriber callback before
attaching the live subscription. The reconnect loop needs the same
catch-up, applied to the shared `SubscriptionStore` rather than to one
callback.

### Use Cases

- **Flexible Server maintenance restart** — Azure restarts the database
  for a patch. The listener's TCP connection drops, events keep being
  inserted by other app replicas (or queued commands draining after the
  restart), and the listener reconnects seconds later. Every event
  inserted during the outage must reach live projections.
- **Transient network drop / failover** — a brief partition kills the
  listen socket; the keepalive (ADR-0037) detects the dead socket and
  the loop reconnects. Catch-up closes the gap with no operator action.
- **Cold-path command drain after a stall** — a backlog of commands
  commits in a burst right as the listener is mid-reconnect; without
  catch-up the burst is partially or wholly lost from live dispatch.

### Design Goals

1. **No event silently skipped from live dispatch across a reconnect** —
   this is a correctness fix; the acceptance bar is zero skipped events.
2. **Reuse the proven catch-up read path**, not a second
   implementation — the forward-read-from-position loop already exists
   and is tested; the reconnect path routes through the same mechanism.
3. **Track the last-processed `globalPosition` across reconnects** so
   catch-up knows where the gap starts, without persisting anything new
   (the events table is already the durable source of truth).
4. **At-least-once is acceptable; at-most-once is not** — duplicate
   delivery of a boundary event is tolerable because event replay is
   idempotent by `globalPosition`; a dropped event is not tolerable.
5. **No new public API and no new vocabulary for Jess** — the listener
   is framework-internal; the fix must be invisible to application code.

### GitHub Issue

- [#682: WI-3 — Replay from last globalPosition on LISTEN/NOTIFY listener reconnect](https://github.com/neohaskell/NeoHaskell/issues/682)

## Decision

### 1. Insert a catch-up read into the reconnect loop, before live wait

`listenerWithReconnect` re-issues `LISTEN` and then immediately blocks
in `waitForNotifications`. We insert one step **between** them: after
`LISTEN` is established on the fresh connection but before the live
wait begins, replay every event from the last-processed
`globalPosition` (exclusive) up to the current max, dispatching each
through the same `SubscriptionStore` the live handler uses.

Ordering is load-bearing. `LISTEN` must be active **before** the
catch-up read takes its max-position snapshot, so an event committed
*during* the catch-up read is either picked up by the read or delivered
live by the now-active `LISTEN` (overlap, never gap). Issuing the
catch-up before `LISTEN` would reopen the exact window we are closing.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Catch-up before `LISTEN` | Rejected | Reopens the lost-NOTIFY window for events committed between the read snapshot and `LISTEN` taking effect. |
| Catch-up after `LISTEN`, before live wait | **Chosen** | `LISTEN` active first guarantees overlap (at-least-once), never a gap. |
| No catch-up; rely on periodic full re-subscribe | Rejected | Re-derives the entire log on every reconnect — `O(eventCount)` per drop; defeats the live-dispatch design. |
| Persist a checkpoint table for the listener | Rejected | The events table is already the durable cursor; a second persisted cursor adds a write path and a new failure mode (ADR-0059 owns persistent query cursors — this is a transient in-memory cursor). |

### 2. Track last-processed position in a Var owned by `connectTo`

The listener async task created by `connectTo` outlives every
individual reconnect — `listenerWithReconnect` recurses within the same
task. We add a `Var Int64` (last-processed `globalPosition`,
`StreamPosition`-typed) allocated **once** in `connectTo`'s scope,
alongside the existing `shutdownRef` and `currentConnectionsRef`. It is:

- **Initialised** to the current max `globalPosition` at listener start,
  so the first connection does not replay the entire history (a fresh
  listener inherits the same "subscribe from now" semantics it has
  today — historical replay is the job of `subscribeFromPosition`, not
  the live listener).
- **Advanced** by the live notification handler each time an event is
  dispatched, so it always reflects the high-water mark of what live
  dispatch has delivered.
- **Read** by the catch-up step on each reconnect to know where the gap
  begins, and **advanced** again as catch-up dispatches each event.

Because the Var lives in the async task's closure (not in a connection
or in the database), it survives every reconnect for the life of the
listener and is discarded only when the listener is cancelled.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| `Var Int64` in `connectTo` closure | **Chosen** | Survives reconnect, no persistence, matches the existing `Var`-based state in `connectTo`. |
| Re-query max position from DB on each reconnect as the cursor | Rejected | The DB max is *ahead* of what live dispatch delivered during the outage; using it as the start would skip exactly the gap events we must replay. |
| Persist the cursor in a Postgres row | Rejected | Adds a durable write path for a value the events table already implies; out of scope (correctness fix, not a new subsystem). |

### 3. Route catch-up through `SubscriptionStore.dispatch`, not a new subscription

`subscribeToAllEventsFromPositionImpl` feeds catch-up events to a
**single** callback because it is registering **one** subscription. The
listener is different: it is the shared delivery channel for **all**
live subscriptions. So the listener's catch-up reuses the *read* half
of the existing path — read events forward from a position via the
EventStore's forward-read — but sinks each event into
`SubscriptionStore.dispatch event.streamId event`, the exact sink the
live `processNotification` already uses (Notifications.hs line 115).

Concretely, the catch-up step calls the existing position-based read
(`readAllEventsForwardFrom` / `selectMaxGlobalPosition` in
`Internal.hs`) over the listener's `queryConnection`, iterates the
returned events in `globalPosition` order, and for each one runs the
same dispatch the live handler runs. This keeps one dispatch sink and
one read implementation; the reconnect path adds only the glue.

### 4. Idempotency and at-least-once semantics

Catch-up is **at-least-once**. A `NOTIFY` that arrived just before the
socket died might have been dispatched live *and* fall inside the
catch-up range, so a boundary event can be delivered twice. This is
safe:

- The `SubscriptionStore` already filters per subscription on
  `startingGlobalPosition` with `eventPos >= startPos`
  (`SubscriptionStore.dispatch`, lines 167–169), so a subscription that
  started after the duplicate's position ignores it.
- Event-sourced projection apply is **idempotent by `globalPosition`** —
  re-applying an already-seen event is a no-op for any correct read
  model. ADR-0059's checkpointed rebuild relies on the same property.
- The handler's existing per-callback error isolation
  (`wrapCallback` / `runAllIgnoringErrors`) means a duplicate that *does*
  upset one subscriber cannot stall the others.

We explicitly **reject** trying to make delivery exactly-once
(deduplication table, two-phase ack) — it would add a write path and
contention for a guarantee event sourcing does not need.

### 5. No new public API

The change is entirely inside `connectTo` (and a small internal helper
shared with the forward-read path). `EventStore`'s public surface,
`Application` wiring, and every subscription API are untouched. Jess
never sees this; existing apps gain the correctness fix on upgrade with
no code change.

### 5a. Interaction with full service restart (scope boundary)

This ADR (WI-3) scopes **only the in-process listener reconnect** — the
case where the `LISTEN` socket drops (Flexible Server maintenance,
failover, transient network blip) **while the app process stays alive**.
In that case the in-memory `lastProcessedRef` cursor described above is
exactly what closes the gap. The cases below are **out of scope**;
they are recorded only so a reader does not mistake the in-memory cursor
for a cross-restart durability mechanism.

- **Full service restart is out of scope and is already handled
  elsewhere.** On a full restart (process crash, redeploy, ACA
  scale-to-zero), the in-memory `lastProcessedRef` cursor is lost and a
  fresh listener initialises **"from now"** at the current max
  `globalPosition` (the same "subscribe from now" semantics the listener
  has today). The listener does **not** attempt to replay across a
  restart — and it does not need to. **Read-model projections recover via
  the persistent `QueryObjectStore` checkpoints plus the startup rebuild
  from [ADR-0059 (#650)](0059-async-query-rebuild-with-persistent-checkpoints.md)**:
  on boot, each query rebuilds forward from its durable checkpoint, so
  projection correctness across a restart depends on that persisted
  cursor, **not** on this ADR's in-memory listener cursor. The two cursors
  are deliberately separate: ADR-0059 owns the durable, cross-restart
  query cursor; this ADR owns the transient, in-memory live-dispatch
  cursor that only has to survive a reconnect *within* one process.

- **Outbound-integration subscribers across a restart are a separate,
  out-of-scope concern.** Unlike read-model projections, outbound
  integration subscribers do **not** today have a persistent
  per-subscriber checkpoint, so an event committed while the process is
  down is not automatically re-delivered to them on the next boot.
  Closing that gap is explicitly **not** part of WI-3 — it is tracked
  separately under
  [#571](https://github.com/neohaskell/NeoHaskell/issues/571), and it
  interacts with the scale-to-zero graceful-shutdown cluster
  ([#252](https://github.com/neohaskell/NeoHaskell/issues/252) /
  [#653](https://github.com/neohaskell/NeoHaskell/issues/653) /
  [#661](https://github.com/neohaskell/NeoHaskell/issues/661) /
  [#662](https://github.com/neohaskell/NeoHaskell/issues/662)). This ADR
  neither solves nor regresses that case; it only closes the
  same-process reconnect gap.

### 6. Type Definitions

No new public types. The internal cursor is the existing position type:

```haskell
-- Internal to connectTo's closure (illustrative, not a new export):
-- last-processed global position, shared across reconnects
lastProcessedRef :: Var Int64
```

### 7. Public API

No additions or changes to the public API. `connectTo`'s signature is
unchanged:

```haskell
connectTo ::
  Task Text (Hasql.Connection, Hasql.Connection) ->
  SubscriptionStore ->
  Task Text (Task Text Unit)
```

The catch-up is an internal helper invoked inside the reconnect loop,
reusing the existing forward-read from `Internal.hs`.

## Consequences

### Positive

- **The silent lost-NOTIFY gap is closed** — events inserted while the
  listener is down reach live projections and integrations after
  reconnect. This is the highest-value correctness item in epic #679.
- **One read implementation, one dispatch sink** — the reconnect path
  reuses the tested forward-read and the existing `SubscriptionStore`
  fan-out; no parallel catch-up logic to keep in sync.
- **No persistence, no migration, no new dependency** — the events
  table is the only durable cursor; the in-memory `Var` is the only new
  state, scoped to the listener's lifetime.
- **Invisible to applications** — no new vocabulary, no config knob, no
  API change; Jess gets the fix for free on upgrade.

### Negative

- **At-least-once on the reconnect boundary** — a boundary event may be
  dispatched twice. Acceptable because projection apply is idempotent by
  `globalPosition`, but it is a behavioural note worth recording.
- **Catch-up cost is `O(events-in-the-gap)`** — a very long outage means
  a larger catch-up read on reconnect. Bounded by the actual gap, not by
  total log size, so it is proportional to downtime and self-limiting.

### Risks

- **Cursor under-advance on a partial dispatch** — if the live handler
  advances `lastProcessedRef` before a callback failure is fully
  absorbed, a later catch-up might not re-cover that event. Mitigated by
  advancing the cursor on successful read/dispatch and leaning on
  at-least-once: under-advance causes a re-deliver (safe), never a skip.
- **Catch-up read itself fails mid-reconnect** — if the forward-read
  errors, the reconnect attempt fails and the supervised loop retries
  with backoff, so the gap is retried rather than lost.
- **Ordering regression** — if a future refactor moves the catch-up
  before `LISTEN`, the gap reopens silently. Mitigated by the regression
  test (below) and a comment marking the ordering as load-bearing.

### Mitigations

- **Regression test (acceptance criterion):** kill the listen
  connection mid-stream, insert events during the outage, force a
  reconnect, and assert **no events are skipped** from live dispatch
  after reconnect. Lives in
  `core/test/Service/EventStore/Postgres/NotificationsSpec.hs`
  (Postgres-gated, self-skips when `POSTGRES_AVAILABLE` is unset).
- **Empirical validation:** verify against an induced Flexible Server
  maintenance restart on the direct `5432` endpoint (insert during the
  outage, confirm catch-up recovers it) — per epic #679's validation
  checklist.
- **Load-bearing-ordering comment** at the catch-up call site so a
  future reader does not reorder `LISTEN` and catch-up.

## References

- [#682: WI-3 — Replay from last globalPosition on listener reconnect](https://github.com/neohaskell/NeoHaskell/issues/682)
- [#679: Epic — Deploy-readiness on Azure Database for PostgreSQL Flexible Server](https://github.com/neohaskell/NeoHaskell/issues/679)
- [ADR-0037: LISTEN connection keepalives + supervised reconnect loop (#397)](0037-postgres-listen-keepalive-reconnect.md)
- [ADR-0039: Connection-leak lifecycle + EventStore.close (#439)](0039-fix-listen-notify-connection-leak.md)
- [ADR-0059: Async query rebuild with persistent checkpoints (#650)](0059-async-query-rebuild-with-persistent-checkpoints.md)
- [#401: Structured logging at silent-failure points (this closes one)](https://github.com/neohaskell/NeoHaskell/issues/401)
- [#399: Sibling silent-failure on the command path](https://github.com/neohaskell/NeoHaskell/issues/399)
- [#571: Outbound-integration subscriber recovery across restart (out of scope here)](https://github.com/neohaskell/NeoHaskell/issues/571)
- [#252: Scale-to-zero graceful shutdown](https://github.com/neohaskell/NeoHaskell/issues/252)
- [#653: Scale-to-zero shutdown cluster](https://github.com/neohaskell/NeoHaskell/issues/653)
- [#661: Scale-to-zero shutdown cluster](https://github.com/neohaskell/NeoHaskell/issues/661)
- [#662: Scale-to-zero shutdown cluster](https://github.com/neohaskell/NeoHaskell/issues/662)
- [core/service/Service/EventStore/Postgres/Notifications.hs](../../core/service/Service/EventStore/Postgres/Notifications.hs)
- [core/service/Service/EventStore/Postgres/Internal.hs](../../core/service/Service/EventStore/Postgres/Internal.hs)
- [core/service/Service/EventStore/Postgres/SubscriptionStore.hs](../../core/service/Service/EventStore/Postgres/SubscriptionStore.hs)
