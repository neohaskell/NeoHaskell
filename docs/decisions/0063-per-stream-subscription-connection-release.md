# ADR-0063: Release Per-Stream Subscription Connections on Unsubscribe

> Issue: [#683 — WI-4: Release per-stream subscription connections on unsubscribe](https://github.com/neohaskell/NeoHaskell/issues/683)

## Status

Proposed

## Context

### Current State

`subscribeToStreamEvents` is the only EventStore subscription that opens
a **dedicated, unpooled** Postgres connection. Unlike the global and
entity subscriptions — which dispatch off the shared listener pair —
each stream subscription issues its own `LISTEN <stream>` on a private
connection so that stream-scoped `NOTIFY` events reach exactly the
subscribers for that stream.

`subscribeToStreamEventsImpl`
(`core/service/Service/EventStore/Postgres/Internal.hs`) acquires that
connection and never hands ownership of it to anything that can release
it:

```haskell
subscribeToStreamEventsImpl ops cfg store entityName streamId callback =
  ops |> withConnectionAndError cfg \conn -> do
    -- A dedicated, unpooled connection is acquired here…
    connection <-
      Hasql.acquire (toConnectionSettings cfg)
        |> Task.fromIOEither
        |> Task.mapError (\err -> SubscriptionError (SubscriptionId "stream") (err |> toText))
    Notifications.subscribeToStream connection streamId
      |> Task.mapError StorageFailure
    -- … then the SubscriptionId is returned, but nothing remembers `connection`.
    currentMaxPosition <- ...
    subscriptionId <-
      store
        |> SubscriptionStore.addStreamSubscriptionFromPosition entityName streamId currentMaxPosition callback
        |> Task.mapError (\err -> SubscriptionError (SubscriptionId "stream") (err |> toText))
    Task.yield subscriptionId
```

`unsubscribeImpl` only removes the in-memory subscription entry — it has
no reference to the connection, so the connection is **leaked**:

```haskell
unsubscribeImpl store id =
  store
    |> SubscriptionStore.removeSubscription id
    |> Task.mapError (\err -> SubscriptionError id (err |> toText))
```

The `connection` value goes out of scope the moment
`subscribeToStreamEventsImpl` returns. It is never stored, never paired
with its `SubscriptionId`, and `removeSubscription` only deletes the
`SubscriptionInfo` from the `streamSubscriptions` map. The underlying
libpq connection stays open against Postgres until the whole process
exits.

### The gap

Under stream-subscription **churn** — clients that subscribe to a
short-lived stream, consume it, and unsubscribe — every cycle leaks one
connection. The deploy target for the per-client baseline (epic #679)
is **Azure Database for PostgreSQL — Flexible Server, Burstable
`B1ms`**, which allows **50 `max_connections`, ≈35 usable**. ADR-0060
established the steady-state pooled+fixed budget as **`19 + N`**, where
`N` is the number of active stream subscriptions; that arithmetic only
holds if `N` is bounded by *active* subscriptions. A leak makes `N`
grow monotonically with *cumulative* subscribe calls, so a deployment
that opens and closes a few dozen stream subscriptions exhausts the
ceiling and surfaces `FATAL: remaining connection slots are reserved` —
a production outage, not a degraded mode.

This is the same class of defect ADR-0039 (#439) fixed for the listener
and init-listen connections: a raw connection acquired without a release
path. ADR-0039 gave the `EventStore` an explicit `close` lifecycle and
wired cleanup through every layer that acquires a connection. The
per-stream subscription connection was not covered by that work because
it is acquired lazily, per subscribe call, after the store is already
running — its natural release point is **unsubscribe**, not store
`close`.

### Use Cases

- **Stream-subscription churn on `B1ms`** — a client subscribes to a
  per-order or per-session stream, drains it, and unsubscribes. Over a
  day this is thousands of subscribe/unsubscribe cycles. The connection
  budget must return to baseline after each cycle, not climb.
- **Bounded concurrent subscriptions** — an operator needs to know,
  from the documentation, how many stream subscriptions can be active
  at once on a given tier before the per-stream connections crowd out
  the pools and admin headroom from ADR-0060's budget.
- **Test confidence** — a regression test must be able to assert that
  N subscribe/unsubscribe cycles leave zero residual per-stream
  connections, so the leak cannot silently return.

### Design Goals

1. **Release on unsubscribe** — the dedicated connection a stream
   subscription opens is released exactly when that subscription is
   removed, with no leak across subscribe/unsubscribe cycles.
2. **One owner, keyed by `SubscriptionId`** — the connection's lifetime
   is tied to the same `SubscriptionId` the caller already holds, so
   there is no second handle for the caller to track and no way to
   release the wrong connection.
3. **Mirror the ADR-0039 lifecycle** — reuse the established
   "acquire returns a cleanup `Task`, run it via `Task.finally`"
   pattern rather than inventing a new ownership mechanism, so the
   per-stream path looks like the listener path it sits next to.
4. **Release is best-effort and never blocks unsubscribe** — a failed
   connection release (already-dead socket, network drop) must be
   logged, not thrown back to the caller of `unsubscribe`. Unsubscribe
   semantics are "this subscription is gone", which must hold even if
   the connection was already gone.
5. **Bounded and documented concurrency cap** — the maximum concurrent
   stream subscriptions per tier is written down against ADR-0060's
   `19 + N ≤ 35` budget, so `N` is a known, enforced ceiling rather
   than an accident.
6. **Invisible to Jess** — no new public surface. Jess calls
   `subscribeToStreamEvents` and `unsubscribe` exactly as before; the
   connection lifecycle is internal to the Postgres EventStore.

### GitHub Issue

- [#683: WI-4 — Release per-stream subscription connections on unsubscribe](https://github.com/neohaskell/NeoHaskell/issues/683)
- Parent epic: [#679 — Deploy-readiness on Azure Database for PostgreSQL Flexible Server](https://github.com/neohaskell/NeoHaskell/issues/679)

## Decision drivers

- **The leak is a hard deploy blocker, not a slow degradation.** On
  `B1ms` the per-stream connections are the *only* unbounded term in
  ADR-0060's budget. Left unreleased they are guaranteed to exhaust the
  35-usable ceiling under any real churn. Fixing the release path is the
  load-bearing change.
- **The `SubscriptionId` is already the unit of ownership.** The caller
  receives a `SubscriptionId` from subscribe and passes it back to
  `unsubscribe`. Tying the connection's lifetime to that same id means
  no new handle, no new API, and release-the-right-connection by
  construction.
- **ADR-0039 already solved this shape.** The listener and init-listen
  connections release via a cleanup `Task` run from `Task.finally`. The
  per-stream connection differs only in *when* it is released
  (unsubscribe, not store-close). Reusing the pattern keeps the codebase
  coherent and the reviewer's mental model intact.
- **Unsubscribe must stay total.** If releasing the connection could
  throw, an unsubscribe of a subscription whose connection already died
  would fail — leaving the entry removed but surfacing an error for a
  no-op cleanup. Release is therefore best-effort + logged.
- **The cap must be documented, not just possible.** ADR-0060 deferred
  the per-stream `N` bound to this work item. The budget is only
  trustworthy if the ceiling on `N` is written into the deploy guidance
  (WI-6, #685), not left implicit.

## Considered options

### Option 1 — Store a per-subscription cleanup `Task` in the `SubscriptionStore`, run it on remove (chosen)

Extend `SubscriptionInfo` with an `onRemove :: Task Text Unit` cleanup
action (a no-op for global and entity subscriptions, which open no
dedicated connection). `subscribeToStreamEventsImpl` builds the cleanup
that releases its connection and stores it alongside the subscription;
`removeSubscription` runs the stored cleanup (best-effort, logged) after
deleting the entry. The connection is owned by the same `SubscriptionId`
the caller holds.

- The cleanup is colocated with the subscription it belongs to, so the
  store is the single source of truth for "what does removing this
  subscription entail".
- Global/entity subscriptions pass a no-op, so the change is additive
  and their behaviour is unchanged.
- Mirrors ADR-0039's `connectTo`-returns-a-cleanup-`Task` pattern
  exactly, one level down (per subscription instead of per store).

### Option 2 — A separate `ConcurrentVar (Map SubscriptionId Hasql.Connection)` registry in `Internal.hs`

Keep a side table mapping each stream `SubscriptionId` to its raw
connection; `unsubscribeImpl` looks the id up, releases the connection,
and deletes the entry.

- Rejected: introduces a *second* structure keyed by `SubscriptionId`
  that must be kept in lockstep with the `SubscriptionStore`. Any path
  that removes a subscription without consulting the registry (or vice
  versa) re-introduces the leak or double-releases. The store already
  owns subscription lifetime; bolting a parallel map beside it splits
  the invariant across two modules. Also leaks the `Hasql.Connection`
  type out of `Notifications`/`Internal` into a new registry type.

### Option 3 — Release-only-at-`EventStore.close` (extend ADR-0039's `close`)

Track all per-stream connections and release them in bulk when the store
closes, like the listener pair.

- Rejected: does not fix the leak. The whole point of churn is that
  subscriptions come and go *while the store stays open*. Releasing only
  at `close` means a long-lived app still accumulates one connection per
  historical subscribe call until shutdown — exactly the failure #683
  describes. `close`-time release is necessary as a backstop (Option 1
  composes with it) but insufficient as the primary mechanism.

### Option 4 — Reuse the shared listener connection instead of a dedicated one

Multiplex stream `LISTEN` channels onto the existing listener pair so no
per-stream connection is ever opened.

- Rejected: a real architectural change to the notification layer, far
  beyond a leak fix, and it changes the dispatch semantics ADR-0037 and
  ADR-0039 established. It also re-opens the `LISTEN`-channel-scaling
  question (one connection can hold many `LISTEN`s, but channel
  bookkeeping, reconnect catch-up per channel, and back-pressure become
  non-trivial). Worth its own ADR if the per-stream model proves too
  heavy; out of scope for WI-4, whose remit is "release what we acquire".

| Option | Verdict | Reason |
|--------|---------|--------|
| 1. Cleanup `Task` in `SubscriptionStore`, run on remove | **Chosen** | Single owner keyed by `SubscriptionId`; additive; mirrors ADR-0039; no new public API. |
| 2. Side `Map SubscriptionId Connection` registry | Rejected | Splits the lifetime invariant across two structures; easy to desync. |
| 3. Release only at `close` | Rejected | Does not fix churn; accumulates until shutdown. |
| 4. Multiplex onto the listener pair | Rejected | Notification-layer redesign; out of scope for a leak fix. |

## Decision outcome

Adopt **Option 1**. The per-stream connection is owned by its
`SubscriptionId` through a cleanup `Task` carried in the
`SubscriptionStore`, run best-effort on remove. This mirrors ADR-0039's
lifecycle one level down: ADR-0039 made *the store* return a cleanup
`Task` run from `EventStore.close`; WI-4 makes *each stream
subscription* carry a cleanup `Task` run from `removeSubscription`.

### 1. `SubscriptionInfo` carries an `onRemove` cleanup action

`SubscriptionInfo` (`SubscriptionStore.hs`) gains an `onRemove` field.
Global, entity, and the existing stream-from-position constructors
default it to a no-op (`Task.yield unit`); only the dedicated-connection
stream path supplies a real release action.

```haskell
data SubscriptionInfo = SubscriptionInfo
  { callback :: SubscriptionCallback,
    startingGlobalPosition :: Maybe StreamPosition,
    entityNameFilter :: Maybe EntityName,
    -- Run once when this subscription is removed. Releases any dedicated
    -- resource the subscription owns (the per-stream LISTEN connection).
    -- No-op for subscriptions that own no dedicated resource.
    onRemove :: Task Text Unit
  }
```

A new constructor lets the stream path register both the subscription
and its cleanup atomically, so the connection is never tracked without
its release action:

```haskell
addStreamSubscriptionWithCleanup ::
  EntityName ->
  StreamId ->
  Maybe StreamPosition ->
  Task Text Unit ->
  SubscriptionCallback ->
  SubscriptionStore ->
  Task Error SubscriptionId
```

### 2. `removeSubscription` runs the stored cleanup, best-effort

`removeSubscription` reads the `SubscriptionInfo` (across global, stream,
and entity maps), deletes the entry, then runs its `onRemove`. A failed
release is logged and swallowed so unsubscribe stays total:

```haskell
removeSubscription subId store = do
  cleanup <- store |> takeOnRemoveFor subId
  -- delete from global / stream / entity maps (unchanged) …
  cleanup
    |> Task.recover \err -> do
         Log.warn [fmt|Subscription #{toText subId} cleanup failed: #{err}|]
           |> Task.ignoreError
         Task.yield unit
```

`takeOnRemoveFor` finds the subscription's `onRemove` before deletion
and returns `Task.yield unit` when the id is unknown, so removing an
already-removed or never-registered id is a no-op rather than an error.

### 3. `subscribeToStreamEventsImpl` registers the connection's release

The stream subscribe path builds the release action for the connection
it acquired and registers it with the subscription, so the connection
and its release land in the store together:

```haskell
subscribeToStreamEventsImpl ops cfg store entityName streamId callback =
  ops |> withConnectionAndError cfg \conn -> do
    connection <-
      Hasql.acquire (toConnectionSettings cfg)
        |> Task.fromIOEither
        |> Task.mapError (\err -> SubscriptionError (SubscriptionId "stream") (err |> toText))
    Notifications.subscribeToStream connection streamId
      |> Task.mapError StorageFailure

    currentMaxPosition <-
      Sessions.selectMaxGlobalPosition
        |> Sessions.run conn
        |> Task.mapError (toText .> StorageFailure)

    let releaseConnection = Hasql.release connection |> Task.fromIO

    subscriptionId <-
      store
        |> SubscriptionStore.addStreamSubscriptionWithCleanup
             entityName streamId currentMaxPosition releaseConnection callback
        |> Task.mapError (\err -> SubscriptionError (SubscriptionId "stream") (err |> toText))
    Log.debug [fmt|Subscription created: #{toText subscriptionId}|] |> Task.ignoreError
    Task.yield subscriptionId
```

`unsubscribeImpl` is unchanged — it already routes through
`SubscriptionStore.removeSubscription`, which now runs the cleanup. The
release path is therefore a single funnel: every unsubscribe (and every
`close`-time teardown that removes subscriptions) releases the
connection through the same code.

### 4. Concurrency cap documented against the ADR-0060 budget

ADR-0060 fixed the steady-state demand at `19 + N` connections of the 35
usable on `B1ms`, where `N` is the count of **active** stream
subscriptions. With the leak fixed, `N` is genuinely the active count.
The recommended ceiling, reserving headroom for an operator `psql`
session and a maintenance task (ADR-0060's "admin headroom"), is:

```text
  usable on B1ms              35
  fixed demand (ADR-0060)   − 19   (16 pooled + 3 unpooled)
  admin headroom            −  2   (operator psql + maintenance)
  ---------------------------------
  max concurrent stream subs  14   (recommended cap for B1ms)
```

This is **guidance, not a hard runtime limit** — nhcore does not reject
the 15th stream subscription (that would be a silent feature failure for
Jess). Instead the cap is documented in the deployment and
Postgres-event-store guides (WI-6, #685): a `B1ms` deployment should keep
concurrent stream subscriptions at or below ~14, and a tier upgrade
raises the budget (ADR-0060's `poolSize` fields plus the larger tier's
`max_connections`) and therefore the cap. Applications with many
concurrent streams should prefer entity or global subscriptions (which
share the listener pair and cost no per-stream connection) where the
stream-scoped filter is not required.

### 5. Module placement

No new modules. Changes are confined to:

```text
core/service/Service/EventStore/Postgres/SubscriptionStore.hs  -- onRemove field + addStreamSubscriptionWithCleanup + run cleanup in removeSubscription
core/service/Service/EventStore/Postgres/Internal.hs           -- register releaseConnection on subscribe
```

Documentation of the cap lands in WI-6's guide changes (#685), not in
nhcore source.

### Performance & testing

Tier: `moderate`. There is no new per-event work — `onRemove` is built
once at subscribe time and run once at unsubscribe time. Dispatch
(`SubscriptionStore.dispatch`) is unchanged; the new field is not read on
the hot path. The cleanup is a single `Hasql.release` per unsubscribe,
which the runtime was already obligated to do (it simply never did).

Verification (`core/test/Service/EventStore/Postgres/SubscriptionStoreSpec.hs`,
plus a Postgres-gated EventStore spec for the real connection path):

1. **No leak across cycles (regression, primary)** — a Postgres-gated
   spec subscribes to a stream and unsubscribes N times, asserting the
   Postgres connection count returns to the pre-subscription baseline
   (e.g. via `pg_stat_activity`), proving no per-stream connection
   survives unsubscribe. Self-skips when `POSTGRES_AVAILABLE` is unset.
2. **Cleanup runs exactly once on remove (unit)** — a pure
   `SubscriptionStore` spec registers a stream subscription with an
   `onRemove` that flips a `ConcurrentVar`, calls `removeSubscription`,
   and asserts the var flipped exactly once.
3. **Unsubscribe stays total when release fails (boundary)** — register
   an `onRemove` that throws; assert `removeSubscription` yields `unit`
   (does not propagate the error) and the entry is gone.
4. **Removing an unknown id is a no-op (boundary)** — `removeSubscription`
   on a never-registered or already-removed id yields `unit` and runs no
   cleanup.
5. **Global/entity subscriptions are unaffected** — existing
   `SubscriptionStore` specs continue to pass; their `onRemove` defaults
   to a no-op and `dispatch` behaviour is unchanged.

## Public API

**None changed.** `subscribeToStreamEvents`, `unsubscribe`, and the
`EventStore` record are unchanged from the caller's perspective. Jess
subscribes and unsubscribes exactly as before:

```haskell
-- Subscribe to a single stream; release happens automatically on unsubscribe.
subscriptionId <- store.subscribeToStreamEvents entityName streamId handleEvent

-- … later: unsubscribe releases the dedicated connection for us.
store.unsubscribe subscriptionId
```

The only new surface is **internal** to the Postgres EventStore:
`SubscriptionInfo.onRemove` and
`SubscriptionStore.addStreamSubscriptionWithCleanup`, neither of which is
re-exported beyond the Postgres backend. The `SubscriptionStore` module
is an internal implementation detail of the Postgres EventStore, so this
is not a breaking change for application code.

## Consequences

### Positive

1. **The connection leak is fixed.** Every stream subscribe/unsubscribe
   cycle returns to the connection baseline, so `N` in ADR-0060's
   `19 + N` budget is the *active* subscription count, as that ADR
   assumed.
2. **`B1ms` churn no longer exhausts connections.** A deployment can run
   sustained subscribe/unsubscribe churn without climbing toward the
   35-usable ceiling.
3. **One owner, one funnel.** The connection's lifetime is tied to its
   `SubscriptionId`, and all removal (unsubscribe and close-time
   teardown) routes through `removeSubscription`, so there is a single
   release path and no second handle to track.
4. **Consistent with ADR-0039.** The per-stream path now uses the same
   "cleanup `Task` run from a `finally`/remove" lifecycle as the
   listener pair, so the notification layer's resource discipline is
   uniform.
5. **The concurrency cap is documented and tied to the budget.** WI-6
   gets a concrete number (~14 on `B1ms`) derived from ADR-0060's math,
   closing the `N`-bound the budget ADR deferred here.
6. **Invisible to Jess.** No new public function, no new config knob, no
   new error to handle at the call site.

### Negative

1. **`SubscriptionInfo` gains a non-`Show`-able field.** A `Task` field
   cannot derive `Show`, so the existing `deriving (Show)` on
   `SubscriptionInfo` must change (drop `Show`, or add a hand-written
   instance that elides the action). This is a mechanical, internal
   change but it does touch the derived-instance list.
2. **Cleanup failures are swallowed.** A connection that fails to release
   is logged at `warn` and the unsubscribe proceeds. In the rare case of
   a release that fails for a reason *other* than an already-dead socket,
   the connection could linger until process exit — but this is strictly
   better than today (no release at all) and matches ADR-0039's
   best-effort posture for the listener pair.
3. **The cap is advisory, not enforced.** nhcore does not reject the
   15th stream subscription; an operator who ignores the documented cap
   can still over-subscribe. Enforcing it in code is deferred (see
   Risks) because a hard limit is a silent feature failure for Jess.

### Risks

| Risk | Mitigation |
|------|------------|
| A future subscription path opens a dedicated resource but forgets to register an `onRemove`, re-introducing a leak. | `addStreamSubscriptionWithCleanup` makes registering the connection and its cleanup a single call, so the connection cannot be tracked without its release. The Postgres-gated no-leak regression test (Testing §1) fails the build if any stream path leaks. |
| `removeSubscription`'s cleanup throws and breaks unsubscribe. | Cleanup is run under `Task.recover` + `Log.warn`; the boundary test (Testing §3) asserts unsubscribe stays total when `onRemove` throws. |
| Double-remove releases the connection twice. | `takeOnRemoveFor` returns the cleanup and the delete happens once; a second remove of the same id finds no `onRemove` and yields `unit` (Testing §4). `Hasql.release` on an already-released connection is not invoked because the cleanup is taken, not left in place. |
| Operators exceed the advisory cap and exhaust `B1ms`. | The cap is documented in the deploy guide (WI-6, #685) with the budget math; ADR-0060's `acquisitionTimeout` surfaces pool starvation as a clean retryable error rather than a hang; a tier upgrade raises both the budget and the cap. |
| `SubscriptionInfo` losing `Show` breaks a debug/log call site. | The field is internal; any `Show`-dependent site is updated in the same change, and a hand-written instance eliding the `Task` is the fallback if a `Show` is genuinely needed. |

### Mitigations

- The no-leak regression test encodes the acceptance criterion in CI, so
  a future change that re-leaks fails the build rather than production.
- The single-funnel design (all removal through `removeSubscription`)
  means the close-time teardown from ADR-0039 also releases any
  still-active stream connections as a backstop, composing the two
  lifecycles rather than duplicating them.
- The concurrency cap and its derivation are mirrored into the
  deployment and Postgres-event-store guides by WI-6 (#685), so
  operators have the number without reading this ADR.

## References

- [#683: WI-4 — Release per-stream subscription connections on unsubscribe](https://github.com/neohaskell/NeoHaskell/issues/683)
- [#679: Epic — Deploy-readiness on Azure Flexible Server](https://github.com/neohaskell/NeoHaskell/issues/679)
- [#685: WI-6 — Deployment documentation](https://github.com/neohaskell/NeoHaskell/issues/685) — documents the per-tier stream-subscription cap derived here.
- [ADR-0039: Fix LISTEN/NOTIFY Connection Leak in Test Teardown](0039-fix-listen-notify-connection-leak.md) — the `close`/cleanup lifecycle precedent this ADR mirrors one level down.
- [ADR-0060: Explicit Postgres Connection-Pool Budget for Flexible Server B1ms](0060-postgres-pool-budget.md) — establishes the `19 + N ≤ 35` budget; this ADR bounds and documents the `N` term.
- [ADR-0037: PostgreSQL LISTEN Keepalive + Supervised Reconnect](0037-postgres-listen-keepalive-reconnect.md) — the per-stream `LISTEN` mechanism whose connection this ADR releases.
- [ADR-0061: Replay from Last globalPosition on LISTEN/NOTIFY Listener Reconnect](0061-listener-reconnect-catchup.md) — sibling correctness work item (WI-3) on the same notification layer.
- [core/service/Service/EventStore/Postgres/Internal.hs](../../core/service/Service/EventStore/Postgres/Internal.hs) — `subscribeToStreamEventsImpl` / `unsubscribeImpl`.
- [core/service/Service/EventStore/Postgres/SubscriptionStore.hs](../../core/service/Service/EventStore/Postgres/SubscriptionStore.hs) — `SubscriptionInfo` / `removeSubscription`.
- [core/service/Service/EventStore/Postgres/Notifications.hs](../../core/service/Service/EventStore/Postgres/Notifications.hs) — `subscribeToStream` (the per-stream `LISTEN`).
