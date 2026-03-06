# ADR-0039: Fix LISTEN/NOTIFY Connection Leak in Test Teardown

## Status

Accepted

## Context

[GitHub Issue #439](https://github.com/neohaskell/NeoHaskell/issues/439): `nhcore-test-service` fails on macOS CI with 25 out of 566 tests failing. The failure message is `FATAL: sorry, too many clients already` — PostgreSQL's connection limit is exhausted before the test suite finishes.

### Root Cause Chain

The leak is not a single bug but a chain of five compounding problems:

**1. Listener handle discarded at startup.**
`Notifications.connectTo` in `Notifications.hs:51` calls `AsyncTask.run |> discard`. The `AsyncTask` handle is thrown away immediately, so the listener thread can never be cancelled. There is no way to stop it.

**2. Each reconnect acquires two raw connections that are never released.**
The listener loop calls `Hasql.acquire` to obtain a `listenConnection` and a `queryConnection`. When the connection drops, the loop retries. Each retry acquires two new raw connections without releasing the previous pair. The old connections are abandoned in memory.

**3. Test teardown triggers an infinite reconnect storm.**
Tests call `dropPostgres` to drop the Events table at the end of each spec. Dropping the table causes the listener's underlying connection to crash. Because the handle was discarded (problem 1), the listener cannot be stopped. It immediately retries with exponential backoff, consuming two new connections per attempt (problem 2). The storm continues for the rest of the test run.

**4. No test teardown for EventStore instances.**
`Test/Spec.hs` has `afterAll` and `after` hooks commented out. Tests create `EventStore` instances but never clean them up. Even if the reconnect storm were fixed, the initial connections would still accumulate across the full suite.

**5. `initializeSubscriptions` leaks an `initialListenConnection`.**
`initializeSubscriptions` acquires a raw connection to set up database triggers. That connection is never released after setup completes.

### Related Issues (Out of Scope)

- [Issue #397](https://github.com/neohaskell/NeoHaskell/issues/397): PostgreSQL LISTEN keepalive and reconnection strategy. The reconnect algorithm itself is a separate concern addressed in ADR-0037.
- [Issue #405](https://github.com/neohaskell/NeoHaskell/issues/405): False log messages from the listener. Also a separate concern, not addressed here.

## Decision

Fix the leak by giving `EventStore` an explicit lifecycle and wiring cleanup through every layer that acquires a connection.

**1. `Notifications.connectTo` returns a cleanup action.**
Change the return type from `Task Text Unit` to `Task Text (Task Text Unit)`. The inner `Task Text Unit` is a cancellation action. Callers are now responsible for calling it when they're done.

**2. Add a `close` field to the `EventStore` record.**
```haskell
data EventStore = EventStore
  { ...
  , close :: Task Text Unit
  }
```
This gives every EventStore a standard lifecycle hook. The `SimpleEventStore` implementation provides a no-op. The Postgres implementation provides real cleanup.

**3. Wire cleanup through `Internal.hs`.**
- Capture the cleanup action returned by `connectTo`.
- Release `initialListenConnection` immediately after trigger setup completes, rather than holding it for the lifetime of the store.
- Expose the combined cleanup (cancel listener + release any remaining connections) via `EventStore.close`.

**4. Add `afterAll` and `after` hooks to `Test/Spec.hs`.**
Uncomment and implement the hooks so test specs can register teardown actions. This is a general improvement to the test framework, not specific to EventStore.

**5. Update Postgres test specs to call `store.close` before dropping tables.**
Each spec that calls `dropPostgres` must first call `store.close`. This stops the listener before the table disappears, preventing the reconnect storm entirely.

**6. Add a regression test.**
Add a test that creates an EventStore, calls `store.close`, then verifies the listener is no longer running. This prevents the leak from silently reappearing.

## Consequences

### Positive

1. **25 CI failures fixed.** The `FATAL: sorry, too many clients already` error disappears because connections are released in the correct order.

2. **No more connection exhaustion.** Each test run acquires and releases a bounded number of connections regardless of how many specs run.

3. **EventStore has explicit lifecycle management.** The `close` field makes resource ownership clear. Code that creates an EventStore is responsible for closing it, the same pattern used by file handles and database pools throughout the codebase.

4. **Test framework gains `afterAll`/`after` hooks.** Other test specs can now register teardown actions, which is useful beyond just this fix.

### Negative

1. **`EventStore` record gains a new required field.** Every place that constructs an `EventStore` literal must now supply `close`. The `SimpleEventStore` implementation uses a no-op (`Task.yield unit`), so the change is mechanical but touches multiple files.

2. **Production cleanup is out of scope.** `Application.hs` creates an EventStore at startup and runs until the process exits. Wiring `store.close` into the application shutdown sequence is a separate task and is not part of this fix.

## References

- [GitHub Issue #439](https://github.com/neohaskell/NeoHaskell/issues/439) — Connection exhaustion in macOS CI
- [GitHub Issue #397](https://github.com/neohaskell/NeoHaskell/issues/397) — Keepalive and reconnection (out of scope)
- [GitHub Issue #405](https://github.com/neohaskell/NeoHaskell/issues/405) — False log messages (out of scope)
- [ADR-0037](0037-postgres-listen-keepalive-reconnect.md) — PostgreSQL LISTEN Connection Keepalive and Reconnection
- [Notifications.hs](../../core/service/Service/EventStore/Postgres/Notifications.hs) — `connectTo`
- [Test/Spec.hs](../../core/testlib/Test/Spec.hs) — Commented-out `afterAll`/`after` hooks
