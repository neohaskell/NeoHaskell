# ADR-0037: PostgreSQL LISTEN Connection Keepalive and Reconnection

## Status

Proposed

## Context

NeoHaskell's event-driven architecture depends on a dedicated PostgreSQL LISTEN connection to receive real-time notifications. When the database sends a `NOTIFY`, the listener dispatches events to query subscribers and outbound integrations. If this connection silently dies, the application appears healthy but nothing downstream happens.

### The Problem

The LISTEN connection has no TCP keepalive settings and no reconnection logic. In cloud environments, this connection dies routinely:

- **NAT gateway idle timeouts**: Cloud providers (AWS, GCP, Azure) drop idle TCP connections after 350-900 seconds
- **PgBouncer session mode**: Terminates connections during pool rebalancing
- **Load balancer health sweeps**: Some configurations reset long-lived idle connections
- **Serverless database suspend**: Neon, Supabase, and similar providers terminate all connections on suspend

When the connection drops, `waitForNotifications` throws a `FatalError`. The current code catches this via `Task.asResultSafe` (per ADR-0029), logs the crash, and exits the listener. No restart occurs. The application continues running, accepting commands and returning responses, but no events propagate to integrations or query subscribers.

### Root Cause

`Notifications.hs` lines 26-35 establish the LISTEN connection using a raw `Hasql.acquire` call with no keepalive parameters:

```haskell
connectTo ::
  Hasql.Connection ->
  Hasql.Connection ->
  SubscriptionStore ->
  Task Text Unit
connectTo listenConnection queryConnection store = do
  let channelToListen = HasqlNotifications.toPgIdentifier "global"
  HasqlNotifications.listen listenConnection channelToListen
    |> Task.fromIO
    |> discard
  ...
  let listener = do
        result <- Task.asResultSafe do
          (listenConnection
            |> HasqlNotifications.waitForNotifications (handler queryConnection store)
            |> Task.fromIO :: Task Text Unit)
        case result of
          Ok _ ->
            Log.critical "LISTEN/NOTIFY listener returned unexpectedly"
              |> Task.ignoreError
          Err err ->
            Log.critical [fmt|LISTEN/NOTIFY listener crashed: #{err}|]
              |> Task.ignoreError
  listener
    |> AsyncTask.run
    |> discard
```

Two problems compound here:

1. `toConnectionSettings` in `Internal.hs` (lines 108-118) builds connection params with only host, port, dbname, user, and password. No TCP keepalive params are passed to libpq, so the OS uses its default keepalive idle time (typically 2 hours on Linux), which is far longer than any cloud NAT timeout.

2. After catching the crash, the listener exits. There is no loop, no backoff, no reconnection. The `AsyncTask` finishes and is never restarted.

### Relationship to Existing ADRs

| ADR | Relationship |
|-----|-------------|
| ADR-0027 | Added pool health timeouts for pooled connections. This ADR covers the non-pooled LISTEN connection, which has a different lifecycle. |
| ADR-0029 | Added `Task.asResultSafe` to catch IO exceptions in worker loops. This ADR builds on that foundation to add restart logic after the catch. |

### GitHub Issue

[#397: LISTEN connection dies silently in cloud environments](https://github.com/neohaskell/NeoHaskell/issues/397)

## Decision

We make two targeted changes: add TCP keepalive parameters to the LISTEN connection settings, and wrap the listener in a supervised reconnection loop.

### 1. Add TCP Keepalive Parameters

Extend `toConnectionSettings` in `Internal.hs` to pass libpq keepalive parameters via `Param.other`:

```haskell
toConnectionSettings :: PostgresEventStore -> LinkedList Hasql.Setting
toConnectionSettings cfg = do
  let params =
        ConnectionSettingConnection.params
          [ Param.host cfg.host,
            Param.port (fromIntegral cfg.port),
            Param.dbname cfg.databaseName,
            Param.user cfg.user,
            Param.password cfg.password,
            Param.other "keepalives" "1",
            Param.other "keepalives_idle" "30",
            Param.other "keepalives_interval" "10",
            Param.other "keepalives_count" "5"
          ]
  [params |> ConnectionSetting.connection]
```

These values mean:

| Parameter | Value | Meaning |
|-----------|-------|---------|
| `keepalives` | `1` | Enable TCP keepalive on this connection |
| `keepalives_idle` | `30` | Send first keepalive probe after 30 seconds of idle |
| `keepalives_interval` | `10` | Resend probe every 10 seconds if no response |
| `keepalives_count` | `5` | Declare connection dead after 5 unanswered probes |

With these settings, a dead connection is detected within 80 seconds (30 + 5 * 10), well inside any cloud NAT timeout. The OS-level keepalive probes keep the NAT mapping alive during idle periods.

These parameters apply to all connections built from `toConnectionSettings`, including the LISTEN connection and the query connection used for event fetching.

### 2. Supervised Reconnection Loop

Modify `connectTo` to accept a connection factory and wrap the listener in an exponential backoff loop, following the `workerWithRestartLoop` pattern from `Service/Application/Integrations.hs` (lines 319-338):

```haskell
connectTo ::
  Task Text (Hasql.Connection, Hasql.Connection) ->
  SubscriptionStore ->
  Task Text Unit
connectTo acquireConnections store = do
  let loop delayMs = do
        result <- Task.asResultSafe do
          (listenConnection, queryConnection) <- acquireConnections
          let channelToListen = HasqlNotifications.toPgIdentifier "global"
          HasqlNotifications.listen listenConnection channelToListen
            |> Task.fromIO
            |> discard
          Log.info "LISTEN/NOTIFY listener started"
            |> Task.ignoreError
          listenConnection
            |> HasqlNotifications.waitForNotifications (handler queryConnection store)
            |> Task.fromIO
        case result of
          Ok _ -> do
            Log.critical "LISTEN/NOTIFY listener returned unexpectedly"
              |> Task.ignoreError
            loop 1000
          Err err -> do
            Log.critical [fmt|LISTEN/NOTIFY listener crashed: #{err}|]
              |> Task.ignoreError
            AsyncTask.sleep delayMs
            let nextDelay = min 60000 (delayMs * 2 + jitter delayMs)
            loop nextDelay
  loop 1000
    |> AsyncTask.run
    |> discard
```

Key design choices:

- **Both connections recreated together**: When the listener crashes, both `listenConnection` and `queryConnection` are discarded and fresh connections are acquired. This avoids a split-brain state where the query connection is healthy but the listen connection is dead (or vice versa).
- **LISTEN re-registered on new connection**: Each iteration of the loop calls `HasqlNotifications.listen` on the fresh connection. PostgreSQL LISTEN registrations are per-connection and do not survive reconnection.
- **Exponential backoff**: Initial delay is 1 second, doubling each attempt, capped at 60 seconds. A ±25% jitter term prevents thundering herd if multiple services restart simultaneously.
- **No delivery guarantees**: Notifications sent while the connection was down are not replayed. This is consistent with the existing architecture, which treats LISTEN/NOTIFY as a hint to fetch from the event store rather than a reliable delivery mechanism.

### Why These Changes?

- **Keepalive params**: The only way to prevent NAT timeouts from silently killing the connection. libpq supports these natively; no new dependencies needed.
- **Reconnection loop**: The connection will eventually die regardless of keepalives (database restarts, maintenance windows, network partitions). A supervised loop is the correct response to a transient failure.
- **Exponential backoff**: Prevents hammering a database that is temporarily unavailable (e.g., during a serverless cold start or a brief network partition).
- **Minimal scope**: No new dependencies, no changes to the public API surface, no changes to event delivery semantics.

## Consequences

### Positive

1. **Cloud resilience**: The LISTEN connection survives NAT timeouts, PgBouncer rebalancing, and load balancer resets. Event-driven functionality continues working in cloud deployments without manual intervention.

2. **Automatic recovery**: When the connection does die (database restart, maintenance window), the listener restarts automatically within seconds to minutes, depending on backoff state.

3. **No new dependencies**: TCP keepalive is a libpq feature exposed via `Param.other`. The reconnection loop uses existing `Task`, `AsyncTask`, and `Log` primitives.

4. **Follows established pattern**: The reconnection loop mirrors `workerWithRestartLoop` from ADR-0029, keeping the codebase consistent.

5. **Visible failures**: Each crash and each reconnection attempt is logged at `critical` level, making failures observable in production.

### Negative

1. **Slightly more complex code**: `connectTo` grows from a one-shot setup to a loop with backoff state. The connection factory parameter changes the call site in `Application.hs`.

2. **Keepalive overhead**: TCP keepalive probes add a small amount of network traffic on idle connections (one probe every 30 seconds). This is negligible in practice.

3. **Hardcoded keepalive values**: The keepalive parameters are not user-configurable. They are chosen to be conservative and correct for most cloud environments.

### Risks

| Risk | Mitigation |
|------|------------|
| Auth errors retried alongside transient errors | Both are retried with backoff. Auth errors will log repeatedly at `critical` level, making the misconfiguration visible. A future ADR can add error classification if needed. |
| Notifications missed during reconnection window | Consistent with existing behavior. LISTEN/NOTIFY is a hint mechanism; the event store is the source of truth. Query subscribers rebuild from the event store on startup. |
| Backoff delay too long for time-sensitive integrations | 60-second cap is the worst case after many consecutive failures. In practice, a single transient failure recovers in 1-2 seconds. |

## Alternatives Considered

### Alternative 1: Switch to `hasql-listen-notify`

**Approach**: Replace `hasql-notifications` with the `hasql-listen-notify` library, which provides a higher-level API with built-in reconnection.

**Rejected because**:
- Adds a new dependency to nhcore, increasing the build surface and maintenance burden.
- `hasql-listen-notify` has its own reconnection semantics that may not align with NeoHaskell's backoff conventions.
- The fix using `Param.other` and a supervised loop achieves the same result with zero new dependencies and full control over reconnection behavior.

### Alternative 2: Polling fallback

**Approach**: When the LISTEN connection dies, fall back to polling the event store on a fixed interval (e.g., every 5 seconds) until the connection is restored.

**Rejected as future work**:
- Polling adds continuous database load even when the system is healthy.
- The reconnection loop already provides recovery within seconds. Polling would only help if reconnection itself is broken, which is a different failure mode.
- If polling is needed for specific deployment scenarios, it can be added as a separate integration in a future ADR without changing this design.

### Alternative 3: User-configurable keepalive values

**Approach**: Expose `keepalives_idle`, `keepalives_interval`, and `keepalives_count` as fields on `PostgresEventStoreConfig`.

**Rejected as premature**:
- The chosen values (30s idle, 10s interval, 5 probes) are appropriate for all known cloud environments.
- Adding configuration fields increases the API surface and documentation burden.
- If a specific deployment requires different values, a follow-up ADR can add configuration at that point with concrete evidence of the need.
- This mirrors the rationale in ADR-0027, which also chose hardcoded pool timeout defaults over user-configurable values.

## References

- [GitHub Issue #397](https://github.com/neohaskell/NeoHaskell/issues/397) — Original bug report
- [ADR-0027: PostgreSQL Pool Health](0027-postgres-pool-health.md) — Pool health for pooled connections (complementary)
- [ADR-0029: Worker Crash Recovery](0029-worker-crash-recovery.md) — `Task.asResultSafe` and restart loop pattern
- [Notifications.hs](../../core/service/Service/EventStore/Postgres/Notifications.hs) — LISTEN connection implementation
- [Internal.hs](../../core/service/Service/EventStore/Postgres/Internal.hs) — `toConnectionSettings` function
