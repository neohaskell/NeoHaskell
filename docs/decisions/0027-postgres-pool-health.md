# ADR-0027: PostgreSQL Connection Pool Health for Serverless Databases

## Status

Proposed

## Context

NeoHaskell applications use PostgreSQL for both event storage (`EventStore.Postgres`) and file upload state tracking (`FileStateStore.Postgres`). Both implementations use `hasql-pool` for connection pooling to efficiently manage database connections.

### The Problem

When deploying NeoHaskell applications with serverless PostgreSQL providers (Neon, Supabase, PlanetScale), the connection pool returns stale connections after the database suspends due to inactivity, causing operations to fail with:

```text
PoolError (SessionUsageError (QueryError "BEGIN" [] 
  (ClientError (Just "no connection to the server\n"))))
```

**Serverless database behavior:**

1. Database auto-suspends after a configurable idle period (typically 5 minutes)
2. Existing TCP connections are terminated by the database's PgBouncer
3. `hasql-pool` still holds references to these dead connections
4. Next request gets a dead connection from the pool
5. The `BEGIN` transaction fails with "no connection to the server"

**Affected locations:**

Both connection pool configurations use only `staticConnectionSettings` with NO additional pool settings:

1. **EventStore** (`Service.EventStore.Postgres.Internal`) — `toConnectionPoolSettings` function (line 75-77)
2. **FileUpload state store** (`Service.FileUpload.FileStateStore.Postgres`) — `createPool` function (line 158-171)

```haskell
-- Current implementation (both locations)
[HasqlPoolConfig.staticConnectionSettings settings] 
  |> HasqlPoolConfig.settings
```

This results in:
- **`agingTimeout`** (connection max lifetime): Default 1 day — connections live far longer than serverless suspend period
- **`idlenessTimeout`** (connection max idle time): Default 10 minutes — not aggressive enough for serverless
- **`observationHandler`**: None — no visibility into connection lifecycle events

### Root Cause

Serverless databases suspend after idle periods shorter than the default connection lifetime. When suspended:

1. The database's PgBouncer terminates all TCP connections
2. `hasql-pool` retains references to these dead connections
3. The pool's default `agingTimeout` (1 day) means connections are never recycled
4. The pool's default `idlenessTimeout` (10 minutes) is longer than typical serverless suspend periods
5. No health checks or connection validation occurs before use

The pool returns a stale connection, and the first database operation fails.

### Requirements

The solution must:

1. **Prevent stale connections**: Recycle connections before serverless databases suspend
2. **Work with all providers**: Support both serverless and traditional PostgreSQL
3. **Maintain performance**: Avoid adding latency to every operation (no pre-use health checks)
4. **Enable observability**: Provide visibility into connection lifecycle for debugging
5. **Minimal changes**: Leverage `hasql-pool`'s built-in timeout handling
6. **No new dependencies**: Use existing `hasql-pool` configuration API

### Relationship to Existing ADRs

| ADR | Relationship |
|-----|-------------|
| ADR-0004 | Defines the PostgreSQL EventStore patterns we follow |
| ADR-0012 | Defines the PostgreSQL FileStateStore implementation |

## Decision

Configure `hasql-pool` with appropriate timeout settings in both affected locations to ensure connections are recycled before serverless databases suspend.

### 1. Pool Configuration Changes

Add three settings to both `EventStore.Postgres.Internal.toConnectionPoolSettings` and `FileStateStore.Postgres.createPool`:

```haskell
-- Before (both locations)
[HasqlPoolConfig.staticConnectionSettings settings] 
  |> HasqlPoolConfig.settings

-- After (both locations)
[ HasqlPoolConfig.staticConnectionSettings settings
, HasqlPoolConfig.agingTimeout 300      -- 5 minutes max lifetime
, HasqlPoolConfig.idlenessTimeout 60    -- 1 minute max idle time
, HasqlPoolConfig.observationHandler logPoolObservation
] 
  |> HasqlPoolConfig.settings
```

### 2. Timeout Values

**`agingTimeout` (connection max lifetime): 300 seconds (5 minutes)**

- Ensures connections are recycled before serverless databases typically suspend
- Default was 1 day (86400 seconds)
- Rationale: Most serverless providers suspend after 5-15 minutes of inactivity; 5 minutes ensures connections are fresh

**`idlenessTimeout` (connection max idle time): 60 seconds (1 minute)**

- Aggressively releases idle connections so stale ones don't accumulate
- Default was 10 minutes (600 seconds)
- Rationale: Serverless databases charge for active connections; releasing idle connections quickly reduces cost and prevents stale connection buildup

**How `hasql-pool` handles timeouts:**

When a connection exceeds its aging or idleness timeout, `hasql-pool` automatically:
1. Terminates the connection
2. Removes it from the pool
3. Establishes a fresh connection on the next `use` call

No retry logic is needed at the application level—the pool handles connection recycling internally.

### 3. Observation Handler

Add logging for connection lifecycle events to aid debugging:

```haskell
logPoolObservation :: HasqlPool.Observation -> IO ()
logPoolObservation observation = case observation of
  HasqlPool.ConnectionObservation uuid status -> case status of
    HasqlPool.Connecting -> 
      logDebug [fmt|Pool: Connecting {uuid}|]
    HasqlPool.ReadyForUse -> 
      logDebug [fmt|Pool: Ready {uuid}|]
    HasqlPool.InUse -> 
      logDebug [fmt|Pool: In use {uuid}|]
    HasqlPool.Terminated reason -> case reason of
      HasqlPool.Aging -> 
        logInfo [fmt|Pool: Terminated {uuid} (aging timeout)|]
      HasqlPool.Idleness -> 
        logInfo [fmt|Pool: Terminated {uuid} (idleness timeout)|]
      HasqlPool.NetworkError err -> 
        logWarn [fmt|Pool: Terminated {uuid} (network error: {err})|]
      HasqlPool.Release -> 
        logDebug [fmt|Pool: Terminated {uuid} (released)|]
      HasqlPool.InitializationError err -> 
        logError [fmt|Pool: Terminated {uuid} (init error: {err})|]
```

This provides visibility into:
- When connections are recycled due to timeouts
- Network errors indicating database suspension
- Connection initialization failures

### 4. Available hasql-pool Settings (for reference)

The `hasql-pool` library provides these configuration options:

- `size :: Int -> Setting` — Pool size (default: 3)
- `acquisitionTimeout :: DiffTime -> Setting` — Acquisition timeout (default: 10s)
- `agingTimeout :: DiffTime -> Setting` — Max connection lifetime (default: 1 day)
- `idlenessTimeout :: DiffTime -> Setting` — Max idle time (default: 10 min)
- `observationHandler :: (Observation -> IO ()) -> Setting` — Monitoring callback
- `initSession :: Session.Session () -> Setting` — Session run on every new connection
- `staticConnectionSettings :: Connection.Settings.Settings -> Setting` — Connection string
- `dynamicConnectionSettings :: IO Connection.Settings.Settings -> Setting` — Dynamic connection settings

We use `agingTimeout`, `idlenessTimeout`, and `observationHandler` to address the stale connection issue.

### 5. Implementation Locations

**Location 1: EventStore.Postgres.Internal**

```haskell
-- core/service/Service/EventStore/Postgres/Internal.hs
toConnectionPoolSettings :: 
  PostgresEventStoreConfig -> 
  HasqlPoolConfig.Settings
toConnectionPoolSettings config =
  [ HasqlPoolConfig.staticConnectionSettings 
      (config.connectionSettings)
  , HasqlPoolConfig.agingTimeout 300
  , HasqlPoolConfig.idlenessTimeout 60
  , HasqlPoolConfig.observationHandler logPoolObservation
  ] 
    |> HasqlPoolConfig.settings
```

**Location 2: FileStateStore.Postgres**

```haskell
-- core/service/Service/FileUpload/FileStateStore/Postgres.hs
createPool :: 
  PostgresEventStoreConfig -> 
  Task Text HasqlPool.Pool
createPool config = do
  let poolSettings =
        [ HasqlPoolConfig.staticConnectionSettings 
            (config.connectionSettings)
        , HasqlPoolConfig.agingTimeout 300
        , HasqlPoolConfig.idlenessTimeout 60
        , HasqlPoolConfig.observationHandler logPoolObservation
        ] 
          |> HasqlPoolConfig.settings
  
  HasqlPool.acquire poolSettings
    |> Task.fromIO
    |> Task.mapError Text.fromString
```

### 6. Why This Works

**For serverless databases:**
- Connections are recycled every 5 minutes (before typical suspend periods)
- Idle connections are released after 1 minute (prevents stale connection accumulation)
- Observation handler logs network errors when database suspends

**For traditional PostgreSQL:**
- Shorter connection lifetimes have minimal impact (connection setup is fast)
- Idle connection cleanup reduces resource usage
- No behavior changes for active workloads

**No retry logic needed:**
- `hasql-pool` automatically establishes fresh connections when old ones are terminated
- The pool's `use` function handles connection acquisition transparently
- Application code remains unchanged

## Consequences

### Positive

1. **Stale connections eliminated**: Connections are automatically discarded before serverless databases suspend

2. **Works with all providers**: Configuration works for both serverless (Neon, Supabase) and traditional PostgreSQL

3. **No new dependencies**: Uses existing `hasql-pool` configuration API

4. **Observable pool health**: Logging provides visibility into connection lifecycle events

5. **Minimal code changes**: Two-line addition to existing pool configuration

6. **No application changes**: Existing code using `EventStore` and `FileStateStore` works unchanged

7. **Cost optimization**: Aggressive idle connection cleanup reduces serverless database costs

### Negative

1. **More frequent connection establishment**: Connections are recycled every 5 minutes instead of every 24 hours

2. **Slightly higher latency**: First request after idle period incurs connection setup overhead (~50-100ms)

3. **Hardcoded timeout values**: Timeout values are not user-configurable (can be added later if needed)

4. **Observation handler overhead**: Minimal overhead per connection event (logging only)

### Risks

| Risk | Mitigation |
|------|------------|
| Timeout values may need tuning for specific providers | Values are conservative (5 min aging, 1 min idle); can be made configurable in future ADR |
| Connection churn under high load | Pool size (default 3) limits concurrent connections; aging timeout only applies to idle connections |
| Observation handler logging overhead | Logging is debug/info level; can be disabled in production if needed |
| Connection setup latency | Acceptable trade-off for correctness; connection pooling still provides significant performance benefit |

## Alternatives Considered

### Alternative 1: Retry-on-stale wrapper

**Approach**: Catch `ClientError "no connection to the server"` and retry with a fresh connection.

```haskell
withRetryOnStale :: Session a -> Session a
withRetryOnStale session = do
  result <- try session
  case result of
    Left (ClientError (Just "no connection to the server")) -> 
      session  -- Retry once
    _ -> 
      result
```

**Rejected because**:
- Adds complexity to every database operation
- Masks the underlying issue (stale connections should not exist)
- `hasql-pool` already handles connection recycling—we just need to configure it
- Retry logic is error-prone (how many retries? what about other errors?)

### Alternative 2: Expose pool config in PostgresEventStore

**Approach**: Make timeout values user-configurable via `PostgresEventStoreConfig`.

```haskell
data PostgresEventStoreConfig = PostgresEventStoreConfig
  { connectionSettings :: Connection.Settings.Settings
  , poolSize :: Int
  , agingTimeout :: DiffTime
  , idlenessTimeout :: DiffTime
  }
```

**Rejected for now because**:
- Adds API surface area (more configuration to document and maintain)
- Most users don't need to tune these values
- Can be added in a future ADR if needed
- Hardcoded defaults work well for both serverless and traditional PostgreSQL

**Future consideration**: If users report issues with specific providers, we can add configuration options in a follow-up ADR.

### Alternative 3: Connection health check ping

**Approach**: Send `SELECT 1` before each session to validate connection health.

```haskell
healthCheck :: Session ()
healthCheck = statement () (Statement "SELECT 1" mempty mempty True)

withHealthCheck :: Session a -> Session a
withHealthCheck session = do
  healthCheck
  session
```

**Rejected because**:
- Adds latency to every database operation (extra round-trip)
- `hasql-pool` doesn't support pre-use health checks natively
- Would require wrapping every `Session` call
- Timeout-based recycling is more efficient (no per-operation overhead)

### Alternative 4: Use connection_status extension

**Approach**: Use PostgreSQL's `pg_stat_activity` to check connection status.

**Rejected because**:
- Requires additional database permissions
- Adds complexity (query parsing, status interpretation)
- Not supported by all PostgreSQL providers
- Timeout-based recycling is simpler and more portable

## Implementation Plan

### Phase 1: Core Implementation (Single PR)

- Update `EventStore.Postgres.Internal.toConnectionPoolSettings`
- Update `FileStateStore.Postgres.createPool`
- Add `logPoolObservation` helper function
- Update unit tests to verify pool configuration

### Phase 2: Documentation (Same PR)

- Update module documentation for both affected modules
- Add troubleshooting guide for serverless database issues
- Document timeout values and rationale

### Phase 3: Validation (Post-merge)

- Deploy to staging with serverless database (Neon or Supabase)
- Verify no stale connection errors after idle periods
- Monitor observation handler logs for connection lifecycle events

## References

- [ADR-0004: EventStore Abstraction](0004-eventstore-abstraction.md) — PostgreSQL patterns
- [ADR-0012: PostgreSQL FileStateStore](0012-postgres-file-state-store.md) — FileStateStore implementation
- [hasql-pool Configuration API](https://github.com/nikita-volkov/hasql-pool) — Pool settings documentation
- GitHub Issue #393 — Original bug report
