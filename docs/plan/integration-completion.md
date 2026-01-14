# Integration Feature Completion Plan

**Parent Plan**: [integration-implementation.md](integration-implementation.md)  
**Related**: [per-entity-ordering.md](per-entity-ordering.md)  
**Status**: Draft

## Current State

### What's Implemented

| Component | Status | Location |
|-----------|--------|----------|
| `Integration.Outbound` types | ✅ Done | `Integration.hs` |
| `Integration.Inbound` types | ✅ Done | `Integration.hs` |
| `ToAction` typeclass | ✅ Done | `Integration.hs` |
| `Integration.Command.Emit` | ✅ Done | `Integration/Command.hs` |
| `Integration.Timer.every` | ✅ Done | `Integration/Timer.hs` |
| `IntegrationDispatcher` | ✅ Done | `Service/Integration/Dispatcher.hs` |
| Per-entity ordering | ✅ Done | `Dispatcher.hs` |
| Application wiring | ✅ Done | `Application.hs` |

### What's Missing

| Component | Priority | Reason |
|-----------|----------|--------|
| Idle worker cleanup (reaper) | High | Memory leak for long-running apps |
| Resource lifecycle hooks | High | Workers may hold DB connections, interpreters |
| Position tracking per entity | Medium | At-least-once delivery guarantee |
| Integration store | Medium | Resume from last position on restart |
| Error handling strategy | Medium | Currently silently dropped |
| Graceful shutdown | Medium | Workers blocked on channel don't wake |

## The Resource Lifecycle Problem

### Why It Matters

Integration workers aren't stateless event processors. They may hold:

```
Worker A (Postgres integration)
  └── DB connection pool (expensive to create, must be closed)

Worker B (Python interpreter)
  └── Embedded Python runtime (memory-heavy, needs cleanup)

Worker C (gRPC client)
  └── HTTP/2 connection + channel (kernel resources)

Worker D (WebSocket subscription)
  └── Long-lived connection to external system
```

If we kill idle workers without proper cleanup:
- Connection leaks
- Memory leaks (interpreters)
- Orphaned subscriptions
- Resource exhaustion over time

### Current Worker Structure (No Lifecycle)

```haskell
data EntityWorker = EntityWorker
  { channel :: Channel (Event Json.Value)
  , workerTask :: AsyncTask Text Unit
  }
```

Worker loop:
```haskell
workerLoop = do
  event <- Channel.read workerChannel  -- Blocks forever
  processEventForEntity dispatcher event
  workerLoop
```

**Problems**:
1. No way to initialize resources per-entity
2. No way to cleanup when idle
3. No way to restart with fresh resources

## Proposed Architecture

### Phase 1: Worker Lifecycle Hooks

Add lifecycle management to per-entity workers.

#### New Types

```haskell
-- | Configuration for an outbound integration with resource lifecycle.
data OutboundConfig state = OutboundConfig
  { -- | Initialize resources for this entity. Called once when worker spawns.
    initialize :: StreamId -> Task Text state
    
    -- | Process an event using the initialized state.
  , processEvent :: state -> Event Json.Value -> Task Text (Array Integration.CommandPayload)
    
    -- | Cleanup resources. Called when worker is reaped or shutdown.
  , cleanup :: state -> Task Text Unit
  }

-- | Type-erased outbound runner with lifecycle.
data OutboundRunner = OutboundRunner
  { entityTypeName :: Text
  , spawnWorkerState :: StreamId -> Task Text WorkerState
  }

-- | Internal state for a running worker.
data WorkerState = WorkerState
  { processEvent :: Event Json.Value -> Task Text (Array Integration.CommandPayload)
  , cleanup :: Task Text Unit
  }
```

#### How It Works

**1. Worker Spawn (with initialization)**
```haskell
spawnWorker dispatcher streamId = do
  workerChannel <- Channel.new
  lastActivity <- ConcurrentVar.containing currentTimeMs
  
  -- Initialize all runners for this entity
  runnerStates <- dispatcher.outboundRunners
    |> Task.mapArray (\runner -> runner.spawnWorkerState streamId)
  
  let workerLoop = do
        event <- Channel.read workerChannel
        lastActivity |> ConcurrentVar.set currentTimeMs
        runnerStates |> Task.forEach \state ->
          state.processEvent event
        workerLoop
  
  let cleanupAll = runnerStates |> Task.forEach (\state -> state.cleanup)
  
  workerTask <- AsyncTask.run workerLoop
  Task.yield EntityWorker 
    { channel = workerChannel
    , workerTask
    , lastActivityTime = lastActivity
    , cleanup = cleanupAll
    }
```

**2. Reaper (with cleanup)**
```haskell
reaperLoop = do
  AsyncTask.sleep checkInterval
  currentTime <- getCurrentTimeMs
  workers <- dispatcher.entityWorkers |> ConcurrentVar.get
  
  workers |> Map.forEach \(streamId, worker) -> do
    lastActivity <- worker.lastActivityTime |> ConcurrentVar.get
    let idleTime = currentTime - lastActivity
    when (idleTime > idleTimeoutMs) do
      -- Cleanup resources before killing
      worker.cleanup
      AsyncTask.cancel worker.workerTask
      dispatcher.entityWorkers |> ConcurrentVar.modify (Map.delete streamId)
  
  reaperLoop
```

**3. Next event for same entity → Re-initialize**

When an event arrives for an entity whose worker was reaped:
```haskell
dispatch dispatcher event = do
  let streamId = event.streamId
  worker <- getOrSpawnWorker dispatcher streamId  -- Re-runs initialize
  worker.channel |> Channel.write event
```

### Phase 2: Integration Developer API

**Nick's API for stateful integrations:**

```haskell
-- | Create an outbound integration with resource lifecycle.
--
-- Use this when your integration needs expensive resources like:
-- - Database connections
-- - HTTP client pools
-- - Embedded interpreters
-- - Long-lived subscriptions
--
-- @
-- postgresNotifier :: OutboundConfig PostgresConnection
-- postgresNotifier = OutboundConfig
--   { initialize = \streamId -> do
--       conn <- Postgres.connect connectionString
--       Task.yield conn
--   
--   , processEvent = \conn event -> do
--       Postgres.notify conn "events" (Json.encode event)
--       Task.yield []
--   
--   , cleanup = \conn -> do
--       Postgres.close conn
--   }
-- @
Integration.withResources :: OutboundConfig state -> OutboundRunner
```

**Stateless shortcut (current behavior):**

```haskell
-- | Create a stateless outbound integration.
--
-- For integrations that don't need persistent resources.
-- Each event is processed independently.
--
-- @
-- slackNotifier :: OutboundRunner
-- slackNotifier = Integration.stateless \event -> do
--   Http.post "https://slack.com/webhook" (formatEvent event)
--   Task.yield []
-- @
Integration.stateless :: 
  (Event Json.Value -> Task Text (Array CommandPayload)) -> 
  OutboundRunner
```

### Phase 3: Graceful Shutdown

**Problem**: Workers blocked on `Channel.read` don't notice shutdown.

**Solution**: Timeout-based reads with shutdown checks.

```haskell
workerLoop = do
  shouldShutdown <- dispatcher.shutdownSignal |> ConcurrentVar.peek
  if shouldShutdown
    then do
      worker.cleanup
      Task.yield unit
    else do
      -- Try to read with timeout
      maybeEvent <- Channel.readWithTimeout workerChannel pollIntervalMs
      case maybeEvent of
        Just event -> do
          lastActivity |> ConcurrentVar.set currentTimeMs
          processEvent event
          workerLoop
        Nothing -> 
          -- No event, loop and check shutdown again
          workerLoop
```

**Alternative**: Poison pill pattern
```haskell
data WorkerMessage
  = ProcessEvent (Event Json.Value)
  | Shutdown

shutdown dispatcher = do
  workers <- dispatcher.entityWorkers |> ConcurrentVar.get
  workers |> Map.forEach \(_, worker) ->
    worker.channel |> Channel.write Shutdown
```

### Phase 4: Position Tracking (At-Least-Once)

**Current**: No position tracking. If app crashes, events may be reprocessed or lost.

**Goal**: Track last processed position per entity, resume on restart.

```haskell
data IntegrationStore = IntegrationStore
  { getPosition :: StreamId -> Task Error (Maybe StreamPosition)
  , setPosition :: StreamId -> StreamPosition -> Task Error Unit
  }

-- Worker processing with position tracking
processEventWithTracking store event = do
  result <- processEvent event
  when (Array.isNotEmpty result) do
    store.setPosition event.streamId event.metadata.globalPosition
  Task.yield result
```

**Recovery on startup:**
```haskell
startDispatcher store = do
  -- For each active entity, read from last known position
  -- Events already processed won't be reprocessed
```

## Implementation Sequence

```
Phase 1: Worker Lifecycle (Breaking change to OutboundRunner)
├── 1.1 Add WorkerState type with cleanup
├── 1.2 Add lastActivityTime to EntityWorker  
├── 1.3 Add cleanup to EntityWorker
├── 1.4 Implement reaper task
├── 1.5 Update spawnWorker to call initialize
├── 1.6 Update Application.withOutbound for new API
└── 1.7 Tests for lifecycle (init/cleanup called correctly)

Phase 2: Developer API
├── 2.1 Add Integration.withResources function
├── 2.2 Add Integration.stateless helper
├── 2.3 Documentation with examples
└── 2.4 Migration guide for existing integrations

Phase 3: Graceful Shutdown  
├── 3.1 Add Channel.readWithTimeout (or poison pill)
├── 3.2 Update workerLoop to check shutdown
├── 3.3 Ensure cleanup runs on shutdown
└── 3.4 Tests for graceful termination

Phase 4: Position Tracking (Future)
├── 4.1 IntegrationStore interface
├── 4.2 In-memory implementation
├── 4.3 Postgres implementation
└── 4.4 Recovery on startup
```

## Developer Documentation

### For Integration Developers (Nick)

#### Stateless Integration (Simple Case)

For integrations that make HTTP calls or similar stateless operations:

```haskell
module MyApp.Integrations.Slack where

import Integration qualified

-- | Send Slack notifications on certain events.
-- No persistent resources needed - each event is independent.
slackNotifier :: Integration.OutboundRunner
slackNotifier = Integration.stateless \event -> do
  case Json.decode @MyEvent event.event of
    Err _ -> Task.yield []
    Ok decoded -> do
      Http.post webhookUrl (formatSlackMessage decoded)
        |> Task.mapError Integration.NetworkError
      Task.yield []
```

#### Stateful Integration (Resource Lifecycle)

For integrations that need expensive resources:

```haskell
module MyApp.Integrations.Postgres where

import Integration qualified

-- | Notify external Postgres on events.
-- Connection pool is created per entity and cleaned up when idle.
postgresNotifier :: Integration.OutboundConfig PostgresPool
postgresNotifier = Integration.OutboundConfig
  { initialize = \streamId -> do
      Console.print [fmt|[Postgres] Initializing pool for {streamId}|]
      pool <- Postgres.createPool connectionString poolSize
      Task.yield pool
      
  , processEvent = \pool event -> do
      Postgres.withConnection pool \conn -> do
        Postgres.execute conn 
          "INSERT INTO events (stream_id, data) VALUES (?, ?)"
          [event.streamId, event.event]
      Task.yield []
      
  , cleanup = \pool -> do
      Console.print [fmt|[Postgres] Closing pool|]
      Postgres.closePool pool
  }
```

#### Key Guidelines

1. **Make initialization idempotent** - Worker may be killed and restarted
2. **Don't cache entity state** - Events replay from where they left off
3. **Keep cleanup fast** - Reaper won't wait forever
4. **Log lifecycle events** - Helps debug production issues

### For Application Developers (Jess)

No changes needed! The `Application.withOutbound` API stays the same:

```haskell
app = Application.new
  |> Application.withService cartService
  |> Application.withOutbound @CartEntity @CartEvent cartIntegrations
```

Behind the scenes, workers now properly manage their resources.

## Configuration

New configuration options for the dispatcher:

```haskell
data DispatcherConfig = DispatcherConfig
  { -- | How often to check for idle workers (default: 10 seconds)
    reaperCheckInterval :: Int
    
    -- | Kill workers idle longer than this (default: 60 seconds)  
  , idleTimeoutMs :: Int
    
    -- | How long to wait for cleanup on shutdown (default: 5 seconds)
  , shutdownTimeoutMs :: Int
  }

defaultConfig :: DispatcherConfig
defaultConfig = DispatcherConfig
  { reaperCheckInterval = 10_000
  , idleTimeoutMs = 60_000
  , shutdownTimeoutMs = 5_000
  }
```

## Testing Strategy

### Unit Tests

1. **Lifecycle hooks called correctly**
   - `initialize` called when worker spawns
   - `cleanup` called when worker reaped
   - `cleanup` called on shutdown

2. **Reaper behavior**
   - Idle workers reaped after timeout
   - Active workers not reaped
   - Cleanup runs before removal

3. **Re-initialization**
   - Event for reaped entity spawns new worker
   - `initialize` runs again

### Integration Tests

1. **Resource cleanup under load**
   - Many entities → workers created
   - Traffic stops → workers reaped
   - Verify no resource leaks

2. **Graceful shutdown**
   - In-flight events complete
   - All workers cleaned up
   - App exits cleanly

## Open Questions

1. **Reaper implementation**: Background task vs lazy cleanup on dispatch?
   - Background: Predictable cleanup timing
   - Lazy: Simpler, but cleanup only happens when new events arrive

2. **Cleanup timeout**: What if cleanup hangs?
   - Option A: Hard timeout, cancel anyway
   - Option B: Log warning, let it finish
   - Recommendation: Hard timeout with logging

3. **Per-runner vs per-worker cleanup**: Should each OutboundRunner have independent lifecycle, or one lifecycle per entity worker?
   - Per-runner: More flexible, runners can have different timeouts
   - Per-worker: Simpler, all runners for an entity share lifecycle
   - Recommendation: Start with per-worker, add per-runner later if needed

## Files to Create/Modify

| File | Action |
|------|--------|
| `Service/Integration/Dispatcher.hs` | Major refactor - add lifecycle |
| `Service/Integration/Lifecycle.hs` | Create - lifecycle types |
| `Integration.hs` | Add `withResources`, `stateless` |
| `Service/Application.hs` | Update `withOutbound` signature |
| `Channel.hs` | Add `readWithTimeout` (optional) |
| `test/Integration/DispatcherSpec.hs` | Add lifecycle tests |
| `docs/integration-guide.md` | Create - developer documentation |

## Migration Guide

### For existing integrations

Current `OutboundRunner`:
```haskell
data OutboundRunner = OutboundRunner
  { entityTypeName :: Text
  , processEvent :: Event Json.Value -> Task Text (Array CommandPayload)
  }
```

Becomes (backward compatible via helper):
```haskell
-- Old code continues to work via Integration.stateless
myRunner :: OutboundRunner
myRunner = OutboundRunner
  { entityTypeName = "MyEntity"
  , processEvent = \event -> ...  -- Same as before
  }

-- Is equivalent to:
myRunner = Integration.stateless \event -> ...
```

No breaking changes for Jess (application developer).
Nick (integration developer) gets new `withResources` for stateful integrations.
