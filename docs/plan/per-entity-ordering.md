# Per-Entity Ordering for Integrations

**Parent Plan**: [integration-implementation.md](integration-implementation.md)
**Status**: Draft

## Problem

Currently, integrations process events via a global subscription that handles all events sequentially in one task. This has two issues:

1. **No parallelism**: Events for unrelated entities block each other
2. **No ordering guarantee**: If we add parallelism naively, events for the same entity could process out of order

## Goal

**Sequential per entity, parallel across entities:**

- Events for entity A process in order: A1 → A2 → A3
- Events for entity B process in order: B1 → B2 → B3
- A and B can process concurrently

## Architectural Decision: Global vs Per-Entity Subscriptions

**Question**: Should we subscribe per-entity at the EventStore level, or use a global subscription with application-level routing?

**Decision**: Global subscription + application-level routing.

**Rationale**:

1. **PostgreSQL NOTIFY doesn't guarantee ordering**. It's pub-sub, not a queue. Two rapid events could arrive out of order. Application-level queuing is needed regardless.

2. **Per-entity subscriptions don't scale**. Each would need a Postgres connection (~10MB). 10,000 entities = 100GB RAM just for connections.

3. **Separation of concerns**. EventStore handles persistence and notification. Per-entity ordering is an integration concern, not a storage concern.

4. **Existing infrastructure**. The EventStore's `subscribeToStreamEvents` calls LISTEN but the notification handler ignores the channel. Fixing this would be more work than building a dispatcher.

## Current State

```
Global Subscription
       |
       v
  [Event Stream] ─────────────────────────────────>
       |
       v
  processIntegrationEvent (sequential, blocks on each)
       |
       v
  For each OutboundRunner: processEvent → dispatch commands
```

All events flow through a single processing path. No concurrency.

## Target State

```
Global Subscription
       |
       v
  [Event Stream]
       |
       v
  Route by StreamId ─────────────────────────────>
       |            \            \            \
       v             v            v            v
  [Entity A Queue] [Entity B Queue] [Entity C Queue] ...
       |             |            |
       v             v            v
  Worker A        Worker B     Worker C
  (sequential)    (sequential) (sequential)
```

Each entity gets its own queue and worker. Workers process their queue sequentially. Multiple workers run concurrently.

## Implementation Approach

### Option A: Per-Entity Channels (Recommended)

Create a `Channel` per active entity. When an event arrives, route it to the appropriate channel. Spawn a worker per channel that processes events sequentially.

```haskell
data IntegrationDispatcher = IntegrationDispatcher
  { entityWorkers :: ConcurrentVar (Map StreamId EntityWorker)
  , outboundRunners :: Array OutboundRunner
  , commandEndpoints :: Map Text EndpointHandler
  }

data EntityWorker = EntityWorker
  { channel :: Channel (Event Json.Value)
  , workerTask :: AsyncTask Text Unit
  }
```

**Flow:**

1. Event arrives from global subscription
2. Look up or create `EntityWorker` for `event.streamId`
3. Push event to worker's channel
4. Worker processes events from channel sequentially
5. Workers idle-timeout and clean up

**Pros:**
- Natural fit for Haskell's Channel abstraction
- Clear separation of routing vs processing
- Idle cleanup prevents memory leak

**Cons:**
- Need to manage worker lifecycle
- Memory for inactive entities (mitigated by idle timeout)

### Option B: Actor-style Message Passing

Use a mailbox pattern where each entity is an "actor" that receives messages.

**Pros:**
- Well-understood pattern
- Good isolation

**Cons:**
- More complex to implement
- Overkill for our needs

### Option C: Striped Locking

Hash `StreamId` to N buckets. Each bucket has a lock. Events acquire their bucket's lock before processing.

**Pros:**
- Simple, bounded memory
- No worker management

**Cons:**
- Hash collisions cause unnecessary blocking
- Doesn't scale well with many entities

**Decision**: Option A (Per-Entity Channels)

## Detailed Design

### Phase 1: IntegrationDispatcher

Replace direct event processing with a dispatcher that routes to per-entity workers.

```haskell
-- New module: Service/Integration/Dispatcher.hs

data IntegrationDispatcher = IntegrationDispatcher
  { entityWorkers :: ConcurrentVar (Map StreamId EntityWorker)
  , outboundRunners :: Array OutboundRunner
  , commandEndpoints :: Map Text EndpointHandler
  , idleTimeoutMs :: Int
  }

data EntityWorker = EntityWorker
  { channel :: Channel (Event Json.Value)
  , workerTask :: AsyncTask Text Unit
  , lastActivityTime :: ConcurrentVar Int64
  }

-- Create dispatcher
new ::
  Array OutboundRunner ->
  Map Text EndpointHandler ->
  Task Text IntegrationDispatcher

-- Route event to appropriate worker
dispatch ::
  IntegrationDispatcher ->
  Event Json.Value ->
  Task Text Unit

-- Graceful shutdown
shutdown ::
  IntegrationDispatcher ->
  Task Text Unit
```

### Phase 2: Worker Lifecycle

Each worker:
1. Reads from its channel
2. Processes event through all OutboundRunners
3. Dispatches resulting commands
4. Updates last activity time
5. Loops

Worker cleanup:
1. Background reaper task checks `lastActivityTime`
2. Workers idle > timeout get cancelled
3. Channel gets closed
4. Entry removed from `entityWorkers` map

```haskell
-- Inside Dispatcher module

spawnWorker ::
  IntegrationDispatcher ->
  StreamId ->
  Task Text EntityWorker
spawnWorker dispatcher streamId = do
  channel <- Channel.new
  lastActivity <- ConcurrentVar.containing currentTimeMs

  let workerLoop = do
        event <- Channel.read channel
        processEventForEntity dispatcher streamId event
        lastActivity |> ConcurrentVar.set currentTimeMs
        workerLoop

  workerTask <- AsyncTask.run workerLoop
  Task.yield EntityWorker { channel, workerTask, lastActivityTime = lastActivity }

processEventForEntity ::
  IntegrationDispatcher ->
  StreamId ->
  Event Json.Value ->
  Task Text Unit
processEventForEntity dispatcher _streamId event = do
  -- Same logic as current processIntegrationEvent
  dispatcher.outboundRunners
    |> Task.forEach \runner -> do
        payloads <- runner.processEvent event
        payloads |> Task.forEach \payload ->
          dispatchCommand dispatcher.commandEndpoints payload
```

### Phase 3: Idle Worker Cleanup

Prevent memory leak from accumulating workers for inactive entities.

```haskell
-- Reaper runs in background
startReaper ::
  IntegrationDispatcher ->
  Task Text (AsyncTask Text Unit)
startReaper dispatcher = do
  let reaperLoop = do
        AsyncTask.sleep dispatcher.idleTimeoutMs
        currentTime <- getCurrentTimeMs
        workers <- dispatcher.entityWorkers |> ConcurrentVar.get

        workers |> Map.forEach \(streamId, worker) -> do
          lastActivity <- worker.lastActivityTime |> ConcurrentVar.get
          let idleTime = currentTime - lastActivity
          when (idleTime > dispatcher.idleTimeoutMs) do
            -- Cancel worker and remove from map
            AsyncTask.cancel worker.workerTask
            dispatcher.entityWorkers |> ConcurrentVar.modify (Map.delete streamId)

        reaperLoop

  AsyncTask.run reaperLoop
```

### Phase 4: Wire into Application

Modify `Application.hs` to use `IntegrationDispatcher` instead of direct subscription.

```haskell
-- In runWith, replace:
startIntegrationSubscriber eventStore app.outboundRunners combinedCommandEndpoints

-- With:
dispatcher <- Dispatcher.new app.outboundRunners combinedCommandEndpoints
startIntegrationSubscription eventStore dispatcher
```

## Future Improvement: Bounded Worker Pool

Option A creates one worker per active entity. This scales well for typical workloads but could become problematic with millions of concurrent entities. A bounded worker pool provides the same ordering guarantees with fixed memory.

### Concept: Striped Channels

Instead of `Map StreamId EntityWorker`, use a fixed array of N workers. Hash `StreamId` to determine which worker handles events for that entity.

```
Event arrives for entity X
        |
        v
  hash(X.streamId) mod N = slot 3
        |
        v
  Workers[3].channel.push(event)
        |
        v
  Worker 3 processes sequentially
```

**Key insight**: Same entity always hashes to same worker → ordering preserved.

### Design

```haskell
data BoundedDispatcher = BoundedDispatcher
  { workers :: Array WorkerSlot  -- Fixed size, e.g., 64
  , outboundRunners :: Array OutboundRunner
  , commandEndpoints :: Map Text EndpointHandler
  }

data WorkerSlot = WorkerSlot
  { channel :: Channel (Event Json.Value)
  , workerTask :: AsyncTask Text Unit
  }

-- Hash function for routing
routeToSlot :: StreamId -> Int -> Int
routeToSlot streamId workerCount =
  streamId |> toText |> Text.hash |> abs |> mod workerCount
```

### Trade-offs vs Option A

| Aspect | Option A (Per-Entity) | Bounded Pool |
|--------|----------------------|--------------|
| Memory | Grows with active entities | Fixed (N workers) |
| Parallelism | One worker per entity | At most N concurrent |
| Ordering | Perfect per-entity | Perfect per-entity |
| Hash collisions | None | Unrelated entities may share worker |
| Lifecycle | Spawn/cleanup workers | Workers live forever |
| Complexity | Higher (reaper, maps) | Lower (fixed array) |

### When to Use

- **Option A**: Default choice. Good for typical workloads (hundreds to thousands of concurrent entities).
- **Bounded Pool**: Better when:
  - Millions of concurrent entities expected
  - Memory constraints are tight
  - Simpler lifecycle management preferred
  - Some cross-entity blocking is acceptable

### Recommended Values

- **Worker count**: 64 or 128 (power of 2 for fast modulo)
- **Channel capacity**: Unbounded initially, add bounds if backpressure needed

### Migration Path

Start with Option A. If monitoring shows:
- Worker count consistently exceeds threshold (e.g., 10,000)
- Memory pressure from worker map
- GC pauses from worker churn

Then migrate to bounded pool. The external API (`dispatch`) stays the same; only internal routing changes.

## Testing Strategy

### Unit Tests

1. **Ordering within entity**: Send A1, A2, A3 → verify processed in order
2. **Parallelism across entities**: Send A1, B1, A2, B2 → verify A and B interleave
3. **Worker cleanup**: Send event, wait > idle timeout → verify worker removed

### Integration Tests

1. **Hurl test**: Create multiple carts concurrently, add items → verify no race conditions
2. **Load test**: Many entities with rapid events → verify no memory leak

## Migration Path

1. Add `IntegrationDispatcher` module (new code, no changes to existing)
2. Add tests for dispatcher
3. Update `Application.hs` to use dispatcher
4. Remove old `startIntegrationSubscriber` function
5. Update ADR status

## Open Questions

1. **Idle timeout value**: How long before cleaning up inactive workers? (Propose: 60 seconds)
2. **Max concurrent workers**: Start with no cap (Option A). If worker count becomes problematic, migrate to bounded pool (see Future Improvement section above).
3. **Backpressure**: What if a worker's channel fills up? (Propose: unbounded channel initially, add bounded later if needed)

## Files to Create/Modify

| File | Action |
|------|--------|
| `Service/Integration/Dispatcher.hs` | Create |
| `Service/Application.hs` | Modify (use dispatcher) |
| `test/Integration/DispatcherSpec.hs` | Create |

## Implementation Sequence

```
1. Create Dispatcher.hs with basic routing
2. Add spawnWorker and processEventForEntity
3. Add tests for ordering guarantee
4. Add idle worker cleanup (reaper)
5. Wire into Application.hs
6. Add integration tests
7. Remove old startIntegrationSubscriber
```
