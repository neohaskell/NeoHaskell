# Integration Pattern: 50k req/s Hardening Plan

**Branch**: `feat/integration`  
**Status**: Draft  
**Created**: 2026-01-15  
**Related**: [integration-completion.md](integration-completion.md), [integration-bugfixes.md](integration-bugfixes.md)

## Executive Summary

The integration pattern implementation is architecturally sound but has critical race conditions and scalability bottlenecks that prevent production use at high throughput. This document details each issue with full context, root cause analysis, and recommended fixes.

**Target**: 50k events/second with correctness guarantees  
**Current State**: Functional but unsafe under concurrent load

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Critical: Worker Spawn Race Condition](#critical-worker-spawn-race-condition)
3. [Critical: MVar Contention Bottleneck](#critical-mvar-contention-bottleneck)
4. [High: Reaper Race Condition](#high-reaper-race-condition)
5. [High: Unbounded Channel Growth](#high-unbounded-channel-growth)
6. [High: No At-Least-Once Delivery](#high-no-at-least-once-delivery)
7. [High: No Circuit Breaker](#high-no-circuit-breaker)
8. [Medium: Graceful Shutdown](#medium-graceful-shutdown)
9. [Medium: Silent Error Handling](#medium-silent-error-handling)
10. [Low: Inbound Worker Backoff](#low-inbound-worker-backoff)
11. [Implementation Sequence](#implementation-sequence)

---

## Architecture Overview

Before diving into issues, understanding the architecture is essential.

### The Dispatcher Pattern

The `IntegrationDispatcher` routes events from a global subscription to per-entity workers:

```
Global Event Subscription
         |
         v
  IntegrationDispatcher
         |
    Route by StreamId
         |
    /----|----\
   v     v     v
Worker  Worker  Worker
 (A)     (B)     (C)
```

**Key Guarantee**: Events for the same entity (StreamId) are processed sequentially by a single worker. Different entities process in parallel.

### Worker Types

1. **Stateless Workers**: Simple event processing. No resource management.
2. **Lifecycle Workers**: Hold expensive resources (DB pools, interpreters). Have `initialize`, `processEvent`, and `cleanup` hooks.

### Concurrency Primitives

| Primitive | GHC Wrapper | Behavior |
|-----------|-------------|----------|
| `ConcurrentVar` | `MVar` | Mutex-protected variable. Blocks on contention. |
| `ConcurrentMap` | `STMMap.Map` | STM-based concurrent map. Fine-grained locking. |
| `Channel` | `Unagi.Chan` | Unbounded MPSC queue. Lock-free writes. |

### Current Data Structures

```haskell
data IntegrationDispatcher = IntegrationDispatcher
  { entityWorkers :: ConcurrentVar (Map StreamId EntityWorker)
  , lifecycleEntityWorkers :: ConcurrentVar (Map StreamId LifecycleEntityWorker)
  , outboundRunners :: Array OutboundRunner
  , lifecycleRunners :: Array OutboundLifecycleRunner
  , commandEndpoints :: Map Text EndpointHandler
  , shutdownSignal :: ConcurrentVar Bool
  , config :: DispatcherConfig
  , reaperTask :: ConcurrentVar (Maybe (AsyncTask Text Unit))
  }
```

The `entityWorkers` and `lifecycleEntityWorkers` fields are the source of most scalability issues.

---

## Critical: Worker Spawn Race Condition ✅ COMPLETED

**Status**: Fixed in commit (2026-01-15)
**Solution**: Implemented optimistic spawning pattern - pre-spawn worker outside lock, atomically check-and-insert using `ConcurrentVar.modifyReturning`. If race lost, discarded worker is cleaned up via `Stop` message.
**Tests**: Added concurrent dispatch tests verifying 100 simultaneous events are all processed without drops.

### Location

`core/service/Service/Integration/Dispatcher.hs`, function `dispatch`, lines 216-248

### The Problem

When two events for the same entity arrive simultaneously, both threads may spawn workers, causing one to be orphaned.

### Detailed Walkthrough

The current code follows a peek-check-spawn-modify pattern:

```haskell
dispatch dispatcher event = do
  let streamId = event.streamId
  
  workers <- dispatcher.entityWorkers |> ConcurrentVar.peek  -- Step 1: Read
  worker <- case Map.get streamId workers of
    Just existingWorker -> Task.yield existingWorker         -- Step 2: Check
    Nothing -> do
      newWorker <- spawnStatelessWorker dispatcher streamId  -- Step 3: Spawn
      dispatcher.entityWorkers 
        |> ConcurrentVar.modify (Map.set streamId newWorker) -- Step 4: Write
      Task.yield newWorker
  worker.channel |> Channel.write (ProcessEvent event)       -- Step 5: Dispatch
```

**The Race**:

| Time | Thread A (Event 1) | Thread B (Event 2) | Map State |
|------|-------------------|-------------------|-----------|
| T1 | `peek` -> sees empty | | `{}` |
| T2 | | `peek` -> sees empty | `{}` |
| T3 | `Map.get` -> Nothing | | `{}` |
| T4 | | `Map.get` -> Nothing | `{}` |
| T5 | `spawnStatelessWorker` -> W1 | | `{}` |
| T6 | | `spawnStatelessWorker` -> W2 | `{}` |
| T7 | `modify (Map.set)` | | `{streamId: W1}` |
| T8 | | `modify (Map.set)` | `{streamId: W2}` (W1 overwritten!) |
| T9 | `Channel.write` to W1 | | |
| T10 | | `Channel.write` to W2 | |

**Result**:
- Event 1 goes to W1 (orphaned worker, not in map)
- Event 2 goes to W2 (in map, receives future events)
- W1 processes Event 1 but is never reaped (memory leak)
- Future events skip Event 1's effects (ordering violation)

### Why This Matters at Scale

At low throughput, the window between peek and modify is tiny. At 50k req/s with bursty traffic (e.g., batch imports), multiple events for the same entity arriving within microseconds is common.

### Root Cause

The peek-check-spawn-modify sequence is **not atomic**. `ConcurrentVar.peek` releases the lock before `modify` reacquires it.

### Recommended Fix

Use `ConcurrentVar.modifyReturning` which holds the lock across the entire operation:

```haskell
dispatch dispatcher event = do
  let streamId = event.streamId
  
  worker <- dispatcher.entityWorkers |> ConcurrentVar.modifyReturning \workers ->
    case Map.get streamId workers of
      Just existingWorker -> 
        Task.yield (workers, existingWorker)  -- Return existing, map unchanged
      Nothing -> do
        newWorker <- spawnStatelessWorker dispatcher streamId
        Task.yield (workers |> Map.set streamId newWorker, newWorker)
  
  worker.channel |> Channel.write (ProcessEvent event)
```

This ensures only one worker is ever created per entity.

### Testing Strategy

1. Create a test that dispatches 100 events for the same entity simultaneously using `AsyncTask.runAll`
2. Verify exactly one worker exists in the map after all events dispatch
3. Verify all 100 events are processed (no drops)
4. Verify processing order matches dispatch order

---

## Critical: MVar Contention Bottleneck ✅ COMPLETED

**Status**: Fixed in commit (2026-01-15)
**Solution**: Replaced `ConcurrentVar (Map StreamId Worker)` with `ConcurrentMap StreamId Worker` using STM-based stm-containers. Added `ConcurrentMap.getOrInsert` for atomic get-or-create pattern. This eliminates the global MVar lock, allowing concurrent access to different entity keys.
**Performance**: Theoretical throughput increase from ~50k ops/s to ~500k+ ops/s for worker lookups.

### Location

`core/service/Service/Integration/Dispatcher.hs`, data type `IntegrationDispatcher`

### The Problem

Using `ConcurrentVar (Map StreamId Worker)` creates a global lock that serializes all worker lookups.

### How MVar Works

`ConcurrentVar` wraps GHC's `MVar`, which is a mutex-protected box:

```haskell
newtype ConcurrentVar value = ConcurrentVar (GHC.MVar value)

peek :: ConcurrentVar value -> Task _ value
peek (ConcurrentVar ref) = GHC.readMVar ref |> Task.fromIO

modify :: (value -> value) -> ConcurrentVar value -> Task _ Unit
modify transformer (ConcurrentVar ref) =
  GHC.modifyMVar_ ref (transformer .> pure) |> Task.fromIO
```

- `readMVar`: Takes lock, reads value, replaces value, releases lock
- `modifyMVar_`: Takes lock, applies function, replaces value, releases lock

Both operations are **mutually exclusive**. Only one thread can hold the lock at a time.

### The Bottleneck

Every `dispatch` call must:
1. Acquire the lock on `entityWorkers`
2. Read the entire Map
3. (Possibly) modify the Map
4. Release the lock

At 50k req/s, this means 50,000 lock acquisitions per second on a single MVar. GHC's MVar implementation uses OS-level futexes on Linux, which are efficient, but serialization is serialization.

**Theoretical maximum throughput**: ~100k-200k operations/second for an uncontended MVar. With actual Map operations and potential spawning, expect 10k-50k/second.

### GHC Runtime Implications

1. **Convoy Effect**: When many threads contend for an MVar, they form a queue. Even if processing is fast, the queue causes latency spikes.

2. **Fairness**: GHC's MVar is fair (FIFO wakeup), which prevents starvation but exacerbates convoy effects under load.

3. **Context Switching**: Each lock acquisition may involve a context switch if contended, adding ~1-10 microseconds per operation.

### Recommended Fix

Replace `ConcurrentVar (Map k v)` with `ConcurrentMap k v`:

```haskell
-- Before
entityWorkers :: ConcurrentVar (Map StreamId EntityWorker)

-- After
entityWorkers :: ConcurrentMap StreamId EntityWorker
```

`ConcurrentMap` uses `stm-containers` which provides:
- **Fine-grained locking**: Different keys can be accessed concurrently
- **Lock-free reads**: STM optimistic concurrency allows parallel reads
- **Composable transactions**: Multiple operations can be atomic

### Why ConcurrentMap is Better

| Operation | ConcurrentVar (MVar) | ConcurrentMap (STM) |
|-----------|---------------------|---------------------|
| Concurrent reads | Serialized | Parallel |
| Read-write conflict | Blocks | Retry (optimistic) |
| Multi-key atomic | Not possible | Composable |
| Throughput (ops/s) | ~50k | ~500k+ |

### Migration Complexity

The API is similar but not identical:

```haskell
-- ConcurrentVar pattern
workers <- ConcurrentVar.peek entityWorkers
case Map.get streamId workers of ...
ConcurrentVar.modify (Map.set streamId worker) entityWorkers

-- ConcurrentMap pattern
maybeWorker <- ConcurrentMap.get streamId entityWorkers
case maybeWorker of ...
ConcurrentMap.set streamId worker entityWorkers
```

The main difference is that `ConcurrentMap` operations are per-key, not whole-map.

### Note on Atomic Get-or-Create

`ConcurrentMap` doesn't have a built-in `getOrCreate`. You'll need to implement it using STM:

```haskell
getOrCreate :: 
  StreamId -> 
  Task Text EntityWorker ->  -- Creator
  ConcurrentMap StreamId EntityWorker -> 
  Task Text EntityWorker
```

This is slightly more complex but enables true atomicity without holding a global lock.

---

## High: Reaper Race Condition ✅ COMPLETED

**Status**: Fixed in commit (2026-01-15)
**Solution**: Implemented tombstone pattern with `WorkerStatus` (Active/Draining). Reaper marks worker as Draining before removing from map. Dispatcher checks status and spawns new worker if existing is Draining. Added `ConcurrentMap.getOrInsertIf` for conditional atomic replacement.

### Location

`core/service/Service/Integration/Dispatcher.hs`, function `startReaper`, lines 385-422

### The Problem

The reaper can remove a worker from the map while `dispatch` is simultaneously writing to that worker's channel, causing events to be lost.

### Detailed Walkthrough

Current reaper logic:

```haskell
reaperLoop = do
  AsyncTask.sleep dispatcher.config.reaperIntervalMs
  currentTime <- getCurrentTimeMs
  workers <- dispatcher.lifecycleEntityWorkers |> ConcurrentVar.peek
  
  let entries = Map.entries workers
  entries |> Task.forEach \(streamId, worker) -> do
    lastActivity <- worker.lastActivityTime |> ConcurrentVar.peek
    let idleTime = currentTime - lastActivity
    if idleTime > dispatcher.config.idleTimeoutMs
      then do
        worker.channel |> Channel.write Stop      -- Step 1: Send stop
        dispatcher.lifecycleEntityWorkers 
          |> ConcurrentVar.modify (Map.remove streamId)  -- Step 2: Remove from map
      else pass
  
  reaperLoop
```

**The Race**:

| Time | Reaper | Dispatcher (new event) |
|------|--------|----------------------|
| T1 | Checks entity A: idle > timeout | |
| T2 | Decides to reap A | Event for A arrives |
| T3 | | `peek` -> sees worker W (still in map) |
| T4 | Sends `Stop` to W's channel | |
| T5 | Removes W from map | |
| T6 | | Writes event to W's channel |
| T7 | W receives `Stop`, starts cleanup | |
| T8 | W receives event, but is shutting down | |

**Result**: The event may be processed during cleanup (undefined behavior) or dropped entirely.

### Why This is Tricky

The race has a narrow window, but it's deterministic under load:
- Reaper runs every 10 seconds (default)
- At 50k req/s, ~500,000 events during one reaper cycle
- Probability of race = (reap_operation_time / reaper_interval) * events_per_entity
- Even at 1ms reap time, expect ~50 races per cycle for active entities

### Root Cause

The reaper and dispatcher don't coordinate. The map removal and stop signal are not atomic with respect to dispatch.

### Recommended Fix: Tombstone Pattern

Instead of immediately removing from the map, mark the worker as "draining":

```haskell
data WorkerStatus = Active | Draining

data LifecycleEntityWorker = LifecycleEntityWorker
  { channel :: Channel (WorkerMessage (Event Json.Value))
  , workerTask :: AsyncTask Text Unit
  , lastActivityTime :: ConcurrentVar Int
  , workerStates :: Array WorkerState
  , status :: ConcurrentVar WorkerStatus  -- NEW
  }
```

**Reaper behavior**:
1. Set `status` to `Draining`
2. Send `Stop` to channel
3. Do NOT remove from map yet

**Dispatcher behavior**:
1. Check `status` before writing
2. If `Draining`, spawn new worker (old one is finishing)

**Worker behavior**:
1. On `Stop`, process remaining queue
2. Signal completion
3. Reaper removes from map on completion signal

### Alternative: Remove-Then-Stop Order

A simpler fix is to remove from map BEFORE sending Stop:

```haskell
-- Remove first, so new events spawn new worker
dispatcher.lifecycleEntityWorkers 
  |> ConcurrentVar.modify (Map.remove streamId)
-- Then signal old worker to stop
worker.channel |> Channel.write Stop
```

This ensures new events always go to a fresh worker. The old worker drains its queue and exits. Downside: brief period where both old and new workers process events for same entity (ordering violation within that window).

---

## High: Unbounded Channel Growth ✅ COMPLETED

**Status**: Fixed in commit (2026-01-15)
**Solution**: Added bounded channel support to `Channel.hs` using STM TBQueue. Updated `DispatcherConfig` with `workerChannelCapacity` and `channelWriteTimeoutMs` configuration. Both `spawnStatelessWorker` and `spawnLifecycleWorker` now use `Channel.newBounded` with configurable capacity (default: 100). Added `tryWriteWithTimeout` for future backpressure handling.

### Location

`core/concurrency/Channel.hs` - now supports both bounded and unbounded channels

### The Problem (Now Solved)

Under load, if event production exceeds processing rate, channels grow without limit until OOM.

### How Unagi-Chan Works

`unagi-chan` is a high-performance unbounded MPSC (multi-producer, single-consumer) queue:

```haskell
data Channel value = Channel
  { outChannel :: Unagi.OutChan value
  , inChannel :: Unagi.InChan value
  }
```

**Unbounded** means `writeChan` never blocks. If the consumer is slow, messages accumulate indefinitely.

### The Failure Mode

1. Integration makes slow HTTP call (100ms per event)
2. Events arrive at 1000/second for one entity
3. Worker processes 10/second, receives 1000/second
4. After 1 minute: 59,400 events queued
5. Each event is ~1KB JSON: 59MB per entity
6. 1000 entities: 59GB memory
7. OOM kill

### Why This Happens in Practice

Integrations often call external services:
- HTTP APIs with rate limits
- Databases under load
- Third-party services with latency spikes

A single slow service can cause memory exhaustion across all entities using that integration.

### Recommended Fix: Bounded Channels

Add a bounded channel variant:

```haskell
-- New function in Channel.hs
newBounded :: Int -> Task _ (Channel value)
newBounded capacity = do
  -- Use Control.Concurrent.STM.TBQueue or similar
  ...

-- Writing to bounded channel blocks when full
write :: value -> Channel value -> Task _ Unit
write value self = case self of
  BoundedChannel tbqueue -> 
    STM.atomically (STM.writeTBQueue tbqueue value)  -- Blocks when full
  UnboundedChannel unagi -> 
    Unagi.writeChan unagi value
```

### Backpressure Implications

Bounded channels create **backpressure**: when the channel is full, the producer blocks. This has cascading effects:

1. **Dispatcher blocks**: Can't dispatch more events to that entity
2. **Subscription blocks**: Event subscription callback blocks
3. **Database connection held**: Postgres LISTEN connection may timeout

This is actually **correct behavior** for an event-sourced system: if you can't process events, you shouldn't accept more. But it requires careful configuration:

- **Channel capacity**: Too small = frequent blocking. Too large = memory issues.
- **Timeout handling**: Dispatcher should timeout and log, not block forever.
- **Metrics**: Monitor channel depth to detect slow consumers before they cause problems.

### Capacity Sizing

Rule of thumb: `capacity = processing_rate * acceptable_latency`

- Processing rate: 10 events/second
- Acceptable latency: 5 seconds
- Capacity: 50 events

If latency exceeds 5 seconds, investigate the slow integration rather than queue more events.

---

## High: No At-Least-Once Delivery

### Location

Missing entirely - not implemented

### The Problem

If the application crashes, there's no record of which events were processed. On restart, events may be lost or reprocessed arbitrarily.

### Event Sourcing Delivery Guarantees

| Guarantee | Meaning | Requirement |
|-----------|---------|-------------|
| At-most-once | Event processed 0 or 1 times | Fire-and-forget (current state) |
| At-least-once | Event processed 1 or more times | Position tracking + idempotent handlers |
| Exactly-once | Event processed exactly 1 time | Position tracking + transactional handlers |

Most systems aim for **at-least-once** with idempotent handlers, which effectively gives exactly-once semantics.

### What Position Tracking Means

For each entity, track the last successfully processed event position:

```haskell
data IntegrationStore = IntegrationStore
  { getPosition :: StreamId -> Task Error (Maybe StreamPosition)
  , setPosition :: StreamId -> StreamPosition -> Task Error Unit
  }
```

**On event processing**:
1. Process event
2. If successful, update position
3. If failed, don't update (event will be retried)

**On restart**:
1. Read last position for each entity
2. Subscribe from that position
3. Events already processed are skipped

### Implementation Considerations

1. **Granularity**: Per-entity? Per-integration? Per-entity-per-integration?
   - Recommendation: Per-entity is simplest and covers most cases

2. **Storage**: In-memory (lost on restart) vs persistent (Postgres)?
   - Recommendation: Postgres for production, in-memory for tests

3. **Commit timing**: After each event? Batched?
   - Recommendation: After each event for correctness, batched for performance

4. **Idempotency**: Even with position tracking, handlers should be idempotent
   - External calls may succeed but position update may fail
   - On retry, the external call happens again

### The docs/plan/integration-completion.md Document

This document already outlines the `IntegrationStore` interface. It should be implemented as described there.

---

## High: No Circuit Breaker

### Location

Missing entirely - not implemented

### The Problem

If an external service is down, workers retry forever, exhausting resources and causing cascading failures.

### Failure Cascade Scenario

1. Stripe API is down (500 errors)
2. PaymentIntegration retries every request
3. Retry backoff fills channels
4. Memory grows, GC pauses increase
5. Event processing slows for ALL entities
6. Upstream event subscription backs up
7. Database connections timeout
8. Entire application becomes unresponsive

### Circuit Breaker Pattern

A circuit breaker tracks failure rate and stops calling a failing service:

```
     [Closed] ----failures exceed threshold----> [Open]
        ^                                           |
        |                                           |
        +-------success on probe----- [Half-Open] <-+
                                           ^        |
                                           |        |
                                           +--timeout
```

**States**:
- **Closed**: Normal operation, calls go through
- **Open**: Failing, calls immediately rejected
- **Half-Open**: Probing, one call allowed to test recovery

### Implementation Sketch

```haskell
data CircuitBreaker = CircuitBreaker
  { state :: ConcurrentVar CircuitState
  , failureCount :: ConcurrentVar Int
  , lastFailureTime :: ConcurrentVar Int
  , config :: CircuitBreakerConfig
  }

data CircuitBreakerConfig = CircuitBreakerConfig
  { failureThreshold :: Int      -- Open after N failures
  , resetTimeout :: Int          -- Try half-open after N ms
  , halfOpenMaxCalls :: Int      -- Calls allowed in half-open
  }

withCircuitBreaker :: CircuitBreaker -> Task err a -> Task err a
withCircuitBreaker cb action = do
  state <- cb.state |> ConcurrentVar.peek
  case state of
    Open -> Task.throw CircuitOpen
    HalfOpen -> do
      result <- action |> Task.asResult
      case result of
        Ok a -> do
          cb.state |> ConcurrentVar.set Closed
          cb.failureCount |> ConcurrentVar.set 0
          Task.yield a
        Err e -> do
          cb.state |> ConcurrentVar.set Open
          Task.throw e
    Closed -> do
      result <- action |> Task.asResult
      case result of
        Ok a -> Task.yield a
        Err e -> do
          count <- cb.failureCount |> ConcurrentVar.modify (+ 1)
          when (count >= cb.config.failureThreshold) do
            cb.state |> ConcurrentVar.set Open
          Task.throw e
```

### Where to Apply

Circuit breakers should wrap integration actions, not the entire worker:

```haskell
-- In ToAction instance
instance ToAction Email where
  toAction config = Integration.action do
    withCircuitBreaker sendgridCircuit do
      Http.post "https://api.sendgrid.com/..." 
        |> Http.send
```

This allows individual integrations to fail independently.

---

## Medium: Graceful Shutdown

### Location

`core/service/Service/Integration/Dispatcher.hs`, function `shutdown`, lines 425-445

### The Problem

Shutdown sends `Stop` to workers but doesn't wait for them to finish. In-flight events may be lost.

### Current Behavior

```haskell
shutdown dispatcher = do
  dispatcher.shutdownSignal |> ConcurrentVar.set True
  
  statelessWorkers <- dispatcher.entityWorkers |> ConcurrentVar.peek
  Map.entries statelessWorkers |> Task.forEach \(_streamId, worker) -> do
    worker.channel |> Channel.write Stop
  
  lifecycleWorkers <- dispatcher.lifecycleEntityWorkers |> ConcurrentVar.peek
  Map.entries lifecycleWorkers |> Task.forEach \(_streamId, worker) -> do
    worker.channel |> Channel.write Stop
  -- Returns immediately, workers still running
```

### The Problem

1. Stop messages are queued behind pending events
2. Workers may have 100s of events to process
3. Application exits before workers finish
4. Events in queue are lost
5. Position tracking (if implemented) shows events as unprocessed
6. On restart, events are reprocessed (duplicates in external systems)

### Recommended Fix: Drain with Timeout

```haskell
shutdown dispatcher timeoutMs = do
  -- 1. Stop accepting new events
  dispatcher.shutdownSignal |> ConcurrentVar.set True
  
  -- 2. Send stop to all workers
  allWorkers <- getAllWorkers dispatcher
  allWorkers |> Task.forEach \worker ->
    worker.channel |> Channel.write Stop
  
  -- 3. Wait for workers to finish (with timeout)
  let workerTasks = allWorkers |> Array.map (.workerTask)
  AsyncTask.waitAllWithTimeout workerTasks timeoutMs
  
  -- 4. Force-cancel any remaining workers
  workerTasks |> Task.forEach \task ->
    AsyncTask.cancel task |> Task.ignoreError
```

### Timeout Considerations

- **Too short**: Workers killed mid-processing, events lost
- **Too long**: Shutdown takes forever, deployment blocked
- **Recommendation**: 30 seconds for most applications, configurable

### Application-Level Coordination

The `Application.runWith` function should coordinate shutdown:

```haskell
-- In Application.hs
runWith eventStore app = do
  -- ... start everything ...
  
  -- Wait for signal (SIGTERM, SIGINT)
  waitForShutdownSignal
  
  -- Graceful shutdown sequence
  Console.print "[Application] Shutting down..."
  
  -- 1. Stop transports (stop accepting new requests)
  transports |> Task.forEach Transport.stop
  
  -- 2. Drain integration dispatcher
  Dispatcher.shutdown dispatcher 30000
  
  -- 3. Stop query subscribers
  Subscriber.stop subscriber
  
  -- 4. Close event store connections
  EventStore.close eventStore
```

---

## Medium: Silent Error Handling

### Location

Multiple locations in `Dispatcher.hs` and `Application/Integrations.hs`

### The Problem

Errors are logged to console but not surfaced for alerting or metrics.

### Examples

```haskell
-- Dispatcher.hs line 342
Err err -> do
  Console.print [fmt|[Dispatcher] Error processing event...|]
    |> Task.ignoreError

-- Application/Integrations.hs line 84
Err decodeErr -> do
  Console.print [fmt|[Integration] Failed to decode event...|]
    |> Task.ignoreError
  Task.yield Array.empty
```

### Why This Matters

In production:
1. Console logs may not be monitored
2. No metrics means no alerting
3. No error callbacks means no custom handling
4. Hard to distinguish "expected" errors from bugs

### Recommended Fix: Error Callbacks

Add error callback to dispatcher config:

```haskell
data DispatcherConfig = DispatcherConfig
  { idleTimeoutMs :: Int
  , reaperIntervalMs :: Int
  , enableReaper :: Bool
  , onError :: IntegrationError -> Event Json.Value -> Task Text Unit  -- NEW
  }

data IntegrationError
  = DecodeError Text
  | ProcessingError Text
  | DispatchError Text
  | TimeoutError
  deriving (Eq, Show)
```

Usage:

```haskell
-- In Application wiring
let config = DispatcherConfig
  { -- ...
  , onError = \err event -> do
      Metrics.increment "integration_errors" [("type", errorType err)]
      when (isCritical err) do
        Alerting.send "Integration failure" (toText err)
  }
```

### Metrics to Add

| Metric | Type | Labels |
|--------|------|--------|
| `integration_events_processed` | Counter | entity_type, integration |
| `integration_events_failed` | Counter | entity_type, integration, error_type |
| `integration_processing_duration` | Histogram | entity_type, integration |
| `integration_channel_depth` | Gauge | entity_id |
| `integration_workers_active` | Gauge | worker_type |

---

## Low: Inbound Worker Backoff

### Location

`core/service/Service/Application/Integrations.hs`, function `startInboundWorkers`, lines 246-265

### The Problem

Inbound workers use exponential backoff on failure, but the backoff never resets for infinite workers.

### Current Behavior

```haskell
let workerWithRestartLoop backoffMs = do
      result <- Integration.runInbound inboundIntegration emitCommand
        |> Task.asResult
      case result of
        Err err -> do
          Console.print [fmt|...Restarting in #{backoffMs}ms...|]
          AsyncTask.sleep backoffMs
          let nextBackoff = min (backoffMs * 2) 60000
          workerWithRestartLoop nextBackoff
        Ok _ -> Task.yield unit  -- Never reached for infinite workers
```

### Why `Ok` is Never Reached

Inbound workers like `Timer.every` run forever:

```haskell
every config = do
  let loop emit tick = do
        emit (config.toCommand tick)
        AsyncTask.sleep config.interval
        loop emit (tick + 1)  -- Infinite recursion
  Integration.inbound { run = \emit -> loop emit 1 }
```

The `Ok` branch is only reached if `runInbound` returns successfully, which never happens for infinite workers.

### The Consequence

After one failure:
1. Backoff starts at 1000ms
2. Doubles to 2000ms, 4000ms, ..., 60000ms (max)
3. Stays at 60000ms forever

Even if the underlying issue is fixed, the worker waits 60 seconds between restarts.

### Recommended Fix

Reset backoff on successful event processing, not task completion:

```haskell
let workerWithRestartLoop backoffMs = do
      let emitWithReset payload = do
            Dispatcher.dispatchCommand commandEndpoints payload
            -- Reset backoff on success
            workerWithRestartLoop 1000  -- Restart with initial backoff
      
      result <- Integration.runInbound inboundIntegration emitWithReset
        |> Task.asResult
      case result of
        Err err -> do
          Console.print [fmt|...Restarting in #{backoffMs}ms...|]
          AsyncTask.sleep backoffMs
          let nextBackoff = min (backoffMs * 2) 60000
          workerWithRestartLoop nextBackoff
        Ok _ -> Task.yield unit
```

### Alternative: Health Check

Instead of resetting on success, add a health check:

```haskell
-- Periodically check if worker is healthy
-- If healthy for N seconds, reset backoff
```

This is more robust but more complex.

---

## Implementation Sequence

### Phase 1: Critical Fixes (Must Have for Production)

**Estimated Time**: 4-8 hours

| Task | Priority | Effort | Status |
|------|----------|--------|--------|
| Fix worker spawn race condition | Critical | 2h | ✅ Done |
| Replace ConcurrentVar with ConcurrentMap | Critical | 2-4h | ✅ Done |

These two fixes are required for correctness and throughput. Without them, the system is unsafe under concurrent load.

### Phase 2: High-Priority Fixes (Should Have)

**Estimated Time**: 16-24 hours

| Task | Priority | Effort | Status |
|------|----------|--------|--------|
| Fix reaper race condition | High | 2-4h | ✅ Done |
| Implement bounded channels | High | 4-8h | ✅ Done |
| Implement position tracking (IntegrationStore) | High | 8-12h | Pending |

These fixes address data loss scenarios and resource exhaustion.

### Phase 3: Production Hardening (Nice to Have)

**Estimated Time**: 16-24 hours

| Task | Priority | Effort |
|------|----------|--------|
| Implement circuit breaker | High | 8h |
| Implement graceful shutdown | Medium | 4h |
| Add error callbacks and metrics | Medium | 4h |
| Fix inbound worker backoff | Low | 2h |

These fixes improve reliability and observability for production operations.

### Dependencies

```
Phase 1 (Critical)
├── Worker spawn race: No dependencies
└── ConcurrentMap migration: No dependencies

Phase 2 (High)
├── Reaper race: Depends on ConcurrentMap migration (simpler with STM)
├── Bounded channels: No dependencies
└── Position tracking: Depends on bounded channels (need backpressure first)

Phase 3 (Production)
├── Circuit breaker: No dependencies
├── Graceful shutdown: Depends on bounded channels
├── Error callbacks: No dependencies
└── Inbound backoff: No dependencies
```

### Testing Requirements

Each fix should include:

1. **Unit tests**: Verify the fix in isolation
2. **Concurrency tests**: Verify behavior under concurrent load
3. **Integration tests**: Verify with real event store (testbed)
4. **Load tests**: Verify at target throughput (50k req/s)

### Metrics for Success

| Metric | Target |
|--------|--------|
| Throughput | 50,000 events/second |
| Latency p99 | < 100ms |
| Error rate | < 0.01% |
| Memory growth | Bounded (no leaks) |
| Ordering violations | 0 |
| Lost events | 0 |

---

## Appendix: Code Locations Quick Reference

| Issue | File | Function/Line |
|-------|------|---------------|
| Worker spawn race | `Dispatcher.hs` | `dispatch` lines 216-248 |
| MVar bottleneck | `Dispatcher.hs` | Data type lines 138-147 |
| Reaper race | `Dispatcher.hs` | `startReaper` lines 385-422 |
| Unbounded channels | `Channel.hs` | Entire module |
| Position tracking | Missing | N/A |
| Circuit breaker | Missing | N/A |
| Graceful shutdown | `Dispatcher.hs` | `shutdown` lines 425-445 |
| Silent errors | `Dispatcher.hs` | lines 341-343, 363-365 |
| Inbound backoff | `Integrations.hs` | lines 246-265 |

## Appendix: Related Documentation

- [ADR-0008: Integration Pattern](../decisions/0008-integration-pattern.md) - Architecture decision
- [integration-completion.md](integration-completion.md) - Feature completion plan
- [integration-bugfixes.md](integration-bugfixes.md) - Known bugs
- [per-entity-ordering.md](per-entity-ordering.md) - Ordering guarantee design
