# Integration Feature Implementation Plan

**ADR**: [0008-integration-pattern.md](../decisions/0008-integration-pattern.md)
**Status**: Draft

## Overview

This document outlines how to implement the Integration pattern for NeoHaskell, following existing architectural patterns in the codebase.

## Existing Patterns to Follow

| Pattern | Location | Relevance |
|---------|----------|-----------|
| Query Subscriber | `Service/Query/Subscriber.hs` | Model for OutboundSubscriber |
| Query Registry | `Service/Query/Registry.hs` | Model for IntegrationRegistry |
| Query Definition | `Service/Query/Definition.hs` | Model for IntegrationDefinition |
| Application Wiring | `Service/Application.hs` | Where integrations hook in |

Key insight: **Outbound integrations are very similar to Query subscribers** - both subscribe to events and react. The difference is Query updates a read model; Outbound executes side effects and optionally emits commands.

## Module Structure

```
core/service/
  Service/
    Integration.hs                    -- Re-export wrapper
    Integration/
      Core.hs                         -- Outbound, Inbound, IntegrationError types
      Outbound.hs                     -- OutboundSubscriber (like QuerySubscriber)
      Inbound.hs                      -- InboundWorkerManager
      Registry.hs                     -- IntegrationRegistry (like QueryRegistry)
      Definition.hs                   -- IntegrationDefinition (like QueryDefinition)
```

## Phase 1: Core Types and Registry

**Can be done in parallel with Phase 2.**

### 1.1 Create `Service/Integration/Core.hs`

#### How: Type Safety Strategy

The core challenge is maintaining type safety for Jess while allowing type erasure for the runtime registry. We solve this with a **two-layer type system**:

**Layer 1: Fully Typed (What Jess Sees)**

Jess works with concrete types. When she writes `Sendgrid.outbound { toEmail = \user event -> ... }`, the types `user`, `event`, and the resulting command are all known at compile time. The compiler catches mismatches immediately — if Jess tries to use a `User` entity with an `OrderPlaced` event, it fails to compile.

The `Outbound entity event command` type captures all three type parameters. This gives Jess autocomplete, type errors at the right location, and documentation via types.

**Layer 2: Type-Erased (What the Runtime Uses)**

The `OutboundIntegrationHandler` erases types to `Json.Value`. This is necessary because the registry holds handlers for many different entity/event/command combinations. We can't have a heterogeneous collection of differently-typed handlers without existential types or type erasure.

The bridge between layers happens in Nick's integration code. Nick's `outbound` function takes Jess's typed config and produces a type-erased handler. Nick is responsible for JSON encoding/decoding at the boundary.

**Why This Works for DevEx**

- Jess never sees `Json.Value` — her API is fully typed
- Type errors appear in Jess's code, not deep in framework internals
- Nick handles serialization once per integration, not once per usage
- The runtime doesn't need to know about specific entity/event types

#### How: Error Type Design

`IntegrationError` should be a closed sum type (not extensible) because:

1. The framework needs to handle all cases (retry logic, DLQ routing)
2. Nick can always wrap integration-specific errors in `PermanentFailure Text`
3. Jess never constructs these errors — they come from Nick's code

Each constructor carries enough information for the framework to decide what to do:
- `NetworkError`: Typically retryable
- `AuthenticationError`: Not retryable, needs human intervention
- `ValidationError`: Not retryable, bad data
- `RateLimited RetryAfter`: Retryable after specific delay
- `PermanentFailure`: Give up, send to DLQ

```haskell
-- Outbound: Event -> External Effect -> Maybe Command
data Outbound entity event command = Outbound
  { execute :: entity -> event -> Task IntegrationError (Maybe command)
  }

-- Inbound: Long-running worker that emits commands
data Inbound command = Inbound
  { run :: (command -> Task IntegrationError Unit) -> Task IntegrationError Void
  }

-- Type-erased handlers for registry
-- Note: handleEvent receives BOTH entity state AND event
-- The entity is fetched by the subscriber before calling the handler
data OutboundIntegrationHandler = OutboundIntegrationHandler
  { integrationName :: Text
  , entityName :: EntityName
  , handleEvent :: Json.Value -> Event Json.Value -> Task Text Unit
  --               ^ entity      ^ event
  }

data InboundIntegrationHandler = InboundIntegrationHandler
  { integrationName :: Text
  , startWorker :: (Json.Value -> Task Text Unit) -> Task Text Void
  }

-- Errors
data IntegrationError
  = NetworkError Text
  | AuthenticationError Text
  | ValidationError Text
  | RateLimited RetryAfter
  | PermanentFailure Text

-- Helpers
emitCommand :: command -> Task IntegrationError (Maybe command)
noCommand :: Task IntegrationError (Maybe command)
```

### 1.2 Create `Service/Integration/Registry.hs`

#### How: Registry Design

The registry is a runtime lookup table that maps entity names to their handlers. It's intentionally simple — just a `Map` and an `Array`.

**Why Map by EntityName (not EventType)**

We key outbound handlers by `EntityName` rather than event type because:

1. Events are scoped to entities — when we receive an event, we know its entity from the `streamId`
2. Multiple event types for the same entity go to the same handlers
3. This matches how `QueryRegistry` works, keeping patterns consistent

**Merging Strategy**

When multiple integration definitions register handlers for the same entity, we need to merge them. The `mergeInto` function concatenates handler arrays rather than replacing. This allows:

- Multiple integrations reacting to the same entity's events
- Modular composition — each integration definition is independent
- No ordering dependencies between registrations

**Inbound Workers Are Simpler**

Inbound workers don't need entity-based lookup — they're just a flat array. Each worker runs independently, listening to its external source. The registry just collects them for the `InboundWorkerManager` to spawn.

**Immutability**

The registry is immutable. All registration returns a new registry. This means:

- Thread-safe by construction
- No locking needed during lookups
- Can be built incrementally during application startup
- Snapshot at startup, never changes during runtime

Follow `QueryRegistry` pattern:

```haskell
data IntegrationRegistry = IntegrationRegistry
  { outboundHandlers :: Map EntityName (Array OutboundIntegrationHandler)
  , inboundWorkers :: Array InboundIntegrationHandler
  }

empty :: IntegrationRegistry
register :: EntityName -> OutboundIntegrationHandler -> IntegrationRegistry -> IntegrationRegistry
registerInbound :: InboundIntegrationHandler -> IntegrationRegistry -> IntegrationRegistry
getHandlersForEntity :: EntityName -> IntegrationRegistry -> Array OutboundIntegrationHandler
mergeInto :: IntegrationRegistry -> IntegrationRegistry -> IntegrationRegistry
```

## Phase 2: Outbound Subscriber

**Depends on Phase 1.**

### 2.1 Create `Service/Integration/Outbound.hs`

#### How: Subscription Lifecycle

The outbound subscriber follows the same lifecycle as `QuerySubscriber`:

1. **Creation** (`new`): Allocate state, store dependencies, don't start processing yet
2. **Rebuild** (`rebuildAll`): Process historical events to catch up (for integrations, this may mean "skip" or "replay" depending on idempotency)
3. **Start** (`start`): Begin live subscription, process new events as they arrive
4. **Stop** (`stop`): Cancel subscription, clean up resources

**Key Decision: What Happens During Rebuild?**

Unlike queries which must replay all history to rebuild state, integrations are side-effectful. Replaying history could mean re-sending emails. Two options:

1. **Skip historical events**: Start from "now", miss events that occurred while offline
2. **Track position persistently**: Store last-processed position, replay from there on restart

Recommendation: Track position. Store `lastProcessedPosition` in the EventStore or a separate persistence layer. On restart, resume from that position. This gives at-least-once semantics — integrations must be idempotent.

#### How: Entity Fetching

Before calling a handler, we need the entity's current state. The `EntityFetcher` provides this:

1. Extract `entityName` and `entityId` from the event's `streamId`
2. Call `EntityFetcher.fetch` to rebuild entity state from events up to this point
3. Pass `(entity, event)` to the handler

**Why Fetch Per-Event (Not Cache)**

We fetch fresh for each event because the handler needs the entity state *at that point in time*, not the latest state. If events A1, A2, A3 are processed:

- Handler for A1 sees entity after A1
- Handler for A2 sees entity after A1+A2
- Handler for A3 sees entity after A1+A2+A3

This is crucial for correctness — the handler's logic may depend on the state at that moment.

**Performance Consideration**

Fetching per-event can be slow for entities with many events. The existing `SnapshotCache` helps here — it caches intermediate entity states. The `EntityFetcher` should use the cache transparently.

#### How: Command Dispatching (Typed)

When a handler returns a command, we need to dispatch it. Since we decided on typed dispatch:

1. Nick's integration code receives the typed command from Jess's mapper
2. Nick serializes it to JSON and calls the `commandDispatcher`
3. The `commandDispatcher` (provided by Application) deserializes and routes to the correct `CommandExecutor`

**The Type Bridge**

The trick is that Nick knows the command type at compile time (it's a type parameter of the integration). Nick can use `ToJSON` to serialize. The `commandDispatcher` needs to know how to deserialize — this requires either:

- A command registry that maps command names to deserializers
- Including the command type name in the JSON payload

Recommendation: Include a `_type` field in the JSON payload. The dispatcher pattern-matches on this to select the deserializer.

Model on `Service/Query/Subscriber.hs`. Key differences:
- Instead of `QueryUpdater.updateQuery`, calls `OutboundIntegrationHandler.handleEvent`
- Needs access to command dispatcher for emitted commands
- **Fetches entity state before calling handler** (handler receives both entity + event)
- Same lifecycle: `new`, `rebuildAll`, `start`, `stop`

**Entity Fetching Flow:**
```
Event arrives
    ↓
Extract entityName from event.streamId
    ↓
Fetch current entity state (rebuild from events up to this point)
    ↓
Call handler with (entity, event)
    ↓
Handler executes side effect, optionally returns command
    ↓
If command returned, dispatch via commandDispatcher
```

This mirrors how the ADR shows Jess's mappers receiving both `user` and `event`:
```haskell
toEmail = \user event -> ...
onSuccess = \user event response -> ...
```

```haskell
data OutboundSubscriber = OutboundSubscriber
  { eventStore :: EventStore Json.Value
  , entityFetcher :: EntityFetcher  -- to fetch entity state before calling handler
  , registry :: IntegrationRegistry
  , commandDispatcher :: Json.Value -> Task Text Unit
  , lastProcessedPosition :: ConcurrentVar (Maybe StreamPosition)
  , subscriptionId :: ConcurrentVar (Maybe SubscriptionId)
  }

new ::
  EventStore Json.Value ->
  EntityFetcher ->
  IntegrationRegistry ->
  (Json.Value -> Task Text Unit) ->  -- command dispatcher
  Task Text OutboundSubscriber

rebuildAll :: OutboundSubscriber -> Task Text Unit
start :: OutboundSubscriber -> Task Text Unit
stop :: OutboundSubscriber -> Task Text Unit
```

### Ordering Guarantee

**Sequential per entity, parallel across entities:**

- Events for entity A are processed in order (A1 → A2 → A3)
- Events for entity B are processed in order (B1 → B2 → B3)
- A and B can be processed concurrently

**Implementation approach:**
```
Events arrive: [A1, B1, A2, B2, A3]
    ↓
Group by streamId (entity):
  - Queue A: [A1, A2, A3]
  - Queue B: [B1, B2]
    ↓
Process each queue sequentially (spawn worker per entity)
    ↓
Cross-entity parallelism achieved
```

This prevents race conditions (email 2 before email 1 for same user) while maximizing throughput.

#### How: Per-Entity Queuing

The subscriber maintains a `Map StreamId (Channel Event)` — one channel per active entity. When an event arrives:

1. Look up or create the channel for that `streamId`
2. Push the event onto the channel
3. If this is a new channel, spawn a worker that processes events from it sequentially

**Worker Lifecycle**

Each entity's worker:
- Reads events from its channel one at a time
- Processes each fully (including side effects and command dispatch) before reading the next
- Terminates when the channel is empty and a timeout elapses (to avoid orphan workers)

**Backpressure**

If an entity's events pile up faster than the worker can process them, the channel grows. This is bounded by:
- Using bounded channels with a max size
- If full, the main subscriber blocks until space is available
- This propagates backpressure to the EventStore subscription

**Why Not Global Sequential?**

Global sequential (one event at a time across all entities) is simpler but wastes parallelism. If entity A's handler takes 5 seconds (slow API call), entity B's events wait unnecessarily. Per-entity queuing keeps B flowing while A is blocked.

**Cleanup**

Idle entity workers should be cleaned up to avoid resource leaks. Options:
- Timeout: Worker exits after N seconds of no events
- LRU eviction: Keep at most M workers, evict least-recently-used
- Explicit cleanup: When entity is "finalized" (if such a concept exists)

## Phase 3: Inbound Worker Manager

**Depends on Phase 1. Can be done in parallel with Phase 2.**

### 3.1 Create `Service/Integration/Inbound.hs`

#### How: Worker Spawning

The `InboundWorkerManager` is simpler than the `OutboundSubscriber` because inbound workers are independent — no ordering constraints, no event subscription.

On `startAll`:
1. Iterate through `registry.inboundWorkers`
2. For each worker, spawn an `AsyncTask` that runs the worker's `run` function
3. Store the `AsyncTask` handles for later cleanup

On `stopAll`:
1. Cancel all spawned `AsyncTask` handles
2. Wait for graceful shutdown (with timeout)
3. Force-kill any workers that don't stop

#### How: The Emit Callback

Each worker receives an `emit` callback. This callback:
1. Takes a typed command from Nick's integration code
2. Serializes it to JSON (Nick handles this in his integration)
3. Calls the `commandDispatcher` to route to the correct `CommandExecutor`

**Type Safety for Emit**

The `emit` callback is typed in Nick's code. For example, in `Integration.Kafka`:

Nick's code knows the command type at compile time because it's a parameter of the integration config. The `InboundWorkerManager` only sees the type-erased version (`Json.Value -> Task Text Unit`).

**Error Handling in Workers**

If a worker's `run` function throws, the manager should:
1. Log the error
2. Call the global error handler
3. Decide whether to restart the worker or give up

Workers should handle their own retry logic internally (Nick's responsibility), but catastrophic failures bubble up to the manager.

#### How: Graceful Shutdown

When `stopAll` is called:
1. Signal each worker to stop (via a shared shutdown flag or cancellation)
2. Workers should check this flag periodically and exit cleanly
3. Wait up to N seconds for workers to exit
4. Force-cancel any workers that don't respect the signal

**Coordination with Application**

The Application's `runWith` should:
1. Start inbound workers before accepting traffic
2. Stop inbound workers as part of graceful shutdown
3. Ensure in-flight commands complete before exiting

Manages spawning and lifecycle of inbound workers:

```haskell
data InboundWorkerManager = InboundWorkerManager
  { workers :: ConcurrentVar (Array (AsyncTask Text Void))
  , registry :: IntegrationRegistry
  , commandDispatcher :: Json.Value -> Task Text Unit
  }

new ::
  IntegrationRegistry ->
  (Json.Value -> Task Text Unit) ->
  Task Text InboundWorkerManager

startAll :: InboundWorkerManager -> Task Text Unit
stopAll :: InboundWorkerManager -> Task Text Unit
```

Each worker:
- Runs as background `AsyncTask`
- Receives an `emit` callback that dispatches commands
- Runs forever (returns `Void`)

## Phase 4: Definition and Application Integration

**Depends on Phase 2 + 3.**

### 4.1 Create `Service/Integration/Definition.hs`

#### How: The Definition Pattern

A "definition" captures *how to wire* an integration without *actually wiring* it. This deferred execution pattern allows:

1. Jess declares integrations at module level (no side effects)
2. Application collects all definitions
3. At runtime, Application wires them with actual dependencies (EventStore, command dispatcher)

**Why Defer Wiring?**

If Jess's integration code immediately connected to Kafka or started HTTP servers, we'd have:
- Side effects at module load time (bad for testing)
- No control over startup order
- Difficult to mock dependencies

The definition pattern keeps Jess's code pure. The effectful wiring happens in one place: `Application.runWith`.

**What the Definition Contains**

The `IntegrationDefinition` needs:
- `integrationName`: For logging and debugging
- `wireIntegration`: A function that takes dependencies and returns a registry

The `wireIntegration` function receives:
- `EventStore Json.Value`: For outbound integrations that need to subscribe
- `commandDispatcher`: For both outbound (after side effect) and inbound (emit callback)

And returns an `IntegrationRegistry` — the handlers to register.

#### How: Composing Definitions

Jess may use multiple integrations in her app. Each integration provides a definition:

```
Sendgrid.definition :: IntegrationDefinition
Stripe.definition :: IntegrationDefinition
Kafka.definition :: IntegrationDefinition
```

The Application collects these:

```
app = Application.new
  |> Application.withIntegration Sendgrid.definition
  |> Application.withIntegration Stripe.definition
  |> Application.withIntegration Kafka.definition
```

At runtime, `runWith` wires each definition and merges the resulting registries into one combined registry.

Follow `QueryDefinition` pattern:

```haskell
data IntegrationDefinition = IntegrationDefinition
  { integrationName :: Text
  , wireIntegration ::
      EventStore Json.Value ->
      (Json.Value -> Task Text Unit) ->
      Task Text IntegrationRegistry
  }
```

### 4.2 Create `Service/Integration.hs`

Re-export public API from sub-modules.

### 4.3 Modify `Service/Application.hs`

#### How: Application Builder Extensions

The Application type is extended with two new fields:
- `integrationDefinitions`: Array of definitions collected via `withIntegration`
- `onIntegrationError`: Optional global error handler

These follow the existing pattern — `withIntegration` appends to an array, just like `withQuery` does.

**Default Error Handler**

If no `onIntegrationError` is set, provide a sensible default:
- Log the error with context (integration name, event info)
- Continue processing (don't crash the whole subscriber)

This ensures Jess doesn't *have* to configure error handling for things to work.

#### How: Wiring Order in runWith

The order of operations matters:

1. **Build command dispatcher first**: Both outbound and inbound need this. The dispatcher is built from the service definitions — it knows how to route commands to their executors.

2. **Wire integration definitions**: Call each definition's `wireIntegration` with the EventStore and command dispatcher. Merge all resulting registries.

3. **Create OutboundSubscriber**: Needs EventStore, EntityFetcher, merged registry, and command dispatcher.

4. **Create InboundWorkerManager**: Needs merged registry and command dispatcher.

5. **Start inbound workers**: Before accepting traffic, ensure workers are listening.

6. **Start outbound subscription**: After queries are rebuilt (to ensure consistent state), start processing events for integrations.

**Shutdown Order (Reverse)**

1. Stop accepting new traffic
2. Stop outbound subscription
3. Stop inbound workers (with graceful timeout)
4. Close EventStore connection
5. Exit

#### How: Building the Command Dispatcher

The command dispatcher maps command type names to execution logic. It needs:

1. A registry of command types → executors (built from service definitions)
2. A way to deserialize JSON to the correct command type
3. Error handling for unknown command types

**Type-Safe Dispatch**

Each service definition already knows its command types. When building the dispatcher:
- Extract command type info from each service
- Build a map from type name to deserializer + executor
- The dispatcher looks up by type name, deserializes, and executes

This is similar to how the transport layer already routes commands, but invoked internally instead of via HTTP.

Add to Application type:
```haskell
data Application = Application
  { ...
  , integrationDefinitions :: Array IntegrationDefinition
  , onIntegrationError :: IntegrationError -> Task Void Unit
  }
```

Add builder functions:
```haskell
withIntegration :: IntegrationDefinition -> Application -> Application
onIntegrationError :: (IntegrationError -> Task Void Unit) -> Application -> Application
```

In `runWith`:
1. Wire integration definitions
2. Build command dispatcher from services
3. Create OutboundSubscriber with combined registry
4. Create InboundWorkerManager
5. Start inbound workers
6. Start outbound subscription (after query rebuild)

## Decided

### Command Dispatching

**Decision**: Typed dispatch per integration.

The integration's `toCommand` mapper returns a concrete command type. The framework routes it to the appropriate `CommandExecutor` based on the type.

### Ordering

**Decision**: Sequential per entity, parallel across entities.

See "Ordering Guarantee" section above.

### Error Handling Flow

```
handler throws IntegrationError
    |
    v
Per-integration handler (if defined)
    |
    +-- Integration.retry    -> schedule retry
    +-- Integration.halt     -> stop processing
    +-- Integration.useGlobalHandler
            |
            v
    Global error handler (log, DLQ, etc.)
```

## Implementation Sequence

```
Phase 1 (parallel):
├── Service/Integration/Core.hs
└── Service/Integration/Registry.hs

Phase 2 (depends on Phase 1):
└── Service/Integration/Outbound.hs

Phase 3 (depends on Phase 1, parallel with Phase 2):
└── Service/Integration/Inbound.hs

Phase 4 (depends on Phase 2 + 3):
├── Service/Integration/Definition.hs
├── Service/Integration.hs
└── Modify Service/Application.hs

Phase 5 (depends on Phase 4):
└── Tests (unit + integration)
```

## Testing Strategy

#### How: Testing the Two-Persona Split

The two-persona design enables clean testing at each layer.

**Testing Jess's Pure Mappers**

Jess's code is pure functions. Test them directly:
- Input: Entity state, event data
- Output: Email payload, command, etc.
- No mocking needed — just call the function

This is the easiest testing story. Jess can write unit tests without any framework setup.

**Testing Nick's Integration Logic**

Nick's code has side effects. Test with:
- Mock HTTP clients (for outbound)
- Mock message brokers (for inbound)
- Verify correct API calls are made
- Verify commands are emitted correctly

Nick should test his integration package in isolation before shipping.

**Testing the Framework Machinery**

The subscriber, registry, and worker manager need framework-level tests:
- Mock EventStore with controlled event sequences
- Verify ordering guarantees hold
- Verify error handling works correctly

### Specific Test Categories

1. **Unit tests for Registry**: Similar to `Service/Query/RegistrySpec.hs`
   - Register handlers, verify lookup works
   - Test merging of registries
   - Test empty registry edge cases

2. **Unit tests for Outbound**: Similar to `Service/Query/SubscriberSpec.hs`
   - Mock EventStore that emits controlled events
   - Verify handlers called with correct (entity, event) pairs
   - Verify ordering per entity
   - Verify command emission via mock dispatcher

3. **Unit tests for Inbound**:
   - Create test worker that emits predetermined commands
   - Verify commands reach mock dispatcher
   - Test graceful shutdown behavior

4. **Integration tests**:
   - Full Application with test integrations
   - End-to-end: emit event → handler called → command dispatched → event stored
   - Verify at-least-once semantics (replay after restart)

## NOT Building (Future Work)

Per ADR, these are explicitly out of scope:

- Dead Letter Queue
- Circuit Breaker
- Metrics/Observability
- Batch Processing
- Integration Testing Framework

## Key Files to Reference

| File | Why |
|------|-----|
| `Service/Query/Subscriber.hs` | Primary pattern for OutboundSubscriber |
| `Service/Query/Registry.hs` | Pattern for IntegrationRegistry |
| `Service/Query/Definition.hs` | Pattern for IntegrationDefinition |
| `Service/Application.hs` | Where integration wiring hooks in |
| `Service/CommandExecutor/Core.hs` | How commands are executed |
