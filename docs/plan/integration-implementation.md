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

## Phase 3: Inbound Worker Manager

**Depends on Phase 1. Can be done in parallel with Phase 2.**

### 3.1 Create `Service/Integration/Inbound.hs`

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

1. **Unit tests for Registry**: Similar to `Service/Query/RegistrySpec.hs`

2. **Unit tests for Outbound**: Similar to `Service/Query/SubscriberSpec.hs`
   - Mock EventStore
   - Verify handlers called in order
   - Verify command emission

3. **Unit tests for Inbound**:
   - Mock worker that emits test commands
   - Verify commands dispatched

4. **Integration tests**:
   - Full Application with test integrations
   - Verify end-to-end flow

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
