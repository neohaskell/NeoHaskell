# Integration Feature Implementation Plan

**ADR**: [0008-integration-pattern.md](../decisions/0008-integration-pattern.md)
**Status**: Draft

## Overview

This document outlines how to implement the Integration pattern for NeoHaskell, following existing architectural patterns in the codebase.

## Design Summary

The Integration pattern uses a **per-entity function approach**:

- **Jess** writes one integration function per entity that pattern matches events and returns `Integration.Outbound`
- **Nick** builds action builders that accept payloads and command mappers, returning type-erased `Integration.Outbound.Action`
- Commands can target **any service** (cross-service flexibility)
- Type erasure enables heterogeneous command collections in a single `Outbound` batch

## Existing Patterns to Follow

| Pattern | Location | Relevance |
|---------|----------|-----------|
| Query Subscriber | `Service/Query/Subscriber.hs` | Model for OutboundSubscriber |
| Query Registry | `Service/Query/Registry.hs` | Model for entity integration lookup |
| Application Wiring | `Service/Application.hs` | Where integrations hook in |

## Module Structure

```
core/service/
  Service/
    Integration.hs                    -- Re-export wrapper
    Integration/
      Outbound.hs                     -- Outbound type, batch, none, Action, errors
      Inbound.hs                      -- Inbound type and helpers
      Subscriber.hs                   -- Outbound event subscription
      Worker.hs                       -- Inbound worker management
      Store.hs                        -- Position tracking per entity
```

## Phase 1: Core Types

### 1.1 Create `Service/Integration/Outbound.hs`

#### Type Erasure Strategy

The core challenge is allowing Jess to emit heterogeneous command types from a single integration function. We solve this with **type erasure at action creation**:

**Jess's View (Typed)**

Jess works with concrete command types in her mappers:

```haskell
Sendgrid.email { ... }
  (\response -> CreateWelcomeNotification { userId = user.id })
```

The command type `CreateWelcomeNotification` is known at compile time.

**Nick's Bridge (Type Erasure)**

Nick's action builder serializes the command to JSON immediately:

```haskell
email payload toCommand = Integration.Outbound.action do
  response <- -- execute HTTP call
  Integration.Outbound.emitCommand (toCommand response)
  -- emitCommand serializes to CommandPayload internally
```

**Runtime (Type-Erased)**

The `Outbound` collection holds `ActionInternal` values with `Task IntegrationError (Maybe CommandPayload)`. The `CommandPayload` contains:
- `commandType :: Text` (derived from `Typeable`)
- `commandData :: Json.Value` (serialized command)

```haskell
module Service.Integration.Outbound where

-- Opaque collection of outbound actions (heterogeneous command types)
newtype Outbound = Outbound (Array ActionInternal)

data ActionInternal = ActionInternal
  { execute :: Task IntegrationError (Maybe CommandPayload)
  }

data CommandPayload = CommandPayload
  { commandType :: Text
  , commandData :: Json.Value
  }

-- Single action (type-erased)
newtype Action = Action ActionInternal

-- Builder monad for collecting actions
newtype Builder result = Builder (State (Array ActionInternal) result)
  deriving (Functor, Applicative, Monad)

-- Collect actions into an Outbound value
batch :: Builder () -> Outbound
batch (Builder m) = Outbound (execState m [])

-- No integrations for this event
none :: Outbound
none = Outbound []

-- Create a single action (used by Nick's action builders)
action :: Task IntegrationError (Maybe CommandPayload) -> Action
action task = Action (ActionInternal task)

-- Command emission helpers (used inside action builders)
emitCommand :: forall command. (ToJSON command, Typeable command) => command -> Task IntegrationError (Maybe CommandPayload)
emitCommand cmd = Task.yield (Just payload)
  where
    payload = CommandPayload
      { commandType = Text.pack (show (typeRep (Proxy @command)))
      , commandData = Json.toValue cmd
      }

noCommand :: Task IntegrationError (Maybe CommandPayload)
noCommand = Task.yield Nothing

-- Errors
data IntegrationError
  = NetworkError Text
  | AuthenticationError Text
  | ValidationError Text
  | RateLimited RetryAfter
  | PermanentFailure Text
```

### 1.2 Create `Service/Integration/Inbound.hs`

```haskell
module Service.Integration.Inbound where

-- Inbound worker (listens to external source, emits commands)
data Inbound = Inbound
  { run :: (CommandPayload -> Task IntegrationError Unit) -> Task IntegrationError Void
  }
```

### 1.3 Create `Service/Integration/Store.hs`

Position tracking **per entity per integration** for at-least-once semantics:

```haskell
data IntegrationStore = IntegrationStore
  { getPosition :: IntegrationName -> StreamId -> Task Error (Maybe StreamPosition)
  , setPosition :: IntegrationName -> StreamId -> StreamPosition -> Task Error Unit
  }

-- In-memory implementation for testing
inMemory :: Task Error IntegrationStore

-- Persistent implementation (backed by EventStore or separate storage)
persistent :: EventStore Json.Value -> Task Error IntegrationStore
```

**Key insight**: Only store position for events where integrations actually ran (returned non-empty `Actions`), not for all events.

## Phase 2: Outbound Subscriber

**Depends on Phase 1.**

### 2.1 Create `Service/Integration/Subscriber.hs`

The subscriber processes events and invokes per-entity integration functions.

#### Entity Integration Function Registration

The application registers integration functions keyed by entity type:

```haskell
-- Type-erased handler stored in registry
data EntityIntegrationHandler = EntityIntegrationHandler
  { entityName :: EntityName
  , integrationName :: Text
  , handleEvent :: Json.Value -> Event Json.Value -> Task IntegrationError Outbound
  --               ^ entity      ^ event            ^ returns outbound actions to execute
  }

-- Registry maps entity names to their handlers
data IntegrationRegistry = IntegrationRegistry
  { entityHandlers :: Map EntityName (Array EntityIntegrationHandler)
  , inboundWorkers :: Array InboundIntegrationHandler
  }
```

#### Event Processing Flow

```
Event arrives from EventStore subscription
    |
    v
Extract entityName and streamId from event
    |
    v
Fetch current entity state (rebuild from events up to this point)
    |
    v
Look up integration functions for this entity type
    |
    v
For each integration function:
    |
    +-- Call handler(entity, event) -> Outbound
    |
    +-- If Outbound is non-empty:
    |       |
    |       +-- Execute each Action (side effects)
    |       |
    |       +-- For each CommandPayload returned:
    |       |       |
    |       |       +-- Dispatch to CommandExecutor via type name
    |       |
    |       +-- Update IntegrationStore position
    |
    +-- If Outbound is empty (Integration.Outbound.none):
            |
            +-- Skip (don't update position)
```

#### Ordering Guarantee

**Sequential per entity, parallel across entities:**

- Events for entity A are processed in order (A1 -> A2 -> A3)
- Events for entity B are processed in order (B1 -> B2 -> B3)
- A and B can be processed concurrently

**Implementation:**

```haskell
data OutboundSubscriber = OutboundSubscriber
  { eventStore :: EventStore Json.Value
  , entityFetcher :: EntityFetcher
  , registry :: IntegrationRegistry
  , commandDispatcher :: CommandPayload -> Task Text Unit
  , integrationStore :: IntegrationStore
  , entityQueues :: ConcurrentVar (Map StreamId (Channel (Event Json.Value)))
  , subscriptionId :: ConcurrentVar (Maybe SubscriptionId)
  }
```

Per-entity queuing:
1. Maintain `Map StreamId (Channel Event)` - one channel per active entity
2. When event arrives, push to appropriate channel
3. Spawn worker per entity that processes its channel sequentially
4. Workers terminate after idle timeout

## Phase 3: Inbound Worker Manager

**Depends on Phase 1. Can be done in parallel with Phase 2.**

### 3.1 Create `Service/Integration/Worker.hs`

Inbound integrations are simpler - they're independent workers listening to external sources:

```haskell
data InboundIntegrationHandler = InboundIntegrationHandler
  { integrationName :: Text
  , startWorker :: (CommandPayload -> Task IntegrationError Unit) -> Task IntegrationError Void
  }

data InboundWorkerManager = InboundWorkerManager
  { workers :: ConcurrentVar (Array (AsyncTask Text Void))
  , registry :: IntegrationRegistry
  , commandDispatcher :: CommandPayload -> Task Text Unit
  }

new :: IntegrationRegistry -> (CommandPayload -> Task Text Unit) -> Task Text InboundWorkerManager
startAll :: InboundWorkerManager -> Task Text Unit
stopAll :: InboundWorkerManager -> Task Text Unit
```

Each worker:
- Runs as background `AsyncTask`
- Receives an `emit` callback that dispatches commands
- Runs forever (returns `Void`)

## Phase 4: Application Integration

**Depends on Phase 2 + 3.**

### 4.1 Create `Service/Integration.hs`

Re-export public API:

```haskell
module Service.Integration
  ( -- Outbound
    module Service.Integration.Outbound
    -- Inbound
  , module Service.Integration.Inbound
    -- Store
  , IntegrationStore(..)
  ) where

import Service.Integration.Outbound
import Service.Integration.Inbound
import Service.Integration.Store (IntegrationStore(..))
```

### 4.2 Modify `Service/Application.hs`

Add registration functions:

```haskell
-- Register per-entity outbound integration function
withOutbound ::
  forall entity.
  (ToJSON entity, FromJSON entity, Typeable entity) =>
  (entity -> EntityEvent entity -> Outbound) ->
  Application ->
  Application

-- Register inbound worker
withInbound :: Inbound -> Application -> Application

-- Global error handler
onIntegrationError :: (IntegrationError -> Task Void Unit) -> Application -> Application
```

In `runWith`:
1. Build command dispatcher from services (maps command type names to executors)
2. Create IntegrationStore (in-memory or persistent)
3. Build IntegrationRegistry from registered entity integrations + inbound workers
4. Create OutboundSubscriber
5. Create InboundWorkerManager
6. Start inbound workers before accepting traffic
7. Start outbound subscription after query rebuild

#### Command Dispatcher

The dispatcher routes `CommandPayload` to the correct `CommandExecutor`:

```haskell
buildCommandDispatcher :: Array ServiceDefinition -> (CommandPayload -> Task Text Unit)
buildCommandDispatcher services = \payload -> do
  let CommandPayload { commandType, commandData } = payload
  case lookupExecutor commandType of
    Nothing -> Task.throw [fmt|Unknown command type: {commandType}|]
    Just (deserialize, execute) -> do
      command <- deserialize commandData
      execute command
```

## Decided

### Command Dispatching

**Decision**: Typed dispatch via command type name.

Commands are serialized with their type name (from `Typeable`). The dispatcher uses the type name to look up the correct deserializer and executor.

### Ordering

**Decision**: Sequential per entity, parallel across entities.

### Position Tracking

**Decision**: Per-entity, per-integration.

Only store position for events where integrations actually returned non-empty `Outbound`.

## Implementation Sequence

```
Phase 1 (parallel):
+-- Service/Integration/Outbound.hs
+-- Service/Integration/Inbound.hs
+-- Service/Integration/Store.hs

Phase 2 (depends on Phase 1):
+-- Service/Integration/Subscriber.hs

Phase 3 (depends on Phase 1, parallel with Phase 2):
+-- Service/Integration/Worker.hs

Phase 4 (depends on Phase 2 + 3):
+-- Service/Integration.hs
+-- Modify Service/Application.hs
```

## Testing Strategy

### Testing Jess's Integration Functions

Jess's code is pure. Given an entity and event, test that the correct `Outbound` is returned:

```haskell
spec :: Spec
spec = do
  describe "userIntegrations" do
    it "sends welcome email on registration" do
      let user = User { id = "123", email = "test@example.com" }
      let event = UserRegistered { email = "test@example.com", name = "Test" }
      let outbound = userIntegrations user event
      outbound `shouldSatisfy` hasEmailAction "welcome-v2"

    it "returns none for unhandled events" do
      let outbound = userIntegrations user ProfileUpdated { bio = "..." }
      outbound `shouldBe` Integration.Outbound.none
```

### Testing Nick's Action Builders

Nick's code has side effects. Mock HTTP clients and verify:
- Correct API calls are made
- Command mappers are invoked with responses
- Commands are serialized correctly

### Testing Framework Machinery

- Mock EventStore with controlled event sequences
- Verify ordering guarantees (per-entity sequential)
- Verify position tracking updates correctly
- Verify command dispatch routing

## NOT Building (Future Work)

Per ADR, these are explicitly out of scope:

- Dead Letter Queue
- Circuit Breaker
- Metrics/Observability
- Batch Processing

## Key Files to Reference

| File | Why |
|------|-----|
| `Service/Query/Subscriber.hs` | Primary pattern for OutboundSubscriber |
| `Service/Query/Registry.hs` | Pattern for entity integration lookup |
| `Service/Application.hs` | Where integration wiring hooks in |
| `Service/CommandExecutor/Core.hs` | How commands are executed |
