# ADR-0007: Application Module for Multi-Service Composition

## Status

Proposed

## Context

Currently, a Service can have only 1 entity type, only 1 event type, and command types are all associated to those. The philosophy is: 1 service per aggregate (domain boundary), deploy multiple services that interact with each other.

We need a way to compose multiple services into a single runnable application unit.

Two approaches were considered:

1. Modify ServiceDefinition to support multiple entity types via a row type
2. Create a new Application type that merges multiple services

We chose approach 2 because it:

- Preserves domain boundaries (one Service = one aggregate)
- Maintains type safety at the service level
- Keeps testability intact (each service tested in isolation)
- Provides a simple mental model: "A Service handles one aggregate. An Application runs multiple services."

## Decision

Create a new `Application` module that composes multiple `Service` instances into a single runnable unit. Type erasure happens at the composition boundary. The `__internal_runServiceMain` will be removed in favor of `__internal_runApplicationMain`.

### Key Design Decisions

1. **Application owns the runtime**: EventStore, SnapshotCache, and Transport are created once by Application and shared across all services
2. **Type erasure at `add` boundary**: Each service becomes a `ServiceRunner` (existentially quantified) that contributes endpoint handlers
3. **Single shared transport**: One HTTP server handles all commands from all services (e.g., port 8080)
4. **Endpoint collection then run**: Application collects all endpoints from all services first, merges them, then starts the transport once
5. **Single EventStore, entity-prefixed streams**: Services share the EventStore; each writes to streams prefixed by their entity name

### Architecture

```text
Application
├── EventStore (Json.Value) -- shared, created once
├── SnapshotCache (Json.Value) -- shared, created once
├── Transport (e.g., WebTransport) -- shared, one server
└── Services[]
    ├── CartService → endpoints: [CreateCart, AddItem]
    └── OrderService → endpoints: [PlaceOrder, CancelOrder]

All endpoints merged into single Endpoints structure → Transport runs once
```

## Implementation Plan

### Files to Create

#### 1. `core/service/Application.hs` (NEW)

Main module exposing:

- `Application` data type
- `ServiceRunner` existential type
- Builder API: `new`, `add`, `useTransport`, `useEventStore`, `useSnapshotCache`
- `__internal_runApplicationMain :: Application -> IO ()`

### Files to Modify

#### 1. `core/service/Service/ServiceDefinition/Core.hs`

- **Simplify Service type**: Remove all infrastructure type parameters
  - Old: `Service cmds cmdTransports providedTransports eventStoreConfig snapshotCacheConfig`
  - New: `Service cmds` (just the command row)
- Remove builder functions: `useEventStore`, `useSnapshotCache`, `useServer`
- Service now only has `new` and `command` builders
- Remove `__internal_runServiceMain`
- Remove `runService` (the current one that creates EventStore internally)
- Add `collectEndpoints` function that returns endpoint handlers without running transport:

```haskell
collectEndpoints ::
  forall cmds event entity.
  ( Record.AllFields cmds CommandInspect,
    event ~ ServiceEventType cmds,
    entity ~ ServiceEntityType cmds,
    Json.FromJSON event, Json.ToJSON event,
    Json.FromJSON entity, Json.ToJSON entity
  ) =>
  EventStore Json.Value ->
  Maybe (SnapshotCache Json.Value) ->
  Record.ContextRecord Record.I cmds ->
  Record.ContextRecord (Record.Dict CommandInspect) cmds ->
  Task Text (Map Text EndpointHandler)
```

#### 2. `core/service/Service.hs`

- Remove re-export of `__internal_runServiceMain`
- Export `collectEndpoints` (or keep it internal to Application)

#### 3. `core/nhcore.cabal`

- Add `Application` to exposed-modules in service library section

#### 4. `testbed/src/Testbed/Service.hs`

- Remove eventStore/snapshotCache config from service definition
- Service now only defines commands

#### 5. `testbed/launcher/Launcher.hs`

- Change to use `Application.__internal_runApplicationMain`

## Type Definitions

### Simplified Service Type

```haskell
data Service (cmds :: Record.Row Type) = Service
  { commandDefinitions :: Record.ContextRecord Record.I cmds
  , inspectDict :: Record.ContextRecord (Record.Dict CommandInspect) cmds
  }
```

### Application Type

```haskell
data Application = Application
  { serviceRunners :: Array ServiceRunner
  , transport :: Maybe TransportValue
  , eventStoreConfig :: Maybe EventStoreConfigValue
  , snapshotCacheConfig :: Maybe SnapshotCacheConfigValue
  }

-- Existential wrappers for type-erased configs
data EventStoreConfigValue = forall config. (EventStoreConfig config) => EventStoreConfigValue config
data SnapshotCacheConfigValue = forall config. (SnapshotCacheConfig config) => SnapshotCacheConfigValue config

-- Existential wrapper for services - captures all needed constraints
data ServiceRunner = forall cmds.
  ( Record.AllFields cmds CommandInspect
  , Json.FromJSON (ServiceEventType cmds), Json.ToJSON (ServiceEventType cmds)
  , Json.FromJSON (ServiceEntityType cmds), Json.ToJSON (ServiceEntityType cmds)
  ) => ServiceRunner
  { commandDefinitions :: Record.ContextRecord Record.I cmds
  , inspectDict :: Record.ContextRecord (Record.Dict CommandInspect) cmds
  }
```

### Builder API

```haskell
-- Service builders (simplified)
Service.new :: Service '[]
Service.command :: forall cmd cmds. ... => Service cmds -> Service (cmd ': cmds)

-- Application builders
Application.new :: Application

Application.add :: forall cmds.
  ( Record.AllFields cmds CommandInspect
  , Json.FromJSON (ServiceEventType cmds), Json.ToJSON (ServiceEventType cmds)
  , Json.FromJSON (ServiceEntityType cmds), Json.ToJSON (ServiceEntityType cmds)
  ) =>
  Service cmds ->
  Application ->
  Application

Application.useTransport :: Transport t => t -> Application -> Application

Application.useEventStore :: EventStoreConfig c => c -> Application -> Application

Application.useSnapshotCache :: SnapshotCacheConfig c => c -> Application -> Application
```

### Run Implementation

```haskell
__internal_runApplicationMain :: Application -> IO ()
__internal_runApplicationMain app = Task.runOrPanic do
  -- 1. Create EventStore (Json.Value for type erasure)
  eventStore <- case app.eventStoreConfig of
    Nothing -> panic "EventStore config required"
    Just (EventStoreConfigValue config) ->
      EventStore.createEventStore @_ @Json.Value config

  -- 2. Create optional SnapshotCache (Json.Value for type erasure)
  maybeCache <- case app.snapshotCacheConfig of
    Nothing -> Task.yield Nothing
    Just (SnapshotCacheConfigValue config) -> do
      cache <- SnapshotCache.createSnapshotCache @_ @Json.Value config
      Task.yield (Just cache)

  -- 3. Collect endpoints from ALL services
  allEndpoints <- app.serviceRunners
    |> Array.map (\(ServiceRunner {..}) ->
         collectEndpoints eventStore maybeCache commandDefinitions inspectDict)
    |> Task.sequence
    |> Task.map (Array.reduce Map.merge Map.empty)

  -- 4. Run the single transport with all endpoints
  case app.transport of
    Nothing -> panic "Transport config required"
    Just (TransportValue transport) -> do
      let endpoints = Endpoints { transport = transport, commandEndpoints = allEndpoints }
      let runnableTransport = assembleTransport endpoints
      runTransport transport runnableTransport
```

### collectEndpoints (refactored from runService)

Extracts the endpoint-building logic from current `runService`, but:

- Accepts pre-created EventStore/SnapshotCache
- Returns `Map Text EndpointHandler` instead of running the transport
- Handles the `unsafeCoerce` for type erasure (EventStore Json.Value → EventStore event)

## Implementation Order

1. Add `collectEndpoints` to `ServiceDefinition/Core.hs` (extract from `runService`)
2. Create `Application.hs` with types and builder API
3. Implement `__internal_runApplicationMain`
4. Update testbed to use Application pattern
5. Remove `__internal_runServiceMain` and old `runService`
6. Update cabal file

## Example Usage

```haskell
-- Cart/Service.hs
cartService :: Service _
cartService =
  Service.new
    |> Service.command @CreateCart
    |> Service.command @AddItem

-- Order/Service.hs
orderService :: Service _
orderService =
  Service.new
    |> Service.command @PlaceOrder
    |> Service.command @CancelOrder

-- Main.hs
main :: IO ()
main =
  Application.new
    |> Application.add cartService
    |> Application.add orderService
    |> Application.useTransport WebTransport.server
    |> Application.useEventStore postgresConfig
    |> Application.useSnapshotCache InMemorySnapshotCacheConfig
    |> Application.__internal_runApplicationMain
```

## Testing Strategy

- Existing testbed works with minimal changes (wrap single service in Application)
- All existing EventStore tests remain valid (they test EventStore directly)
- Manual test: Start application, verify HTTP endpoints work for all services

## Consequences

### Positive

- Clean separation: Services define commands, Application owns infrastructure
- Multiple services can share a single transport, EventStore, and SnapshotCache
- Type safety preserved at service boundaries
- Simple mental model for newcomers

### Negative

- Breaking change: existing code using `__internal_runServiceMain` must migrate
- Services can no longer be run standalone (must be wrapped in Application)
