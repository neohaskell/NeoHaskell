# Queries Implementation Plan

This document provides a step-by-step implementation guide for the Queries feature in NeoHaskell. Follow these phases in order. Each phase should be completable and testable before moving to the next.

**Reference**: [ADR-0007: Queries (Read Models)](/docs/decisions/0007-queries-read-models.md)

---

## Phase 1: EventStore Caster Function

**Goal**: Enable multiple Services with different event types to share a single EventStore by storing events as JSON internally and providing a casting function for typed access.

**Key Insight**: Instead of creating new types, we reuse existing parameterized types with `Json.Value` at the storage layer and provide a `castEventStore` function for typed access. This is simpler than a two-layer architecture.

### 1.1 Update EventStoreConfig to Create `EventStore Json.Value`

Modify the `EventStoreConfig` class to create an untyped event store that stores JSON directly.

**File**: Update `/Users/nick/Source/NeoHaskell/core/service/Service/EventStore/Core.hs`

```haskell
-- EventStoreConfig creates an EventStore that stores Json.Value directly
-- The serialization/typing happens via castEventStore
class EventStoreConfig config where
  createEventStore :: config -> Task Text (EventStore Json.Value)
```

Note: The constraint `(Json.FromJSON eventType, Json.ToJSON eventType)` is removed since we always create `EventStore Json.Value`.

### 1.2 Update InMemory Implementation

Update the InMemory implementation to store `Event Json.Value` instead of `Event eventType`. This means:

- `StreamStore` stores `Event Json.Value`
- No encoding/decoding needed internally (already Json.Value)
- The `Show eventType` constraint becomes `Show Json.Value` (which exists)

**File**: Update `/Users/nick/Source/NeoHaskell/core/service/Service/EventStore/InMemory.hs`

```haskell
module Service.EventStore.InMemory (
  new,
) where

-- ... imports ...

new :: Task Error (EventStore Json.Value)
new = do
  store <- newEmptyStreamStore
  let eventStore = EventStore
        { insert = insertImpl store,
          -- ... all other methods unchanged in structure
        }
  Task.yield eventStore

-- Internal StreamStore now uses Json.Value
data StreamStore = StreamStore
  { globalStream :: DurableChannel (Event Json.Value),
    streams :: ConcurrentVar (Map (EntityName, StreamId) (DurableChannel (Event Json.Value))),
    globalLock :: Lock,
    subscriptions :: ConcurrentVar (Map SubscriptionId (Subscription Json.Value))
  }
```

The implementation logic remains the same - just the type parameter changes from `eventType` to `Json.Value`.

### 1.3 Update Postgres Implementation

The Postgres implementation already stores JSON in the database (see `PostgresEventRecord`). The changes are minimal:

- Remove the type parameter
- Return `EventStore Json.Value`
- JSON encoding happens on insert, no decoding needed on read (return raw JSON)

**File**: Update `/Users/nick/Source/NeoHaskell/core/service/Service/EventStore/Postgres.hs`

```haskell
module Service.EventStore.Postgres (
  new,
  Config (..),
) where

-- ... imports ...

data Config = Config
  { connectionString :: Text
  }

instance EventStoreConfig Config where
  createEventStore config = new config.connectionString

new :: Text -> Task Error (EventStore Json.Value)
new connectionString = do
  -- ... implementation stores Event Json.Value
```

### 1.4 Create castEventStore Function

Create a function that wraps an `EventStore Json.Value` to provide typed access with automatic JSON encoding/decoding.

**File**: Update `/Users/nick/Source/NeoHaskell/core/service/Service/EventStore/Core.hs` (add to existing module)

```haskell
-- | Cast an untyped EventStore to a typed EventStore.
-- Handles JSON encoding on insert and decoding on read.
-- Events that fail to decode are filtered out (with optional logging).
castEventStore ::
  forall eventType.
  (Json.FromJSON eventType, Json.ToJSON eventType) =>
  EventStore Json.Value ->
  EventStore eventType
castEventStore store = EventStore
  { insert = \payload -> do
      let rawPayload = encodeInsertionPayload payload
      store.insert rawPayload,

    readStreamForwardFrom = \name id pos limit -> do
      rawStream <- store.readStreamForwardFrom name id pos limit
      rawStream |> Stream.mapMaybe (decodeStreamMessage @eventType) |> Task.yield,

    readStreamBackwardFrom = \name id pos limit -> do
      rawStream <- store.readStreamBackwardFrom name id pos limit
      rawStream |> Stream.mapMaybe (decodeStreamMessage @eventType) |> Task.yield,

    readAllStreamEvents = \name id -> do
      rawStream <- store.readAllStreamEvents name id
      rawStream |> Stream.mapMaybe (decodeStreamMessage @eventType) |> Task.yield,

    readAllEventsForwardFrom = \pos limit -> do
      rawStream <- store.readAllEventsForwardFrom pos limit
      rawStream |> Stream.mapMaybe (decodeAllMessage @eventType) |> Task.yield,

    readAllEventsBackwardFrom = \pos limit -> do
      rawStream <- store.readAllEventsBackwardFrom pos limit
      rawStream |> Stream.mapMaybe (decodeAllMessage @eventType) |> Task.yield,

    readAllEventsForwardFromFiltered = \pos limit names -> do
      rawStream <- store.readAllEventsForwardFromFiltered pos limit names
      rawStream |> Stream.mapMaybe (decodeAllMessage @eventType) |> Task.yield,

    readAllEventsBackwardFromFiltered = \pos limit names -> do
      rawStream <- store.readAllEventsBackwardFromFiltered pos limit names
      rawStream |> Stream.mapMaybe (decodeAllMessage @eventType) |> Task.yield,

    subscribeToAllEvents = \handler -> do
      store.subscribeToAllEvents (decodeAndHandle @eventType handler),

    subscribeToAllEventsFromPosition = \pos handler -> do
      store.subscribeToAllEventsFromPosition pos (decodeAndHandle @eventType handler),

    subscribeToAllEventsFromStart = \handler -> do
      store.subscribeToAllEventsFromStart (decodeAndHandle @eventType handler),

    subscribeToEntityEvents = \name handler -> do
      store.subscribeToEntityEvents name (decodeAndHandle @eventType handler),

    subscribeToStreamEvents = \name id handler -> do
      store.subscribeToStreamEvents name id (decodeAndHandle @eventType handler),

    unsubscribe = store.unsubscribe,

    truncateStream = store.truncateStream
  }

-- | Encode a typed InsertionPayload to a JSON InsertionPayload.
encodeInsertionPayload ::
  (Json.ToJSON eventType) =>
  InsertionPayload eventType ->
  InsertionPayload Json.Value
encodeInsertionPayload payload = InsertionPayload
  { streamId = payload.streamId,
    entityName = payload.entityName,
    insertionType = payload.insertionType,
    insertions = payload.insertions |> Array.map encodeInsertion
  }

encodeInsertion :: (Json.ToJSON eventType) => Insertion eventType -> Insertion Json.Value
encodeInsertion insertion = Insertion
  { id = insertion.id,
    event = Json.toJSON insertion.event,
    metadata = insertion.metadata
  }

-- | Decode a JSON Event to a typed Event.
decodeEvent ::
  forall eventType.
  (Json.FromJSON eventType) =>
  Event Json.Value ->
  Maybe (Event eventType)
decodeEvent rawEvent = do
  case Json.fromJSON rawEvent.event of
    Json.Success typedPayload -> Just Event
      { entityName = rawEvent.entityName,
        streamId = rawEvent.streamId,
        event = typedPayload,
        metadata = rawEvent.metadata
      }
    Json.Error _ -> Nothing

-- | Decode a ReadStreamMessage, filtering out events that fail to decode.
decodeStreamMessage ::
  forall eventType.
  (Json.FromJSON eventType) =>
  ReadStreamMessage Json.Value ->
  Maybe (ReadStreamMessage eventType)
decodeStreamMessage message =
  case message of
    StreamReadingStarted -> Just StreamReadingStarted
    StreamEvent rawEvent -> decodeEvent @eventType rawEvent |> Maybe.map StreamEvent
    ToxicStreamEvent contents -> Just (ToxicStreamEvent contents)
    StreamCheckpoint pos -> Just (StreamCheckpoint pos)
    StreamTerminated reason -> Just (StreamTerminated reason)
    StreamCaughtUp -> Just StreamCaughtUp
    StreamFellBehind -> Just StreamFellBehind

-- | Decode a ReadAllMessage, filtering out events that fail to decode.
decodeAllMessage ::
  forall eventType.
  (Json.FromJSON eventType) =>
  ReadAllMessage Json.Value ->
  Maybe (ReadAllMessage eventType)
decodeAllMessage message =
  case message of
    ReadingStarted -> Just ReadingStarted
    AllEvent rawEvent -> decodeEvent @eventType rawEvent |> Maybe.map AllEvent
    ToxicAllEvent contents -> Just (ToxicAllEvent contents)
    Checkpoint pos -> Just (Checkpoint pos)
    Terminated reason -> Just (Terminated reason)
    CaughtUp -> Just CaughtUp
    FellBehind -> Just FellBehind

-- | Wrap a typed handler to accept JSON events, decoding and filtering.
decodeAndHandle ::
  forall eventType.
  (Json.FromJSON eventType) =>
  (Event eventType -> Task Text Unit) ->
  (Event Json.Value -> Task Text Unit)
decodeAndHandle handler rawEvent = do
  case decodeEvent @eventType rawEvent of
    Just typedEvent -> handler typedEvent
    Nothing -> Task.yield unit  -- Skip events that don't match our type
```

### 1.5 Update Existing Code That Uses EventStore

Update code that creates event stores to use the new pattern:

```haskell
-- OLD: EventStore created with type parameter via config
-- eventStore <- createEventStore @MyEvent config

-- NEW: Create JSON store, then cast to typed store (one-liner)
eventStore <- createEventStore config |> Task.map (castEventStore @MyEvent)
```

This change affects:

- `Service.CommandExecutor` - where the command executor creates the event store
- Any tests that create event stores directly
- Any integration code that uses `createEventStore`

### 1.6 Update Existing Tests

All existing EventStore tests should continue to pass. The test setup changes from:

```haskell
-- OLD
eventStore <- InMemory.new @TestEvent

-- NEW
eventStore <- InMemory.new |> Task.map (castEventStore @TestEvent)
```

Run:

```bash
cabal test nhcore-test
```

**Verification Checklist**:

- [ ] All `Service/EventStore/InMemorySpec.hs` tests pass
- [ ] All `Service/EventStore/PostgresSpec.hs` tests pass
- [ ] All EventStore property tests pass
- [ ] Existing service integration tests pass

### Benefits of This Approach

1. **No new types** - Reuse `Event`, `InsertionPayload`, `EventStore` with `Json.Value`
2. **No new modules** - `castEventStore` lives in `EventStore.Core`
3. **Minimal implementation changes** - Implementations just drop the type parameter
4. **Query subscribers use `EventStore Json.Value` directly** - No decoding needed for routing
5. **Services can share a single store** - Create once, cast multiple times for different event types

---

## Phase 2: Core Query Types

**Goal**: Define the fundamental types for the Query system.

### 2.1 Create QueryAction Sum Type

**File**: `/Users/nick/Source/NeoHaskell/core/service/Service/Query/Core.hs`

```haskell
module Service.Query.Core (
  QueryAction (..),
  Query,
  QueryOf (..),
) where

import Basics
import Uuid (Uuid)

-- | Represents the outcome of combining an entity with a query.
data QueryAction query
  = Update query   -- Store/update this query instance
  | Delete         -- Remove this query instance
  | NoOp           -- Take no action
  deriving (Eq, Show, Generic)

-- | Marker typeclass for query types.
-- Generated by Template Haskell.
class Query query

-- | Defines how an entity contributes to a query.
class (Entity entity, Query query) => QueryOf entity query where
  -- | Extract the query instance ID from an entity.
  -- For UserOrders, both User and Order return the user's ID.
  queryId :: entity -> Uuid

  -- | Combine entity state with existing query state.
  -- Called when an entity changes to update the corresponding query.
  combine :: entity -> Maybe query -> QueryAction query
```

### 2.2 Create EntitiesOf Type Family

Add to the same file:

```haskell
-- | Maps a query type to its constituent entity types.
-- Generated by TH: type instance EntitiesOf UserOrders = '[User, Order]
type family EntitiesOf query :: [Type]
```

### 2.3 Create Query Re-export Module

**File**: `/Users/nick/Source/NeoHaskell/core/service/Service/Query.hs`

```haskell
module Service.Query (
  module Reexported,
) where

import Service.Query.Core as Reexported
```

### 2.4 Add to nhcore.cabal

Update `/Users/nick/Source/NeoHaskell/core/nhcore.cabal` exposed-modules:

```cabal
  exposed-modules:
    -- ... existing modules ...
    Service.Query
    Service.Query.Core
```

**Verification Checklist**:

- [ ] Module compiles without errors
- [ ] Types are importable from `Service.Query`

---

## Phase 3: QueryObjectStore

**Goal**: Create the storage abstraction for query instances.

### 3.1 Create QueryObjectStore Interface

**File**: `/Users/nick/Source/NeoHaskell/core/service/Service/QueryObjectStore/Core.hs`

```haskell
module Service.QueryObjectStore.Core (
  QueryObjectStore (..),
  Error (..),
  QueryObjectStoreConfig (..),
) where

import Basics
import Array (Array)
import Json qualified
import Task (Task)
import Text (Text)
import Uuid (Uuid)

data Error
  = StorageError Text
  | SerializationError Text
  deriving (Eq, Show)

-- | Storage interface for query instances.
-- Similar pattern to SnapshotCache but for queries.
data QueryObjectStore query = QueryObjectStore
  { -- | Get a query instance by ID.
    get :: Uuid -> Task Error (Maybe query),

    -- | Atomically update a query instance.
    -- The function receives the current value (or Nothing) and returns the new value.
    -- Critical for handling concurrent updates to the same query instance.
    atomicUpdate :: Uuid -> (Maybe query -> Maybe query) -> Task Error Unit,

    -- | Delete a query instance by ID.
    delete :: Uuid -> Task Error Unit,

    -- | Get all query instances.
    -- Used for HTTP endpoint: GET /queries/{query-name}
    getAll :: Task Error (Array query)
  }

class QueryObjectStoreConfig config where
  createQueryObjectStore ::
    (Json.FromJSON query, Json.ToJSON query) =>
    config ->
    Task Text (QueryObjectStore query)
```

### 3.2 Create InMemory Implementation

**File**: `/Users/nick/Source/NeoHaskell/core/service/Service/QueryObjectStore/InMemory.hs`

```haskell
module Service.QueryObjectStore.InMemory (
  new,
  InMemoryQueryObjectStoreConfig (..),
) where

import Basics
import Array qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Map (Map)
import Map qualified
import Service.QueryObjectStore.Core
import Task (Task)
import Task qualified
import Uuid (Uuid)

data InMemoryQueryObjectStoreConfig = InMemoryQueryObjectStoreConfig

instance QueryObjectStoreConfig InMemoryQueryObjectStoreConfig where
  createQueryObjectStore InMemoryQueryObjectStoreConfig = new

new :: Task Error (QueryObjectStore query)
new = do
  store <- ConcurrentVar.containing Map.empty
  Task.yield QueryObjectStore
    { get = getImpl store,
      atomicUpdate = atomicUpdateImpl store,
      delete = deleteImpl store,
      getAll = getAllImpl store
    }

getImpl :: ConcurrentVar (Map Uuid query) -> Uuid -> Task Error (Maybe query)
getImpl store queryId = do
  storeMap <- ConcurrentVar.peek store
  Task.yield (storeMap |> Map.get queryId)

atomicUpdateImpl :: ConcurrentVar (Map Uuid query) -> Uuid -> (Maybe query -> Maybe query) -> Task Error Unit
atomicUpdateImpl store queryId updateFn = do
  store |> ConcurrentVar.modify \storeMap -> do
    let currentValue = storeMap |> Map.get queryId
    let newValue = updateFn currentValue
    case newValue of
      Just query -> storeMap |> Map.set queryId query
      Nothing -> storeMap |> Map.remove queryId
  Task.yield unit

deleteImpl :: ConcurrentVar (Map Uuid query) -> Uuid -> Task Error Unit
deleteImpl store queryId = do
  store |> ConcurrentVar.modify (Map.remove queryId)
  Task.yield unit

getAllImpl :: ConcurrentVar (Map Uuid query) -> Task Error (Array query)
getAllImpl store = do
  storeMap <- ConcurrentVar.peek store
  storeMap
    |> Map.values
    |> Array.fromLinkedList
    |> Task.yield
```

### 3.3 Create Re-export Module

**File**: `/Users/nick/Source/NeoHaskell/core/service/Service/QueryObjectStore.hs`

```haskell
module Service.QueryObjectStore (
  module Reexported,
) where

import Service.QueryObjectStore.Core as Reexported
```

### 3.4 Add Tests for QueryObjectStore

**File**: `/Users/nick/Source/NeoHaskell/core/test/Service/QueryObjectStore/InMemorySpec.hs`

Create tests following the pattern in `/Users/nick/Source/NeoHaskell/core/test/Service/SnapshotCache/InMemorySpec.hs`:

- Test `get` returns `Nothing` for non-existent ID
- Test `atomicUpdate` creates new entry when `Nothing`
- Test `atomicUpdate` updates existing entry
- Test `atomicUpdate` handles concurrent updates correctly
- Test `delete` removes entry
- Test `getAll` returns all entries

**Verification Checklist**:

- [ ] InMemory implementation compiles
- [ ] All QueryObjectStore tests pass
- [ ] Concurrent update test demonstrates atomicity

---

## Phase 4: Query Subscriber

**Goal**: Create the subscription mechanism that updates queries when events occur.

### 4.1 Create QueryRegistry Module

The registry maps EntityName to a list of query updater functions. This is populated by Template Haskell.

**File**: `/Users/nick/Source/NeoHaskell/core/service/Service/Query/Registry.hs`

```haskell
module Service.Query.Registry (
  QueryRegistry,
  QueryUpdater (..),
  empty,
  register,
  getUpdatersForEntity,
) where

import Basics
import Array (Array)
import Array qualified
import Map (Map)
import Map qualified
import Json qualified
import Service.Event (Event)
import Service.Event.EntityName (EntityName)
import Task (Task)

-- | A function that updates a query when an entity changes.
-- Receives the raw event (Event Json.Value) and handles deserialization,
-- entity reconstruction, and query update internally.
data QueryUpdater = QueryUpdater
  { queryName :: Text,
    updateQuery :: Event Json.Value -> Task Text Unit
  }

-- | Maps entity names to their associated query updaters.
newtype QueryRegistry = QueryRegistry (Map EntityName (Array QueryUpdater))

empty :: QueryRegistry
empty = QueryRegistry Map.empty

register :: EntityName -> QueryUpdater -> QueryRegistry -> QueryRegistry
register entityName updater (QueryRegistry registry) = do
  let currentUpdaters = registry |> Map.get entityName |> Maybe.withDefault Array.empty
  let newUpdaters = currentUpdaters |> Array.append updater
  QueryRegistry (registry |> Map.set entityName newUpdaters)

getUpdatersForEntity :: EntityName -> QueryRegistry -> Array QueryUpdater
getUpdatersForEntity entityName (QueryRegistry registry) =
  registry |> Map.get entityName |> Maybe.withDefault Array.empty
```

### 4.2 Create Query Subscriber

The subscriber uses `EventStore Json.Value` directly (no casting needed for routing).

**File**: `/Users/nick/Source/NeoHaskell/core/service/Service/Query/Subscriber.hs`

```haskell
module Service.Query.Subscriber (
  QuerySubscriber (..),
  new,
  start,
  rebuildAll,
) where

import Basics
import Array qualified
import Console qualified
import Json qualified
import Service.Event (Event)
import Service.Event.EntityName (EntityName)
import Service.Event.StreamPosition (StreamPosition (..))
import Service.EventStore.Core (EventStore)
import Service.EventStore.Core qualified as EventStore
import Service.Query.Registry (QueryRegistry, QueryUpdater (..))
import Service.Query.Registry qualified as Registry
import Stream qualified
import Task (Task)
import Task qualified

data QuerySubscriber = QuerySubscriber
  { eventStore :: EventStore Json.Value,
    registry :: QueryRegistry,
    lastProcessedPosition :: ConcurrentVar (Maybe StreamPosition)
  }

new :: EventStore Json.Value -> QueryRegistry -> Task Text QuerySubscriber
new eventStore registry = do
  lastProcessedPosition <- ConcurrentVar.containing Nothing
  Task.yield QuerySubscriber
    { eventStore,
      registry,
      lastProcessedPosition
    }

-- | Rebuild all queries from the beginning of the event store.
-- Called on application startup before starting live subscription.
rebuildAll :: QuerySubscriber -> Task Text Unit
rebuildAll subscriber = do
  Console.print "Starting query rebuild from event store..."

  -- Read all events from the beginning
  eventStream <- subscriber.eventStore.readAllEventsForwardFrom (StreamPosition 0) (EventStore.Limit maxValue)
    |> Task.mapError toText

  -- Process each event
  maybeLastPosition <- eventStream
    |> Stream.consumeMaybe
      ( \lastPos message -> do
          case message of
            EventStore.AllEvent rawEvent -> do
              processEvent subscriber rawEvent
              let eventPos = rawEvent.metadata.globalPosition
              Task.yield eventPos
            _ -> Task.yield lastPos
      )
      Nothing
    |> Task.mapError toText

  -- Record the last processed position
  case maybeLastPosition of
    Just pos -> do
      subscriber.lastProcessedPosition |> ConcurrentVar.set (Just pos)
      Console.print [fmt|Query rebuild complete. Last position: #{pos}|]
    Nothing ->
      Console.print "Query rebuild complete. No events found."

-- | Start live subscription for query updates.
-- Should be called after rebuildAll completes.
start :: QuerySubscriber -> Task Text Unit
start subscriber = do
  maybeStartPosition <- ConcurrentVar.peek subscriber.lastProcessedPosition

  let startPosition = case maybeStartPosition of
        Just (StreamPosition pos) -> StreamPosition (pos + 1)
        Nothing -> StreamPosition 0

  Console.print [fmt|Starting query subscriber from position #{startPosition}|]

  _subscriptionId <- subscriber.eventStore.subscribeToAllEventsFromPosition
    startPosition
    (processEvent subscriber)
    |> Task.mapError toText

  Task.yield unit

-- | Process a single raw event through all relevant query updaters.
processEvent :: QuerySubscriber -> Event Json.Value -> Task Text Unit
processEvent subscriber rawEvent = do
  let entityName = rawEvent.entityName
  let updaters = Registry.getUpdatersForEntity entityName subscriber.registry

  -- Run all updaters for this entity type
  updaters |> Task.forEach \updater -> do
    result <- updater.updateQuery rawEvent |> Task.asResult
    case result of
      Ok _ -> pass
      Err errorText ->
        Console.print [fmt|Warning: Query updater #{updater.queryName} failed: #{errorText}|]
          |> Task.ignoreError
```

### 4.3 Create QueryUpdater Factory

This module creates `QueryUpdater` instances for a specific query type.

**File**: `/Users/nick/Source/NeoHaskell/core/service/Service/Query/Updater.hs`

```haskell
module Service.Query.Updater (
  createUpdater,
) where

import Basics
import Json qualified
import Service.Entity.Core (Entity (..))
import Service.EntityFetcher.Core (EntityFetcher, EntityFetchResult (..))
import Service.Event (Event)
import Service.Query.Core (Query, QueryOf (..), QueryAction (..))
import Service.Query.Registry (QueryUpdater (..))
import Service.QueryObjectStore.Core (QueryObjectStore)
import Task (Task)
import Task qualified

-- | Create a QueryUpdater for a specific entity-query relationship.
createUpdater ::
  forall entity query.
  ( Entity entity,
    Query query,
    QueryOf entity query,
    Json.FromJSON (EventOf entity)
  ) =>
  Text ->  -- Query name for logging
  EntityFetcher entity (EventOf entity) ->
  QueryObjectStore query ->
  QueryUpdater
createUpdater queryName entityFetcher queryStore = QueryUpdater
  { queryName = queryName,
    updateQuery = \rawEvent -> do
      -- Reconstruct the entity from its event stream
      let entityName = rawEvent.entityName
      let streamId = rawEvent.streamId

      fetchResult <- entityFetcher.fetch entityName streamId
        |> Task.mapError toText

      case fetchResult of
        EntityNotFound ->
          -- Entity doesn't exist, nothing to update
          Task.yield unit
        EntityFound fetchedEntity -> do
          let entity = fetchedEntity.state
          let targetQueryId = queryId @entity @query entity

          -- Atomically update the query
          queryStore.atomicUpdate targetQueryId \maybeExistingQuery -> do
            case combine @entity @query entity maybeExistingQuery of
              Update newQuery -> Just newQuery
              Delete -> Nothing
              NoOp -> maybeExistingQuery

          Task.yield unit
  }
```

**Verification Checklist**:

- [ ] QueryRegistry stores and retrieves updaters correctly
- [ ] QuerySubscriber processes events and routes to correct updaters
- [ ] RebuildAll processes all historical events
- [ ] Live subscription starts from correct position after rebuild

---

## Phase 5: Template Haskell for Queries

**Goal**: Create the `deriveQuery` TH function that generates boilerplate.

### 5.1 Create deriveQuery Template Haskell

**File**: `/Users/nick/Source/NeoHaskell/core/service/Service/Query/TH.hs`

```haskell
module Service.Query.TH (
  deriveQuery,
) where

import Control.Monad.Fail qualified as MonadFail
import Core
import Data.Hashable qualified as Hashable
import GHC.Base (String)
import Language.Haskell.TH.Lib qualified as THLib
import Language.Haskell.TH.Syntax qualified as TH
import Service.CommandExecutor.TH (deriveKnownHash)
import Text qualified

-- | Derive Query-related instances for a query type.
--
-- Usage:
--   deriveQuery ''UserOrders ['User, 'Order]
--
-- Generates:
--   - type instance NameOf UserOrders = "user-orders"
--   - type instance EntitiesOf UserOrders = '[User, Order]
--   - instance Query UserOrders
--   - instance KnownHash "UserOrders"
deriveQuery :: TH.Name -> [TH.Name] -> THLib.DecsQ
deriveQuery queryTypeName entityTypeNames = do
  let queryTypeStr = TH.nameBase queryTypeName

  -- Convert to kebab-case for URL: UserOrders -> user-orders
  -- Use Text.toKebabCase from nhcore
  let kebabName = Text.toKebabCase (Text.fromLegacy queryTypeStr) |> Text.toLegacy

  -- Lookup required type families and classes
  nameOfTypeFamilyName <- lookupOrFail "NameOf"
  entitiesOfTypeFamilyName <- lookupOrFail "EntitiesOf"
  queryClassName <- lookupOrFail "Query"

  -- Generate: type instance NameOf QueryType = "query-type"
  let nameOfInstance = TH.TySynInstD
        (TH.TySynEqn Nothing
          (TH.ConT nameOfTypeFamilyName `TH.AppT` TH.ConT queryTypeName)
          (TH.LitT (TH.StrTyLit kebabName)))

  -- Generate: type instance EntitiesOf QueryType = '[Entity1, Entity2]
  let entityTypeList = foldr
        (\entityName acc -> TH.PromotedConsT `TH.AppT` TH.ConT entityName `TH.AppT` acc)
        TH.PromotedNilT
        entityTypeNames
  let entitiesOfInstance = TH.TySynInstD
        (TH.TySynEqn Nothing
          (TH.ConT entitiesOfTypeFamilyName `TH.AppT` TH.ConT queryTypeName)
          entityTypeList)

  -- Generate: instance Query QueryType
  let queryInstance = TH.InstanceD
        Nothing
        []
        (TH.ConT queryClassName `TH.AppT` TH.ConT queryTypeName)
        []

  -- Generate KnownHash instance
  knownHashInstances <- deriveKnownHash queryTypeStr

  pure ([nameOfInstance, entitiesOfInstance, queryInstance] ++ knownHashInstances)

lookupOrFail :: String -> TH.Q TH.Name
lookupOrFail name = do
  result <- TH.lookupTypeName name
  case result of
    Just n -> pure n
    Nothing -> MonadFail.fail [fmt|Could not find type: #{name}. Ensure you have `import Core` at the top of your module.|]
```

### 5.2 Add Query TH to Re-exports

Update `/Users/nick/Source/NeoHaskell/core/service/Service/Query.hs`:

```haskell
module Service.Query (
  module Core,
  module TH,
) where

import Service.Query.Core as Core
import Service.Query.TH as TH (deriveQuery)
```

**Verification Checklist**:

- [ ] `deriveQuery` generates correct `NameOf` instance
- [ ] `deriveQuery` generates correct `EntitiesOf` type instance
- [ ] `deriveQuery` generates `Query` marker instance
- [ ] `deriveQuery` generates `KnownHash` instance

---

## Phase 6: Application Layer

**Goal**: Create the Application type that combines multiple Services and Queries.

### 6.1 Create Application Type

**File**: `/Users/nick/Source/NeoHaskell/core/service/Service/Application.hs`

```haskell
module Service.Application (
  Application,
  new,
  withService,
  withQuery,
  useEventStore,
  useSnapshotCache,
  useQueryObjectStore,
  useServer,
  run,
) where

import Basics
import Array (Array)
import Array qualified
import Map (Map)
import Map qualified
import Service.Query.Registry (QueryRegistry)
import Service.Query.Registry qualified as Registry
import Service.EventStore.Core (EventStoreConfig, EventStore)
import Service.SnapshotCache.Core (SnapshotCacheConfig)
import Service.QueryObjectStore.Core (QueryObjectStoreConfig)
import Service.Transport (Transport)
import Task (Task)
import Json qualified

-- | Application combines multiple Services and Queries with shared infrastructure.
data Application
  (services :: [Type])
  (queries :: [Type])
  (eventStoreConfig :: Type)
  (snapshotCacheConfig :: Type)
  (queryObjectStoreConfig :: Type)
  (transports :: [Type])
  = Application
  { services :: Array ServiceRunner,
    queryRegistry :: QueryRegistry,
    eventStoreConfig :: eventStoreConfig,
    snapshotCacheConfig :: Maybe snapshotCacheConfig,
    queryObjectStoreConfig :: Maybe queryObjectStoreConfig,
    transports :: Array TransportRunner
  }

-- Existential wrappers for heterogeneous collections
data ServiceRunner = forall service. ServiceRunner
  { runService :: EventStore Json.Value -> Maybe SnapshotCache -> Task Text Unit
  }

data TransportRunner = forall transport. Transport transport => TransportRunner
  { transport :: transport
  }

-- | Create a new empty Application.
new :: Application '[] '[] Unit Unit Unit '[]
new = Application
  { services = Array.empty,
    queryRegistry = Registry.empty,
    eventStoreConfig = unit,
    snapshotCacheConfig = Nothing,
    queryObjectStoreConfig = Nothing,
    transports = Array.empty
  }

-- | Add a Service to the Application.
-- Services no longer configure their own EventStore; they receive it from the Application.
withService ::
  forall service services queries eventStoreConfig snapshotCacheConfig queryObjectStoreConfig transports.
  service ->
  Application services queries eventStoreConfig snapshotCacheConfig queryObjectStoreConfig transports ->
  Application (service ': services) queries eventStoreConfig snapshotCacheConfig queryObjectStoreConfig transports
withService service app = do
  -- TODO: Create ServiceRunner that can run the service with a shared EventStore Json.Value
  app { services = app.services }  -- Placeholder

-- | Register a Query type with the Application.
withQuery ::
  forall query services queries eventStoreConfig snapshotCacheConfig queryObjectStoreConfig transports.
  (Query query) =>
  Application services queries eventStoreConfig snapshotCacheConfig queryObjectStoreConfig transports ->
  Application services (query ': queries) eventStoreConfig snapshotCacheConfig queryObjectStoreConfig transports
withQuery app = do
  -- TODO: Register query updaters in the registry
  app

-- | Configure the EventStore for the Application.
-- Creates an EventStore Json.Value that all services share.
useEventStore ::
  forall config services queries snapshotCacheConfig queryObjectStoreConfig transports.
  (EventStoreConfig config) =>
  config ->
  Application services queries _ snapshotCacheConfig queryObjectStoreConfig transports ->
  Application services queries config snapshotCacheConfig queryObjectStoreConfig transports
useEventStore config app = app { eventStoreConfig = config }

-- | Configure the SnapshotCache for the Application.
useSnapshotCache ::
  forall config services queries eventStoreConfig queryObjectStoreConfig transports.
  (SnapshotCacheConfig config) =>
  config ->
  Application services queries eventStoreConfig _ queryObjectStoreConfig transports ->
  Application services queries eventStoreConfig config queryObjectStoreConfig transports
useSnapshotCache config app = app { snapshotCacheConfig = Just config }

-- | Configure the QueryObjectStore for the Application.
useQueryObjectStore ::
  forall config services queries eventStoreConfig snapshotCacheConfig transports.
  (QueryObjectStoreConfig config) =>
  config ->
  Application services queries eventStoreConfig snapshotCacheConfig _ transports ->
  Application services queries eventStoreConfig snapshotCacheConfig config transports
useQueryObjectStore config app = app { queryObjectStoreConfig = Just config }

-- | Add a transport (HTTP server, CLI, etc.) to the Application.
useServer ::
  forall transport services queries eventStoreConfig snapshotCacheConfig queryObjectStoreConfig transports.
  (Transport transport) =>
  transport ->
  Application services queries eventStoreConfig snapshotCacheConfig queryObjectStoreConfig transports ->
  Application services queries eventStoreConfig snapshotCacheConfig queryObjectStoreConfig (transport ': transports)
useServer transport app = do
  app { transports = app.transports |> Array.append (TransportRunner transport) }

-- | Run the Application.
-- Startup sequence:
-- 1. Initialize EventStore Json.Value (shared by all services)
-- 2. Initialize SnapshotCache
-- 3. Initialize QueryObjectStore
-- 4. Rebuild all Queries from EventStore
-- 5. Start Query subscriber
-- 6. Start HTTP server with Service + Query endpoints
run ::
  forall services queries eventStoreConfig snapshotCacheConfig queryObjectStoreConfig transports.
  ( EventStoreConfig eventStoreConfig,
    SnapshotCacheConfig snapshotCacheConfig,
    QueryObjectStoreConfig queryObjectStoreConfig
  ) =>
  Application services queries eventStoreConfig snapshotCacheConfig queryObjectStoreConfig transports ->
  Task Text Unit
run app = do
  Console.print "Starting Application..."

  -- 1. Initialize EventStore Json.Value (shared by all services)
  eventStore <- createEventStore app.eventStoreConfig
  Console.print "EventStore initialized."

  -- 2. Initialize SnapshotCache (if configured)
  maybeSnapshotCache <- case app.snapshotCacheConfig of
    Just config -> do
      cache <- createSnapshotCache config
      Console.print "SnapshotCache initialized."
      Task.yield (Just cache)
    Nothing -> Task.yield Nothing

  -- 3. Initialize QueryObjectStore (if configured)
  maybeQueryStore <- case app.queryObjectStoreConfig of
    Just config -> do
      store <- createQueryObjectStore config
      Console.print "QueryObjectStore initialized."
      Task.yield (Just store)
    Nothing -> Task.yield Nothing

  -- 4. Create and rebuild Query subscriber
  querySubscriber <- QuerySubscriber.new eventStore app.queryRegistry
  QuerySubscriber.rebuildAll querySubscriber

  -- 5. Start live Query subscription
  QuerySubscriber.start querySubscriber

  -- 6. Run services and start transports
  -- Services receive the shared EventStore Json.Value and use castEventStore internally
  -- TODO: Start all services with shared infrastructure
  -- TODO: Start all transports with combined endpoints

  Console.print "Application started."

  -- Keep running (transports handle their own event loops)
  Task.yield unit
```

### 6.2 Update Service to Not Require EventStore Configuration

Modify `/Users/nick/Source/NeoHaskell/core/service/Service/ServiceDefinition/Core.hs` to make EventStore and SnapshotCache configuration optional at the Service level. Services receive these from the Application.

This is a significant refactor. The key changes:

- Remove `eventStoreConfig` from `Service` type
- Remove `snapshotCacheConfig` from `Service` type
- Add a new function `createServiceRunner` that returns a function `(EventStore Json.Value, Maybe SnapshotCache) -> Task Text Unit`

**Verification Checklist**:

- [ ] Application type compiles
- [ ] `new`, `withService`, `withQuery` chain correctly
- [ ] `useEventStore`, `useSnapshotCache`, `useQueryObjectStore` configure correctly
- [ ] `run` executes the correct startup sequence

---

## Phase 7: Query HTTP Endpoints

**Goal**: Auto-generate HTTP endpoints for querying read models.

### 7.1 Extend Transport to Support Query Endpoints

Update `/Users/nick/Source/NeoHaskell/core/service/Service/Transport.hs`:

```haskell
data Endpoints transport = Endpoints
  { transport :: transport,
    commandEndpoints :: Map Text EndpointHandler,
    queryEndpoints :: Map Text QueryEndpointHandler  -- NEW
  }

-- Handler for query endpoints: returns JSON array of all query instances
type QueryEndpointHandler = Task Text Bytes
```

### 7.2 Update WebTransport to Handle Query Routes

Update `/Users/nick/Source/NeoHaskell/core/service/Service/Transport/Web.hs`:

Add handling for `GET /queries/{query-name}`:

```haskell
assembleTransport endpoints request respond = do
  case Wai.pathInfo request of
    -- Existing command handling
    ["commands", commandName] -> ...

    -- NEW: Query handling
    ["queries", queryName] -> do
      case Map.get queryName endpoints.queryEndpoints of
        Just handler -> do
          responseBytes <- handler
          let response200 =
                responseBytes
                  |> Bytes.toLazyLegacy
                  |> Wai.responseLBS HTTP.status200 [(HTTP.hContentType, "application/json")]
          respond response200
        Nothing ->
          notFound [fmt|Query not found: #{queryName}|]

    _ -> notFound "Not found"
```

### 7.3 Create Query Endpoint Handler Factory

**File**: `/Users/nick/Source/NeoHaskell/core/service/Service/Query/Endpoint.hs`

```haskell
module Service.Query.Endpoint (
  createQueryEndpoint,
) where

import Basics
import Bytes (Bytes)
import Json qualified
import Service.Query.Core (Query)
import Service.QueryObjectStore.Core (QueryObjectStore)
import Task (Task)

-- | Create an endpoint handler for a query type.
-- Returns all instances of the query as a JSON array.
createQueryEndpoint ::
  forall query.
  (Query query, Json.ToJSON query) =>
  QueryObjectStore query ->
  Task Text Bytes
createQueryEndpoint queryStore = do
  allQueries <- queryStore.getAll
    |> Task.mapError toText

  let jsonArray = Json.toJSON allQueries
  let responseBytes = Json.encode jsonArray |> Bytes.fromLazyLegacy

  Task.yield responseBytes
```

**Verification Checklist**:

- [ ] GET /queries/{query-name} returns 200 with JSON array
- [ ] GET /queries/unknown returns 404
- [ ] Query endpoint integrates with WebTransport

---

## Phase 8: Service Simplification

**Goal**: Remove EventStore/SnapshotCache/Server configuration from Service, as these are now at Application level.

### 8.1 Simplify Service Type

Update `/Users/nick/Source/NeoHaskell/core/service/Service/ServiceDefinition/Core.hs`:

Remove:

- `useEventStore` function
- `useSnapshotCache` function
- `useServer` function
- `eventStoreConfig` field from Service type
- `snapshotCacheConfig` field from Service type
- `transports` field from Service type

The Service type becomes simpler:

```haskell
data Service (commandRow :: Record.Row Type) = Service
  { commandDefinitions :: Record commandRow,
    inspectDict :: Record.ContextRecord (Record.Dict CommandInspect) commandRow
  }
```

### 8.2 Update Service to Work with Application

Create a new function that returns what the Application needs to run the service:

```haskell
-- | Prepare a service to be run by an Application.
-- Returns the entity name and a function that creates command handlers.
-- The service uses castEventStore internally to get typed access.
prepareForApplication ::
  forall cmds event entity.
  ( event ~ ServiceEventType cmds,
    entity ~ ServiceEntityType cmds
  ) =>
  Service cmds ->
  ( EntityName,
    EventStore Json.Value -> Maybe (SnapshotCache entity) -> Map Text EndpointHandler
  )
```

### 8.3 Update Testbed

Update any existing integration tests or examples that use the old Service API to use the new Application-based approach.

**Verification Checklist**:

- [ ] Service compiles without EventStore/SnapshotCache configuration
- [ ] Application can run Services correctly
- [ ] Existing tests pass after migration

---

## Phase 9: Integration Testing

**Goal**: Verify the complete Queries feature works end-to-end.

### 9.1 Create End-to-End Test

**File**: `/Users/nick/Source/NeoHaskell/core/test/Service/QueryIntegrationSpec.hs`

Test scenario with two services (User and Order) and one query (UserOrders):

```haskell
spec :: Spec
spec = do
  describe "Query Integration" do
    it "updates UserOrders query when User is created" do
      -- Setup Application with UserService, OrderService, and UserOrders query
      -- Create a user via command
      -- Verify UserOrders query contains the user

    it "updates UserOrders query when Order is added" do
      -- Create user, create order
      -- Verify UserOrders query contains both

    it "rebuilds queries on startup" do
      -- Create some events
      -- Simulate restart (create new QuerySubscriber)
      -- Verify queries are rebuilt correctly

    it "handles concurrent query updates" do
      -- Create user
      -- Rapidly create multiple orders concurrently
      -- Verify all orders appear in UserOrders query
```

### 9.2 Test Query Rebuild on Startup

Verify that when the Application starts:

1. All historical events are processed
2. Queries are fully populated before live subscription starts
3. No events are missed between rebuild and live subscription

### 9.3 Test Concurrent Query Updates

Use property-based testing to verify that concurrent updates to the same query instance are handled correctly via `atomicUpdate`.

### 9.4 Test Query Deletion and Recreation

Verify that when `combine` returns `Delete`:

1. The query instance is removed from QueryObjectStore
2. Subsequent entity changes can recreate the query

**Verification Checklist**:

- [ ] End-to-end test with multiple services and queries passes
- [ ] Query rebuild test passes
- [ ] Concurrent update test passes
- [ ] Query deletion/recreation test passes

---

## Module Summary

New modules to create:

| Module Path                            | Purpose                              |
| -------------------------------------- | ------------------------------------ |
| `Service/Query.hs`                     | Re-export wrapper                    |
| `Service/Query/Core.hs`                | QueryAction, Query, QueryOf types    |
| `Service/Query/TH.hs`                  | deriveQuery Template Haskell         |
| `Service/Query/Registry.hs`            | EntityName to QueryUpdater mapping   |
| `Service/Query/Subscriber.hs`          | Event subscription and rebuild logic |
| `Service/Query/Updater.hs`             | Factory for QueryUpdater instances   |
| `Service/Query/Endpoint.hs`            | HTTP endpoint handler factory        |
| `Service/QueryObjectStore.hs`          | Re-export wrapper                    |
| `Service/QueryObjectStore/Core.hs`     | QueryObjectStore interface           |
| `Service/QueryObjectStore/InMemory.hs` | InMemory implementation              |
| `Service/Application.hs`               | Application type and runner          |

Modules to update:

| Module Path                         | Change                                                |
| ----------------------------------- | ----------------------------------------------------- |
| `Service/EventStore/Core.hs`        | Add `castEventStore`, update `EventStoreConfig`       |
| `Service/EventStore/InMemory.hs`    | Store `Event Json.Value` instead of `Event eventType` |
| `Service/EventStore/Postgres.hs`    | Return `EventStore Json.Value`                        |
| `Service/ServiceDefinition/Core.hs` | Remove EventStore/SnapshotCache config                |
| `Service/Transport.hs`              | Add query endpoint support                            |
| `Service/Transport/Web.hs`          | Handle `/queries/{name}` routes                       |

**Note**: No new `Service/RawEventStore` module is needed. The `castEventStore` function lives in `EventStore.Core` and provides typed access over `EventStore Json.Value`.

---

## Dependency Graph

```
Phase 1: EventStore Caster Function
    |
    v
Phase 2: Core Query Types
    |
    v
Phase 3: QueryObjectStore
    |
    +---> Phase 4: Query Subscriber (depends on 1, 2, 3)
    |         |
    |         v
    |     Phase 5: Template Haskell (depends on 2, 4)
    |
    +---> Phase 6: Application Layer (depends on 1, 2, 3, 4, 5)
              |
              v
          Phase 7: Query HTTP Endpoints (depends on 3, 6)
              |
              v
          Phase 8: Service Simplification (depends on 6)
              |
              v
          Phase 9: Integration Testing (depends on all)
```

---

## Notes for Implementation

1. **Backward Compatibility**: The existing typed `EventStore eventType` API continues to work via `castEventStore`. The change to `EventStore Json.Value` at the storage layer is internal.

2. **Error Handling**: Use `Task Error` for operations that can fail. Prefer `Task.mapError toText` when crossing module boundaries.

3. **Logging**: Use `Console.print` for diagnostic output. Consider adding a proper logging abstraction later.

4. **Performance**: The full rebuild on startup may be slow for large event stores. This is acceptable for v1; checkpoint persistence can be added later.

5. **Testing Strategy**: Each phase should have its own tests. Integration tests in Phase 9 verify the complete system.

6. **Code Style**: Follow NeoHaskell conventions (pipe operator, explicit forall, no point-free style). See `/Users/nick/Source/NeoHaskell/CLAUDE.md` for details.

7. **Caster Pattern Benefits**:
   - Services can share a single `EventStore Json.Value`
   - Query subscribers work directly with `EventStore Json.Value` (no decoding for routing)
   - Each service casts to its own event type via `castEventStore`
   - No duplication of Event/InsertionPayload types
