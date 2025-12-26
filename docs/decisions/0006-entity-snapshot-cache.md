# ADR-0006: Entity Snapshot Cache

## Status

Accepted

## Context

NeoHaskell's event sourcing system reconstructs entity state by replaying all events from the beginning of a stream. While this approach provides strong consistency guarantees and a complete audit trail, it introduces a performance challenge: as entities accumulate events over time, fetching their current state becomes increasingly expensive.

Consider a shopping cart entity that has processed hundreds of item additions, removals, and quantity changes over its lifetime. Every time we need to fetch the cart's current state to process a new command, we must:

1. Read all events from the EventStore
2. Apply each event to the initial state using the entity's `update` function
3. Return the final computed state

For entities with thousands of events, this replay operation dominates command processing latency and puts unnecessary load on the EventStore.

### Requirements

The solution must:

1. **Be optional**: Not all services need caching; simple use cases should not pay the complexity cost
2. **Be transparent**: The caching layer should not change the semantics of entity fetching
3. **Support multiple backends**: In-memory for development/testing, potentially Postgres or Redis for production
4. **Integrate cleanly**: Work with the existing `ServiceDefinition` builder pattern
5. **Be type-safe**: The cache must be typed to the service's entity type, catching mismatches at compile time

### Design Constraints

- All commands in a service share the same entity type (enforced by existing `ServiceEntityType` type family)
- The cache must store not just the entity state, but also the stream position at which that state was computed
- Cache updates should be fire-and-forget to avoid blocking the command path on cache failures
- The `EntityFetcher` abstraction must remain clean and testable

## Decision

We implemented an entity snapshot cache as a trait (record-of-functions pattern) with the following components:

### 1. SnapshotCache Trait

Located in `Service/SnapshotCache/Core.hs`:

```haskell
data SnapshotCache state = SnapshotCache
  { get :: EntityName -> StreamId -> Task Error (Maybe (Snapshot state)),
    set :: Snapshot state -> Task Error Unit,
    delete :: EntityName -> StreamId -> Task Error Unit,
    clear :: Task Error Unit
  }

class SnapshotCacheConfig config where
  createSnapshotCache ::
    (Json.FromJSON state, Json.ToJSON state) =>
    config ->
    Task Text (SnapshotCache state)
```

The trait follows the same pattern as `EventStore`: a record type for operations and a type class for configuration-based creation.

### 2. Snapshot Types

Located in `Service/SnapshotCache/Snapshot.hs`:

```haskell
data SnapshotKey = SnapshotKey
  { entityName :: EntityName,
    streamId :: StreamId
  }

data Snapshot state = Snapshot
  { key :: SnapshotKey,
    state :: state,
    position :: StreamPosition
  }
```

The `position` field is critical: it records the stream position at which the snapshot was taken, enabling incremental event reads.

### 3. InMemory Implementation

Located in `Service/SnapshotCache/InMemory.hs`:

```haskell
data InMemorySnapshotCacheConfig = InMemorySnapshotCacheConfig

data SnapshotStore state = SnapshotStore
  { snapshots :: ConcurrentVar (Map SnapshotKey (Snapshot state)),
    lock :: Lock
  }
```

Thread-safe using `ConcurrentVar` for the state and `Lock` for write synchronization. Read operations (get) are lock-free since `ConcurrentVar.peek` is atomic.

### 4. EntityFetcher Integration

The `EntityFetcher` module gained:

**New type for fetch results with position tracking:**

```haskell
data FetchedEntity state = FetchedEntity
  { state :: state,
    lastPosition :: Maybe StreamPosition
  }

data EntityFetchResult state
  = EntityNotFound
  | EntityFound (FetchedEntity state)
```

**New cache-aware constructor:**

```haskell
newWithCache ::
  EventStore event ->
  SnapshotCache state ->
  state ->
  (event -> state -> state) ->
  Task Error (EntityFetcher state event)
```

The cache-aware fetcher:

1. Checks the cache for an existing snapshot
2. If found, reads events from `snapshot.position + 1` onwards (not from beginning)
3. Applies only the new events on top of the cached state
4. Updates the cache with the final state and position (fire-and-forget)
5. Returns the entity state

### 5. ServiceDefinition Integration

The `Service` type gained a fifth type parameter for the snapshot cache configuration:

```haskell
data Service
    (commandRow :: Record.Row Type)
    (commandTransportNames :: [Symbol])
    (providedTransportNames :: [Symbol])
    (eventStoreConfig :: Type)
    (snapshotCacheConfig :: Type)
  = Service { ..., snapshotCacheConfig :: Maybe snapshotCacheConfig }
```

A new `useSnapshotCache` function mirrors `useEventStore`:

```haskell
useSnapshotCache ::
  (SnapshotCacheConfig snapshotCacheConfig) =>
  snapshotCacheConfig ->
  Service cmds commandTransportNames providedTransportNames eventStoreConfig _ ->
  Service cmds commandTransportNames providedTransportNames eventStoreConfig snapshotCacheConfig
```

The existing `ServiceEntityType` type family extracts the common entity type from all commands, which is used to type the cache:

```haskell
type family ServiceEntityType (cmds :: Record.Row Type) :: Type where
  ServiceEntityType '[] = Never
  ServiceEntityType ((label 'Record.:= cmdDef) ': rest) = CmdEntity cmdDef
```

At service startup (`runService`), the cache is created once and typed to the service's entity type:

```haskell
maybeCache <- case maybeSnapshotCacheConfig of
  Just config -> do
    cache <- SnapshotCache.createSnapshotCache @_ @entity config
    Task.yield (Just cache)
  Nothing -> Task.yield Nothing
```

### User API

```haskell
import Service.SnapshotCache.InMemory (InMemorySnapshotCacheConfig (..))

service =
  Service.new
    |> Service.useServer WebTransport.server
    |> Service.useEventStore PostgresEventStore { ... }
    |> Service.useSnapshotCache InMemorySnapshotCacheConfig
    |> Service.command @AddItemToCart
    |> Service.command @RemoveItemFromCart
```

The `useSnapshotCache` call is optional. Without it, the service uses the original `EntityFetcher.new` which replays all events on every fetch.

### Module Structure

```text
core/service/
  Service/
    SnapshotCache.hs           -- Re-export wrapper
    SnapshotCache/
      Core.hs                  -- SnapshotCache trait + SnapshotCacheConfig class + Error
      Snapshot.hs              -- Snapshot + SnapshotKey types
      InMemory.hs              -- InMemorySnapshotCacheConfig + implementation
```

This follows the established pattern: flat structure with one level of nesting for implementation variants.

## Consequences

### Positive

1. **Significant performance improvement**: For entities with many events, fetch operations read only new events since the last snapshot instead of replaying the full history.

2. **Optional complexity**: Services that do not need caching pay no complexity cost; simply omit the `useSnapshotCache` call.

3. **Implementation flexibility**: The trait pattern allows future implementations (Postgres, Redis) without changing the `EntityFetcher` or `ServiceDefinition` code.

4. **Type safety**: The cache is typed to the service's entity type, catching mismatches at compile time via the `ServiceEntityType` type family.

5. **Clean integration**: Follows the existing patterns (`useEventStore`, `useServer`) for a consistent user experience.

6. **Testability**: The `EntityFetcher.newWithCache` function takes the cache as an argument, making it easy to test with mock caches.

7. **Fire-and-forget cache updates**: Cache update failures do not block or fail command processing; the system degrades gracefully to full replay.

### Negative

1. **Additional type parameter**: The `Service` type now has five type parameters, increasing type signature complexity.

2. **Eventual consistency**: The cache is updated after fetch, so a fetch immediately after an insert may not see the cached state. This is acceptable because the EventStore remains the source of truth.

3. **Memory usage (InMemory)**: The in-memory implementation stores full entity states in memory. For services with many entities or large entity states, this could be significant.

4. **No automatic invalidation**: Snapshots are not automatically invalidated when events are truncated. Services using stream truncation should manually clear affected cache entries.

### Future Work

1. **Postgres implementation**: `SnapshotCache.Postgres` for production deployments requiring persistence across restarts.

2. **TTL/eviction policies**: The current InMemory implementation has no eviction; adding LRU or TTL policies would help with memory management.

3. **Snapshot frequency control**: Currently, the cache is updated on every fetch. A configuration option to snapshot only every N events or N seconds could reduce write overhead.

4. **Metrics**: Expose cache hit/miss rates for observability.

## References

- `/Users/nick/Source/NeoHaskell/core/service/Service/SnapshotCache/Core.hs` - SnapshotCache trait
- `/Users/nick/Source/NeoHaskell/core/service/Service/SnapshotCache/Snapshot.hs` - Snapshot types
- `/Users/nick/Source/NeoHaskell/core/service/Service/SnapshotCache/InMemory.hs` - InMemory implementation
- `/Users/nick/Source/NeoHaskell/core/service/Service/EntityFetcher/Core.hs` - EntityFetcher with cache support
- `/Users/nick/Source/NeoHaskell/core/service/Service/ServiceDefinition/Core.hs` - Service integration
- `/Users/nick/Source/NeoHaskell/docs/decisions/0004-eventstore-abstraction.md` - EventStore ADR (similar trait pattern)
