# ADR-0004: EventStore Abstraction

## Status

Accepted

## Context

NeoHaskell implements Event Sourcing and CQRS as its core architectural pattern. At the heart of this pattern lies the EventStore, which serves as the single source of truth for all state changes in the system. This ADR documents the abstract EventStore contract, its guarantees, and the behaviors that any conforming implementation must provide.

Event Sourcing requires that:
1. All state changes are captured as immutable events
2. Events are ordered and can be replayed to reconstruct state
3. Concurrent modifications are handled safely through optimistic concurrency control
4. The system can support projections, subscriptions, and replays

The EventStore abstraction must be implementation-agnostic, allowing for different backing technologies (in-memory for testing, PostgreSQL for production, potentially Kafka, EventStoreDB, or other systems in the future) while providing consistent guarantees across all implementations.

## Decision

### The EventStore Record Type

The EventStore is defined as a record type parameterized by the event type:

```haskell
data EventStore eventType = EventStore
  { insert :: InsertionPayload eventType -> Task Error InsertionSuccess
  , readStreamForwardFrom :: EntityName -> StreamId -> StreamPosition -> Limit -> Task Error (Stream (ReadStreamMessage eventType))
  , readStreamBackwardFrom :: EntityName -> StreamId -> StreamPosition -> Limit -> Task Error (Stream (ReadStreamMessage eventType))
  , readAllStreamEvents :: EntityName -> StreamId -> Task Error (Stream (ReadStreamMessage eventType))
  , readAllEventsForwardFrom :: StreamPosition -> Limit -> Task Error (Stream (ReadAllMessage eventType))
  , readAllEventsBackwardFrom :: StreamPosition -> Limit -> Task Error (Stream (ReadAllMessage eventType))
  , readAllEventsForwardFromFiltered :: StreamPosition -> Limit -> Array EntityName -> Task Error (Stream (ReadAllMessage eventType))
  , readAllEventsBackwardFromFiltered :: StreamPosition -> Limit -> Array EntityName -> Task Error (Stream (ReadAllMessage eventType))
  , subscribeToAllEvents :: (Event eventType -> Task Text Unit) -> Task Error SubscriptionId
  , subscribeToAllEventsFromPosition :: StreamPosition -> (Event eventType -> Task Text Unit) -> Task Error SubscriptionId
  , subscribeToAllEventsFromStart :: (Event eventType -> Task Text Unit) -> Task Error SubscriptionId
  , subscribeToEntityEvents :: EntityName -> (Event eventType -> Task Text Unit) -> Task Error SubscriptionId
  , subscribeToStreamEvents :: EntityName -> StreamId -> (Event eventType -> Task Text Unit) -> Task Error SubscriptionId
  , unsubscribe :: SubscriptionId -> Task Error Unit
  , truncateStream :: EntityName -> StreamId -> StreamPosition -> Task Error Unit
  }
```

### Core Types

**StreamPosition**: A 64-bit integer representing a position in either a stream or the global event log. Positions are 0-indexed.

**StreamId**: Uniquely identifies a stream within an entity type. Derived from entity identifiers.

**EntityName**: A text identifier for the entity type (e.g., "Cart", "User", "Order").

**InsertionPayload**: Contains the events to insert, along with:
- `streamId`: Target stream
- `entityName`: Entity type
- `insertionType`: Concurrency control mode
- `insertions`: Array of events with metadata

**InsertionType**: Controls optimistic concurrency:
- `StreamCreation`: Expects the stream does not exist
- `InsertAfter StreamPosition`: Expects specific stream position
- `ExistingStream`: Expects the stream already exists
- `AnyStreamState`: No position check (use with caution)

### Guaranteed Behaviors

Any EventStore implementation MUST provide these guarantees:

#### 1. Global Stream Ordering

All events across all streams are assigned a monotonically increasing global position. When reading from the global stream:
- Events appear in strict global position order
- No gaps exist in the global position sequence
- Each event has a unique global position

**Test coverage**: `GlobalStreamOrdering.Spec`
- Verifies correct total event count across streams
- Verifies all events have assigned global positions
- Verifies events are globally ordered (positions increase monotonically)

#### 2. Individual Stream Ordering

Within a single stream:
- Events are assigned sequential local positions starting from 0
- Local positions are strictly increasing
- Reading forward returns events in ascending local position order
- Reading backward returns events in descending local position order
- Global positions within a stream are strictly increasing (but may have gaps)

**Test coverage**: `IndividualStreamOrdering.Spec`
- Verifies correct event count per stream
- Verifies local positions are sequential (0, 1, 2, ...)
- Verifies global positions are strictly increasing within a stream
- Verifies forward reads return correct order
- Verifies backward reads return reverse order
- Tests reading from middle positions

#### 3. Optimistic Concurrency Control

When multiple writers attempt to append to the same stream concurrently:
- At most one writer succeeds when using `InsertAfter` with the same position
- Failed writers receive a `ConsistencyCheckFailed` error
- The global stream has no gaps even when concurrent writes fail
- Failed writes do NOT appear in either the stream or global log

**Test coverage**: `OptimisticConcurrency.Spec`
- Verifies only one concurrent write succeeds to same position
- Verifies consistency error when stream position is stale
- Verifies global stream has no gaps after failed concurrent writes

#### 4. Idempotency by Event ID

Events are deduplicated by their unique event ID:
- Inserting the same event ID twice does not create duplicates
- The second insertion either succeeds silently or fails gracefully
- The stream contains exactly one copy of each unique event

**Test coverage**: `OptimisticConcurrency.Spec` ("insertion is idempotent by event id")
- Verifies inserting same events twice results in original count only

#### 5. Local Position Stamping

The EventStore stamps local positions on events:
- Events inserted without explicit local positions receive auto-assigned positions
- Positions are derived from current stream length at insert time
- Caller-provided positions are preserved when explicitly set
- Events read from subscriptions include stamped positions

**Test coverage**: `LocalPositionStamping.Spec`
- Verifies positions are stamped when using `payloadFromEvents` helper
- Verifies subscription events include positions
- Verifies sequential positions across multiple inserts
- Verifies caller-provided positions are preserved

#### 6. Batch Validation

Insert operations validate batch constraints:
- Empty insertion arrays are rejected with `EmptyPayload` error
- Batches exceeding 100 events are rejected with `PayloadTooLarge` error
- Batches of exactly 100 events are accepted
- Meaningful error messages are provided for validation failures

**Test coverage**: `BatchValidation.Spec`
- Verifies empty batches are rejected
- Verifies oversized batches (>100 events) are rejected
- Verifies maximum size batches (100 events) succeed
- Verifies error messages are meaningful

#### 7. Stream Truncation

Truncation removes events before a given position:
- `truncateStream` removes all events with local position < specified position
- Events at or after the position are retained
- Truncating at position 0 keeps all events
- Truncating beyond stream length removes all events

**Test coverage**: `StreamTruncation.Spec`
- Verifies truncation keeps events from position onwards
- Verifies position 0 truncation keeps all events
- Verifies beyond-end truncation removes all events

#### 8. Subscription Semantics

Subscriptions provide real-time event notifications:

**`subscribeToAllEvents`**: Receives only events inserted AFTER subscription creation ("from now on").

**`subscribeToAllEventsFromPosition`**: Receives events after the specified global position, including catching up from that point.

**`subscribeToAllEventsFromStart`**: Receives ALL events, including historical ones, then continues with live events.

**`subscribeToEntityEvents`**: Filters to events for a specific entity type.

**`subscribeToStreamEvents`**: Filters to events for a specific entity and stream.

Additional guarantees:
- Subscriber errors do not affect event store operations (fire-and-forget with error isolation)
- Multiple concurrent subscribers receive all events without interference
- Events stop being delivered after `unsubscribe` is called
- High-frequency publishing does not lose events
- Events are delivered in order

**Test coverage**: `Subscriptions.Spec`, `Subscriptions.SimpleSpec`
- Verifies all subscription modes (from now, from position, from start)
- Verifies entity and stream filtering
- Verifies subscriber error isolation
- Verifies multiple concurrent subscribers
- Verifies unsubscription stops delivery
- Verifies high-frequency publishing without data loss

### Read Operations

All read operations support:
- **Forward reading**: From a position toward the end
- **Backward reading**: From a position toward the beginning
- **Limit**: Maximum number of events to return
- **Entity filtering**: Optionally filter to specific entity types (global reads only)

Read messages include metadata:
- `ReadingStarted` / `StreamReadingStarted`: Stream opened
- `AllEvent` / `StreamEvent`: Actual event data
- `ToxicAllEvent` / `ToxicStreamEvent`: Undecodable event with metadata
- `Checkpoint`: Position marker for progress tracking
- `CaughtUp`: Reached end of available events
- `FellBehind`: Subscription falling behind live events
- `Terminated`: Stream closed with reason

**Test coverage**: `ReadAllForwardsFromStart.Spec`, `ReadAllBackwardsFromEnd.Spec`
- Verifies forward/backward reading
- Verifies entity filtering
- Verifies partial reads with resumption
- Verifies limit enforcement
- Verifies ordering guarantees

### Error Types

```haskell
data Error
  = StreamNotFound EntityName StreamId
  | EventNotFound StreamId StreamPosition
  | StorageFailure Text
  | SubscriptionNotFound SubscriptionId
  | SubscriptionError SubscriptionId Text
  | TruncationError EntityName StreamId StreamPosition Text
  | InsertionError InsertionFailure
  | ReadingAllError Text

data InsertionFailure
  = ConsistencyCheckFailed
  | InsertionFailed Text
  | PayloadTooLarge
  | EmptyPayload
```

### Integration with System Components

The EventStore is the foundational component that other service layer components depend on:

**EntityFetcher**: Uses `readAllStreamEvents` to load all events for a stream and fold them with a reduction function to compute current entity state.

**CommandHandler**: Uses the EventStore's `insert` operation to persist events produced by command decisions. Implements retry logic with exponential backoff when `ConsistencyCheckFailed` occurs.

**Reactors** (future): Will use subscriptions to react to events and trigger side effects or produce new commands.

**Projections** (future): Will use `readAllEventsForwardFrom` or subscriptions to build read models.

### Configuration Interface

Implementations provide a configuration type class:

```haskell
class EventStoreConfig config where
  createEventStore ::
    (Json.FromJSON eventType, Json.ToJSON eventType) =>
    config -> Task Text (EventStore eventType)
```

This allows different implementations to have their own configuration types while providing a uniform creation interface.

## Consequences

### Positive

1. **Implementation Independence**: The abstract contract allows swapping implementations without changing business logic.

2. **Testability**: In-memory implementations can be used for fast, isolated tests while production uses PostgreSQL.

3. **Consistent Guarantees**: All implementations must pass the same specification tests, ensuring behavioral consistency.

4. **Future Extensibility**: New backing stores can be added by implementing the interface and passing the specification tests.

5. **Clear Contract**: The specification tests serve as executable documentation of expected behavior.

### Negative

1. **Implementation Complexity**: Each implementation must provide all guarantees, which can be complex for some backing stores.

2. **Lowest Common Denominator**: Some backing stores may support features (like server-side projections) that cannot be exposed through this interface.

3. **Test Execution Time**: Running the full specification suite against real databases (PostgreSQL) takes significant time.

### Implementation Requirements Checklist

Any new EventStore implementation must:

1. Pass all tests in `Test.Service.EventStore`:
   - `ReadAllForwardsFromStart.Spec`
   - `ReadAllBackwardsFromEnd.Spec`
   - `IndividualStreamOrdering.Spec`
   - `GlobalStreamOrdering.Spec`
   - `OptimisticConcurrency.Spec`
   - `StreamTruncation.Spec`
   - `Subscriptions.SimpleSpec`
   - `Subscriptions.Spec`
   - `BatchValidation.Spec`
   - `LocalPositionStamping.Spec`

2. Implement the `EventStoreConfig` type class for its configuration type.

3. Ensure events are JSON-serializable (`ToJSON`/`FromJSON` constraints).

4. Handle toxic events (undecodable events) gracefully by returning `ToxicStreamEvent`/`ToxicAllEvent` messages rather than failing.

## References

- Event Sourcing pattern: https://martinfowler.com/eaaDev/EventSourcing.html
- CQRS pattern: https://martinfowler.com/bliki/CQRS.html
- Optimistic Concurrency in Event Stores: https://eventstore.com/docs/
