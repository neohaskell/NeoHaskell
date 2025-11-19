# CommandHandler Test Suite

This test suite drives the implementation of the CommandHandler service using TDD (Test-Driven Development).

## CommandHandler Responsibilities

The CommandHandler is responsible for:

1. **Fetch Entity State** - Load current state using EntityFetcher
2. **Call Command Decision** - Execute `decide` method on the command
3. **Handle Decision Result**:
   - `AcceptCommand`: Insert events into EventStore
   - `RejectCommand`: Return rejection reason
4. **Retry Logic** - Retry on consistency conflicts with 100ms delay
5. **Stop Conditions** - Don't retry when `StreamCreation` and stream already exists

## Test Structure

```
Test/Service/CommandHandler/
├── Core.hs                        # CommandHandlerResult types
├── Execute/
│   ├── Context.hs                # Test context with stores and fetchers
│   └── Spec.hs                   # Comprehensive test specifications
└── README.md                     # This file
```

## Test Coverage

### Basic Command Execution

1. **✅ Creates new stream successfully**
   - Command with `StreamCreation` InsertionType
   - Events inserted into EventStore
   - Returns `CommandAccepted` with stream ID

2. **✅ Rejects on business rules**
   - Command decision returns `RejectCommand`
   - No events inserted
   - Returns `CommandRejected` with reason

3. **✅ Appends to existing stream**
   - Command with `ExistingStream` InsertionType
   - Events appended to existing stream
   - Returns `CommandAccepted`

### Retry Logic

1. **✅ Retries on consistency check failure**
   - EventStore returns `ConsistencyCheckFailed`
   - CommandHandler waits 100ms
   - Refetches entity state
   - Recalls `decide` with fresh state
   - Retries insertion
   - Returns `CommandAccepted` with `retriesAttempted > 0`

2. **✅ Stops retrying for StreamCreation when stream exists**
   - Command uses `StreamCreation` InsertionType
   - EventStore returns `StreamAlreadyExists`
   - CommandHandler does NOT retry
   - Returns `CommandFailed` with appropriate error

3. **✅ Uses 100ms delay between retries**
   - Verify timing between retry attempts
   - Ensures exponential backoff or fixed delay

### Concurrency Handling

1. **✅ Handles concurrent commands on same stream**
   - Multiple commands target same entity
   - One succeeds immediately
   - Others retry with fresh state
   - All eventually succeed (or fail appropriately)

2. **✅ Handles concurrent commands on different streams**
   - Commands on different streams don't interfere
   - All execute independently
   - No unnecessary retries

### Error Scenarios

1. **✅ Returns CommandRejected for business rule failures**
   - Command decision rejects
   - No retry attempted
   - Clear error message returned

2. **✅ Handles EventStore errors gracefully**
   - Storage failures propagate appropriately
   - Clear error messages
   - Returns `CommandFailed`

3. **✅ Handles tenant commands correctly**
   - Tenant ID passed to decision
   - Tenant-scoped StreamId generation
   - Tenant isolation maintained

## CommandHandler Interface (To Be Implemented)

```haskell
module Service.CommandHandler where

import Core
import Service.Command (Command, CommandResult, TenantCommand)
import Service.EntityFetcher (EntityFetcher)
import Service.Event (EntityName, InsertionType)
import Service.EventStore (EventStore)

data CommandHandler = CommandHandler
  { -- Execute a regular (non-tenant) command
    execute ::
      forall command entity event.
      (Command command, Entity command ~ entity) =>
      EventStore event ->
      EntityFetcher entity event ->
      EntityName ->
      command ->
      Task Text CommandHandlerResult,

    -- Execute a tenant command
    executeTenant ::
      forall command entity event.
      (TenantCommand command, Entity command ~ entity) =>
      EventStore event ->
      EntityFetcher entity event ->
      EntityName ->
      Uuid ->  -- Tenant ID
      command ->
      Task Text CommandHandlerResult
  }

data CommandHandlerResult
  = CommandAccepted
      { streamId :: StreamId,
        eventsAppended :: Int,
        retriesAttempted :: Int
      }
  | CommandRejected
      { reason :: Text
      }
  | CommandFailed
      { error :: Text,
        retriesAttempted :: Int
      }
```

## Implementation Algorithm

### Execute Flow

```haskell
execute eventStore fetcher entityName command = do
  -- 1. Get StreamId from command
  let sid = streamId command

  -- 2. Fetch current entity state
  entity <- fetcher.fetch entityName sid

  -- 3. Call decide
  let decision = decide command (if entity.version == 0 then Nothing else Just entity)

  -- 4. Handle decision
  case decision of
    RejectCommand reason ->
      return (CommandRejected {reason})

    AcceptCommand insertionType events -> do
      -- 5. Try to insert events (with retry logic)
      executeWithRetry insertionType events sid 0

executeWithRetry insertionType events sid retries = do
  -- Build insertion payload
  let payload = buildPayload entityName sid insertionType events

  -- Try to insert
  result <- eventStore.insert payload |> Task.asResult

  case result of
    Ok _ ->
      -- Success!
      return (CommandAccepted {streamId = sid, eventsAppended = length events, retriesAttempted = retries})

    Err (ConsistencyCheckFailed _) ->
      -- Check if we should retry
      if insertionType == StreamCreation
        then
          -- Stream already exists, don't retry
          return (CommandFailed {error = "Stream already exists", retriesAttempted = retries})
        else do
          -- Wait 100ms
          Task.sleep 100

          -- Refetch entity
          entity <- fetcher.fetch entityName sid

          -- Recall decide
          let decision = decide command (if entity.version == 0 then Nothing else Just entity)

          case decision of
            RejectCommand reason ->
              return (CommandRejected {reason})
            AcceptCommand newInsertionType newEvents ->
              -- Retry
              executeWithRetry newInsertionType newEvents sid (retries + 1)

    Err other ->
      -- Other errors don't retry
      return (CommandFailed {error = toText other, retriesAttempted = retries})
```

## Key Design Decisions

### 1. Retry Strategy

- **Fixed 100ms delay** between retries (not exponential backoff for MVP)
- **Unlimited retries** for `ExistingStream` (will eventually succeed or timeout)
- **No retries** for `StreamCreation` when stream exists (business rule violation)

### 2. Consistency Model

- **Optimistic concurrency control** - Commands may fail due to concurrent modifications
- **Eventual consistency** - Retries ensure commands eventually succeed
- **Idempotency** - Commands should be idempotent (same command = same result)

### 3. Error Handling

- **Business rule failures** → `CommandRejected` (no retry)
- **Consistency conflicts** → Retry with fresh state
- **Storage failures** → `CommandFailed` (no retry for MVP)

### 4. Tenant Isolation

- Tenant ID included in StreamId generation
- Tenant ID passed to decision logic
- Separate execute methods for clarity

## Running the Tests

To run the CommandHandler tests:

```bash
cabal test nhcore-test
```

To run only CommandHandler-specific tests:

```bash
cabal test nhcore-test --test-options="--match CommandHandler"
```

## Next Steps

1. **Implement CommandHandler.Core** module with the interface
2. **Implement retry logic** with 100ms delay
3. **Implement tenant command support**
4. **Add comprehensive error handling**
5. **Add integration tests** with both InMemory and Postgres EventStores
6. **Add performance tests** for retry scenarios
7. **Document CommandHandler usage** in examples

## Integration with Other Components

### EntityFetcher
- Used to load current entity state
- Provides clean separation of concerns

### EventStore
- Receives insertion payloads
- Handles concurrency via optimistic locking
- Returns consistency errors for retry

### Command
- Provides `decide` method for business logic
- Returns `InsertionType` to guide retry strategy
- Pure function (no side effects)

## Future Enhancements

1. **Configurable retry policy** (max retries, backoff strategy)
2. **Metrics and observability** (retry counts, latency)
3. **Circuit breaker** for failing commands
4. **Command batching** for bulk operations
5. **Distributed tracing** with correlation IDs
