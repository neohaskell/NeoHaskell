# ADR-0032: SimpleEventStore with Optional JSONL Persistence

## Status

Proposed

## Context

NeoHaskell's event-sourcing architecture requires an EventStore implementation for persisting and retrieving events. Currently, two implementations exist:

1. **InMemoryEventStore**: Fast, thread-safe, but loses all data on process exit
2. **PostgresEventStore**: Production-ready with full persistence, but requires Docker/PostgreSQL setup

This creates friction in several scenarios:

**CLI Tools**: Command-line applications need persistent state between runs but don't justify a database dependency. Examples include task managers, note-taking apps, or local development tools.

**Local Development**: Developers want to experiment with event-sourcing patterns without installing PostgreSQL. The current InMemory implementation requires re-seeding data after every restart.

**Simple Applications**: Small utilities or personal projects don't need the complexity of a database server but still benefit from event sourcing's audit trail and time-travel debugging.

**Testing**: While InMemory works well for unit tests, integration tests sometimes need to verify persistence behavior without the overhead of PostgreSQL.

The InMemory implementation is well-tested (passes all 10 EventStore specification tests from ADR-0004) and provides excellent concurrency guarantees using `DurableChannel`, `ConcurrentVar`, and `Lock`. However, its name suggests it's only suitable for testing, when it could serve production use cases with minimal enhancement.

## Decision

Rename `InMemoryEventStore` to `SimpleEventStore` and add an optional file-based persistence mode using JSONL (JSON Lines) files.

### Configuration Type

```haskell
data SimpleEventStore = SimpleEventStore
  { basePath :: Path,
    persistent :: Bool
  }
  deriving (Eq, Show)
```

**Fields:**

- `basePath`: Directory where event files are stored (e.g., `.neo/events/`)
- `persistent`: When `True`, events are persisted to JSONL files; when `False`, behaves like the current InMemory implementation

### EventStoreConfig Instance

```haskell
instance EventStoreConfig SimpleEventStore where
  createEventStore :: SimpleEventStore -> Task Text (EventStore Json.Value)
  createEventStore config = do
    store <- newEmptyStreamStore
    
    -- If persistent mode, load existing events from disk
    when config.persistent do
      loadEventsFromDisk config.basePath store
    
    let eventStore = EventStore
          { insert = insertImpl config store,
            readStreamForwardFrom = readStreamForwardFromImpl store,
            -- ... other operations unchanged
          }
    
    Task.yield eventStore
```

The `createEventStore` function initializes the in-memory data structures, then optionally loads historical events from disk to rebuild state.

### Persistence Strategy

**Directory Structure:**

```text
.neo/events/
├── Cart/
│   ├── cart-123.jsonl
│   └── cart-456.jsonl
└── User/
    └── user-789.jsonl
```

**File Naming:** `{EntityName}/{StreamId}.jsonl`

**Format:** One JSON object per line (JSONL), each representing a serialized `Event Json.Value`:

```json
{"entityName":"Cart","streamId":"cart-123","event":{"type":"ItemAdded","productId":"prod-1","quantity":2},"metadata":{"eventId":"evt-001","timestamp":"2026-02-19T10:30:00Z","localPosition":0,"globalPosition":42}}
{"entityName":"Cart","streamId":"cart-123","event":{"type":"ItemRemoved","productId":"prod-1"},"metadata":{"eventId":"evt-002","timestamp":"2026-02-19T10:31:00Z","localPosition":1,"globalPosition":43}}
```

**Why JSONL?**

- **Append-only writes**: Each event is a single line, making atomic appends trivial
- **Crash safety**: Partial writes corrupt at most one line, which can be detected and skipped
- **Line-by-line parsing**: Corrupted lines don't prevent reading valid events
- **Human-readable**: Easy to inspect, debug, or manually edit if needed
- **No schema migration**: JSON flexibility handles event schema evolution

### Startup Behavior

When `persistent = True`:

1. Scan `basePath` directory for all `*.jsonl` files
2. For each file, parse JSONL lines into `Event Json.Value` objects
3. Replay events through the in-memory `DurableChannel` structures to rebuild state
4. Validate global/local position consistency (warn on gaps, continue on corruption)
5. Resume normal operation with in-memory state synchronized to disk

**Error Handling:**

- Missing `basePath`: Create directory structure automatically
- Corrupted JSONL lines: Log warning, skip line, continue parsing
- Duplicate event IDs: Rely on existing idempotency guarantees (no-op on duplicate insert)

### Write Behavior

The `insert` operation is modified to append events to disk after successful in-memory insertion:

```haskell
insertImpl ::
  SimpleEventStore ->
  StreamStore ->
  InsertionPayload Json.Value ->
  Task Error InsertionSuccess
insertImpl config store payload = do
  -- Existing in-memory insertion logic (unchanged)
  success <- insertInMemory store payload
  
  -- If persistent mode, append to JSONL file
  when config.persistent do
    let filePath = buildEventFilePath config.basePath payload.entityName payload.streamId
    let jsonLines = payload.insertions |> Array.map eventToJsonLine
    File.appendText filePath jsonLines
      |> Task.mapError (\err -> InsertionError (InsertionFailed [fmt|File write failed: {err}|]))
  
  Task.yield success
```

**Atomicity Guarantee:** The in-memory insertion succeeds first (with optimistic concurrency checks), then the file write occurs. If the file write fails, the in-memory state is already updated, and the next startup will detect the missing events as a gap (logged as a warning but not fatal).

**Concurrency:** The existing `Lock` in `StreamStore` serializes all inserts, so file writes are naturally serialized per stream. Different streams write to different files, avoiding lock contention.

### Backward Compatibility

All existing EventStore specification tests (from ADR-0004) must pass for both modes:

- `persistent = False`: Identical behavior to current InMemory implementation
- `persistent = True`: Same guarantees, plus events survive process restarts

**Test Strategy:**

1. Run all 10 spec test suites against `SimpleEventStore { persistent = False }`
2. Run all 10 spec test suites against `SimpleEventStore { persistent = True }` with a temporary directory
3. Add new test: Insert events, restart EventStore, verify events are reloaded

**Migration Path:**

Existing code using `InMemory.new` can switch to:

```haskell
-- Before
eventStore <- InMemory.new

-- After (non-persistent mode, identical behavior)
eventStore <- SimpleEventStore.new SimpleEventStore
  { basePath = Path.fromText ".neo/events",
    persistent = False
  }

-- After (persistent mode)
eventStore <- SimpleEventStore.new SimpleEventStore
  { basePath = Path.fromText ".neo/events",
    persistent = True
  }
```

## Consequences

### Positive

1. **No External Dependencies**: CLI tools and simple apps can use event sourcing without PostgreSQL, Docker, or any database setup.

2. **Simple Configuration**: Two-field record type with clear semantics. Default to `persistent = False` for testing, opt-in to `persistent = True` for production CLI tools.

3. **CLI-Friendly**: Perfect for command-line applications that need persistent state between runs (task managers, note apps, local dev tools).

4. **Human-Readable Storage**: JSONL files can be inspected, debugged, or even manually edited with a text editor.

5. **Gradual Migration Path**: Start with `persistent = False` for prototyping, flip to `persistent = True` when ready, migrate to PostgreSQL when scaling demands it.

6. **Audit Trail**: Even simple apps get event sourcing's benefits: full audit history, time-travel debugging, and event replay.

7. **Crash Safety**: Append-only JSONL writes minimize corruption risk. Partial writes affect at most one line.

8. **File-Per-Stream Isolation**: No lock contention between different entity streams. Each stream writes to its own file.

### Negative

1. **Not Production-Ready for High Concurrency**: File I/O is slower than PostgreSQL. Suitable for CLI tools and low-traffic apps, but not for high-throughput web services.

2. **No Cross-Process Safety**: Multiple processes writing to the same JSONL files will corrupt data. This is a single-process EventStore.

3. **No Transactions Across Streams**: PostgreSQL can atomically commit events across multiple streams. JSONL files cannot.

4. **Manual Cleanup Required**: Old events accumulate in JSONL files. No automatic truncation or archival (though `truncateStream` can be implemented by rewriting files).

5. **Startup Latency**: Loading large JSONL files on startup can be slow. Mitigated by keeping event counts reasonable (suitable for CLI tools, not for years of production data).

### Risks and Mitigations

**Risk: File Corruption on Crash**

- **Mitigation**: JSONL format allows line-by-line parsing. Corrupted lines are logged and skipped. Append-only writes minimize corruption surface area.

**Risk: Disk Space Exhaustion**

- **Mitigation**: Document that `persistent = True` is for CLI tools and small apps, not for unbounded event streams. Recommend periodic archival or migration to PostgreSQL for long-running services.

**Risk: Slow Startup with Large Files**

- **Mitigation**: Lazy loading (future enhancement) or snapshotting (future ADR). For now, document that SimpleEventStore is for "simple" use cases (thousands of events, not millions).

**Risk: Accidental Use in Multi-Process Scenarios**

- **Mitigation**: Documentation clearly states this is a single-process EventStore. Add runtime warning if multiple processes detect concurrent access (future enhancement).

## References

- ADR-0004: EventStore Abstraction (specification tests and guarantees)
- GitHub Issue #411: Rename InMemoryEventStore to SimpleEventStore
- JSONL Specification: <https://jsonlines.org/>
