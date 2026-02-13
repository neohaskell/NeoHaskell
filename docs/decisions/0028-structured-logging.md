# ADR-0028: Structured Logging with fast-logger

## Status

Proposed

## Context

NeoHaskell lacks a proper structured logging system for observable services. The current `Console` module provides only basic debug output and user-facing print functionality, with no log levels, no automatic metadata, and no structured output format.

### Current State

1. **Console.log is debug-only**: The `Console.log` function is gated behind the `NEOHASKELL_DEBUG` environment variable and returns raw `IO Unit`. It's intended for temporary debugging, not production logging.

2. **Console.print for user output**: The `Console.print` function is used for user-facing output with manual `[fmt|...|]` formatting. It has no concept of log levels or structured data.

3. **No structured logging**: Framework internals (Dispatcher, CommandExecutor, EventStore) use `Console.print` with manual formatting like:
   ```haskell
   Console.print [fmt|[INFO] Processing command: {commandName}|]
   ```

4. **82 Console.print/log calls across 17 files**: A comprehensive audit reveals widespread use of Console functions that need migration to a proper logging system:
   - `core/service/Service/CommandExecutor.hs`
   - `core/service/Service/Dispatcher.hs`
   - `core/service/EventStore/Postgres.hs`
   - `core/service/EventStore/InMemory.hs`
   - And 13 more files

5. **No automatic metadata**: Call-site information (file, line), timestamps, and request context must be manually added to every log message.

### Use Cases

- **Framework observability**: Track command execution, event processing, and query subscriptions with automatic context (requestId, correlationId, streamId)
- **Application logging**: Allow user applications to emit structured logs with consistent formatting
- **Production debugging**: JSON output for log aggregation systems (CloudWatch, Datadog, Elasticsearch)
- **Request tracing**: Automatic propagation of request IDs through command and event processing pipelines

### Design Goals

1. **Zero-config API**: Simple functions like `Log.info "message"` with no explicit logger passing
2. **Structured output**: JSON format with automatic timestamps, levels, and call-site metadata
3. **Thread-local context**: Automatic request ID propagation without polluting function signatures
4. **NeoHaskell style**: Qualified module design, pipe-friendly, returns `Task _ Unit`
5. **Performance**: Fast-logger backend for high-throughput logging
6. **Non-intrusive**: NOT re-exported from Core — explicit opt-in via `import Log qualified`

### GitHub Issue

- [#402: Add structured logging module (Log)](https://github.com/neohaskell/NeoHaskell/issues/402)

## Decision

### 1. Module Name: `Log`

We choose `Log` over alternatives:

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| `Logger` | Rejected | Implies a logger object/handle, but we use a global logger pattern |
| `Logging` | Rejected | Gerund form is less idiomatic for NeoHaskell modules (cf. `Task`, `Result`, `Text`) |
| `Log` | **Chosen** | Short, clear, matches common usage (`Log.info`, `Log.debug`), no ecosystem conflicts |

### 2. Module Location: `core/core/Log.hs`

Located in the `core` source directory alongside `Task`, `Result`, and `Text`:
- Fundamental infrastructure, but NOT re-exported from `Core.hs`
- Requires explicit `import Log qualified` to use
- Keeps logging opt-in rather than polluting every module's namespace

**Why NOT re-exported?**
- Not every module needs logging (e.g., pure data types, simple utilities)
- Makes logging usage explicit and intentional
- Avoids namespace pollution with `debug`, `info`, `warn`, `critical`

### 3. Log Levels

```haskell
data Level = Debug | Info | Warn | Error
  deriving (Eq, Ord, Show, Generic)
```

Four standard levels with `critical` as the API function name for `Error`:

| Level | API Function | Use Case |
|-------|--------------|----------|
| `Debug` | `Log.debug` | Detailed diagnostic information (event payloads, internal state) |
| `Info` | `Log.info` | General informational messages (command started, query subscribed) |
| `Warn` | `Log.warn` | Warning conditions (deprecated API usage, fallback behavior) |
| `Error` | `Log.critical` | Error conditions (command failed, integration error) |

**Why `critical` instead of `error`?**
- Avoids confusion with `Prelude.error` (which panics/crashes)
- `error` in Haskell means "unrecoverable failure", but `Log.critical` is just logging
- Clearer intent: "this is critical information" vs "this will crash"

### 4. API Design

```haskell
-- Logging functions (all require HasCallStack)
debug :: (HasCallStack) => Text -> Task _ Unit
info :: (HasCallStack) => Text -> Task _ Unit
warn :: (HasCallStack) => Text -> Task _ Unit
critical :: (HasCallStack) => Text -> Task _ Unit

-- Context management (framework-internal only)
withScope :: Array (Text, Text) -> Task err value -> Task err value
```

**Key design decisions:**

- **HasCallStack**: Automatic call-site metadata (file, line) without manual annotation
- **Returns Task**: Consistent with NeoHaskell's effect system, allows composition with `|>`
- **Text messages**: Simple string messages, not structured data (use JSON serialization before logging if needed)
- **withScope is internal**: Not intended for application code — used by framework to inject request IDs

**Usage examples:**

```haskell
-- Simple logging
do
  Log.info "Application started"
  Log.debug [fmt|Processing {count} items|]
  Log.warn "Deprecated API endpoint called"
  Log.critical [fmt|Database connection failed: {errorMsg}|]

-- With scope (framework-internal)
withScope [("requestId", "a1b2c3d4"), ("correlationId", "req-xyz-789")] do
  Log.info "Processing command"
  -- Logs will include requestId and correlationId
```

### 5. Global Logger Pattern

```haskell
{-# NOINLINE globalLogger #-}
globalLogger :: (GhcLogger.TimedFastLogger, IO ())
globalLogger = GhcUnsafe.unsafePerformIO do
  timeCache <- GhcLogger.newTimeCache ("%Y-%m-%dT%H:%M:%S%z" :: GhcLogger.TimeFormat)
  GhcLogger.newTimedFastLogger timeCache (GhcLogger.LogStdout GhcLogger.defaultBufSize)
```

**Why global logger?**

| Approach | Verdict | Rationale |
|----------|---------|-----------|
| Reader monad | Rejected | Requires threading logger through all functions, pollutes signatures |
| Implicit parameter | Rejected | Not beginner-friendly, requires language extension |
| Global logger | **Chosen** | Simplest API, standard pattern in Haskell logging (monad-logger, katip) |

**Safety of unsafePerformIO:**
- Mitigated by `{-# NOINLINE #-}` pragma (ensures single initialization)
- Standard pattern used by `monad-logger`, `katip`, `fast-logger` itself
- Logger is write-only and thread-safe (fast-logger guarantees)

### 6. Thread-Local Context

```haskell
{-# NOINLINE globalContext #-}
globalContext :: ConcurrentMap GhcThread.ThreadId (Map Text Text)
globalContext = GhcUnsafe.unsafePerformIO do
  ConcurrentMap.new |> Task.runOrPanic

withScope :: Array (Text, Text) -> Task err value -> Task err value
withScope fields task = do
  threadId <- GhcThread.myThreadId |> Task.fromIO
  existingContext <- ConcurrentMap.get threadId globalContext
  let baseContext = existingContext |> Maybe.withDefault Map.empty
  let newContext = fields |> Array.foldl (\(key, value) acc -> acc |> Map.set key value) baseContext
  ConcurrentMap.set threadId newContext globalContext
  Task.finally
    ( case existingContext of
        Just ctx -> ConcurrentMap.set threadId ctx globalContext
        Nothing -> ConcurrentMap.remove threadId globalContext
    )
    task
```

**Why thread-local context?**

- **Automatic propagation**: Request IDs flow through command execution without manual passing
- **Clean signatures**: No need for `requestId` parameters in every function
- **Scoped lifetime**: Context is automatically cleaned up when scope exits

**Why NOT propagate to child threads?**

- Long-lived worker threads would have stale context from the first request
- Short-lived parallel tasks (AsyncTask.run) could propagate context in future work
- Explicit is better than implicit for cross-thread boundaries

**Cleanup on exceptions:**

- `Task.finally` ensures context is removed even if action fails
- Prevents memory leaks from abandoned thread contexts

### 7. Output Format

JSON structured output to stdout, one object per line:

```json
{"time":"2026-02-13T10:30:00+0000","level":"Info","message":"Application started","file":"src/Main.hs","line":12}
```

With scope context:

```json
{"time":"2026-02-13T10:30:05+0000","level":"Debug","message":"Appending 3 events","file":"EventStore/Postgres.hs","line":88,"requestId":"a1b2c3d4","correlationId":"req-xyz-789"}
```

**Field descriptions:**

| Field | Type | Source | Example |
|-------|------|--------|---------|
| `time` | ISO 8601 | System clock | `"2026-02-13T10:30:00+0000"` |
| `level` | String | Log level | `"Debug"`, `"Info"`, `"Warn"`, `"Error"` |
| `message` | String | User-provided | `"Processing command"` |
| `file` | String | HasCallStack | `"Service/CommandExecutor.hs"` |
| `line` | Integer | HasCallStack | `42` |
| `requestId` | String | withScope | `"a1b2c3d4"` (optional) |
| `correlationId` | String | withScope | `"req-xyz-789"` (optional) |
| `streamId` | String | withScope | `"cart-123"` (optional) |

**Why JSON?**

- Standard format for log aggregation systems
- Easy to parse and query
- Supports arbitrary context fields without schema changes

**Why stdout?**

- Standard practice for containerized applications (12-factor app)
- Log aggregation systems capture stdout/stderr
- Simpler than file-based logging with rotation

### 8. Integration Points

The framework will use `withScope` at four key boundaries to inject context:

#### 8.1. CommandExecutor.execute

```haskell
execute :: forall command. (Command command) => command -> Task CommandError Unit
execute cmd = do
  let requestId = generateRequestId ()
  let correlationId = Command.correlationId cmd
  withScope [("requestId", requestId), ("correlationId", correlationId)] do
    Log.info [fmt|Executing command: {Command.name cmd}|]
    -- ... command execution logic
```

**Context injected:**
- `requestId`: Unique ID for this command execution
- `correlationId`: User-provided correlation ID from command

#### 8.2. Dispatcher.processStatelessEvent

```haskell
processStatelessEvent :: Event -> Task _ Unit
processStatelessEvent event = do
  let correlationId = Event.correlationId event
  let streamId = Event.streamId event
  withScope [("correlationId", correlationId), ("streamId", streamId)] do
    Log.debug [fmt|Processing stateless event: {Event.name event}|]
    -- ... event processing logic
```

**Context injected:**
- `correlationId`: Traces back to originating command
- `streamId`: Identifies the aggregate instance

#### 8.3. Dispatcher.processLifecycleEvent

Same pattern as `processStatelessEvent`:

```haskell
processLifecycleEvent :: Event -> Task _ Unit
processLifecycleEvent event = do
  let correlationId = Event.correlationId event
  let streamId = Event.streamId event
  withScope [("correlationId", correlationId), ("streamId", streamId)] do
    Log.debug [fmt|Processing lifecycle event: {Event.name event}|]
    -- ... lifecycle logic
```

#### 8.4. QuerySubscriber

```haskell
subscribeLoop :: Query query => EventStore -> query -> Task _ Unit
subscribeLoop store query = do
  -- ... subscription setup
  forever do
    event <- EventStore.readNext store
    let correlationId = Event.correlationId event
    withScope [("correlationId", correlationId)] do
      Log.debug [fmt|Query {Query.name query} processing event {Event.name event}|]
      -- ... query update logic
```

**Context injected:**
- `correlationId`: Links query updates to originating commands

### 9. Dependency: fast-logger

```haskell
-- nhcore.cabal
build-depends:
  , fast-logger >= 3.0
```

**Why fast-logger?**

| Library | Verdict | Rationale |
|---------|---------|-----------|
| `monad-logger` | Rejected | Requires monad transformer stack, not compatible with Task |
| `katip` | Rejected | Heavy dependency, complex API, overkill for our needs |
| `co-log` | Rejected | Requires effect system integration, not beginner-friendly |
| `fast-logger` | **Chosen** | Minimal, fast, widely used, simple API, no monad constraints |

**fast-logger features:**
- Thread-safe writes to stdout
- Buffered output for performance
- No monad constraints (works with any IO-based effect)
- Battle-tested (used by Warp, Yesod, etc.)

### 10. HasCallStack for Call-Site Metadata

```haskell
info :: (HasCallStack) => Text -> Task _ Unit
info msg = do
  let location = GhcStack.getCallStack GhcStack.callStack
  let (file, line) = extractLocation location
  -- ... log with file and line
```

**Why HasCallStack?**

- Automatic call-site information without manual annotation
- Already used in `Console.log` — familiar pattern
- Zero runtime cost when not used
- Standard GHC feature, no language extensions required

**Precedent in NeoHaskell:**

```haskell
-- Console.hs already uses HasCallStack
log :: (HasCallStack) => Text -> IO Unit
```

### 11. NOT Re-Exported from Core

```haskell
-- Core.hs (NO Log re-export)
module Core
  ( -- ... existing exports
  ) where

-- User code must explicitly import
import Log qualified
```

**Why NOT re-export?**

- Logging is not needed in every module (pure data types, utilities)
- Makes logging usage explicit and intentional
- Avoids namespace pollution with common names (`debug`, `info`, `warn`)
- Follows principle of least surprise — logging is opt-in

**Comparison with other modules:**

| Module | Re-exported? | Rationale |
|--------|--------------|-----------|
| `Task` | Yes | Core effect type, used everywhere |
| `Result` | Yes | Core error handling, used everywhere |
| `Text` | Yes | Core string type, used everywhere |
| `Log` | **No** | Infrastructure, not needed in every module |
| `Console` | No | User-facing output, not universal |

## Consequences

### Positive

1. **Observable services**: Framework internals emit structured logs with automatic context, enabling production debugging and monitoring.

2. **Zero-config API**: Simple `Log.info "message"` with no logger passing or monad constraints.

3. **Automatic context propagation**: Request IDs flow through command execution and event processing without manual threading.

4. **Standard JSON output**: Compatible with log aggregation systems (CloudWatch, Datadog, Elasticsearch).

5. **Call-site metadata**: HasCallStack provides file and line information automatically.

6. **Performance**: fast-logger provides buffered, thread-safe writes with minimal overhead.

7. **Non-intrusive**: NOT re-exported from Core — logging is explicit opt-in.

8. **Migration path**: Clear upgrade path from Console.print to Log.info for 82 call sites across 17 files.

### Negative

1. **Global logger requires unsafePerformIO**: Mitigated by `{-# NOINLINE #-}` pragma and standard pattern from monad-logger/katip.

2. **Thread-local context cleanup**: Requires `Task.finally` to prevent memory leaks. Mitigated by framework-internal usage only.

3. **No automatic child thread propagation**: Context does not flow to AsyncTask.run by default. This is intentional to avoid stale context in long-lived workers.

4. **Breaking change for Console.log users**: Console.log will be deprecated in favor of Log.debug. Migration guide required.

5. **Runtime log level filtering via `LOG_LEVEL` env var**: Defaults to `Info`. Accepts `Debug`, `Info`, `Warn`, `Error`, `Critical` (case-insensitive). Read once at startup.

### Risks

1. **unsafePerformIO safety**: The global logger uses `unsafePerformIO` for initialization. This is safe because:
   - `{-# NOINLINE #-}` ensures single initialization
   - Logger is write-only and thread-safe
   - Standard pattern used by monad-logger, katip, fast-logger

2. **Thread-local context leaks**: If `withScope` is used without proper cleanup, thread contexts accumulate in memory. Mitigated by:
   - `Task.finally` ensures cleanup on exceptions
   - Framework-internal usage only (not exposed to user code)
   - ConcurrentMap.delete removes context when scope exits

3. **HasCallStack overhead**: Minimal runtime cost (GHC optimizes away when not used), but adds constraint to function signatures. Acceptable trade-off for automatic call-site metadata.

4. **JSON serialization performance**: Constructing JSON for every log message has overhead. Mitigated by:
   - fast-logger's buffered writes
   - Structured logging is standard practice in production systems
   - Future work: binary log format for extreme performance needs

## References

- [GitHub Issue #402](https://github.com/neohaskell/NeoHaskell/issues/402) — Original feature request
- [Console.hs](../../core/core/Console.hs) — Current debug/print functions
- [Task.hs](../../core/core/Task.hs) — NeoHaskell's effect type
- [ConcurrentMap.hs](../../core/concurrency/ConcurrentMap.hs) — Thread-safe map for context storage
- [Json.hs](../../core/json/Json.hs) — JSON serialization
- [Service/CommandExecutor.hs](../../core/service/Service/CommandExecutor.hs) — Command execution integration point
- [Service/Dispatcher.hs](../../core/service/Service/Dispatcher.hs) — Event processing integration points
- [fast-logger](https://hackage.haskell.org/package/fast-logger) — Logging backend library
- [monad-logger](https://hackage.haskell.org/package/monad-logger) — Reference for global logger pattern
- [katip](https://hackage.haskell.org/package/katip) — Reference for structured logging design
