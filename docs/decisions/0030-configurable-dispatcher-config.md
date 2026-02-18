# ADR-0030: Configurable Dispatcher Config via Application Builder

## Status

Proposed

## Context

NeoHaskell's integration dispatcher uses a hardcoded `Dispatcher.defaultConfig` in `startIntegrationSubscriber` (Application/Integrations.hs). This creates problems for applications with non-standard requirements:

### Current Behavior

The `eventProcessingTimeoutMs` is hardcoded to `Just 30000` (30 seconds) in `Dispatcher.defaultConfig`. Applications calling slow external APIs have no way to customize this timeout without forking the framework.

### Real-World Use Cases

1. **AI/LLM integrations**: OpenAI, Anthropic, and similar APIs can take 60-120 seconds for complex requests
2. **Batch processing**: Outbound integrations that trigger batch jobs may need longer timeouts
3. **File processing**: Large file uploads/downloads to external services
4. **Legacy system integrations**: Slow SOAP/XML-RPC endpoints

### Current Workaround

None. Applications are forced to either:
- Accept silent timeouts after 30 seconds
- Fork the framework and modify `Dispatcher.defaultConfig`
- Avoid using outbound integrations for slow operations

### Why This Matters

The dispatcher config controls critical operational parameters:
- `eventProcessingTimeoutMs`: How long before a worker is considered hung
- `workerChannelCapacity`: Backpressure for high-throughput scenarios
- `channelWriteTimeoutMs`: Timeout for channel writes
- `idleTimeoutMs`, `reaperIntervalMs`, `enableReaper`: Worker lifecycle tuning

Applications should control these parameters without framework modifications.

### GitHub Issue

[#407: Expose DispatcherConfig through Application builder](https://github.com/neohaskell/NeoHaskell/issues/407)

## Use Cases

### Use Case 1: AI Integration with Long Timeouts

```haskell
-- Application calling OpenAI with 2-minute timeout
Application.new
  |> Application.withDispatcherConfig
       Dispatcher.defaultConfig { eventProcessingTimeoutMs = Just 120000 }
  |> Application.withService aiService
  |> Application.run
```

### Use Case 2: High-Throughput Event Processing

```haskell
-- Application with larger channel capacity for burst traffic
Application.new
  |> Application.withDispatcherConfig
       Dispatcher.defaultConfig
         { workerChannelCapacity = 500
         , channelWriteTimeoutMs = 10000
         }
  |> Application.withService eventProcessor
  |> Application.run
```

### Use Case 3: Disable Reaper for Testing

```haskell
-- Test environment with reaper disabled
Application.new
  |> Application.withDispatcherConfig
       Dispatcher.defaultConfig { enableReaper = False }
  |> Application.withService testService
  |> Application.run
```

## Requirements

1. **Backwards compatible**: Existing applications continue working without changes
2. **Simple API**: Follow existing `with*` pattern (e.g., `withCors`, `withHealthCheck`)
3. **No factory needed**: DispatcherConfig doesn't depend on other app config
4. **Re-export types**: Application module should re-export `DispatcherConfig` and `defaultConfig`
5. **Thread through**: Config must reach `Dispatcher.newWithLifecycleConfig` in `startIntegrationSubscriber`

## Decision

We add `dispatcherConfig` to the `Application` record and expose it via `withDispatcherConfig`.

### Type Definitions

#### Application Record (Service/Application.hs)

Add field to `Application` record:

```haskell
data Application = Application
  { -- ... existing fields ...
  , dispatcherConfig :: Maybe Dispatcher.DispatcherConfig
  }
```

#### Builder Function (Service/Application.hs)

Add `with*` function following existing pattern:

```haskell
withDispatcherConfig ::
  Dispatcher.DispatcherConfig ->
  Application ->
  Application
withDispatcherConfig config app =
  app { dispatcherConfig = Just config }
```

#### Re-exports (Service/Application.hs)

Re-export dispatcher types for convenience:

```haskell
module Service.Application
  ( -- ... existing exports ...
  , Dispatcher.DispatcherConfig(..)
  , Dispatcher.defaultConfig
  ) where
```

#### Integration Point (Service/Application/Integrations.hs)

Update `startIntegrationSubscriber` signature:

```haskell
startIntegrationSubscriber ::
  Maybe Dispatcher.DispatcherConfig ->
  EventStore ->
  Array OutboundRunner ->
  Array OutboundLifecycleRunner ->
  Map Text EndpointHandler ->
  ActionContext ->
  Task Text (Maybe IntegrationDispatcher)
startIntegrationSubscriber maybeConfig eventStore outboundRunners lifecycleRunners endpointHandlers actionContext = do
  let config = case maybeConfig of
        Just c -> c
        Nothing -> Dispatcher.defaultConfig
  
  -- ... rest of function uses `config` instead of `Dispatcher.defaultConfig` ...
```

#### Caller Update (Service/Application.hs)

Thread config from `Application` record to `startIntegrationSubscriber`:

```haskell
-- In runWithResolved function
integrationDispatcher <-
  Integrations.startIntegrationSubscriber
    app.dispatcherConfig  -- Pass Maybe DispatcherConfig
    eventStore
    outboundRunners
    lifecycleRunners
    endpointHandlers
    actionContext
```

### Usage Pattern

```haskell
-- Default behavior (30-second timeout)
Application.new
  |> Application.withService myService
  |> Application.run

-- Custom timeout for slow APIs
Application.new
  |> Application.withDispatcherConfig
       Dispatcher.defaultConfig { eventProcessingTimeoutMs = Just 120000 }
  |> Application.withService myService
  |> Application.run

-- Multiple customizations
Application.new
  |> Application.withDispatcherConfig
       Dispatcher.defaultConfig
         { eventProcessingTimeoutMs = Just 90000
         , workerChannelCapacity = 200
         , enableReaper = True
         }
  |> Application.withService myService
  |> Application.run
```

### Why This Approach?

1. **Matches existing patterns**: `withCors`, `withHealthCheck` use the same `Maybe X` + default pattern
2. **No factory needed**: DispatcherConfig is a simple record with no dependencies
3. **Focused changes**: New record field, builder function, module re-exports, call-site and signature updates, and tests
4. **Type-safe**: Compiler enforces correct config structure
5. **Discoverable**: Re-exports make `DispatcherConfig` and `defaultConfig` available from Application module

### Why Not an Environment Variable?

Issue #407 proposed two alternatives: (a) reading `EVENT_PROCESSING_TIMEOUT_MS` from the environment, and (b) exposing `DispatcherConfig` via the builder. We chose (b) because:

1. **Single field vs. full config**: An env var only solves `eventProcessingTimeoutMs`. Customizing `workerChannelCapacity`, `channelWriteTimeoutMs`, or `enableReaper` would require proliferating additional env vars.
2. **Implicit vs. explicit**: Env vars introduce implicit global configuration invisible at the call site. The builder makes configuration explicit and visible in the application wiring.
3. **Testability**: Builder configuration is trivially testable — pass a config value and verify behavior. Env vars require test harness setup and cleanup, and risk cross-test contamination.
4. **Type safety**: `DispatcherConfig` fields are typed and validated at the call site. Env vars require string parsing with runtime failure modes.
5. **Extensibility**: If `DispatcherConfig` gains new fields, the builder approach extends naturally. Env vars require new variable names, parsing, and documentation for each field.

## Consequences

### Positive

1. **Applications control dispatcher behavior**: No more hardcoded timeouts
2. **Backwards compatible**: Existing apps continue working (Nothing → defaultConfig)
3. **Follows framework conventions**: Same pattern as `withCors`, `withHealthCheck`
4. **Type-safe**: Compiler catches config errors
5. **Focused code changes**: Record field, builder function, module re-exports, call-site update, and tests
6. **No new dependencies**: Uses existing Dispatcher module
7. **Discoverable API**: Re-exports make config types easy to find

### Negative

1. **More configuration surface**: Applications can misconfigure dispatcher (e.g., disable reaper in production)
2. **Documentation needed**: Need to explain when/why to customize dispatcher config
3. **Testing burden**: Need tests for custom configs

### Risks

1. **Misconfiguration**: Applications could set `eventProcessingTimeoutMs = Nothing` and hang indefinitely. Mitigated by documentation and sensible defaults.
2. **Breaking changes in future**: If DispatcherConfig gains new fields, applications using record syntax may break. Mitigated by using `defaultConfig { field = value }` pattern in docs.
3. **Performance tuning complexity**: Applications may cargo-cult config values without understanding them. Mitigated by comprehensive documentation with use cases.

### Migration Path

1. **For framework maintainers**: Add field to Application record, update startIntegrationSubscriber signature, thread config through
2. **For application developers**: No changes required unless custom config is needed
3. **For documentation**: Add section to Application guide explaining dispatcher config

## References

- [GitHub Issue #407](https://github.com/neohaskell/NeoHaskell/issues/407) — Feature request
- [ADR-0029: Worker Crash Recovery](0029-worker-crash-recovery.md) — Context for 30-second default timeout
- [ADR-0008: Integration Pattern](0008-integration-pattern.md) — Integration architecture
- [Service/Application.hs](../../core/service/Service/Application.hs) — Application builder
- [Service/Application/Integrations.hs](../../core/service/Service/Application/Integrations.hs) — Integration subscriber
- [Service/Integration/Dispatcher.hs](../../core/service/Service/Integration/Dispatcher.hs) — Dispatcher implementation
