# ADR-0029: Worker Crash Recovery for Outbound Integrations

## Status

Proposed

## Context

NeoHaskell's integration dispatcher silently drops concurrent outbound HTTP calls when worker threads crash from uncaught IO exceptions. This manifests as GitHub issue #400: "Concurrent outbound HTTP integration calls silently dropped."

### Current Behavior

When multiple outbound integrations fire HTTP calls concurrently, some calls silently fail to return. Neither `onSuccess` nor `onError` callbacks are invoked, leaving the application in an inconsistent state.

### Root Cause

The `Task.asResult` function in `safeWorkerLoop` (Dispatcher.hs) catches only ExceptT errors, not IO exceptions. When a worker thread dies from an uncaught IO exception:

1. The `Err` branch in `safeWorkerLoop` never executes
2. `ConcurrentMap.remove` is never called
3. The dead worker entry stays in the `entityWorkers` map
4. Subsequent events are dispatched to the dead worker's channel
5. The channel is never read, so events are silently lost

### Secondary Issues

1. **No timeout backstop**: `eventProcessingTimeoutMs` defaults to `Nothing`, so hanging operations never recover
2. **Reaper doesn't run for stateless workers**: The reaper only starts when lifecycle runners exist (line 319 of Dispatcher.hs), leaving dead stateless workers in the map indefinitely

### Affected Code

- `core/core/Task.hs`: Missing exception-safe result conversion
- `core/service/Service/Integration/Dispatcher.hs`: Both worker loops (stateless and lifecycle) use unsafe `asResult`

### GitHub Issue

[#400: Concurrent outbound HTTP integration calls silently dropped](https://github.com/neohaskell/NeoHaskell/issues/400)

## Decision

We implement four changes to fix the bug and prevent future occurrences:

### 1. Add `Task.asResultSafe` to Task.hs

Create a new function that catches both ExceptT errors AND IO exceptions:

```haskell
asResultSafe ::
  forall err value err2.
  (Show err) =>
  Task err value ->
  Task err2 (Result Text value)
asResultSafe task = do
  result <-
    task.runTask
      |> Except.runExceptT
      |> Exception.try @SomeException
      |> fromIO
  case result of
    Either.Left someException ->
      yield (Result.Err (show someException |> Text.fromLinkedList))
    Either.Right (Either.Left err) ->
      yield (Result.Err (show err |> Text.fromLinkedList))
    Either.Right (Either.Right value) ->
      yield (Result.Ok value)
```

This ensures the `Err` branch fires on ANY exception, not just ExceptT errors.

### 2. Use `asResultSafe` in Both Worker Loops

Replace `Task.asResult` with `Task.asResultSafe` in:
- Stateless worker loop (Dispatcher.hs)
- Lifecycle worker loop (Dispatcher.hs)

This guarantees `ConcurrentMap.remove` is called when workers crash, cleaning up dead entries.

### 3. Set Default Timeout to 30 Seconds

Change `eventProcessingTimeoutMs` default from `Nothing` to `Just 30000` in `DispatcherConfig`.

Defense-in-depth: even if something novel hangs, workers recover after 30 seconds.

### 4. Start Reaper Unconditionally

Remove the `Array.isEmpty lifecycleRunners` check when `enableReaper = True`.

This ensures dead stateless workers are eventually reaped even without lifecycle runners.

### Why These Changes?

- **asResultSafe**: Catches all exceptions, not just ExceptT errors
- **30-second timeout**: Prevents indefinite hangs from novel failure modes
- **Unconditional reaper**: Cleans up dead stateless workers
- **Minimal changes**: ~15 lines across 2 files, no new dependencies

## Consequences

### Positive

1. **Workers properly recover from crashes**: Dead workers are removed from the map, allowing new events to spawn fresh workers
2. **No silent failures**: All exceptions are caught and logged, making failures visible
3. **Defense-in-depth**: 30-second timeout prevents indefinite hangs
4. **Stateless worker cleanup**: Reaper runs even without lifecycle runners
5. **Minimal code changes**: ~15 lines across 2 files, no new dependencies
6. **No breaking API changes**: `asResultSafe` is a new function, existing code unaffected

### Negative

1. **Broad exception catching**: `SomeException` catches all exceptions including `AsyncCancelled`. This is intentional for worker loops — when a worker is force-cancelled (e.g., channel write timeout), catching `AsyncCancelled` ensures the worker is properly deregistered from the map. Normal shutdown uses the `Stop` message channel path, not async cancellation.
2. **Default timeout change**: 30-second timeout may need tuning per deployment. Mitigated by making it configurable via `DispatcherConfig`.
3. **Test updates required**: Existing tests may rely on `eventProcessingTimeoutMs = Nothing`. Migration guide needed.

### Risks

1. **SomeException is broad**: Could mask programming errors. Mitigated by logging the exception before continuing, making failures visible in logs.
2. **Timeout affects existing deployments**: Changing the default could affect existing systems. Mitigated by making it configurable and documenting the change in release notes.
3. **AsyncCancelled handling**: `asResultSafe` intentionally catches `AsyncCancelled` — in worker loops, this ensures dead workers are deregistered even when force-cancelled. Normal shutdown is handled via the `Stop` message path (not async cancellation), so shutdown behavior is unaffected. The force-cancel test in `DispatcherSpec.hs` validates this.

### Migration Path

1. **For framework maintainers**: Update tests that rely on `eventProcessingTimeoutMs = Nothing` to explicitly set it
2. **For application developers**: No changes required unless custom timeout is needed
3. **For deployments**: Monitor logs for new exception messages, tune timeout if needed

## References

- [GitHub Issue #400](https://github.com/neohaskell/NeoHaskell/issues/400) — Original bug report
- [ADR-0008: Integration Pattern](0008-integration-pattern.md) — Integration architecture
- [ADR-0015: HTTP Outbound Integration](0015-http-outbound-integration.md) — HTTP integration design
- [Task.hs](../../core/core/Task.hs) — NeoHaskell's effect type
- [Service/Integration/Dispatcher.hs](../../core/service/Service/Integration/Dispatcher.hs) — Worker loop implementation
