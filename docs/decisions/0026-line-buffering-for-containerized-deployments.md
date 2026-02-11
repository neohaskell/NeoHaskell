# ADR-0026: Line Buffering for Containerized Deployments

## Status

Proposed

## Context

NeoHaskell apps using `Console.print` for startup logging face invisible output when deployed in containerized environments (Docker, Fly.io, Kubernetes, Railway). This occurs because GHC's runtime defaults stdout to block buffering (~8KB buffer) when `isatty()` returns false, which is always the case in containers where stdout is redirected to logging infrastructure.

The result: startup logs, health check confirmations, and diagnostic output from `Console.print` remain buffered and invisible until the buffer fills or the process exits. This breaks observability during critical deployment phases.

### Use Cases

- **Docker deployments**: Developers running `docker logs` see no output from `Console.print` calls during application startup
- **Fly.io deployments**: Platform health checks and startup logs are invisible in the Fly.io dashboard until buffer fills
- **Kubernetes deployments**: `kubectl logs` shows no output from initialization steps, making debugging startup failures impossible
- **Railway/Render deployments**: Similar buffering issues prevent real-time visibility into application startup

### GitHub Issue

- [#390: Application.run should set stdout/stderr to LineBuffering for containerized deployments](https://github.com/neohaskell/NeoHaskell/issues/390)

## Decision

### 1. Add Buffering Mode Configuration to Application.run

Add `hSetBuffering stdout LineBuffering` and `hSetBuffering stderr LineBuffering` as the very first operations in `Application.run`, before even loading the `.env` file. This ensures all subsequent output (including .env loading logs, config validation, EventStore initialization) is immediately visible.

The buffering setup becomes step "0" in the startup sequence:

```haskell
run :: Application -> Task Text Unit
run app = do
  -- 0. Configure stdout/stderr for container visibility
  GhcIO.hSetBuffering GhcIO.stdout GhcIO.LineBuffering
  GhcIO.hSetBuffering GhcIO.stderr GhcIO.LineBuffering
  
  -- 1. Load .env file if present
  Environment.loadEnvFileIfPresent ".env"
  
  -- 2. Load config
  case app.configSpec of ...
  
  -- 3. Validate EventStore and create it
  eventStore <- case app.eventStoreFactory of ...
```

### 2. Scope of Change

**Only `Application.run` is modified** — not `runWith` or `runWithAsync`:
- `run` is the primary entry point for production applications
- `runWith` is for testing with provided EventStores and doesn't need buffering control
- `runWithAsync` delegates to `runWith`, inheriting the same testing-focused scope

### 3. Files Modified

| File | Change |
|------|--------|
| `core/service/Service/Application.hs` | Add `import System.IO qualified as GhcIO` and two `hSetBuffering` calls at the start of `run` |

### 4. Security Considerations

No security implications. Buffering mode affects only when data is flushed to the OS, not the content or access control of the data.

### 5. Performance Considerations

**Negligible impact for intended use case:**
- LineBuffering adds a flush operation per line vs block buffering's ~8KB threshold
- For startup logs (typically <100 lines), the overhead is unmeasurable
- `Console.print` is designed for human-readable output, not high-throughput logging

**Production logging guidance:**
- For high-volume production logging, applications should use structured logging frameworks (e.g., `katip`, `fast-logger`) that handle buffering efficiently
- `Console.print` remains appropriate for startup diagnostics, health check confirmations, and development output

### 6. Example Usage

No API changes. Existing code works identically, but with visible output in containers:

```haskell
-- Before this ADR, users had to manually configure buffering:
main :: IO ()
main = do
  GhcIO.hSetBuffering GhcIO.stdout GhcIO.LineBuffering
  GhcIO.hSetBuffering GhcIO.stderr GhcIO.LineBuffering
  app |> Application.run |> Task.runOrPanic

-- After this ADR, Application.run handles it automatically:
main :: IO ()
main = do
  app |> Application.run |> Task.runOrPanic
```

## Consequences

### Positive

1. **Immediate observability**: Startup logs, health check confirmations, and diagnostic output are visible in real-time across all container platforms
2. **Zero API changes**: Existing applications gain the benefit automatically on upgrade
3. **Minimal code change**: Two lines added to `Application.run`, no new dependencies
4. **Follows platform conventions**: LineBuffering is the standard for containerized applications (matches Python's `-u` flag, Node.js default behavior)

### Negative

1. **Slight performance overhead**: Each `Console.print` call triggers a flush. For high-volume logging (>1000 lines/sec), this could add measurable overhead
2. **Not configurable**: Applications cannot opt out of LineBuffering without forking `Application.run`

### Risks

1. **Performance regression for high-volume Console.print usage**: Mitigated by documentation guidance to use proper logging frameworks for production logging
2. **Unexpected behavior change**: Applications relying on block buffering behavior (unlikely) will see different flush timing. Mitigated by the fact that block buffering in containers was already unpredictable
