# Integration Module Bug Fixes

This document describes bugs found during code review of the `feat/integration` branch. Each issue includes the problem, location, and suggested fix.

---

## 1. Timer.every Delays First Command Emission

**Severity**: Medium  
**File**: `core/service/Integration/Timer.hs`, lines 103-111

### Problem

The `Timer.every` function sleeps before emitting the first command. If a user sets a 30-second interval, no command will be emitted for the first 30 seconds.

```haskell
every config = do
  let loop emit tick = do
        AsyncTask.sleep config.interval  -- Sleeps FIRST
        emit (config.toCommand tick)     -- Then emits
        loop emit (tick + 1)
  Integration.inbound
    Integration.InboundConfig
      { run = \emit -> loop emit 1
      }
```

Most timer implementations emit immediately, then wait for the interval before subsequent emissions. The documentation states "tick count (starting at 1)" which suggests users expect tick 1 to arrive immediately.

### Fix

Swap the order of `emit` and `sleep`:

```haskell
every config = do
  let loop emit tick = do
        emit (config.toCommand tick)     -- Emit first
        AsyncTask.sleep config.interval  -- Then sleep
        loop emit (tick + 1)
  Integration.inbound
    Integration.InboundConfig
      { run = \emit -> loop emit 1
      }
```

### Testing

Update or add tests to verify:
1. First tick arrives immediately (within milliseconds, not after interval)
2. Subsequent ticks arrive at interval spacing

---

## 2. Silent Decode Errors in Outbound Processing

**Severity**: Low  
**File**: `core/service/Service/Application.hs`, lines 740-742

### Problem

When an event fails to decode in `withOutbound`, the error is silently dropped:

```haskell
processEvent = \rawEvent -> do
    case Json.decode @event rawEvent.event of
      Err _ -> Task.yield Array.empty  -- Silent drop, no logging
      Ok decodedEvent -> do
        -- ... process event
```

This makes debugging integration issues difficult. If an integration receives events that fail to parse (wrong event type, schema mismatch, etc.), there's no indication of the problem.

### Fix

Add logging when decode fails. Include entity type name and stream ID for context:

```haskell
processEvent = \rawEvent -> do
    case Json.decode @event rawEvent.event of
      Err decodeErr -> do
        Console.print [fmt|[Integration] Failed to decode event for #{typeName} (stream: #{rawEvent.streamId}): #{decodeErr}|]
          |> Task.ignoreError
        Task.yield Array.empty
      Ok decodedEvent -> do
        -- ... process event
```

Note: `typeName` is already bound on line 736.

---

## 3. Silent Processing Errors in Dispatcher

**Severity**: Low  
**File**: `core/service/Service/Integration/Dispatcher.hs`, lines 360-367 and 379-386

### Problem

Both `processStatelessEvent` and `processLifecycleEvent` silently drop processing errors:

```haskell
result <- runner.processEvent event |> Task.asResult
case result of
  Ok commands -> do
    -- dispatch commands
  Err _err ->
    Task.yield unit  -- Silent drop
```

The `Err _err` pattern shows the error is intentionally ignored. For a production system, this makes it impossible to diagnose failing integrations.

### Fix

Add logging in both functions. Example for `processStatelessEvent`:

```haskell
result <- runner.processEvent event |> Task.asResult
case result of
  Ok commands -> do
    commands
      |> Task.forEach \payload -> do
        dispatchCommand dispatcher.commandEndpoints payload
  Err err -> do
    Console.print [fmt|[Dispatcher] Error processing event (stream: #{event.streamId}): #{err}|]
      |> Task.ignoreError
```

Apply the same pattern to `processLifecycleEvent` on lines 379-386.

---

## 4. Dispatcher.dispatchCommand Silently Drops Unhandled Commands

**Severity**: Low  
**File**: `core/service/Service/Integration/Dispatcher.hs`, lines 389-403

### Problem

When a command is emitted but no handler exists, the Dispatcher version silently drops it:

```haskell
dispatchCommand endpoints payload = do
  let cmdType = payload.commandType
  case Map.get cmdType endpoints of
    Just handler -> do
      -- ... handle command
    Nothing ->
      Task.yield unit  -- Silent drop
```

Compare to `Service.Application.dispatchCommand` (lines 608-610) which logs:

```haskell
Nothing -> do
  Console.print [fmt|[Integration] No handler found for command: #{cmdType}|]
```

This inconsistency means debugging behavior differs depending on which code path handles the command.

### Fix

Match the Application behavior:

```haskell
dispatchCommand endpoints payload = do
  let cmdType = payload.commandType
  case Map.get cmdType endpoints of
    Just handler -> do
      let cmdBytes = Json.encodeText payload.commandData |> Text.toBytes
      let responseCallback _ = Task.yield unit
      handler cmdBytes responseCallback
        |> Task.mapError (\err -> [fmt|Command dispatch failed for #{cmdType}: #{err}|])
    Nothing -> do
      Console.print [fmt|[Integration] No handler found for command: #{cmdType}|]
        |> Task.ignoreError
```

---

## Optional: Consolidate Duplicate Code

**Severity**: Style  
**Files**: `Service/Application.hs` and `Service/Integration/Dispatcher.hs`

### Problem

There are two nearly identical `dispatchCommand` implementations. Additionally, these types are duplicated:
- `OutboundRunner`
- `OutboundLifecycleRunner`
- `WorkerState`

### Suggestion

Consider extracting `dispatchCommand` to a shared module (perhaps `Service.Integration.Internal` or similar). The Dispatcher types could be reused by Application to eliminate duplication.

This is a refactoring opportunity, not a bug fix. Address after the functional bugs are resolved.
