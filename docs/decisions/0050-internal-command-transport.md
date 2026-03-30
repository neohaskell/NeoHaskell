# ADR-0050: Internal Command Transport

## Status

Proposed

## Context

### Current State

NeoHaskell commands currently declare their transport exposure through `TransportsOf`, and the service runtime builds command handlers grouped by transport name. This works well for public transports such as `WebTransport`, but it creates a gap for commands that are meant to be triggered only by the system.

Integration-emitted commands are dispatched through `Dispatcher.dispatchCommand`, which flattens all registered command handlers and invokes the selected handler with `Auth.anonymousContext`. That means a command can be technically dispatchable from integrations, but if its decision logic checks `ctx.user`, the integration path sees `Nothing` and the command is rejected.

At the same time, commands that should only exist for process-manager or integration workflows are often declared with `WebTransport` because that is the only practical way to make them dispatchable today. The testbed already documents this mismatch:

```haskell
-- NOTE: Using WebTransport for testability. In production, this would use InternalTransport
type instance TransportsOf ReserveStock = '[WebTransport]
```

This causes two problems:

1. **Intent is not explicit**: Jess cannot tell from the type-level declaration whether a command is public or internal-only.
2. **Public surface grows accidentally**: Commands used only for internal orchestration are exposed as HTTP endpoints through `WebTransport`.
3. **Integration auth behavior is surprising**: A command can look valid for system dispatch but still reject because it was written for a user-authenticated web flow.

### Use Cases

- **Process manager command dispatch**: An outbound integration reacts to `ItemAdded` and emits `ReserveStock` to another bounded context without exposing `ReserveStock` as a public HTTP endpoint.
- **Internal workflow commands**: A background integration records derived information such as transcription or metrics by emitting commands that should only ever be called by the runtime.
- **Split public and internal intent**: Jess keeps `UploadProposalPdf` as a user-facing authenticated web command and creates a separate internal command for system-only follow-up work.

### Design Goals

1. **Make transport intent explicit**: Jess should be able to mark a command as internal-only in one obvious place.
2. **Keep internal commands off the web surface**: Internal commands must remain dispatchable by integrations while never appearing in `WebTransport` routes or OpenAPI output.
3. **Preserve the simple command model**: A command should still declare transports through `TransportsOf`; if Jess needs both public and internal behavior, she should create two commands with distinct intent.
4. **Avoid extra runtime ceremony**: Jess should not need to register or run a fake server transport just to make internal dispatch work.

### GitHub Issue

- [#398: feat(service): Support internal-only commands that skip auth and aren't exposed via web transport](https://github.com/neohaskell/NeoHaskell/issues/398)

## Decision

### 1. Transport Name: `InternalTransport`

The new internal-only command marker is named `InternalTransport`.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| `SystemTransport` | Rejected | Too broad. "System" could refer to OS integration, scheduled jobs, or framework internals rather than command exposure. |
| `PrivateTransport` | Rejected | "Private" is vague and does not clearly describe the runtime behavior. |
| `BackgroundTransport` | Rejected | Not all internal commands are background work, and the name hides the dispatch and exposure semantics. |
| `InternalTransport` | **Chosen** | Clear, explicit, and aligned with the issue language. Jess can immediately infer that the command is callable by the system but not part of the public API. |

### 2. Module Placement

`InternalTransport` lives in its own transport module alongside the existing transport adapters.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| `core/service/Service/Command/Core.hs` | Rejected | `TransportsOf` is declared there, but concrete transports belong in the transport layer, not in command abstraction. |
| `core/service/Service/Application/Internal.hs` | Rejected | The feature is not application boot logic; it is a transport concept used by command definitions and service runtime wiring. |
| `core/service/Service/Transport/Internal.hs` | **Chosen** | Matches the established transport layout with `Service.Transport.Web` and `Service.Transport.Cli`. Jess can discover transport adapters in one directory. |

**File layout:**

```text
core/service/Service/Transport/Internal.hs
```

### 3. Runtime Behavior: Compile-Time Transport Filtering

`InternalTransport` is a type-level marker in `TransportsOf`. It is **not** a `Transport` instance — it has no `buildHandler`, `assembleTransport`, or `runTransport` methods.

The separation between public and internal commands is enforced at compile time through type families in `Service.ServiceDefinition.Core`:

1. **`PublicTransports`** — A type family that filters `InternalTransport` out of a command's transport list. Only the remaining public transports are passed to `BuildHandlersForTransports`, so internal commands never produce public endpoint handlers or schemas.
2. **`IncludesInternalTransport`** — A type family that returns `'True` if `InternalTransport` appears in the transport list. At runtime, `includeInDispatchMap` uses this to decide whether a command contributes a transport-independent dispatch handler. Only `InternalTransport` commands are included in this map — `WebTransport`-only commands are not.
3. **`AssertNoMixedTransports`** — A type family that produces a compile-time error if a command declares both `InternalTransport` and any public transport. This enforces the rule that a single command serves one intent.

The runtime data flow uses a 3-tuple instead of the previous 2-tuple:

1. `Service.ServiceDefinition.Core.buildEndpointsByTransport` returns `(public handlers by transport, public schemas by transport, dispatch handlers)`.
2. `Service.Application.runWithResolved` merges the three maps independently across all services, detecting duplicate command names at merge time.
3. The dispatch handler map — which contains **only `InternalTransport` commands** — flows directly to `Dispatcher.dispatchCommand` and inbound integration workers, bypassing the public transport layer entirely.

This means integrations can dispatch commands, but only commands explicitly declared with `InternalTransport`. A `WebTransport` command is not reachable from integrations; if an action needs to be triggerable from both an HTTP call and an integration, two separate commands with separate names must be defined. This keeps the public transport layer and the integration dispatch layer distinct. Jess marks a command as internal in one obvious place, and the compiler prevents accidental exposure.

### 4. Type Definition

```haskell
data InternalTransport = InternalTransport

type instance NameOf InternalTransport = "InternalTransport"
```

This type exists only as a marker for `TransportsOf` and for type-level filtering in service definition code.

### 5. Command Declaration Model

Commands continue to declare transport exposure through `TransportsOf`.

```haskell
type instance TransportsOf RecordTranscription = '[InternalTransport]

type instance TransportsOf UploadProposalPdf = '[WebTransport]
```

If Jess needs both a public authenticated action and an internal workflow action, she writes two commands with separate names and decision logic rather than trying to make one command serve both roles. A single command cannot combine `InternalTransport` and public transports.

This keeps the mental model simple:

- **One command = one intent**
- **Transport list = exposure policy**
- **Need both public and internal entry points = define two commands**

### 6. Public API and Runtime Hooks

The transport marker module is minimal:

```haskell
module Service.Transport.Internal (
  InternalTransport (..),
) where

data InternalTransport = InternalTransport

type instance NameOf InternalTransport = "InternalTransport"
```

The type families that implement the compile-time filtering live in `Service.ServiceDefinition.Core`:

```haskell
-- Filters InternalTransport out of the transport list, leaving only public transports.
type family PublicTransports (transports :: [Type]) :: [Type]

-- Returns 'True if InternalTransport is in the transport list.
type family IncludesInternalTransport (transports :: [Type]) :: Bool

-- Produces a compile-time error if InternalTransport is mixed with public transports.
type family AssertNoMixedTransports (transports :: [Type]) :: Constraint
```

The runtime entry points are owned by service runtime modules:

```haskell
-- Service.ServiceDefinition.Core
buildEndpointsByTransport ::
  EventStore Json.Value ->
  Record.ContextRecord Record.I cmds ->
  Map Text TransportValue ->
  Task Text (Map Text (Map Text EndpointHandler), Map Text (Map Text EndpointSchema), Map Text EndpointHandler)

-- Service.Integration.Dispatcher
dispatchCommand ::
  Map Text EndpointHandler ->
  Integration.CommandPayload ->
  Task Text Unit
```

Implementation consequences of this API:

- `Service.ServiceDefinition.Core` uses `PublicTransports` to filter the transport list before building public handlers, and `IncludesInternalTransport` to decide whether a command contributes a dispatch handler.
- `Service.Application` merges the 3-tuple across all services, detecting duplicate command names at merge time as `Task` errors.
- `Service.Application.Transports` remains responsible only for runnable, user-facing transports and never sees `InternalTransport`.
- `Service.Transport.Web` only sees command handlers registered under actual public transports such as `WebTransport`.

### 7. Authentication Model

`InternalTransport` means **the command is not user-authenticated because it is not user-addressable**. The runtime dispatch path remains system-triggered and uses an anonymous, non-user request context.

This is not a replacement for authenticated web commands. It is a separate command category:

- `WebTransport`: public surface; auth may be required by the command and by the web runtime.
- `InternalTransport`: runtime-only surface; never exposed over HTTP.

Jess should not write an `InternalTransport` command whose purpose depends on user identity from `ctx.user`. `InternalTransport` does not grant a privileged or synthetic user identity; it only removes web exposure and web-auth mediation. If the action requires an authenticated user, it should remain a web command.

## Consequences

### Positive

- Jess can declare internal-only workflow commands explicitly at the type level.
- Internal commands stop leaking into the HTTP surface and OpenAPI docs.
- Process-manager integrations can emit commands without the awkward "pretend this is a web endpoint" workaround.
- The testbed comment about `ReserveStock` gains a real implementation path.

### Negative

- The service runtime returns a 3-tuple from endpoint building, adding a separate dispatch handler map alongside the public transport maps.
- Jess must model public and internal entry points as separate commands if both are needed. Attempting to mix `InternalTransport` with public transports in a single `TransportsOf` declaration produces a compile-time error.
- `Service.ServiceDefinition.Core` gains several type families for compile-time transport filtering.

### Risks

- Internal commands could still accidentally depend on `ctx.user`, leading to confusing rejection if the business logic is not modeled correctly.
- The endpoint-building code could regress and accidentally expose internal commands through web routes.
- Duplicate command names across services could silently mask each other in the integration dispatch map if merges are not validated.

### Mitigations

- Add regression tests proving that internal commands are dispatchable through integrations but absent from `WebTransport` endpoint maps.
- Detect duplicate command names when merging dispatch handlers across services.
- Keep the module API tiny and the ADR explicit that `InternalTransport` is marker-only.
- Document the split-intent rule: if the action needs both web and internal entry points, create two commands.

## References

- [#398: feat(service): Support internal-only commands that skip auth and aren't exposed via web transport](https://github.com/neohaskell/NeoHaskell/issues/398)
- [ADR-0003: Command Abstraction and Flow](0003-command-abstraction-and-flow.md)
- [ADR-0034: CliTransport — Command-Line Interface Transport](0034-cli-transport.md)
- [core/service/Service/Command/Core.hs](../../core/service/Service/Command/Core.hs)
- [core/service/Service/ServiceDefinition/Core.hs](../../core/service/Service/ServiceDefinition/Core.hs)
- [core/service/Service/Application.hs](../../core/service/Service/Application.hs)
- [core/service/Service/Application/Transports.hs](../../core/service/Service/Application/Transports.hs)
- [core/service/Service/Integration/Dispatcher.hs](../../core/service/Service/Integration/Dispatcher.hs)
- [core/service/Service/Transport/Internal.hs](../../core/service/Service/Transport/Internal.hs)
- [core/service/Service/Transport/Cli.hs](../../core/service/Service/Transport/Cli.hs)
- [testbed/src/Testbed/Stock/Commands/ReserveStock.hs](../../testbed/src/Testbed/Stock/Commands/ReserveStock.hs)
