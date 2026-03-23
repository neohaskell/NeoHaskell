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

### 3. Runtime Behavior: Marker-Only Internal Transport

`InternalTransport` is a type-level marker in `TransportsOf`, not a normal runnable `Transport`.

The service runtime handles it through a separate dispatch path:

1. `Service.ServiceDefinition.Core` builds a transport-independent dispatch handler for every command.
2. Commands marked with `InternalTransport` contribute to the dispatch map used by `Dispatcher.dispatchCommand`.
3. `InternalTransport` contributes no public endpoint handlers, no endpoint schemas, and no runnable server.
4. `Application.runWithResolved` merges the dispatch map directly instead of flattening public transport handlers for integration dispatch.

This keeps the public transport layer and the integration dispatch layer distinct. Jess still marks a command as internal in one obvious place, but she never registers or runs an internal transport adapter.

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

If Jess needs both a public authenticated action and an internal workflow action, she writes two commands with separate names and decision logic rather than trying to make one command serve both roles.

This keeps the mental model simple:

- **One command = one intent**
- **Transport list = exposure policy**
- **Need both public and internal entry points = define two commands**

### 6. Public API and Runtime Hooks

```haskell
module Service.Transport.Internal (
  InternalTransport (..),
) where

data InternalTransport = InternalTransport

type instance NameOf InternalTransport = "InternalTransport"

dispatchCommand ::
  Map Text EndpointHandler ->
  Integration.CommandPayload ->
  Task Text Unit

buildEndpointsByTransport ::
  EventStore Json.Value ->
  Record.ContextRecord Record.I cmds ->
  Map Text TransportValue ->
  Task Text (Map Text (Map Text EndpointHandler), Map Text (Map Text EndpointSchema), Map Text EndpointHandler)
```

Implementation consequences of this API:

- `Service.ServiceDefinition.Core` must filter public transports away from `InternalTransport` while still building dispatch handlers for commands that use it.
- `Service.Application` must merge dispatch handlers separately from public transport endpoint maps.
- `Service.Application.Transports` remains responsible only for runnable, user-facing transports.
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

- The service runtime gains a second command-routing path: public transport handlers and internal dispatch handlers are no longer the same map.
- Jess must model public and internal entry points as separate commands if both are needed.
- Several service runtime modules must coordinate the distinction between runnable transports and internal-only handlers.

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
- [core/service/Service/Transport/Cli.hs](../../core/service/Service/Transport/Cli.hs)
- [testbed/src/Testbed/Stock/Commands/ReserveStock.hs](../../testbed/src/Testbed/Stock/Commands/ReserveStock.hs)
