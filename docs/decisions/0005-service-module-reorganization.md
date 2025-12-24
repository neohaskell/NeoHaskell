# ADR-0005: Service Module Reorganization

## Status

Accepted (Implemented)

## Context

The `core/service/` directory had evolved organically and presented several challenges for contributors:

### Problem 1: `Service/Command/Core.hs` was a Grab-Bag

This single file contained 7+ distinct concepts:

| Concept                                  | What It Is            | Why It Was There        |
| ---------------------------------------- | --------------------- | ----------------------- |
| `Command` typeclass                      | Command contract      | Core to commands        |
| `Entity` typeclass                       | Entity reconstruction | Used by commands        |
| `Event` typeclass                        | Event routing         | Used by entities        |
| `Decision` GADT                          | Decision monad        | Used by decide function |
| `DecisionContext` + `runDecision`        | Decision execution    | Interprets Decision     |
| `CommandResult`                          | Decision outcome      | Result of runDecision   |
| `EntityOf`, `EventOf`, `NameOf`, `ApiOf` | Type families         | Link types together     |

A TypeScript developer looking for "how to define an entity" would not think to look in `Service/Command/Core.hs`.

### Problem 2: Naming Didn't Reflect Reality

| Old Name         | What It Actually Does                       | The Confusion                       |
| ---------------- | ------------------------------------------- | ----------------------------------- |
| `CommandHandler` | Event-sourced command executor with retries | "Handler" implies simple callback   |
| `WebApi`         | WAI/Warp HTTP adapter                       | Boxes you into "web" only           |
| `ApiBuilder`     | Transport abstraction                       | "API" is vague                      |
| `buildCmdEP`     | Creates command pipeline                    | Cryptic abbreviation                |
| `ApiOf`          | Maps command to transport adapter           | "API" less precise than "Transport" |

### Problem 3: Future-Proofing

- `WebApi` naming prevents clean `CliTransport` addition
- `ApiBuilder` doesn't communicate it's about transport adapters
- The ES/CQRS terminology (Decider, Transport, Executor) is more accurate

## Decision

We reorganized the service modules as follows:

### 1. Split the Grab-Bag

Extracted from the monolithic `Service/Command/Core.hs`:

**New `Service/Entity/Core.hs`:**

- `Entity` typeclass (with `EntityIdType`, `initialStateImpl`, `updateImpl`)
- `Event` typeclass (with `getEventEntityIdImpl` for routing)
- `EntityOf` type family
- `EventOf` type family

**New `Decider.hs` (top-level, consolidated):**

- `Decision` GADT and Functor/Applicative/Monad instances
- `DecisionContext` and `runDecision`
- `CommandResult` type
- Smart constructors: `generateUuid`, `acceptNew`, `acceptExisting`, `acceptAfter`, `acceptAny`, `reject`

**Slimmed `Service/Command/Core.hs`:**

- `Command` typeclass (with `IsMultiTenant`, `getEntityIdImpl`, `decideImpl`)
- `NameOf` type family
- `TransportOf` type family (renamed from `ApiOf`)
- Multi-tenant type families (`GetEntityIdFunction`, `DecideFunction`)
- Re-exports from `Service.Entity` and `Decider` for backward compatibility

### 2. Renamed Handler to Executor

| Old                                | New                       |
| ---------------------------------- | ------------------------- |
| `Service.CommandHandler` (concept) | `Service.CommandExecutor` |
| `CommandHandlerResult`             | `ExecutionResult`         |

**New modules created:**

- `Service/CommandExecutor/Core.hs` - Main execution logic with retry/concurrency handling
- `Service/CommandExecutor/TH.hs` - Template Haskell helpers (e.g., `deriveKnownHash`)
- `Service/CommandExecutor.hs` - Re-export wrapper

**Deleted modules (clean break):**

- `Service/CommandHandler/Core.hs`
- `Service/CommandHandler/TH.hs`
- `Service/CommandHandler.hs`

Rationale: "Handler" implies a simple callback. "Executor" better conveys the orchestration of the full event-sourcing flow with retries and concurrency control.

### 3. Renamed Api to Transport

| Old                    | New                       |
| ---------------------- | ------------------------- |
| `ApiBuilder` typeclass | `Transport` typeclass     |
| `WebApi` type          | `WebTransport` type       |
| `ApiOf` type family    | `TransportOf` type family |

**New modules created:**

- `Service/Transport.hs` - `Transport` typeclass, `EndpointHandler` type, `Endpoints` type
- `Service/Transport/Web.hs` - `WebTransport` implementation with WAI/Warp

**Deleted modules (clean break):**

- `Service/Api/ApiBuilder.hs`
- `Service/Api/WebApi.hs`

Rationale: "Transport" is standard ES/CQRS terminology for the adapter layer that handles HTTP, CLI, gRPC, etc. This enables natural sibling modules like `Transport.Cli` in the future.

### 4. Created Response Module

**New `Service/Response.hs`:**

- `CommandResponse` type (user-facing: `Accepted`, `Rejected`, `Failed`)
- `fromExecutionResult` function to convert internal `ExecutionResult` to public `CommandResponse`

This separates the internal execution details (retry counts, event counts) from the client-facing API response.

### 5. Renamed buildCmdEP to createHandler

In `Service/ServiceDefinition/Core.hs`:

- `buildCmdEP` renamed to `createHandler`

Rationale: Clear, descriptive name that explains what the function does.

### Final Structure

```text
core/service/
  Decider.hs                    -- Decision monad + CommandResult + smart constructors + runDecision
  Service.hs                    -- Re-exports ServiceDefinition.Core
  Trigger.hs                    -- (unchanged)

  Service/
    Command.hs                  -- Re-export wrapper
    Command/
      Core.hs                   -- Command typeclass + NameOf + TransportOf + re-exports

    Entity.hs                   -- Re-export wrapper
    Entity/
      Core.hs                   -- Entity + Event typeclasses + EntityOf + EventOf

    Transport.hs                -- Transport typeclass + EndpointHandler + Endpoints
    Transport/
      Web.hs                    -- WebTransport implementation

    CommandExecutor.hs          -- Re-export wrapper
    CommandExecutor/
      Core.hs                   -- ExecutionResult + CommandExecutor + execute
      TH.hs                     -- Template Haskell helpers

    Response.hs                 -- CommandResponse + fromExecutionResult
    Error.hs                    -- (unchanged)

    Event.hs                    -- Event wrapper types (unchanged)
    Event/                      -- StreamId, StreamPosition, etc. (unchanged)

    EntityFetcher.hs            -- (unchanged)
    EntityFetcher/              -- (unchanged)

    EventStore.hs               -- (unchanged)
    EventStore/
      Core.hs
      InMemory.hs
      Postgres.hs
      Postgres/
        Internal/               -- (NOT flattened - deferred)
          Core.hs
          Notifications.hs
          PostgresEventRecord.hs
          Sessions.hs
          SubscriptionStore.hs

    Definition/                 -- (NOT merged - deferred)
      TypeLevel.hs
      Validation.hs

    ServiceDefinition.hs
    ServiceDefinition/
      Core.hs                   -- createHandler (was buildCmdEP)
```

## What Was NOT Implemented

The following items from the original plan were deferred:

1. **Postgres module flattening** - `Service/EventStore/Postgres/Internal/*` was not moved to `Service/EventStore/Postgres/*`
2. **Definition merge** - `Service/Definition/*` was not merged into `Service/ServiceDefinition/*`

These deferrals were pragmatic choices to minimize churn while achieving the primary naming and organization goals.

**Note:** A clean break was made - all deprecated modules (`CommandHandler/*`, `Api/*`, `Decision.hs`, `CommandResponse.hs`) were deleted entirely. No backward-compatibility shims were kept.

## Consequences

### Positive

1. **Predictable locations**: Contributors can find Entity code in `Service.Entity`, not `Service.Command.Core`
2. **ES/CQRS aligned**: Terminology matches industry standards (Transport, Decider, Executor)
3. **Future-proof**: `Transport.Web` and `Transport.Cli` are natural siblings
4. **Single responsibility**: Each module has one clear purpose
5. **Separation of concerns**: Internal `ExecutionResult` is separate from client-facing `CommandResponse`

### Negative

1. **Breaking internal imports**: All internal imports needed updating (clean break, no shims)
2. **ADR-0002 and ADR-0003 reference old names**: These ADRs document `WebApi` and `CommandHandler` terminology
3. **Re-exports for convenience**: `Service/Command/Core.hs` re-exports Entity/Event/Decider types for backward compatibility in user code

### Migration Path

Internal code should migrate to new imports:

- `Service.CommandHandler` -> `Service.CommandExecutor`
- `Service.Api.*` -> `Service.Transport.*`
- `ApiOf` -> `TransportOf`
- Entity/Event typeclasses -> `Service.Entity`

The `Core` module's public API facade was not affected by these changes.

## References

- `/Users/nick/Source/NeoHaskell/core/service/Service/Entity/Core.hs` - New Entity module
- `/Users/nick/Source/NeoHaskell/core/service/Decider.hs` - Consolidated Decision module
- `/Users/nick/Source/NeoHaskell/core/service/Service/Transport.hs` - New Transport abstraction
- `/Users/nick/Source/NeoHaskell/core/service/Service/CommandExecutor/Core.hs` - New executor logic
- `/Users/nick/Source/NeoHaskell/docs/decisions/0002-webapi-adapter-architecture.md` - Previous API architecture (uses old terminology)
- `/Users/nick/Source/NeoHaskell/docs/decisions/0003-command-abstraction-and-flow.md` - Command flow documentation (uses old terminology)
