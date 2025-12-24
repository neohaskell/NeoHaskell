# ADR-0005: Service Module Reorganization

## Status

Accepted

## Context

The `core/service/` directory has evolved organically and presents several challenges for contributors:

### Problem 1: `Service/Command/Core.hs` is a Grab-Bag

This single 133-line file contains 7+ distinct concepts:

| Concept | What It Is | Why It's Here |
|---------|------------|---------------|
| `Command` typeclass | Command contract | Core to commands |
| `Entity` typeclass | Entity reconstruction | Used by commands |
| `Event` typeclass | Event routing | Used by entities |
| `Decision` GADT | Decision monad | Used by decide function |
| `DecisionContext` + `runDecision` | Decision execution | Interprets Decision |
| `CommandResult` | Decision outcome | Result of runDecision |
| `EntityOf`, `EventOf`, `NameOf`, `ApiOf` | Type families | Link types together |

A TypeScript developer looking for "how to define an entity" would not think to look in `Service/Command/Core.hs`.

### Problem 2: Naming Doesn't Reflect Reality

| Current Name | What It Actually Does | The Confusion |
|--------------|----------------------|---------------|
| `CommandHandler` | Event-sourced command executor with retries | "Handler" implies simple callback |
| `WebApi` | WAI/Warp HTTP adapter | Boxes you into "web" only |
| `ApiBuilder` | Transport abstraction | "API" is vague |
| `buildCmdEP` | Creates command pipeline | Cryptic abbreviation |

### Problem 3: Future-Proofing

- `WebApi` naming prevents clean `CliApi` addition
- `ApiBuilder` doesn't communicate it's about transport adapters
- The ES/CQRS terminology (Decider, Adapter, Transport) is more accurate

### Problem 4: Deep Nesting

Postgres modules are 5 levels deep: `Service/EventStore/Postgres/Internal/Core.hs`

### Problem 5: Orphan Folders

`Service/Definition/` contains `TypeLevel.hs` and `Validation.hs` but is separate from `Service/ServiceDefinition/`.

## Decision

We will reorganize the service modules as follows:

### 1. Split the Grab-Bag

Extract from `Service/Command/Core.hs`:

**New `Service/Entity/Core.hs`:**
- `Entity` typeclass
- `Event` typeclass (for routing)
- `EntityOf` type family
- `EventOf` type family

**New `Decider.hs` (top-level, consolidated):**
- `Decision` GADT and instances
- `DecisionContext` and `runDecision`
- Smart constructors (`generateUuid`, `acceptNew`, `acceptExisting`, `acceptAfter`, `acceptAny`, `reject`)

**Slimmed `Service/Command/Core.hs`:**
- `Command` typeclass
- `CommandResult` type
- `NameOf` type family
- `TransportOf` type family (renamed from `ApiOf`)
- Multi-tenant type families

### 2. Rename Handler to Executor

| Old | New |
|-----|-----|
| `Service.CommandHandler` | `Service.CommandExecutor` |
| `CommandHandler` type | `CommandExecutor` |
| `CommandHandlerResult` | `ExecutionResult` |

Rationale: "Handler" implies a simple callback. This module orchestrates the full event-sourcing flow with retries.

### 3. Rename Api to Transport

| Old | New |
|-----|-----|
| `Service.Api.ApiBuilder` | `Service.Transport` |
| `Service.Api.WebApi` | `Service.Transport.Web` |
| `ApiBuilder` typeclass | `Transport` |
| `WebApi` type | `WebTransport` |
| `ApiOf` type family | `TransportOf` |

Rationale: "Transport" is standard ES/CQRS terminology for the adapter layer that handles HTTP, CLI, gRPC, etc.

### 4. Flatten Postgres Modules

Move from `Service/EventStore/Postgres/Internal/*` to `Service/EventStore/Postgres/*`.

### 5. Merge Definition into ServiceDefinition

Move `Service/Definition/*` to `Service/ServiceDefinition/*`.

### 6. Cleanup

- Rename `buildCmdEP` to `createHandler`
- Rename `CommandResponse.hs` to `Response.hs`

### Final Structure

```
core/service/
  Decider.hs                    -- Decision monad + smart constructors + runDecision
  Service.hs                    -- Re-exports ServiceDefinition.Core
  Trigger.hs                    -- (unchanged)

  Service/
    Command.hs                  -- Re-export wrapper
    Command/
      Core.hs                   -- Command typeclass + CommandResult + NameOf + TransportOf

    Entity.hs                   -- Re-export wrapper
    Entity/
      Core.hs                   -- Entity + Event typeclasses + EntityOf + EventOf

    Transport.hs                -- Transport typeclass (was ApiBuilder)
    Transport/
      Web.hs                    -- WebTransport (was WebApi)

    CommandExecutor.hs          -- Re-export wrapper (was CommandHandler)
    CommandExecutor/
      Core.hs                   -- Execution logic
      TH.hs                     -- Template Haskell

    Response.hs                 -- CommandResponse types (was CommandResponse.hs)
    Error.hs                    -- (unchanged)

    Event.hs                    -- Event wrapper types (unchanged)
    Event/                      -- (unchanged)

    EntityFetcher.hs            -- (unchanged)
    EntityFetcher/              -- (unchanged)

    EventStore.hs               -- (unchanged)
    EventStore/
      Core.hs
      InMemory.hs
      Postgres.hs
      Postgres/
        Core.hs                 -- (was Internal/Core.hs)
        Notifications.hs        -- (was Internal/Notifications.hs)
        EventRecord.hs          -- (was Internal/PostgresEventRecord.hs)
        Sessions.hs             -- (was Internal/Sessions.hs)
        SubscriptionStore.hs    -- (was Internal/SubscriptionStore.hs)

    ServiceDefinition.hs
    ServiceDefinition/
      Core.hs
      TypeLevel.hs              -- (was Definition/TypeLevel.hs)
      Validation.hs             -- (was Definition/Validation.hs)
```

## Consequences

### Positive

1. **Predictable locations**: Contributors can find Entity code in `Service.Entity`, not `Service.Command.Core`
2. **ES/CQRS aligned**: Terminology matches industry standards (Transport, Decider, Executor)
3. **Future-proof**: `Transport.Web` and `Transport.Cli` are natural siblings
4. **Flatter structure**: Postgres modules are easier to navigate
5. **Single responsibility**: Each module has one clear purpose

### Negative

1. **Breaking change**: All internal imports need updating (mitigated: Core.hs facade protects users)
2. **Churn**: Many files need to be moved/renamed
3. **ADR-0002 and ADR-0003 reference old names**: These ADRs document `WebApi` and `CommandHandler` terminology

### Trade-offs

1. **Clean break over compatibility**: We chose to delete old modules rather than maintain deprecated re-exports
2. **Separate Entity/Event modules**: Event typeclass (for routing) lives with Entity since they're coupled via `EntityOf`

## References

- `/Users/nick/Source/NeoHaskell/core/service/Service/Command/Core.hs` - Current grab-bag module
- `/Users/nick/Source/NeoHaskell/core/service/Decision.hs` - Current Decision smart constructors
- `/Users/nick/Source/NeoHaskell/docs/decisions/0002-webapi-adapter-architecture.md` - Previous API architecture
- `/Users/nick/Source/NeoHaskell/docs/decisions/0003-command-abstraction-and-flow.md` - Command flow documentation
