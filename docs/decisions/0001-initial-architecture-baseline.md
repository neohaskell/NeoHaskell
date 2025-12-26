# ADR-0001: Initial Architecture Baseline

## Status

Accepted

## Context

This is the baseline Architecture Decision Record (ADR) for the NeoHaskell project, capturing the state of the codebase as of December 2025. NeoHaskell is a dialect of Haskell focused on newcomer-friendliness and productivity. This document serves as the foundation for all future architectural decisions by documenting the current patterns, module organization, and design choices.

The project is currently on the `feat/command-execution` branch, indicating active development of the command execution infrastructure.

## Decision

We document the following as the established architecture of NeoHaskell:

### 1. Monorepo Structure

The project is organized as a monorepo with the following top-level packages:

```text
NeoHaskell/
  cli/           -- nhcli: Command line tool, executable "neo"
  core/          -- nhcore: Core library with NeoHaskell's standard library
  testbed/       -- nhtestbed: Integration testing application (Cart example)
  website/       -- Documentation website (Astro/Starlight)
```

### 2. Core Library Organization (nhcore)

The core library is organized into logical subdirectories within `core/`:

| Directory         | Purpose                                       | Key Modules                                                     |
| ----------------- | --------------------------------------------- | --------------------------------------------------------------- |
| `core/`           | Fundamental types and primitives              | Array, Text, Maybe, Result, Task, Basics, Function              |
| `concurrency/`    | Concurrent programming primitives             | AsyncTask, Channel, Lock, ConcurrentVar, DurableChannel, Stream |
| `service/`        | Event sourcing and CQRS infrastructure        | Service.\*, EventStore, Command, Decision                       |
| `traits/`         | Type classes (NeoHaskell calls them "traits") | Mappable, Appendable, Combinable, Applicable, Default, ToText   |
| `system/`         | System interaction                            | File, Directory, Path, Environment, Time, Subprocess            |
| `http/`           | HTTP client functionality                     | Http, Http.Client                                               |
| `json/`           | JSON serialization                            | Json                                                            |
| `meta/`           | Metaprogramming utilities                     | TypeName                                                        |
| `testlib/`        | Testing infrastructure                        | Test, Test.Spec, Test.Service._, Test.AppSpec._                 |
| `options-parser/` | CLI argument parsing                          | Command (options-parser version)                                |

### 3. The Core Module

All user-facing types and functions are re-exported through `Core.hs`. This module serves as the implicit prelude for NeoHaskell code (using `NoImplicitPrelude`). Key exports include:

- Primitive types: `Text`, `Int`, `Float`, `Char`, `Bool`
- Collection types: `Array`, `LinkedList`, `Map`, `Set`
- Effect types: `Task`, `IO`, `Maybe`, `Result`
- Concurrency: `Channel`, `Lock`, `ConcurrentVar`, `AsyncTask`
- Event sourcing: `Command`, `Entity`, `Decision`, `StreamId`, `InsertionType`
- Utilities: `Uuid`, `Path`, `DateTime`, `Version`

### 4. Event Sourcing Architecture

The project implements a comprehensive Event Sourcing / CQRS pattern:

#### 4.1 Core Concepts

| Concept        | Module                             | Description                                              |
| -------------- | ---------------------------------- | -------------------------------------------------------- |
| Command        | `Service.Command.Core`             | User intent with `getEntityIdImpl` and `decideImpl`      |
| Entity         | `Service.Command.Core`             | Aggregate state with `initialStateImpl` and `updateImpl` |
| Event          | `Service.Command.Core`             | Immutable facts with `getEventEntityIdImpl`              |
| Decision       | `Decision`, `Service.Command.Core` | Pure decision monad for command logic                    |
| EventStore     | `Service.EventStore.Core`          | Persistence interface for events                         |
| CommandHandler | `Service.CommandHandler.Core`      | Orchestrates command execution with retry logic          |
| EntityFetcher  | `Service.EntityFetcher.Core`       | Reconstructs entities from event streams                 |

#### 4.2 EventStore Implementations

Two implementations exist:

1. **InMemory** (`Service.EventStore.InMemory`): Thread-safe in-memory store using `DurableChannel`, `Lock`, and `ConcurrentVar`. Suitable for testing.

2. **Postgres** (`Service.EventStore.Postgres`): Production-ready PostgreSQL-backed store with:
   - `Service.EventStore.Postgres.Internal.Core`
   - `Service.EventStore.Postgres.Internal.Sessions`
   - `Service.EventStore.Postgres.Internal.Notifications`
   - `Service.EventStore.Postgres.Internal.SubscriptionStore`
   - `Service.EventStore.Postgres.Internal.PostgresEventRecord`

#### 4.3 EventStore Interface

The `EventStore eventType` record provides:

- `insert`: Append events with consistency checks
- `readStreamForwardFrom` / `readStreamBackwardFrom`: Stream-level reads
- `readAllEventsForwardFrom` / `readAllEventsBackwardFrom`: Global reads
- `readAllEventsForwardFromFiltered` / `readAllEventsBackwardFromFiltered`: Filtered reads
- `subscribeToAllEvents`, `subscribeToEntityEvents`, `subscribeToStreamEvents`: Subscriptions
- `truncateStream`: Stream truncation

### 5. Service Definition and API Builder

#### 5.1 Service Definition

`Service.ServiceDefinition.Core` provides a type-safe builder for composing services:

```haskell
service :: Service _ _ _ _
service =
  Service.new
    |> Service.command @AddItem
    |> Service.command @CreateCart
    |> Service.useServer WebApi.server
    |> Service.useEventStore postgresConfig
```

Key features:

- Type-level tracking of registered commands via `commandRow`
- Type-level API name tracking via `commandApiNames` and `providedApiNames`
- Pluggable event store configuration via `EventStoreConfig` typeclass

#### 5.2 ApiBuilder Pattern

`Service.Api.ApiBuilder` defines the transport-agnostic API abstraction:

```haskell
class ApiBuilder api where
  type Request api
  type Response api
  type RunnableApi api

  assembleApi :: ApiEndpoints api -> RunnableApi api
  runApi :: api -> RunnableApi api -> Task Text Unit
  buildCommandHandler :: ... -> ApiEndpointHandler
```

Currently implemented:

- **WebApi** (`Service.Api.WebApi`): HTTP server using Warp with `/commands/<name>` routing

### 6. Command Pattern with Template Haskell

`Service.CommandHandler.TH` provides the `command` splice that:

1. Validates `getEntityId` and `decide` function signatures
2. Checks `EntityOf` type instance exists
3. Validates `EntityIdType` (defaults to `Uuid`)
4. Supports multi-tenancy mode via `IsMultiTenant` type family
5. Generates `Command` instance with `getEntityIdImpl` and `decideImpl`
6. Generates `NameOf` type instance
7. Generates `KnownHash` instance for type-level string hashing

Usage in command modules:

```haskell
type instance EntityOf CreateCart = CartEntity
type instance ApiOf CreateCart = WebApi

command ''CreateCart
```

### 7. Decision Monad

The `Decision` type is a free monad providing:

- `generateUuid`: Generate UUIDs within decisions
- `acceptNew`: Accept with stream creation semantics
- `acceptExisting`: Accept for existing streams only
- `acceptAfter`: Accept with optimistic concurrency (position-based)
- `acceptAny`: Accept regardless of stream state
- `reject`: Reject with reason message

### 8. NeoHaskell Code Style Conventions

The codebase follows these strict conventions:

1. **Pipe operator over dollar**: `value |> transform |> finalize`
2. **No point-free style**: Always explicit function application
3. **Do-blocks for bindings**: Even pure code uses `do` with `let`
4. **Strict imports**: Explicit type imports + qualified module imports
5. **GHC prefix**: `import Data.List qualified as GhcList`
6. **Explicit forall**: `forall element result. (element -> result) -> ...`
7. **Case-of only**: No pattern matching in function definitions
8. **Result over Either**: Use `Result error value`
9. **String interpolation**: `[fmt|Hello {name}!|]`
10. **Type-specific yield**: `Task.yield x` over `pure` or `return`
11. **No external deps**: Only nhcore modules

### 9. Testing Infrastructure

The project has comprehensive testing:

1. **Test.Spec**: HSpec-based test framework
2. **Test.Service.EventStore.\***: Behavior-driven EventStore tests
   - BatchValidation, GlobalStreamOrdering, IndividualStreamOrdering
   - OptimisticConcurrency, StreamTruncation, Subscriptions
   - ReadAllBackwardsFromEnd, ReadAllForwardsFromStart
3. **Test.Service.Command.\***: Command testing utilities
4. **Test.Service.CommandHandler.\***: Handler testing
5. **Test.Service.EntityFetcher.\***: Entity fetching tests
6. **Test.AppSpec.\***: Application-level specification testing

### 10. Current Development State (feat/command-execution)

The command execution feature is in active development. Per `command-execution-example.md`:

**Completed:**

- Service definition builder (`Service.new`, `Service.command`, `Service.useServer`, `Service.useEventStore`)
- WebApi server with HTTP routing to `/commands/<name>`
- Command parsing and JSON deserialization
- CommandHandler with retry logic and exponential backoff
- Event persistence through EventStore.insert
- CommandResponse types (Accepted, Rejected, Failed)

**Architecture in place:**

- Commands flow: HTTP Request -> WebApi.assembleApi -> buildCommandHandler -> CommandHandler.execute -> EventStore.insert
- Entity state reconstruction via EntityFetcher
- Optimistic concurrency handling with automatic retry

## Consequences

### Positive

1. **Clear separation of concerns**: Event sourcing components are well-isolated
2. **Type-safe service composition**: Commands and APIs are tracked at the type level
3. **Pluggable infrastructure**: EventStore implementations are swappable via typeclass
4. **Testable**: In-memory EventStore enables fast, isolated testing
5. **Familiar patterns**: ES/CQRS patterns recognizable to domain-driven developers

### Negative / Areas for Improvement

1. **Module naming inconsistency**: Some modules use `Service.*` prefix, others don't (e.g., `Decision` vs `Service.Command.Core`)
2. **Deep nesting in Postgres modules**: 5 levels deep (`Service.EventStore.Postgres.Internal.*`)
3. **TH magic**: The `command` splice hides significant complexity
4. **Scattered type family instances**: `EntityOf`, `NameOf`, `ApiOf` defined in command modules

### Future Considerations

1. Consider flattening Postgres internal modules
2. Evaluate moving type family instances closer to their definitions
3. Document the complete command execution flow for contributors
4. Consider renaming `Decision` module to `Service.Decision` for consistency
5. Add CLI adapters beyond WebApi (e.g., `Adapter.Cli`)

## References

- [core/nhcore.cabal](../../core/nhcore.cabal) - Core library package definition
- [core/core/Core.hs](../../core/core/Core.hs) - Public API exports
- [core/service/Service/ServiceDefinition/Core.hs](../../core/service/Service/ServiceDefinition/Core.hs) - Service builder
- [core/service/Service/CommandHandler/Core.hs](../../core/service/Service/CommandHandler/Core.hs) - Command execution
- [core/service/Service/EventStore/Core.hs](../../core/service/Service/EventStore/Core.hs) - EventStore interface
- [core/service/Service/Api/WebApi.hs](../../core/service/Service/Api/WebApi.hs) - HTTP API implementation
- [testbed/](../../testbed/) - Integration testing example
- [command-execution-example.md](../../command-execution-example.md) - Feature documentation
