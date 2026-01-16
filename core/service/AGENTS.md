# Service Layer Knowledge Base

**Directory:** `core/service/` — Event-sourcing + CQRS infrastructure

## STRUCTURE

```text
service/
├── Decider.hs           # Decision monad: acceptNew, acceptExisting, reject
├── Integration.hs       # Outbound (react to events) + Inbound (external→commands)
└── Service/
    ├── Application.hs   # Orchestrates services, queries, integrations
    ├── Command/         # Command typeclass, decideImpl
    ├── CommandExecutor/ # Execute with retry + optimistic concurrency
    ├── Entity/          # Entity/Event typeclasses
    ├── EntityFetcher/   # Reconstruct entity from event stream
    ├── EventStore/      # Append-only log (InMemory, Postgres)
    ├── Query/           # CQRS read models (Registry, Updater, Subscriber)
    └── SnapshotCache/   # Entity state caching
```

## TYPE FAMILIES

| Family | Maps | Example |
|--------|------|---------|
| `EntityOf cmd` | Command → Entity | `EntityOf AddItem = CartEntity` |
| `EventOf entity` | Entity → Event | `EventOf CartEntity = CartEvent` |
| `NameOf t` | Type → Symbol | `NameOf AddItem = "AddItem"` |
| `EntitiesOf query` | Query → [Entity] | `EntitiesOf CartSummary = '[CartEntity]` |

## COMMAND FLOW

```text
Command → CommandExecutor → EntityFetcher → decideImpl → EventStore
              └── retry on conflict (exponential backoff, max 10) ←─┘
```

## DECISION MONAD (`Decider.hs`)

```haskell
acceptNew [events]            -- Only if stream doesn't exist
acceptExisting [events]       -- Only if stream exists  
acceptAfter position [events] -- Optimistic concurrency
reject "reason"               -- Reject command
```

## QUERY PIPELINE

`EventStore → QuerySubscriber → QueryUpdater → QueryOf.combine → QueryObjectStore`  
Startup: `rebuildAll` (replay) → `start` (live subscription)

## INTEGRATION PATTERNS

| Pattern | Use | API |
|---------|-----|-----|
| Outbound | React to events | `withOutbound @Entity @Event fn` |
| Inbound | External → commands | `withInbound (Integration.inbound cfg)` |
| Lifecycle | Stateful resources | `withOutboundLifecycle @Entity cfg` |

## WHERE TO LOOK

| Task | Location |
|------|----------|
| Define command | `Command/Core.hs` — `decideImpl`, `getEntityIdImpl` |
| Execute command | `CommandExecutor/Core.hs` — `execute` |
| Define entity | `Entity/Core.hs` — `initialStateImpl`, `updateImpl` |
| Append events | `EventStore/Core.hs` — `insert`, `subscribeToAllEvents` |
| Define query | `Query/Core.hs` — `QueryOf`, `combine` |
| Wire app | `Application.hs` — `withService`, `withQuery`, `withOutbound` |

## APPLICATION WIRING

```haskell
Application.new
  |> Application.withEventStore postgresConfig
  |> Application.withService cartService
  |> Application.withQuery @CartSummary
  |> Application.withOutbound @CartEntity @CartEvent integrations
  |> Application.run
```
