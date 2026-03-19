# ADR-0049: OutboundIntegration Typeclass with Typed Event Dispatch

## Status

Proposed

## Context

### Current State

NeoHaskell's outbound integration pattern requires Jess to write a single function that receives the full event ADT and pattern-matches on every constructor:

```haskell
cartIntegrations :: TestbedConfig -> CartEntity -> CartEvent -> Integration.Outbound
cartIntegrations _config cart event = case event of
  CartCreated {} -> Integration.none
  ItemAdded {stockId, quantity} -> Integration.batch
    [ Integration.outbound Command.Emit
        { command = ReserveStock
            { stockId = stockId
            , quantity = quantity
            , cartId = cart.cartId
            }
        }
    ]
```

This pattern has three problems:

1. **Exhaustiveness burden**: Every time a new event constructor is added to the ADT, Jess must update every integration function with a new `case` branch — even if that integration doesn't care about the new event. For an entity with 15 event types and 3 integrations, adding one event forces changes in 3 files.

2. **Monolithic coupling**: All integration logic for an entity lives in one function (or one module). The `cartIntegrations` function handles stock reservation, notification sending, and analytics — concerns that belong in separate modules.

3. **No type-level routing**: The framework has no way to know which events a handler cares about. Every event is passed to every handler, and the handler must return `Integration.none` for events it doesn't process. This is wasted work at runtime and wasted attention for Jess.

The `EventVariantOf` typeclass (ADR-0047, #575, already merged) provides the extraction mechanism: `toVariant :: event -> Maybe eventVariant`. This ADR builds on that foundation to enable typed, per-event-variant outbound integration handlers.

### Use Cases

- **Single-event handler (common case)**: Jess writes a handler that reacts to exactly one event type. When `ItemAdded` fires, reserve stock. She doesn't need to handle `CartCreated`, `ItemRemoved`, or any other event — the framework routes only `ItemAdded` events to her handler.

- **Multiple handlers per entity**: An entity has several integrations, each reacting to different events. `ReserveStockOnItemAdded` handles `ItemAdded`. `NotifyOwnerOnCartCreated` handles `CartCreated`. `UpdateAnalyticsOnItemRemoved` handles `ItemRemoved`. Each is a separate module with a single responsibility.

- **Process Manager pattern**: A handler reacts to one event and emits a command to another bounded context. This is the cross-domain coordination pattern. The handler is typed to the exact event it processes, making the data flow explicit in the type signature.

### Design Goals

1. **One handler, one event**: Each handler type is typed to the exact event variant it processes. No `case` expression, no `Integration.none` for unhandled events. The framework dispatches based on `toVariant`.

2. **Module-per-handler**: Each handler lives in its own module under `Integrations/`. The module name describes the action and the trigger: `ReserveStockOnItemAdded`. This makes the codebase navigable — Jess finds integrations by browsing the directory.

3. **TH macro parity**: The `outboundIntegration` TH macro follows the same pattern as `command` and `deriveQuery` — Jess writes the business logic functions, the macro validates signatures and generates boilerplate instances.

4. **Zero ceremony for Jess**: Jess writes a data type, a `handleEvent` function, a `type instance EntityOf`, and calls the TH macro. That's it. No typeclass instances to write manually, no registration boilerplate.

5. **Replaces the old pattern**: The existing `withOutbound` function that accepts `entity -> event -> Integration.Outbound` is superseded. All outbound integrations use the typed `OutboundIntegration` pattern. The testbed's `cartIntegrations` function will be migrated to per-handler modules.

### GitHub Issue

- [#576: Event Model DSL: `OutboundIntegration` typeclass + dispatch generation](https://github.com/neohaskell/NeoHaskell/issues/576)

## Decision

### 1. Typeclass Name: `OutboundIntegration`

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| `EventHandler` | Rejected | Too generic — "handler" implies side effects and doesn't communicate the outbound direction. Collides conceptually with web request handlers. |
| `EventReactor` | Rejected | "Reactor" is used in some ES/CQRS literature but is unfamiliar to Jess. NeoHaskell already uses "integration" terminology throughout. |
| `IntegrationHandler` | Rejected | Redundant — "handler" adds nothing over "integration." Also, "handler" is discouraged for pure logic in NeoHaskell conventions. |
| `OutboundIntegration` | **Chosen** | Matches the existing `Integration.Outbound` type and `withOutbound` wiring function. Jess already knows "outbound integration" from the current API. The name is domain-accurate (ES/CQRS outbound integration), explicit (specifies direction), and consistent with existing naming. |

### 2. Module Placement

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| `core/service/Service/Integration/OutboundIntegration.hs` | Rejected | Nests under `Integration/` which currently holds `Types.hs` and `Dispatcher.hs` (internal runtime modules). The typeclass is user-facing, not internal. |
| `core/service/OutboundIntegration.hs` | Rejected | Flat placement alongside `Integration.hs` and `EventVariantOf.hs`. However, the typeclass has a TH companion module, so a directory is justified. |
| `core/service/Service/OutboundIntegration/Core.hs` + `TH.hs` | **Chosen** | Follows the established pattern: `Service/Command/Core.hs` + `Service/CommandExecutor/TH.hs`, `Service/Query/Core.hs` + `Service/Query/TH.hs`. The `Core.hs` holds the typeclass and `HandledEvent` type family. The `TH.hs` holds the macro. One level of nesting for a concept with sub-modules. |

**File layout:**

```
core/service/Service/OutboundIntegration/
  Core.hs    -- OutboundIntegration typeclass, HandledEvent type family
  TH.hs      -- outboundIntegration TH macro
```

### 3. Type Definition: `OutboundIntegration` Typeclass

```haskell
-- | Typeclass for typed outbound integration handlers.
--
-- Each handler type handles a specific event variant for an entity.
-- The framework dispatches events to handlers via 'EventVariantOf.toVariant':
--
-- 1. Decode JSON → full event ADT
-- 2. Call 'toVariant' to extract the specific variant
-- 3. 'Nothing' → skip this handler (event doesn't match)
-- 4. 'Just variant' → call 'handleEventImpl' with the entity and variant
--
-- Jess never implements this typeclass manually — the 'outboundIntegration'
-- TH macro generates the instance from her 'handleEvent' function.
--
-- @
-- data ReserveStockOnItemAdded = ReserveStockOnItemAdded
--   deriving (Generic, Typeable, Show)
--
-- type instance EntityOf ReserveStockOnItemAdded = CartEntity
--
-- handleEvent :: CartEntity -> ItemAdded -> Integration.Outbound
-- handleEvent cart itemAdded = Integration.batch [...]
--
-- outboundIntegration ''ReserveStockOnItemAdded
-- @
class OutboundIntegration handler where
  -- | The specific event variant this handler processes.
  --
  -- Inferred by the TH macro from the second parameter of 'handleEvent'.
  type HandledEvent handler :: Type

  -- | Process an event for the given entity state.
  --
  -- The entity is reconstructed from the event stream via replay.
  -- The event is the specific variant this handler is typed to.
  handleEventImpl :: EntityOf handler -> HandledEvent handler -> Integration.Outbound
```

**Key design decisions:**

- **`HandledEvent` is an associated type family**, not a standalone type family. This keeps the event type tightly coupled to the handler — each handler declares exactly which event variant it processes. The TH macro infers this from the `handleEvent` function signature.

- **`EntityOf` is reused** from `Service.Entity.Core`. The handler declares `type instance EntityOf MyHandler = MyEntity`, and the typeclass uses `EntityOf handler` to determine the entity type. This is the same pattern used by `Command`.

- **The return type is `Integration.Outbound`**, not `Task`. Integration handlers are pure functions that produce a collection of actions. Side effects happen when the runtime executes those actions.

- **No `Maybe` return**. The handler is only called when `toVariant` succeeds, so it always has a valid event to process. The framework handles the `Nothing` case (skip) before calling `handleEventImpl`.

### 4. Dispatch Mechanism

The dispatch flow for a typed handler:

```
Event (JSON) → decode to event ADT → toVariant → Maybe eventVariant
                                                    │
                                          Nothing → skip (handler doesn't care)
                                          Just v  → handleEventImpl entity v
```

Multiple handlers per entity work naturally. Each handler's `toVariant` extracts its variant or returns `Nothing`. The framework iterates over all registered handlers for the entity type.

### 5. TH Macro: `outboundIntegration`

The `outboundIntegration` TH macro follows the established pattern of `command` and `deriveQuery`:

```haskell
-- | Generate OutboundIntegration-related instances for a handler type.
--
-- Usage:
--
-- @
-- data ReserveStockOnItemAdded = ReserveStockOnItemAdded
--   deriving (Generic, Typeable, Show)
--
-- type instance EntityOf ReserveStockOnItemAdded = CartEntity
--
-- handleEvent :: CartEntity -> ItemAdded -> Integration.Outbound
-- handleEvent cart itemAdded = Integration.batch [...]
--
-- outboundIntegration ''ReserveStockOnItemAdded
-- @
--
-- Validates at compile time:
--
-- * 'EntityOf' instance exists for the handler
-- * 'EventOf' resolves from the entity
-- * 'handleEvent' function exists with correct signature
-- * 'EventVariantOf' instance exists for the handled event type
--
-- Generates:
--
-- * @instance OutboundIntegration ReserveStockOnItemAdded@ with 'HandledEvent' inferred
-- * @type instance NameOf ReserveStockOnItemAdded = "ReserveStockOnItemAdded"@
-- * @instance KnownHash "ReserveStockOnItemAdded"@
--
-- Does NOT generate:
--
-- * 'ToSchema' (not an HTTP endpoint)
-- * 'TransportsOf' (no transport routing)
outboundIntegration :: TH.Name -> THLib.DecsQ
```

**What the macro validates:**

| Check | Error if Missing |
|-------|-----------------|
| `EntityOf` type instance | "Missing EntityOf type instance for handler 'X'. Please add: `type instance EntityOf X = YourEntityType`" |
| `EventOf` resolves from entity | "Could not resolve EventOf for entity 'Y'. Ensure your entity has `type instance EventOf Y = YourEventType`" |
| `handleEvent` function exists | "Missing 'handleEvent' function for handler 'X'. Please add: `handleEvent :: YourEntity -> YourEventVariant -> Integration.Outbound`" |
| `handleEvent` signature matches | "handleEvent has incorrect signature. Expected: `YourEntity -> YourEventVariant -> Integration.Outbound`" |
| `EventVariantOf` instance exists | "Missing EventVariantOf instance. The event variant 'V' must be convertible to/from the entity's event ADT 'E'. Add: `instance EventVariantOf E V where ...`" |

**What the macro generates:**

```haskell
-- Given:
--   data ReserveStockOnItemAdded = ReserveStockOnItemAdded
--   type instance EntityOf ReserveStockOnItemAdded = CartEntity
--   handleEvent :: CartEntity -> ItemAdded -> Integration.Outbound

-- The macro generates:

-- 1. OutboundIntegration instance with HandledEvent inferred from handleEvent's second parameter
instance OutboundIntegration ReserveStockOnItemAdded where
  type HandledEvent ReserveStockOnItemAdded = ItemAdded
  handleEventImpl = handleEvent

-- 2. NameOf type instance (same pattern as command and deriveQuery)
type instance NameOf ReserveStockOnItemAdded = "ReserveStockOnItemAdded"

-- 3. KnownHash instance (same pattern as command and deriveQuery)
instance KnownHash "ReserveStockOnItemAdded" where
  hashVal _ = <computed hash>
```

### 6. What Jess Writes (Target DX)

```haskell
module Testbed.Cart.Integrations.ReserveStockOnItemAdded (
  ReserveStockOnItemAdded (..),
  handleEvent,
) where

import Core
import Integration qualified
import Integration.Command qualified as Command
import Service.OutboundIntegration.TH (outboundIntegration)
import Testbed.Cart.Core (CartEntity (..))
import Testbed.Cart.Events.ItemAdded (ItemAdded (..))
import Testbed.Stock.Commands.ReserveStock (ReserveStock (..))

data ReserveStockOnItemAdded = ReserveStockOnItemAdded
  deriving (Generic, Typeable, Show)

type instance EntityOf ReserveStockOnItemAdded = CartEntity

handleEvent :: CartEntity -> ItemAdded -> Integration.Outbound
handleEvent cart itemAdded = Integration.batch
  [ Integration.outbound Command.Emit
      { command = ReserveStock
          { stockId = itemAdded.stockId
          , quantity = itemAdded.quantity
          , cartId = cart.cartId
          }
      }
  ]

outboundIntegration ''ReserveStockOnItemAdded
```

**Jess Test evaluation:**

1. **Can Jess discover this through autocomplete?** Yes — she imports `Service.OutboundIntegration.TH` and sees `outboundIntegration`. The pattern matches `command` and `deriveQuery` she already uses.

2. **Can Jess understand the type signature without docs?** Yes — `handleEvent :: CartEntity -> ItemAdded -> Integration.Outbound` reads as "given a cart and an ItemAdded event, produce outbound actions." No typeclasses, no type families, no constraints visible.

3. **Can Jess use this correctly on the first try?** Yes — the structure mirrors `CreateCart.hs` (data type, EntityOf, business logic function, TH macro). She copies the pattern and changes the names.

4. **If Jess makes a mistake, does the compiler guide her?** Yes — the TH macro produces actionable error messages for every missing piece (see validation table above).

5. **Does this feel like the APIs she already knows?** Yes — same pattern as `command ''CreateCart` and `deriveQuery ''CartSummary [''CartEntity]`.

### 7. DSL Combinator Constraints (Future #578)

When the Event Model DSL structural combinators are implemented (#578), the `outboundIntegration` combinator in the DSL will have these constraints:

```haskell
outboundIntegration ::
  forall handler entity event eventVariant.
  ( OutboundIntegration handler
  , Typeable handler
  , entity ~ EntityOf handler
  , event ~ EventOf entity
  , eventVariant ~ HandledEvent handler
  , EventVariantOf event eventVariant
  , Json.FromJSON entity
  , Json.FromJSON event
  , TypeName.Inspectable entity
  , Default entity
  , Entity entity
  ) =>
  WiringBuilder () ->
  EntityBuilder ()
```

This is not part of the current ADR's scope but documents the intended integration point. The typeclass and TH macro are designed so these constraints are satisfiable for any handler generated by the `outboundIntegration` TH macro.

### 8. Runtime Dispatch Integration

The existing `createOutboundRunner` function in `Service.Application.Integrations` creates a type-erased `OutboundRunner` from a function `entity -> event -> Integration.Outbound`. This function is replaced by `createTypedOutboundRunner` which wraps a typed `OutboundIntegration` handler into an `OutboundRunner`:

```haskell
-- | Create an OutboundRunner from a typed OutboundIntegration handler.
--
-- This wraps the typed handler with:
-- 1. JSON decoding of the full event ADT
-- 2. Entity state reconstruction via event replay
-- 3. Event variant extraction via 'toVariant'
-- 4. Dispatch to 'handleEventImpl' only when the variant matches
--
-- When 'toVariant' returns 'Nothing', the handler is skipped (returns empty actions).
createTypedOutboundRunner ::
  forall handler entity event eventVariant.
  ( OutboundIntegration handler
  , entity ~ EntityOf handler
  , event ~ EventOf entity
  , eventVariant ~ HandledEvent handler
  , EventVariantOf event eventVariant
  , Json.FromJSON entity
  , Json.FromJSON event
  , Json.FromJSON (EventOf entity)
  , TypeName.Inspectable entity
  , Default entity
  , Entity entity
  ) =>
  OutboundRunner
```

This function replaces `createOutboundRunner` as the primary way to create outbound runners. The existing `withOutbound` in `Application.hs` will be updated to accept typed handlers instead of raw functions. It reuses the existing `fetchEntityState` function from `Service.Application.Integrations` for entity reconstruction.

### 9. Public API

**`Service.OutboundIntegration.Core` exports:**

```haskell
module Service.OutboundIntegration.Core (
  -- * Typeclass
  OutboundIntegration (..),
) where
```

**`Service.OutboundIntegration.TH` exports:**

```haskell
module Service.OutboundIntegration.TH (
  outboundIntegration,
) where
```

**Re-export from `Core.hs`:**

The `OutboundIntegration` typeclass should be re-exported from `Core.hs` so that Jess can write `type instance EntityOf` and have the typeclass available:

```haskell
-- In Core.hs, add:
import Service.OutboundIntegration.Core as Reexported (OutboundIntegration (..))
```

### 10. GHC Extensions Required

| Extension | Why Needed | Already in nhcore? |
|-----------|-----------|-------------------|
| `TypeFamilies` | `HandledEvent` associated type family | Yes (project-wide) |
| `TemplateHaskell` | TH macro | Yes (used by `command`, `deriveQuery`) |
| `AllowAmbiguousTypes` | `createTypedOutboundRunner` uses type applications | Yes (used in `Service.Application.Integrations`) |

No new extensions need to be added to `nhcore.cabal`.

## Consequences

### Positive

- **Single-responsibility handlers**: Each handler module has one job — react to one event type. Jess can understand, test, and modify each handler independently. Adding a new event to the ADT doesn't force changes in unrelated handlers.

- **Discoverable integration landscape**: Browsing `Integrations/` reveals every integration for an entity. The module names describe the action and trigger: `ReserveStockOnItemAdded`, `NotifyOwnerOnCartCreated`. Jess can find integrations by name.

- **Familiar pattern**: The handler structure (data type + EntityOf + business logic + TH macro) mirrors the `command` pattern Jess already knows. She copies `CreateCart.hs`, changes the names, and it works.

- **Compile-time safety**: The TH macro validates that `EntityOf` exists, `EventOf` resolves, `handleEvent` has the correct signature, and `EventVariantOf` is available. Jess gets actionable errors at compile time, not runtime `case` match failures.

- **Foundation for DSL wiring**: The typeclass constraints are designed to be satisfiable by the future Event Model DSL combinators (#578). The `outboundIntegration` combinator in the DSL will "just work" with handlers generated by this TH macro.

### Negative

- **More files per integration**: Each handler is a separate module. An entity with 5 integrations has 5 files instead of 1. This is intentional (single responsibility) but increases file count.

- **TH macro complexity**: The `outboundIntegration` macro must validate `handleEvent`'s signature by inspecting TH reified types. This is non-trivial Template Haskell code, similar in complexity to the existing `command` macro.

- **Migration required**: Existing full-ADT integration functions (`entity -> event -> Integration.Outbound`) must be split into per-handler modules. The testbed's `cartIntegrations` function will be migrated as part of this feature. This is intentional — one pattern, one way to do it.

### Risks

1. **`EventVariantOf` instance missing**: If Jess uses a standalone event type (e.g., `ItemAdded` as its own type, not a constructor of `CartEvent`) but forgets to write the `EventVariantOf CartEvent ItemAdded` instance, the TH macro will fail with an error. This is the correct behavior — the error message guides her to add the instance.

2. **Handler not registered in Application**: Jess writes a handler module but forgets to wire it into `Application.withOutbound`. The handler compiles but never runs. This is the same risk as forgetting to register a command — it's inherent to the wiring pattern and will be addressed by the DSL (#578).

3. **Performance of multiple handlers**: Each handler for an entity type requires a separate `toVariant` call per event. For an entity with 10 handlers, each event triggers 10 `toVariant` calls (9 returning `Nothing`). This is negligible — `toVariant` is a simple pattern match, and the identity instance is inlined away.

### Mitigations

1. **TH error messages**: The macro produces specific, actionable error messages for every validation failure. Jess reads the error and knows exactly what to add.

2. **Testbed example**: The testbed will include a `ReserveStockOnItemAdded` handler as a reference implementation. Jess copies this pattern for her own handlers.

3. **Documentation**: The handler module template will be documented in the architecture guide. The recommended naming convention (`ActionOnEvent`) makes the purpose of each handler self-documenting.

4. **DSL wiring (future)**: Issue #578 will provide a declarative DSL that automatically registers handlers, eliminating the risk of forgetting to wire them.

## References

- [#576: Event Model DSL: `OutboundIntegration` typeclass + dispatch generation](https://github.com/neohaskell/NeoHaskell/issues/576)
- [#575: Event Model DSL: `EventVariantOf` typeclass + Decider modifications](https://github.com/neohaskell/NeoHaskell/issues/575) — dependency (merged)
- [#578: Event Model DSL: structural combinators](https://github.com/neohaskell/NeoHaskell/issues/578) — future DSL wiring
- [#573: Event Model DSL](https://github.com/neohaskell/NeoHaskell/issues/573) — parent issue
- [ADR-0047: EventVariantOf Typeclass](0047-eventvariantof-typeclass.md) — foundation typeclass
- [core/service/EventVariantOf.hs](../../core/service/EventVariantOf.hs) — `EventVariantOf` typeclass with `toVariant`
- [core/service/Integration.hs](../../core/service/Integration.hs) — `Integration.Outbound` type and combinators
- [core/service/Service/Integration/Types.hs](../../core/service/Service/Integration/Types.hs) — `OutboundRunner` type-erased runner
- [core/service/Service/Application/Integrations.hs](../../core/service/Service/Application/Integrations.hs) — `createOutboundRunner`, `fetchEntityState`
- [core/service/Service/CommandExecutor/TH.hs](../../core/service/Service/CommandExecutor/TH.hs) — `command` TH macro (pattern to follow)
- [core/service/Service/Query/TH.hs](../../core/service/Service/Query/TH.hs) — `deriveQuery` TH macro (pattern to follow)
- [testbed/src/Testbed/Cart/Integrations.hs](../../testbed/src/Testbed/Cart/Integrations.hs) — current full-ADT integration pattern
