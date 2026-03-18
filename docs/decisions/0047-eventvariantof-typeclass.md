# ADR-0047: EventVariantOf Typeclass for Type-Safe Event Variant Wrapping

## Status

Proposed

## Context

### Current State

NeoHaskell's event-sourcing framework requires every command's `decide` function to return a `Decision event`, where `event` is the entity's event ADT (e.g., `CartEvent`). The Decider smart constructors (`acceptNew`, `acceptExisting`, `acceptAny`, `acceptAfter`) currently accept `Array a` and produce `Decision a`, where `a` must exactly match the event ADT type:

```haskell
-- Current signatures in Decider.hs
acceptNew :: (Array a) -> Decision a
acceptExisting :: (Array a) -> Decision a
acceptAny :: (Array a) -> Decision a
acceptAfter :: StreamPosition -> (Array a) -> Decision a
```

This means every event in the array must already be a constructor of the event ADT. Today, this works because all events are defined as constructors of a single sum type:

```haskell
data CartEvent
  = CartCreated { entityId :: Uuid, ownerId :: Text }
  | ItemAdded { entityId :: Uuid, stockId :: Uuid, quantity :: Int }
  deriving (Eq, Show, Ord, Generic)
```

And decide functions construct events directly using these constructors:

```haskell
decide cmd entity _ctx = case entity of
  Nothing -> Decider.reject "Cart not found!"
  Just cart ->
    Decider.acceptExisting
      [ ItemAdded { entityId = cart.cartId, stockId = cmd.stockId, quantity = cmd.quantity } ]
```

This pattern works for simple domains but creates friction as domains grow. The parent issue (#573, Event Model DSL) envisions a future where each event variant can be its own standalone type, localized to the slice (command module) that produces it. To enable that pattern, we need a typeclass that can wrap individual event types into the entity's event ADT.

### Use Cases

- **Single-type events (common case)**: Jess writes `Decider.acceptExisting [ItemAdded {...}]` and it works exactly as today — no change required. The event ADT is a variant of itself.

- **Mixed-type events in one decision**: A command produces multiple different event types in a single decision. Today, this requires all constructors to be from the same ADT. With `EventVariantOf`, Jess can mix types using the `event` helper:
  ```haskell
  Decider.acceptExisting
    [ event (PdfUploaded { entityId = id, pdfRef = ref })
    , event (TimestampRecorded { entityId = id, timestamp = now })
    ]
  ```

- **Events-as-types pattern (future)**: Each event variant is its own standalone type defined in the command module that produces it, rather than a constructor in a centralized ADT. The `EventVariantOf` instance provides the bridge from the standalone type to the entity's event ADT. This is the foundation for the Event Model DSL (#573).

- **Cross-domain event reuse**: An event type defined in one bounded context can declare itself as a variant of another context's event ADT, enabling controlled cross-domain event sharing without coupling the ADTs.

### Design Goals

1. **Zero-cost for existing code**: All existing `decide` functions that pass event ADT constructors directly must continue to compile and work without modification. This is the backward compatibility requirement.

2. **Jess-friendly API**: The `event` helper function must be discoverable, obvious in purpose, and require no understanding of typeclasses to use. Jess sees `event (MyEvent {...})` and understands "wrap this into the event type."

3. **Type-safe at compile time**: If Jess tries to pass an event type that doesn't have an `EventVariantOf` instance, the compiler error must clearly say what's wrong and how to fix it.

4. **Foundation for Event Model DSL**: This typeclass is the first building block of #573. It must be designed so that future TH-generated instances (from the Event Model DSL) slot in naturally.

### GitHub Issue

- [#575: Event Model DSL: `EventVariantOf` typeclass + Decider modifications](https://github.com/neohaskell/NeoHaskell/issues/575)
- [#573: Event Model DSL](https://github.com/neohaskell/NeoHaskell/issues/573) — parent issue

## Decision

### 1. Typeclass Name: `EventVariantOf`

The typeclass maps an event variant type to its parent event ADT via a single method `fromVariant`.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| `ToEvent` | Rejected | Too generic — could mean serialization, conversion, or construction. Doesn't communicate the variant relationship. |
| `EventWrapper` | Rejected | Implies a wrapper type, not a typeclass. "Wrapper" suggests runtime overhead. |
| `IsEventOf` | Rejected | Reads as a predicate ("is this an event of X?") rather than a conversion. |
| `EventVariantOf` | **Chosen** | Reads naturally: "PdfUploaded is an EventVariantOf DocumentEvent." Communicates the variant-to-ADT relationship. Matches the ES/CQRS concept of event variants. |

### 2. Module Placement: `core/service/EventVariantOf.hs`

The module lives in the `service` source directory alongside `Decider.hs`, not in `traits/`. This is a domain-specific typeclass for event-sourcing, not a general-purpose trait like `Mappable` or `Appendable`.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| `core/traits/EventVariantOf.hs` | Rejected | `traits/` contains general-purpose typeclasses (`Mappable`, `Appendable`, `Combinable`). `EventVariantOf` is domain-specific to event-sourcing. |
| `core/service/Service/Entity/EventVariantOf.hs` | Rejected | Unnecessary nesting. The typeclass is used by `Decider.hs`, not by `Entity/Core.hs`. Placing it under `Entity/` implies it's part of the entity abstraction. |
| `core/service/EventVariantOf.hs` | **Chosen** | Same level as `Decider.hs` and `Integration.hs`. Flat structure. The service source directory is the right home for event-sourcing infrastructure. |

### 3. Type Definition

```haskell
-- | Declares that @eventVariant@ can be converted into @event@.
--
-- This typeclass enables individual event types to be wrapped into
-- an entity's event ADT. The Decider smart constructors use this
-- constraint to accept variant types directly.
--
-- The identity instance @EventVariantOf event event@ is provided,
-- so existing code that passes the event ADT directly continues to work.
--
-- Example:
--
-- @
-- -- The event ADT
-- data CartEvent
--   = CartCreated { entityId :: Uuid, ownerId :: Text }
--   | ItemAdded { entityId :: Uuid, stockId :: Uuid, quantity :: Int }
--
-- -- Identity instance (provided by default — every ADT is a variant of itself)
-- instance EventVariantOf CartEvent CartEvent where
--   fromVariant cartEvent = cartEvent
--
-- -- Standalone event type with explicit instance
-- data PdfUploaded = PdfUploaded { entityId :: Uuid, pdfRef :: Text }
--
-- instance EventVariantOf DocumentEvent PdfUploaded where
--   fromVariant uploaded = DocumentPdfUploaded uploaded.entityId uploaded.pdfRef
-- @
class EventVariantOf event eventVariant where
  -- | Convert an event variant into the parent event ADT.
  fromVariant :: eventVariant -> event
```

**Key design decisions in the type definition:**

- **Parameter order is `event eventVariant`** (target first, source second). This matches the reading order "X is an EventVariantOf Y" and follows the Haskell convention where the "output" type comes first in multi-param typeclasses (like `MonadReader r m` where `m` is the monad you're working in).

- **No functional dependency or type family**. A single variant type could theoretically be a variant of multiple event ADTs (cross-domain reuse). Functional dependencies would prevent this. The Decider smart constructors constrain the relationship through the `Decision event` return type, so GHC can always infer the correct instance.

- **No default method**. `fromVariant` has no sensible default — each instance must explicitly define the conversion.

### 4. Identity Instance for Backward Compatibility

```haskell
-- | Every event type is a variant of itself.
--
-- This instance ensures that existing code passing the event ADT directly
-- to Decider smart constructors continues to work without modification.
--
-- @
-- -- This still works (CartEvent is a variant of CartEvent):
-- Decider.acceptExisting [ItemAdded { entityId = id, stockId = sid, quantity = 1 }]
-- @
instance EventVariantOf event event where
  fromVariant eventValue = eventValue
  {-# INLINE fromVariant #-}
```

This is the critical backward compatibility mechanism. Because `CartEvent` is an `EventVariantOf CartEvent CartEvent`, all existing code that passes `[CartCreated {...}]` to `acceptExisting` continues to compile. The identity `fromVariant` is a no-op that GHC will inline away.

**Note**: This requires the `{-# LANGUAGE FlexibleInstances #-}` extension. This is already commonly used in the nhcore codebase (e.g., in `Service.Command.Core` and `Collection`). The `IncoherentInstances` extension is **not** needed — GHC's instance resolution picks the identity instance when both type parameters are the same, and a more specific instance when they differ.

### 5. The `event` Helper Function

```haskell
-- | Wrap an event variant into the parent event ADT.
--
-- This is an alias for 'fromVariant' with a friendlier name.
-- Use it when building arrays of mixed event types in a single decision:
--
-- @
-- Decider.acceptExisting
--   [ event (PdfUploaded { entityId = id, pdfRef = ref })
--   , event (TimestampRecorded { entityId = id, timestamp = now })
--   ]
-- @
--
-- For single-type arrays, you don't need this — just pass the events directly:
--
-- @
-- Decider.acceptExisting [ItemAdded { entityId = id, stockId = sid, quantity = 1 }]
-- @
event :: forall event eventVariant. (EventVariantOf event eventVariant) => eventVariant -> event
event variant = fromVariant variant
{-# INLINE event #-}
```

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| `fromVariant` only (no alias) | Rejected | `fromVariant` is a typeclass method name — it's precise but not what Jess would guess. She'd try `event` or `toEvent` first. |
| `toEvent` | Rejected | Implies the variant "becomes" an event, but it already IS an event. `toEvent` also collides conceptually with serialization patterns (`toJSON`, `toText`). |
| `wrap` | Rejected | Too generic. `wrap` could mean anything. Doesn't communicate the event domain. |
| `event` | **Chosen** | Reads naturally in context: `event (PdfUploaded {...})`. Jess sees "make this an event." Short, obvious, domain-specific. Discoverable via autocomplete on the `EventVariantOf` module. |

### 6. Modified Decider Smart Constructors

The Decider smart constructors gain an `EventVariantOf` constraint and map `fromVariant` over the input array before wrapping in the `Accept` constructor:

```haskell
-- | Accept a command regardless of stream state, emitting the given events.
--
-- Events can be the entity's event ADT directly, or any type with an
-- 'EventVariantOf' instance for that ADT.
--
-- @
-- Decider.acceptAny [CartCreated { entityId = id, ownerId = owner }]
-- @
acceptAny :: forall event variant. (EventVariantOf event variant) => Array variant -> Decision event
acceptAny variants = do
  let events = variants |> Array.map fromVariant
  Accept AnyStreamState events

-- | Accept a command only if the stream doesn't exist yet, emitting the given events.
--
-- @
-- Decider.acceptNew [CartCreated { entityId = id, ownerId = owner }]
-- @
acceptNew :: forall event variant. (EventVariantOf event variant) => Array variant -> Decision event
acceptNew variants = do
  let events = variants |> Array.map fromVariant
  Accept StreamCreation events

-- | Accept a command only if the stream already exists, emitting the given events.
--
-- @
-- Decider.acceptExisting [ItemAdded { entityId = id, stockId = sid, quantity = 1 }]
-- @
acceptExisting :: forall event variant. (EventVariantOf event variant) => Array variant -> Decision event
acceptExisting variants = do
  let events = variants |> Array.map fromVariant
  Accept ExistingStream events

-- | Accept a command only if the stream is at the specified position, emitting the given events.
--
-- @
-- Decider.acceptAfter position [ItemAdded { entityId = id, stockId = sid, quantity = 1 }]
-- @
acceptAfter :: forall event variant. (EventVariantOf event variant) => StreamPosition -> Array variant -> Decision event
acceptAfter pos variants = do
  let events = variants |> Array.map fromVariant
  Accept (InsertAfter pos) events
```

**Why `Array.map fromVariant` inside the constructor?** The `Decision` GADT's `Accept` constructor stores `Array a` where `a` matches the `Decision a` type parameter. The smart constructors accept `Array variant` and must produce `Decision event`. The `fromVariant` mapping converts `Array variant` to `Array event` before wrapping in `Accept`.

**Performance**: For the identity instance (`EventVariantOf event event`), `fromVariant` is `identity` and GHC will inline it away, making `Array.map fromVariant` equivalent to `identity` on the array. There is zero runtime cost for existing code.

### 7. Re-export from Core

The `EventVariantOf` typeclass and the `event` helper should be re-exported from `Core.hs` so that Jess never needs to import `EventVariantOf` directly:

```haskell
-- In Core.hs, add:
import EventVariantOf as Reexported (EventVariantOf (..), event)
```

This makes `event` available everywhere without an explicit import, matching how `Decision`, `Entity`, and other service types are already available through `Core`.

### 8. Public API

```haskell
module EventVariantOf
  ( -- * Typeclass
    EventVariantOf (..)
    -- * Helper
  , event
  ) where
```

The module exports:
- `EventVariantOf (..)` — the typeclass with its `fromVariant` method, so users can write instances
- `event` — the friendly alias for `fromVariant`

### 9. GHC Extensions Required

The `EventVariantOf` module requires these extensions beyond what nhcore already enables project-wide:

| Extension | Why Needed | Already in nhcore? |
|-----------|-----------|-------------------|
| `MultiParamTypeClasses` | `EventVariantOf` has two type parameters | Yes (implied by `TypeFamilies`) |
| `FlexibleInstances` | Identity instance `EventVariantOf event event` uses a type variable in both positions | Yes (used in many modules) |

No new extensions need to be added to `nhcore.cabal`. The existing project-wide extensions cover all requirements.

## Consequences

### Positive

- **Zero breakage**: The identity instance `EventVariantOf event event` ensures all existing `decide` functions compile without modification. Jess's existing code works exactly as before.

- **Enables events-as-types pattern**: Individual event types can be standalone records with `EventVariantOf` instances, enabling the Event Model DSL (#573) where each event is localized to its producing command module.

- **Natural mixed-type arrays**: The `event` helper lets Jess build arrays of mixed event types without manual ADT wrapping: `[event (PdfUploaded {...}), event (TimestampRecorded {...})]`.

- **Discoverable API**: `event` is a single, obvious function name. Jess types `event (` and autocomplete shows the signature. The function name reads naturally in context.

- **Foundation for TH generation**: Future Event Model DSL macros can generate `EventVariantOf` instances automatically, making the typeclass invisible to Jess while providing the type-safe plumbing.

- **Zero runtime cost for common case**: The identity instance inlines to `identity`, so `Array.map fromVariant` on a homogeneous array is optimized away by GHC.

### Negative

- **Typeclass complexity for advanced users**: Developers who read the Decider source will see `EventVariantOf` constraints on the smart constructors. This is more complex than the current unconstrained `Array a`. However, Jess never reads Decider internals — she uses the smart constructors through autocomplete.

- **Potential for confusing type errors**: If Jess passes an event type that doesn't have an `EventVariantOf` instance, GHC will produce a "No instance for (EventVariantOf SomeEvent WrongType)" error. This is less friendly than the current "couldn't match type" error, though it's more informative about what's actually wrong.

- **Identity instance uses overlapping resolution**: The identity instance `EventVariantOf event event` overlaps with any specific instance `EventVariantOf MyEvent MyVariant` when `MyVariant ~ MyEvent`. GHC resolves this correctly (the more specific instance wins), but it requires understanding of instance resolution for anyone maintaining the typeclass.

### Risks

1. **Instance resolution ambiguity**: If a type has both an identity instance and a specific instance (e.g., `EventVariantOf CartEvent CartEvent` from the blanket instance AND a user-written `EventVariantOf CartEvent CartEvent`), GHC will report an overlapping instance error. This is unlikely in practice since users would only write instances for distinct variant types.

2. **Orphan instance temptation**: Users might define `EventVariantOf` instances in command modules rather than alongside the event type definition. This creates orphan instances that can cause compilation issues. The documentation and future TH macros should guide users to define instances in the correct location.

3. **`event` name collision**: The name `event` is common in event-sourcing codebases. It could collide with local variable names. However, since it's re-exported from `Core` and used qualified-style in practice (`event (MyEvent {...})`), collisions are manageable — Jess can shadow it with a local binding if needed.

### Mitigations

1. **Instance resolution**: Document clearly that users should never write `instance EventVariantOf MyEvent MyEvent` — the blanket identity instance handles this. The future TH macro will enforce this by only generating instances for distinct variant types.

2. **Orphan instances**: The architecture document will specify that `EventVariantOf` instances must be defined in the same module as the event variant type. The future TH macro will generate instances in the correct location automatically.

3. **Name collision**: The `event` function is short and domain-specific. If a collision occurs, Jess can use the qualified form `EventVariantOf.event` or rename her local variable. This is the same tradeoff as `map`, `filter`, and other common names in nhcore.

4. **Type error quality**: Add a custom type error using `TypeError` from `GHC.TypeLits` in a future iteration if the default GHC error proves too confusing. For now, the "No instance for (EventVariantOf ...)" message is adequate — it names the exact types involved.

## References

- [#575: Event Model DSL: `EventVariantOf` typeclass + Decider modifications](https://github.com/neohaskell/NeoHaskell/issues/575)
- [#573: Event Model DSL](https://github.com/neohaskell/NeoHaskell/issues/573) — parent issue
- [core/service/Decider.hs](../../core/service/Decider.hs) — Decider smart constructors to be modified
- [core/service/Service/Entity/Core.hs](../../core/service/Service/Entity/Core.hs) — Entity/Event typeclasses and type families
- [core/traits/Mappable.hs](../../core/traits/Mappable.hs) — existing typeclass pattern in nhcore
- [testbed/src/Testbed/Cart/Core.hs](../../testbed/src/Testbed/Cart/Core.hs) — example event ADT pattern
- [testbed/src/Testbed/Cart/Commands/AddItem.hs](../../testbed/src/Testbed/Cart/Commands/AddItem.hs) — example decide function using current API
