---
name: neohaskell-concept-derivation
description: Derive NeoHaskell concepts (Query, Command, Event, Outbound integration) through their canonical Template Haskell marker instead of hand-writing the instances the marker generates. Use whenever creating or editing a query, command, event, entity, or any concept that has a TH marker — reach for deriveQuery / command / event / outboundIntegration and author ONLY the required companion functions and business-logic instances; never hand-write the Show/Generic/JSON/ToSchema/NameOf/EntitiesOf/Query/KnownHash instances the marker emits.
---

# NeoHaskell concept derivation

Every core concept type (query, command, event, outbound integration) has a
Template Haskell **marker** that emits the mechanical instances it needs. The
marker is the canonical way to declare the concept. Your job is to author the
**data type** plus the **required companion functions and business-logic
instances** — never the boilerplate the marker owns.

## The rule

**Use the marker. Author only what the marker cannot: the data type, the
required companion functions, the type-family wiring, and the business-logic
instances. Never hand-write an instance the marker emits.**

Markers are idempotent (they call `Service.TH.Boilerplate.emitInstanceIfMissing`,
which reifies existing instances and skips them). So a hand-written
`deriving (Show, Generic)` or `instance ToJSON` next to a marker is *silently
redundant* at best — and a hard `Duplicate instance` GHC error if you place it
*after* the marker. Both are the noise this skill exists to remove.

## The markers (ground truth — read the module, don't recall)

| Concept | Marker | Module | Emits (skipped if already in scope) | You author |
|---|---|---|---|---|
| Query | `deriveQuery ''Q [''E1, ''E2]` | `Service.Query.TH` | `Show`, `Generic`, `FromJSON`, `ToJSON`, `ToSchema`, `NameOf Q = "Q"`, `EntitiesOf Q = '[E1,E2]`, `Query` (wires `canAccessImpl = canAccess`, `canViewImpl = canView`, and `maxResultsImpl = maxResults` if `maxResults` is defined), `KnownHash` | the data type; **`canAccess`** and **`canView`** (required, top-level); `maxResults :: Int` (optional cap); one **`QueryOf entity Q`** instance per contributing entity (business logic) |
| Command | `command ''C` | `Service.CommandExecutor.TH` | `Show`, `Generic`, `FromJSON`, `ToJSON`, `NameOf C = "C"`, `Command` (wires `getEntityIdImpl = getEntityId`, `decideImpl = decide`, and the multi-tenancy mode), `KnownHash` | the data type; **`getEntityId`** and **`decide`** (required); **`type instance EntityOf C = SomeEntity`** (required); `type instance TransportsOf C = '[WebTransport]` for HTTP commands |
| Event | `event ''E` | `Service.Event.TH` | `Show`, `Generic`, `FromJSON`, `ToJSON` | the data type; for a domain event driven through the store: `type instance EventOf/EntityOf` and `instance Event` (business `getEventEntityIdImpl`) |
| Outbound | `outboundIntegration ''O` | `Service.OutboundIntegration.TH` | `KnownHash` + the JSON/deriving boilerplate | the data type + the companion functions the module's error messages name — **read `Service/OutboundIntegration/TH.hs`** before use |

`NameOf` is the **verbatim type name** (`"CartSummary"`, `"AddItem"`), NOT
kebab-case. Kebab-case is only for HTTP URLs; the marker uses the exact type
name as the dispatch/checkpoint key (see `Service.Query.TH`).

### Entities have no marker

An entity is business logic, so it is **hand-written**: the record + `deriving
(Generic)` + `instance FromJSON/ToJSON` + `type instance NameOf` +
`type instance EventOf` + `instance Entity` (`initialStateImpl`/`updateImpl`).
There is nothing to defer to a marker here — writing these instances is correct,
not a violation. (`type instance EntityOf <event> = <entity>` lives with the
entity too.)

## Canonical shapes

Query (`testbed/src/Testbed/Cart/Queries/CartSummary.hs` is the reference):

```haskell
data CartSummary = CartSummary
  { cartSummaryId :: Uuid, ownerId :: Text, itemCount :: Int }
  -- no `deriving` clause for Show/Generic — the marker emits them

canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = AccessControl.authenticatedAccess   -- secure default

canView :: Maybe UserClaims -> CartSummary -> Maybe AccessError
canView = AccessControl.ownerOnly (.ownerId)

-- business logic the marker cannot know:
instance QueryOf CartEntity CartSummary where
  queryId cart = cart.cartId
  combine cart _ = ...

deriveQuery ''CartSummary [''CartEntity]   -- MARKER LAST
```

Command (`testbed/src/Testbed/Cart/Commands/AddItem.hs`):

```haskell
data AddItem = AddItem { cartId :: Uuid, quantity :: Int }

getEntityId :: AddItem -> Maybe Uuid
getEntityId cmd = Just cmd.cartId

decide :: AddItem -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide cmd entity _ctx = ...

type instance EntityOf AddItem = CartEntity
type instance TransportsOf AddItem = '[WebTransport]

command ''AddItem   -- MARKER LAST
```

## Common pitfalls

1. **Hand-writing marker-owned instances** — `deriving (Show, Generic)`,
   `instance ToJSON/FromJSON`, `instance ToSchema`, `type instance NameOf`,
   `type instance EntitiesOf`, `instance Query`, or `instance KnownHash`
   alongside the marker. Delete them; the marker owns them. After the marker
   they are a `Duplicate instance` error.
2. **Assuming `NameOf` is kebab-case.** It is the exact type name.
3. **Missing companion / type-family.** `deriveQuery` without `canAccess`/
   `canView`, or `command` without `getEntityId`/`decide`/`EntityOf`, fails at
   compile with a message naming exactly what to add — add it, don't fight it.
4. **Marker not last.** Companions, `QueryOf`/`Entity`/`Event` instances, and
   `type instance` wiring must appear *before* the marker call; TH resolves
   them via `lookupValueName`/`reifyInstances` at splice time.
5. **Copying a pre-marker module.** Some older modules (e.g.
   `Testbed.Cart.Core`) hand-write everything and predate the markers — do not
   copy that style for a new concept.

## Verification checklist

- [ ] The data type has NO `deriving` clause for `Show`/`Generic` and NO
      hand-written `ToJSON`/`FromJSON`/`ToSchema`.
- [ ] Required companions are defined ABOVE the marker (query → `canAccess`,
      `canView`; command → `getEntityId`, `decide`).
- [ ] Required `type instance`s are ABOVE the marker (command → `EntityOf`,
      `TransportsOf`; query passes its entity list as the marker's 2nd arg).
- [ ] Business-logic instances (`QueryOf`, `Entity`, `Event`) are hand-written
      (correct — the marker does not own these).
- [ ] The marker call is the LAST declaration for the type.
- [ ] It compiles: `./dev check` (or run the concept's spec with `./dev test`).
      A missing-companion TH error tells you precisely what to add.
