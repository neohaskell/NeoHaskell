---
name: neohaskell-concept-derivation
description: Declare NeoHaskell concepts with deriveEvent, deriveCommand, deriveEntity, deriveQuery, and deriveOutboundIntegration from Core. Author data and business functions; let the markers generate their mechanical instances.
---

# NeoHaskell concept derivation

Import `Core` for every concept marker. Use the canonical `deriveXXX` names;
legacy `event`, `command`, and `outboundIntegration` TH names remain compatibility
exports from their defining modules. `Core.event` is a value-level event injection
helper, distinct from `deriveEvent`.

## The rule

Write the data type and business functions. Do not hand-write the instances the
marker generates. Custom instances belong before the marker so it can preserve
them; do not duplicate generated instances after it. Do not add source language
pragmas to reader-owned Neo projects: the CLI configures the compiler extensions.

| Concept | Marker from Core | Generates | Application code |
|---|---|---|---|
| Event payload or sum | `deriveEvent ''CartCreated` | Show, Generic, FromJSON, ToJSON | Data declaration; Eq only when separately needed |
| Command | `deriveCommand ''AddItem` | Show, Generic, JSON, ToSchema, NameOf, KnownHash, Command wiring | `EntityOf`, transport mapping when needed, `getEntityId`, `decide`, optional access control and multi-tenancy declarations |
| Entity | `deriveEntity ''CartEntity ''CartEvent` | Generic, JSON, NameOf, EventOf, event's EntityOf, Default, Entity and event-routing wiring | `initialState`, `update`, `getEventEntityId`; custom instances when needed |
| Query | `deriveQuery ''CartSummary [''CartEntity]` | Show, Generic, JSON, ToSchema, NameOf, EntitiesOf, KnownHash, Query wiring | `canAccess`, `canView`, optional `maxResults`; one `QueryOf` business projection per entity |
| Outbound handler | `deriveOutboundIntegration ''ReserveStock` | NameOf, KnownHash, OutboundIntegration and HandledEvent wiring | `EntityOf`, `handleEvent` |

Entity derivation does **not** require Show on fields. It preserves custom
instances and a custom NameOf. EventOf and EntityOf must agree with the supplied
entity and event types. An existing Entity instance retains its custom identifier
type. Outbound derivation does **not** generate JSON or Show instances.

The defining modules are `Service.Event.TH`, `Service.CommandExecutor.TH`,
`Service.Entity.TH`, `Service.Query.TH`, and `Service.OutboundIntegration.TH`.
Read those modules for the exact companion contract and diagnostics.

## Declaration order

Put required companion functions and command/handler type-family declarations
**before** their marker. A splice sees preceding declarations.

For queries, put the marker **before** `QueryOf` instances: the marker generates
NameOf, EntitiesOf and Query, which the projection instances may require. The
blanket advice "marker last in the module" is incorrect.

```haskell
canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess = AccessControl.authenticatedAccess

canView :: Maybe UserClaims -> CartSummary -> Maybe AccessError
canView = AccessControl.ownerOnly (.ownerId)

deriveQuery ''CartSummary [''CartEntity]

instance QueryOf CartEntity CartSummary where
  queryId cart = cart.cartId
  combine cart previous = ...
```

Keep each entity, event payload, command, query and integration in its appropriate
application module. Small documentation fragments may omit module/import headers;
the complete project must retain its own imports and compile as separate files.

## Verification

- No redundant Show/Generic/JSON/ToSchema or marker-owned wiring.
- Companions are in scope before the splice; projections follow query derivation.
- No confusion between `Core.event` (injection) and `deriveEvent` (derivation).
- New examples use the Core exports, not separate TH imports.
- Compile via `./dev check`; run relevant concept specs via `./dev test`.
- Existing legacy-name tests continue to cover compatibility.

This skill is checked by `./dev neo-skills-check`; implementation and export
contracts are covered by the registered service and integration TH specs.
