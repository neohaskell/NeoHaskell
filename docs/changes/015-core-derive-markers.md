# Change 015: Expose consistent concept derivation helpers through Core

Application authors import `Core` and use `deriveEvent`, `deriveCommand`,
`deriveEntity`, `deriveQuery`, and `deriveOutboundIntegration`. Existing marker
names remain compatible. Nick explicitly requested these exports and confirmed
adding entity derivation in the local session on 2026-09-16.

```yaml spec
issue: adhoc:core-derive-markers
kind: feature
touches: [core-primitives, commands, entities, queries, event-store, service-th, integration-runtime, website, governance-docs]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

```diff signatures
+ Service.Event.TH: deriveEvent :: Name -> DecsQ
+ Service.CommandExecutor.TH: deriveCommand :: Name -> DecsQ
+ Service.Entity.TH: deriveEntity :: Name -> Name -> DecsQ
+ Service.OutboundIntegration.TH: deriveOutboundIntegration :: Name -> DecsQ
```

`Core` reexports all five canonical helpers; its value-level `event` remains
available. `deriveQuery` retains its current signature. The other existing TH
names remain available from their defining modules.

`deriveEntity ''CartEntity ''CartEvent` generates Generic/JSON, NameOf, EventOf,
EntityOf for the event, Default, Entity, and event-routing instances. Application
code provides `initialState`, `update`, and `getEventEntityId` before the marker.
Existing instances are preserved, including custom EntityIdType and JSON
instances. Entity derivation does not impose Show on entity fields. Missing
companions receive actionable compile-time errors. Business decisions, reducers,
event routing, and query projections remain application code.

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Core exposes event derivation while retaining value-level event injection | `hspec:nhcore-test-service:core/test/Service/Entity/THSpec.hs#derives an event while retaining Core.event injection` | unit | none |
| C2 | Entity derivation delegates state, update, routing and default behavior | `hspec:nhcore-test-service:core/test/Service/Entity/THSpec.hs#delegates updates and routes the event despite a local Event type` | unit | none |
| C3 | Missing entity companions fail and existing instances remain valid | `hspec:nhcore-test-service:core/test/Service/Entity/THSpec.hs#requires initialState when deriving a missing Entity instance`<br>`hspec:nhcore-test-service:core/test/Service/Entity/THSpec.hs#preserves custom JSON, name, default, entity behavior and identifier type` | unit | none |
| C4 | Canonical command, query and integration exports compile with existing behavior | `hspec:nhcore-test-service:core/test/Service/CommandExecutor/THSpec.hs#[impl-driven] emits Show instance on fresh command (has Generic, missing Show/ToJSON/FromJSON)`<br>`hspec:nhcore-test-service:core/test/Service/Query/THSpec.hs#generates Query instance for UserOrders`<br>`hspec:nhcore-test-integration:core/test/OutboundIntegrationSpec.hs#generates a compilable OutboundIntegration instance` | unit | none |
| C5 | Progressive examples and their downloadable projects use the canonical names | `script:website/scripts/check-docs.mjs#--check` | unit | none |

## User impact

New examples need no separate TH imports. Entities need one declaration helper
beside their application functions. Existing applications need no migration;
the previous names still work. Tutorial downloads require the framework version
that contains these new helpers.

## ADR

Not required — additive helpers within existing capabilities, without new
dependencies or extension-point registrations.

## Implementation plan

- Break Core import back-edges in CommandExecutor/TH, Query/TH,
  OutboundIntegration/TH and TH/Boilerplate using Basics, Appendable and Maybe.
- Add aliases in the existing TH modules and reexports in core/core/Core.hs.
- Copy-adapt Service/Entity/TH.hs from Event/TH.hs and Query/TH.hs; use canonical
  quoted Entity.Core names rather than a consumer's potentially shadowed Event.
- Copy-adapt core/test/Service/Entity/THSpec.hs from Service/Event/THSpec.hs;
  register its fixtures in core/nhcore.cabal and core/test-service/Main.hs.
- Migrate selected existing marker fixtures to prove Core exports while keeping
  legacy fixtures. Remove redundant direct deriveQuery imports.
- Update website examples, documentation excerpts, authoring rules and concept
  skill guidance. Regenerate archives, evidence hashes and codemap artifacts.

Resolved primitives: TH.reifyInstances, TH.lookupValueName, TH.recover,
emitInstanceIfMissing, emitStockDeriving, emitEmptyInstance, Entity(..), Event(..),
NameOf, EntityOf, EventOf, Default(..). Existing test neighbors prove the marker
and JSON conventions. The HIE index was unavailable; bounded caller lookup was
used as instructed by who-calls.
