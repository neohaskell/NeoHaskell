---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Event Model Slices now require implemented components. A declared command, query, entity, or event payload without its required implementation fails compilation. Applications cannot describe future components with `plannedEntity`, `plannedCommand`, `plannedProduces`, `plannedQuery`, `plannedReadsFrom`, or `plannedOnEvent`.

## Migration

If your application used a `planned*` builder, implement that component before adding it to a Feature. Replace `plannedEntity` with `entity`, `plannedCommand` with `command`, `plannedProduces` with `produces`, `plannedQuery` with `query`, `plannedReadsFrom` with `readsFrom`, and `plannedOnEvent` with `onEvent`. Define the matching `Entity`, `Command`, `Query`, `QueryOf`, and event payload relationships. Generate each entity Event ADT with a separate `deriveEntityEvents` call. Keep the existing `Application` registrations until Event Model runtime wiring is available.

### Verify

Compile the application and run the command, query, and integration tests for every declared Slice. In this repository, run `nix develop --command python3 core/test/Service/EventModel/check_compiler_rejections.py` and `nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict`.

## Agent prompt

```text
Find every plannedEntity, plannedCommand, plannedProduces, plannedQuery, plannedReadsFrom, plannedOnEvent, and sliceIsPlanned use in my NeoHaskell application. Implement the declared entity, command, query, and payload behavior before replacing each planned builder with entity, command, produces, query, readsFrom, or onEvent. Generate event ADTs separately with deriveEntityEvents, and preserve the existing Application registrations until Event Model runtime wiring is available. Remove any topology-only Slice whose behavior is not being implemented. Compile the app, run its command, query, and integration tests, and report the changed files, results, and any missing implementation that still blocks compilation.
```
