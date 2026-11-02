---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

An Event Model now rejects an inbound Slice that omits or repeats a command and target entity pair promised by its typed worker. Validation reports assembled Slice errors with their Feature, Chapter, and Slice paths before application workers start. Applications with incomplete inbound declarations must list every typed worker output once.

## Migration

For each `inboundIntegration @Handler` Slice, compare its `triggersCommand` and `onTargetEntity` declarations with the handler's `InboundOutputsOf` pairs. Add missing pairs and remove repeated pairs. If the application starts with `EventModel.run`, the Slice registers the handler. Remove any separate `Application.withInboundHandler @Handler` registration for that handler in the model-backed application. If the application assembles and runs `Application` directly, keep its `Application.withInboundHandler @Handler` registration.

### Verify

Compile the application and run its inbound integration tests. For an application that starts with `EventModel.run`, evaluate its named `eventModel` before starting workers. In this repository, run `nix develop --command python3 core/test/Service/EventModel/check_compiler_rejections.py`, `nix develop --command cabal test --project-file=cabal.project.dev nhtestbed-spec-demo --test-show-details=direct`, and `nix develop --command .agents/skills/neohaskell-test/scripts/run-testbed --strict` against an isolated PostgreSQL database.

## Agent prompt

```text
Find each inboundIntegration @Handler Slice and the executable that starts my NeoHaskell application. Compare its triggersCommand and onTargetEntity pairs with the handler's InboundOutputsOf type. Add every missing pair and remove any repeated pair. If the executable calls EventModel.run, use the Slice as the handler registration and remove any separate Application.withInboundHandler @Handler registration for that handler in the model-backed application. If the executable assembles and runs Application directly, keep Application.withInboundHandler @Handler. Compile the application, evaluate its named eventModel if it uses one, run its inbound integration tests, and report changed files and results.
```
