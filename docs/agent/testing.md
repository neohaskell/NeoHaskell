# Testing

Run `neo test --ci` from a generated project's root. The command reconciles the project, runs Cabal tests, discovers Hurl files, and starts the application for Hurl tests.

```sh
neo test --ci
```

## Decision tests

Write Hspec modules under `tests/` with names ending in `Spec`. Reconciliation maintains `tests/Spec.hs` and the generated test suite.

Execute `Decision` values before inspecting acceptance or rejection. Supply a `DecisionContext`, call `Decider.runDecision`, and assert the observed result. Merely constructing a decision does not run its capabilities.

Test missing entities, rejected input, and accepted events. For state transitions, apply emitted events to the reducer and check the state consumers observe.

## HTTP scenarios

Put Hurl scenarios under `tests/`. In an end-to-end test, create an entity, change it, and read the projected result. Retry the query read because projections update asynchronously.

`neo test` probes port 8080 before Hurl execution. Keep the test application's port and scenario URLs consistent with that port. A scenario file by itself does not start a server.

For direct Haskell checks, use the project's Nix shell.

```sh
nix develop --command cabal test
```

The generated flake defines the project's toolchain.

## Integration tests

When adding a remote integration, test its endpoint interaction against a deterministic fake service. Assert the outbound request, the callback command, and the resulting domain state. For OAuth2, cover authorization, token exchange, invalid or replayed state, and disconnect. Use [side effect guidance](advanced-side-effects.md) and [OAuth2 guidance](advanced-oauth2.md) for the boundaries to exercise.
