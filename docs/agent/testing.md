# Testing

Run `neo test --ci` from a generated project's root. The test command in `neo/src/commands/test.rs` reconciles the project, runs Cabal tests, discovers Hurl files, and starts the application for Hurl tests.

```sh
neo test --ci
```

## Decision tests

Write Hspec modules under `tests/` with names ending in `Spec`. Reconciliation maintains `tests/Spec.hs` and the generated test suite. See test suite discovery in `neo/src/reconcile/test_suite.rs` and the Cabal generator in `neo/src/reconcile/cabal.rs`.

Execute `Decision` values before inspecting acceptance or rejection. The increment specification in `neo/starter/tests/Decider/Counter/IncrementCounterSpec.hs` supplies `DecisionContext`, calls `Decider.runDecision`, and asserts the observed result. Merely constructing a decision does not run its capabilities.

Test missing entities, rejected input, and accepted events. For state transitions, apply emitted events to the reducer and check the state consumers observe. The starter reducer in `neo/starter/src/Starter/Counter/Entity.hs` and decider in `core/service/Decider.hs` expose the required operations.

## HTTP scenarios

Put Hurl scenarios under `tests/`. The Counter flow in `neo/starter/tests/scenarios/counter-flow.hurl` creates an entity, changes it, and reads the projected result. Its query reads retry because projections update asynchronously.

The current CLI test runner in `neo/src/commands/test.rs` probes port 8080 before Hurl execution. Keep the test application's port and scenario URLs consistent with that runner. A scenario file by itself does not start a server.

For direct Haskell checks, use the project's Nix shell.

```sh
nix develop --command cabal test
```

The generated Nix template in `neo/assets/templates/flake.nix.j2` owns the project toolchain.

## Framework acceptance evidence

The repository testbed runner in `testbed/scripts/run-tests.sh` starts the testbed and runs its Hurl files. Invoke that runner within the repository's Nix development shell because the runner calls Cabal. The testbed README in `testbed/README.md` describes its prerequisites.

The stock reservation scenario in `testbed/tests/scenarios/stock-reservation.hurl` covers local command dispatch. The generic HTTP scenario in `testbed/tests/integrations/generic-http-side-effect.hurl` covers a remote request, callback, and domain result. The OAuth2 scenario in `testbed/tests/integrations/oauth2-account-connection.hurl` covers authorization, token exchange, the callback command, and disconnect against a fake external provider. The route specs in `core/test/Auth/OAuth2/RoutesSpec.hs` cover invalid or replayed state separately.

When adding a remote integration, test its actual endpoint interaction against a deterministic fake service and assert the resulting domain state. Use [side effect guidance](advanced-side-effects.md) and [OAuth2 guidance](advanced-oauth2.md) for the boundaries to exercise.
