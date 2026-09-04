---
name: neohaskell-event-store-regression-specs
description: Scaffold compiling red CommandExecutor and event-store regression specs from the project test helpers. Use for insertion-type guards, recorded append or fetch observations, scripted consistency conflicts and refetches, barrier-controlled concurrent appends, StreamCreation races, or AnyStreamState behavior.
---

# Command/event-store regression specs

Produce one compiling test whose new expectation is red for the requested behavior. Localization is the [recipe catalog](routing-smoke.yaml), not a source-tree search.

## Procedure

1. Select exactly one recipe from `routing-smoke.yaml` and read only its `canonical_example` plus `core/testlib/Test/Service/EventStore/Regression.hs`.
2. Copy-adapt the canonical `it` block in its existing spec module. Use the listed helpers; preserve every existing expectation.
3. Add one expectation naming the regression. Setup failures are invalid: the targeted run must reach and fail that expectation.
4. Run the recipe's `compile_command`, then `test_command`. Record compilation success and the exact intended assertion failure.
5. Hand production changes to `neohaskell-implementer`. That agent may repair setup, but keeps the new expectation unchanged.
6. A fresh reviewer checks the test against the original production code and confirms the intended assertion is what goes red.

## Helper contract

`Test.Service.EventStore.Regression` is compiled with `nhcore` and provides:

- `seedStream` — create a stream from domain events.
- `recordInsertions` — wrap a store and read every attempted payload.
- `recordFetchedRevisions` — wrap a fetcher and read fetched local revisions.
- `requireInsertionType` — reject at the store boundary unless the exact type arrives.
- `failFirstWithConsistencyConflict` — deterministically fail the first append, then delegate.
- `newInsertBarrier`, `barrierBeforeInsert`, `awaitInsertions`, `releaseInsertions` — rendezvous concurrent appends without timing sleeps.

A scaffold is complete only when it compiles, goes red at its new expectation, and touches no unrelated source file.
