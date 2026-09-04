---
name: neohaskell-event-store-regression-specs
description: Scaffold compiling red CommandExecutor and event-store regression specs from exact project examples. Use for insertion-type guards, recorded appends or fetch revisions, scripted consistency conflicts and refetches, barrier-controlled concurrent appends, StreamCreation races, or AnyStreamState behavior.
---

# Command/event-store regression specs

Create one compiling test whose new expectation is red for the requested behavior. This skill is the routing source: select one row, read only its canonical example and `core/testlib/Test/Service/EventStore/Regression.hs`, then copy-adapt the named test.

## Recipe routing

| Recipe | Use when | Smoke backend | Smoke examples | Canonical file | Canonical test | Helpers |
|---|---|---|---|---|---|---|
| `insertion-guard` | Require one exact `InsertionType` at the store boundary | `in-memory` | `2` | `core/testlib/Test/Service/CommandHandler/Execute/Spec.hs` | `canonically records a consistency conflict, refetch, and re-decision` | `seedStream`, `requireInsertionType`, `recordInsertions` |
| `record-payloads-and-revisions` | Observe attempted payloads and fetched local revisions | `in-memory` | `2` | `core/testlib/Test/Service/CommandHandler/Execute/Spec.hs` | `canonically records a consistency conflict, refetch, and re-decision` | `recordInsertions`, `recordFetchedRevisions` |
| `consistency-conflict-refetch` | Fail the first append and prove refetch plus re-decision | `in-memory` | `2` | `core/testlib/Test/Service/CommandHandler/Execute/Spec.hs` | `canonically records a consistency conflict, refetch, and re-decision` | `failFirstWithConsistencyConflict`, `recordFetchedRevisions`, `recordInsertions` |
| `postgres-append-barrier` | Hold two PostgreSQL appends at the insert boundary without sleeps | `postgres` | `3` | `core/testlib/Test/Service/EventStore/OptimisticConcurrency/Spec.hs` | `will only allow one event to be appended, when two writers try to append at the same time` | `newInsertBarrier`, `barrierBeforeInsert`, `awaitInsertions`, `releaseInsertions` |
| `stream-creation-race` | Race two PostgreSQL `StreamCreation` decisions | `postgres` | `3` | `core/testlib/Test/Service/EventStore/OptimisticConcurrency/Spec.hs` | `allows only one concurrent StreamCreation` | `newInsertBarrier`, `barrierBeforeInsert`, `awaitInsertions`, `releaseInsertions` |
| `any-stream-state` | Prove unconditional `AnyStreamState` behavior | `in-memory` | `3` | `core/testlib/Test/Service/EventStore/OptimisticConcurrency/Spec.hs` | `keeps AnyStreamState deliberately unconditional` | `seedStream` |

## Procedure

1. Choose the single row matching the request. Read its canonical file only far enough to copy the named `it` block, plus the compiled helper module. Do not inspect unrelated source files.
2. Copy-adapt that block in the owning spec. Preserve every existing expectation.
3. Add one expectation naming the requested regression. Setup failures do not count: the unchanged production code must reach and fail the new expectation.
4. Run `./dev check lib:nhcore`, then `./dev test "<canonical test>" nhcore-test-service`. PostgreSQL rows require `POSTGRES_AVAILABLE=true`. Record compilation success, the row's exact example count, exactly one failure, and its intended sentinel.
5. Hand production changes to `neohaskell-implementer`. It may repair setup but must keep the new expectation unchanged.
6. Ask a fresh reviewer to confirm that unchanged production code goes red at the new expectation.

## Mechanical smoke

Run `./dev regression-smoke`. Each canonical test runs across every registered backend, but only the row's smoke backend emits `INTENTIONAL_RED:<recipe>:<backend>`. The smoke requires the exact total example count and exactly one failure, so an unavailable or broken PostgreSQL setup adds a failure or loses the sentinel instead of hiding behind another backend. `./dev regression-smoke --self-test` validates routing without building.

A scaffold is complete only when it compiles, goes red at its new expectation, and reads or changes no unrelated source file.
