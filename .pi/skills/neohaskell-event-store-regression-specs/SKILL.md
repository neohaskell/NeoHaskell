---
name: neohaskell-event-store-regression-specs
description: Scaffold compiling CommandExecutor and event-store regression specs from six mutation-proved canonical examples. Use for insertion guards, observed payloads and revisions, consistency-conflict retries, PostgreSQL append races, StreamCreation races, or AnyStreamState behavior.
---

# Command/event-store regression specs

Select exactly one route below. Copy only its named canonical test and the listed compiled helpers. Each canonical test passes against the real implementation and fails at its named semantic assertion against a committed controlled mutation; unconditional failures and sentinel-only tests do not qualify.

## Recipe routing

| Recipe | Representative prompt | Smoke backend | Smoke examples | Locator | Known-bad behavior | Expected assertion | Helpers |
|---|---|---|---|---|---|---|---|
| `insertion-guard` | Require StreamCreation and show the exact rejected insertion type | `in-memory` | `2` | `hspec:nhcore-test-service:core/testlib/Test/Service/CommandHandler/Execute/Spec.hs#rejects an unexpected insertion type with exact guard evidence` | bypass the insertion guard | `REGRESSION_ASSERT:insertion-guard` | `requireInsertionType`, `knownBad`, `assertBehavior` |
| `record-payloads-and-revisions` | Record the complete append payload and exact fetched revision | `in-memory` | `2` | `hspec:nhcore-test-service:core/testlib/Test/Service/CommandHandler/Execute/Spec.hs#records exact insertion payloads and fetched revisions` | bypass both recording wrappers | `REGRESSION_ASSERT:record-payloads-and-revisions` | `seedStream`, `recordInsertions`, `recordFetchedRevisions`, `knownBad`, `assertBehavior` |
| `consistency-conflict-refetch` | Prove a conflict refetches, re-decides, drops the stale payload, and appends after the fresh revision | `in-memory` | `2` | `hspec:nhcore-test-service:core/testlib/Test/Service/CommandHandler/Execute/Spec.hs#records the exact insertion precondition after refetch` | submit the stale revision on the second append attempt | `REGRESSION_ASSERT:consistency-conflict-refetch` | `seedStream`, `failFirstWithConsistencyConflict`, `recordFetches`, `recordFetchedRevisions`, `recordInsertions`, `knownBad`, `assertBehavior` |
| `postgres-append-barrier` | Coordinate two PostgreSQL appends at the same expected position without sleeps | `postgres` | `3` | `hspec:nhcore-test-service:core/testlib/Test/Service/EventStore/OptimisticConcurrency/Spec.hs#will only allow one event to be appended, when two writers try to append at the same time` | make both non-colliding writes unconditional | `REGRESSION_ASSERT:postgres-append-barrier` | `newInsertBarrier`, `barrierBeforeInsert`, `awaitInsertions`, `releaseInsertions`, `knownBad`, `assertBehavior` |
| `stream-creation-race` | Race two StreamCreation appends and verify one durable creation fact | `postgres` | `3` | `hspec:nhcore-test-service:core/testlib/Test/Service/EventStore/OptimisticConcurrency/Spec.hs#allows only one concurrent StreamCreation and persists one creation fact` | turn both creations into unconditional appends | `REGRESSION_ASSERT:stream-creation-race` | `newInsertBarrier`, `barrierBeforeInsert`, `awaitInsertions`, `releaseInsertions`, `knownBad`, `assertBehavior` |
| `any-stream-state` | Prove two stale concurrent AnyStreamState appends both persist in durable order | `in-memory` | `3` | `hspec:nhcore-test-service:core/testlib/Test/Service/EventStore/OptimisticConcurrency/Spec.hs#persists both AnyStreamState events in durable order` | impose a stale InsertAfter precondition on one append | `REGRESSION_ASSERT:any-stream-state` | `seedStream`, `newInsertBarrier`, `barrierBeforeInsert`, `awaitInsertions`, `releaseInsertions`, `knownBad`, `assertBehavior` |

## Procedure

1. Match the request to one representative prompt and choose that row; do not combine routes.
2. Read only the locator's exact test block and the listed helpers. Copy-adapt the complete semantic setup and assertions.
3. Write the new expectation first. It must name a plausible wrong implementation and compare exact observed values; wildcard constructors do not prove the contract.
4. Run the locator through `./dev test "<exact-match>" <suite>` and record the selected example count.
5. Demonstrate the unchanged or controlled known-bad behavior fails exactly at `REGRESSION_ASSERT:<recipe>`, then demonstrate the known-good behavior passes the same test.
6. PostgreSQL routes require `POSTGRES_AVAILABLE=true`; every concurrent path uses a barrier and a bounded runner, never timing sleeps.

## Mechanical smoke

Run `./dev regression-smoke`. It executes every locator twice: known-good must compile and pass; the row's controlled known-bad behavior must compile and fail once at the named semantic assertion. `./dev regression-smoke --self-test` also proves the six representative prompts route to distinct recipes and rejects zero-match, compile, setup, timeout, unconditional-failure, and wrong-assertion transcripts.
