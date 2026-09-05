# Change 008: Harden pipeline evidence and regression smoke

Make PR #863 fail closed at every evidence boundary: structured proving-test locators, an authenticated Gate 1 red-reproduction receipt, semantically independent regression recipes with known-good/known-bad discrimination, fresh-base intake, bounded HIE fallbacks, comparable workflow timing, and telemetry that preserves retries and interrupted work.

```yaml spec
issue: issue#862
kind: feature
touches: [dev-pipeline, testlib]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

The test helper surface remains additive. Pipeline command formats are internal repository tooling contracts and are proved by their script locators below.

```diff signatures
+ Test.Service.EventStore.Regression: awaitInsertions :: Int -> InsertBarrier -> Task error Unit
+ Test.Service.EventStore.Regression: barrierBeforeInsert :: InsertBarrier -> EventStore event -> EventStore event
+ Test.Service.EventStore.Regression: data InsertBarrier
+ Test.Service.EventStore.Regression: failFirstWithConsistencyConflict :: EventStore event -> Task error (EventStore event)
+ Test.Service.EventStore.Regression: newInsertBarrier :: Task error InsertBarrier
+ Test.Service.EventStore.Regression: recordFetchedRevisions :: forall k state (event :: k) error. EntityFetcher state event -> Task error (EntityFetcher state event, Task error (Array (Maybe StreamPosition)))
+ Test.Service.EventStore.Regression: recordFetches :: forall k state (event :: k) error. EntityFetcher state event -> Task error (EntityFetcher state event, Task error (Array (EntityFetchResult state)))
+ Test.Service.EventStore.Regression: recordInsertions :: EventStore event -> Task error (EventStore event, Task error (Array (InsertionPayload event)))
+ Test.Service.EventStore.Regression: releaseInsertions :: Int -> InsertBarrier -> Task error Unit
+ Test.Service.EventStore.Regression: requireInsertionType :: InsertionType -> EventStore event -> EventStore event
+ Test.Service.EventStore.Regression: seedStream :: EventStore event -> EntityName -> StreamId -> Array event -> Task Text Unit
```

## Criteria

Every proving-test cell contains only typed locators. Multiple locators are independent exact proofs, never free-form aliases.

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Specs reject malformed, absent, unsupported, duplicate, ambiguous, unresolved, zero-match, wrong-suite, and level-incompatible proving-test locators | `script:scripts/spec-check#--self-test` | unit | none |
| C2 | A bug cannot receive Gate 1 approval or advance without a current C1 expected-red receipt bound to exact spec/test blobs, fetched base SHA, tested HEAD, locator, selected example, and failure identity | `script:scripts/pipeline-state#--self-test` | unit | none |
| C3 | Six independent canonical regression examples assert exact state, revision, insertion precondition, stale-payload non-persistence, and durable concurrency contents; PostgreSQL claims execute against PostgreSQL | `hspec:nhcore-test-service:core/testlib/Test/Service/CommandHandler/Execute/Spec.hs#records the exact insertion precondition after refetch`<br>`hspec:nhcore-test-service:core/testlib/Test/Service/EventStore/OptimisticConcurrency/Spec.hs#persists both AnyStreamState events in durable order` | integration | postgres:real |
| C4 | Regression smoke runs each committed known-good fixture green and its controlled known-bad mutation red at the named semantic assertion; sentinel-only, setup, compile, timeout, and zero-match failures are rejected; representative prompts route to all six recipes | `script:scripts/regression-scaffold-smoke#--self-test` | unit | none |
| C5 | Acceptance, integration, and unit levels are checked against exact Hurl paths or registered suite/fixture boundary metadata rather than title words | `script:scripts/spec-check#--self-test` | unit | none |
| C6 | Intake assigns and verifies the viewer, fetches and records the owning remote base SHA, checks ancestry and live PR identity, reports every Conventional Commit offender, and atomically rejects duplicate change IDs across worktrees | `script:scripts/pipeline-state#--self-test` | unit | none |
| C7 | Cold, stale, corrupt, busy, and timed-out HIE states never build implicitly and always produce a bounded executable fallback; watcher refresh follows only a new successful reload | `script:scripts/refresh-hiedb#--self-test`<br>`script:scripts/who-calls#--self-test`<br>`script:scripts/watch#--self-test` | unit | none |
| C8 | One versioned benchmark protocol runs the same fixed regression-scaffolding task for baseline and candidate under declared cold/warm prerequisites, emits machine-readable samples plus median/p95, and gates the 240-second warm threshold without discarding correctness failures as noise | `script:scripts/pipeline-benchmark#--self-test` | unit | none |
| C9 | Telemetry v6 preserves repeated and interrupted attempts, excludes human waits mechanically, distinguishes missing from measured-zero intervals, reads v4/v5 records, and never double-counts nested or retried activity | `script:scripts/telemetry.py#--self-test`<br>`script:scripts/retrospect#--self-test` | unit | none |

## User impact

No public application API breaks. Contributors get earlier, deterministic failures instead of false-green specs, red tests, stale localization data, or incomparable timing claims. The new benchmark is local/nightly rather than PR-blocking because shared-runner wall-clock noise is not deterministic.

## ADR

Not required — no trigger. This tightens the existing pipeline and test helper contracts without a breaking API, dependency, capability, or extension point.
