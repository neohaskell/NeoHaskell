# Change 008: Speed up pipeline test scaffolding

Make issue-backed pipeline work visibly claimed and conventionally named, remove cold full builds from localization, and provide compiled command/event-store spec recipes that a low-reasoning agent can adapt into a trustworthy red regression test within a 2–4 minute warm-worktree target.

```yaml spec
issue: issue#862
kind: feature
touches: [dev-pipeline, testlib, commands, event-store, concurrency]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

The change adds internal pipeline tooling and test-only helpers without changing NeoHaskell's public application API.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | Issue-backed intake assigns the current GitHub viewer before work and reports assignment failures without starting the run | `scripts/pipeline-intake --self-test` "claims issue-backed work before pipeline initialization" | unit |
| C2 | Commit subjects and PR titles accept the documented scoped and unscoped Conventional Commit forms and identify each invalid value | `scripts/conventional-name-check --self-test` "validates commit subjects and PR titles" | unit |
| C3 | A missing or stale HIE index never causes `who-calls` to launch `cabal build all`, and a cold lookup returns an executable bounded fallback | `scripts/who-calls --self-test` "returns a bounded cold-index fallback without building" | unit |
| C4 | Refreshing a warm HIE index indexes existing artifacts incrementally and reuses worktree/toolchain-keyed cache state | `scripts/refresh-hiedb --self-test` "incrementally indexes existing HIE artifacts" | integration |
| C5 | The resident watcher refreshes the symbol index after a changed module compiles without blocking typecheck feedback | `scripts/watch --self-test` "keeps the HIE index warm after successful recompilation" | integration |
| C6 | Command-executor/event-store test requests route to a registered project skill whose references resolve to compiled canonical examples | `scripts/doctor` "validates command executor spec skill registration and references" | unit |
| C7 | Compiled test helpers and examples cover insertion guards, recorded payloads and revisions, first-write consistency failure with refetch, barrier-controlled PostgreSQL concurrency, stream-creation races, and unconditional appends without timing sleeps | `CommandHandler Execute Specification Tests` "provides deterministic command executor regression recipes" against PostgreSQL | integration |
| C8 | A low-reasoning routing smoke adapts each canonical recipe into a compiling test that is red at the intended assertion without unrelated source exploration | `command-executor-specs routing smoke` "scaffolds trustworthy red tests from canonical recipes" | integration |
| C9 | Telemetry records localization/indexing, test scaffolding, compilation, and test execution durations separately | `scripts/telemetry.py --self-test` "records pipeline performance substage timings" | unit |

## User impact

Contributors and agents receive visible issue ownership, consistent commit and PR naming, bounded localization latency, and reusable concurrency-test scaffolding. NeoHaskell application APIs and runtime behavior are unchanged. The intended warm-worktree benchmark for a routine spec plus compiling red regression test is 2–4 minutes.

## ADR

Not required — no trigger (breaking / new-dependency / new-capability / new-extension-point all false).
