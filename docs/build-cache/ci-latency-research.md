# CI build and test latency: research and proposed experiments

Research by the primary agent at source `21f102c`, 2026-09-25. This is a proposal,
not a change to required checks or permission to skip tests. The requester wants
both compilation and test execution minimized in CI, including safe affected-work
selection. Local save-to-diagnostics is a separate target, not a replacement for
CI optimization. Root owns research, critical thinking and decisions; Luna only
executes specific instructions with explicit acceptance checks.

The follow-up [finer-grained design](finer-grained-ci-design.md) examines service
shards, PostgreSQL issue #843, basics/generic-test/service-helper layering, and
separate codemap/doctest architectures. The failed Text pilot remains rejected;
it does not rule out testing a different boundary after compile-cost profiling.

## What the repository actually does

`test.yml` has one broad `compiled` decision. For qualifying changes it builds the
whole `ci-components` bundle before six consuming jobs can start. Five Hspec suites
and Hurl/readiness fan out after that barrier. Codemap and doctest run alongside
the producer. `test-macos.yml` has one producer followed by five suite jobs.
Neither Haskell workflow currently has a concurrency group to cancel superseded
runs; checks/Neo/installer workflows already do. Draft and changed-file semantics
must remain explicit in the required aggregate gate.

The original Cabal package graph is still `nhcore -> nhintegrations -> nhtestbed`
(arrows mean dependency to consumer). Nix can order independent components, but
more GitHub jobs cannot remove this dependency chain. A monolithic nhcore change
still has broad effects. The measured Text extraction slowed representative edits,
so this research does not revive that rejected extraction.

Every consumer currently retrieves the complete project bundle, including library
outputs it does not execute. The full Linux closure is 6,019.3 MiB unpacked; one
test executable plus the unchanged shared runtime is only 380.6–397.4 MiB. A bundle
for all four non-service suites is 535.6 MiB / 90 paths. macOS differs: the full
bundle is 5,591.6 MiB; four non-service suites plus runtime are 4,400.0 MiB / 210
paths. Its nhintegrations-only closure is 736.6 MiB, while core test closures retain
much larger transitive dependencies. These are NAR sizes, not compressed transfer
sizes or predicted seconds. Never prune referenced paths to force a smaller result.

Source: publisher manifests from run36163339478, archived at
[352dbff](https://github.com/neohaskell/NeoHaskell/tree/352dbff/docs/build-cache/evidence/checkpoint-2026-09-25-public-cache-failure).
The traversal and single-suite observations are reproducible from
[f63ff9d](https://github.com/neohaskell/NeoHaskell/tree/f63ff9d/docs/build-cache/evidence/checkpoint-2026-09-25-closure-scope).

## Execution versus orchestration

Three same-worker Linux repetitions at17f9a14 give these first-pass medians:

| Workload | Seconds | Interpretation |
|---|---:|---|
| Core |3.649|Already short; not the primary execution bottleneck |
| Auth |0.558|Separate worker setup costs far more than execution |
| Core integration |8.437|Several explicit waits; profile individual examples |
| Service |38.308|Largest execution target; shared database fixtures |
| Integration package |0.634|Separate worker setup costs far more than execution |
| Hurl |9.286|Files already parallel within each of four sequential groups |
| Cold-start/readiness |7.445|Retain actual external fixture and readiness assertions |

All passes ran3,088 examples,0 failures,67 pending. The sum of the four non-service
suite medians is about13.3s; that is an illustrative grouping estimate, not a
measured grouped-job median. Existing observations and raw commands are in
[6f507c7](https://github.com/neohaskell/NeoHaskell/tree/6f507c7/docs/build-cache/evidence/checkpoint-2026-09-25-colocated).

Actual c0d34a8 Linux producer jobs lasted414/433s; component build steps356/365s.
Consumer Nix installation alone took roughly24–25s. Service execution took38–39s,
while whole service jobs took107/121s. Artifact download/import added tens of
seconds per consumer; macOS had much larger transfer overhead. Existing full-CI
observations are small-sample and some Linux jobs were contended; no causal speedup
claim follows. Timing provenance is in
[631d9c1](https://github.com/neohaskell/NeoHaskell/tree/631d9c1/docs/build-cache/evidence/checkpoint-2026-09-25-current).

Once builds/transfers improve, other jobs can become the critical path: Linux
codemap generation took209/225s (jobs253/279s), and doctest execution160/164s
(jobs203s). They must be included in end-to-end optimization. A faster service
suite alone cannot make the entire current workflow finish in a few seconds.

## Prioritized experiments

1. **Eliminate obsolete and repeated work.** Add PR-scoped cancellation of
   superseded test runs, with groups isolated by workflow and PR. Do not cancel
   unrelated PRs, required main/release runs or benchmark repetitions. Reuse signed
   component outputs through Cachix and upstream caches. Corrected retrieval run
   [36170358310](https://github.com/neohaskell/NeoHaskell/actions/runs/36170358310)
   has succeeded on both platforms; exact roots, signed retrieval logs and fresh
   core execution (1,095 examples, 0 failures, 3 pending each) are archived at
   [89c2c79](https://github.com/neohaskell/NeoHaskell/tree/89c2c79/docs/build-cache/evidence/checkpoint-2026-09-25-public-cache-success).
   This is one correctness run per platform, not a matched performance comparison.
2. **Compare narrow fanout with a hybrid grouping.** Keep service and Hurl/readiness
   on independently isolated workers. Compare four separate non-service jobs with
   one worker running those four binaries sequentially. About13s of sequential
   execution remains below service's38s, so grouping could save installs/transfers
   without lengthening the execution bottleneck. This is a hypothesis: measure
   actual critical path and runner sum, platform by platform. Use the existing
   runtime initially to isolate packaging effects. Each option gets three matched
   observations, identical sources/caches/counts, queue time reported separately.
3. **Profile then parallelize the long test groups.** Enable per-example timing
   and slow-item reporting on an experiment without changing assertions. Existing
   Hspec2.11.14 binaries support `--times`, `--print-slow-items`, `--jobs` and `--seed`
   (confirmed with a realized binary's `--help`). Target service and integration
   rather than spending effort on the already sub-second auth/package suites.
4. **Introduce affected-work planning in shadow mode.** Produce selected build
   targets/test suites and reasons, but continue running all required work. Compare
   predictions with actual derivation changes and full-suite results. Activate only
   proven coarse cases first; unknowns run the full affected surface. Selection
   must drive the producer target set too, otherwise test skipping leaves the
   compilation bill unchanged.
5. **Remove the next measured critical path.** Investigate reusable Nix-produced
   Haddock/Hoogle inputs for codemap and source/fixture-aware doctest selection.
   Preserve the current routes until equivalent outputs and failure behavior pass.
   Evaluate persistent stores/preinstalled runner images after transfer accounting;
   keep untrusted forks isolated. No new host/provider is provisioned by this plan.

## Safe test parallelism in this codebase

`-threaded` and `-with-rtsopts=-N` are already enabled. They do not mark all Hspec
examples parallel: Hspec requires an explicit `parallel` annotation. Fourteen
spec roots under core/test already opt in, while inspected service/testlib roots
largely do not. The NeoHaskell `Test.Spec.parallel` wrapper already exists; preserve
the dialect if adding annotations. Tune worker count against CPU/memory and the
RTS capability count instead of making every process use every CPU.

A blanket `parallel` around the service tree is unsafe. For example,
`core/test/Service/FileUpload/FileStateStore/PostgresSpec.hs` drops the shared
`file_upload_state` table during setup. PostgreSQL fixtures commonly hardcode
localhost:5432 and database neohaskell. Multiple service processes need real
isolation (separate database/server/schema as appropriate, configurable fixtures,
ports, temporary/upload directories), not just distinct Hspec filters. Some tests
also count connections or test ordering; running unrelated work concurrently can
change what those assertions observe.

Prefer explicitly independent in-process examples first. If sharding is warranted,
use measured durations to balance stable spec groups, with isolated fixtures and a
count/inventory check proving the shard union covers every original example once.
Merge service reports so runtime criteria continue to see their real fixtures.
A deliberately failing shard must fail the job and aggregate gate.

Fixed waits are another target. `core/test/Integration/DispatcherSpec.hs` waits2s
for100 events before verifying the count. An observed-completion wait with the
same bounded timeout may finish promptly while preserving the assertion. It needs
a failing/missing-event regression proof. Do not simply shorten sleeps: other
waits are the behavior under test, such as the1s rebuild timeout in ReadinessSpec
and listener initialization cancellation in NotificationsSpec. Keep real timeout
coverage; consider injected clocks only where a valid seam exists and real runtime
coverage remains. Existing expectations are not authorized to change.

Hurl7.0.0's local help confirms `--test` already uses parallel execution. The four
groups are sequential in `testbed/scripts/run-tests.sh`; combining independent
groups could help, but first check shared entities/files/server state. The cold-start
script must not contend with another testbed on its fixed port. Hurl's own request
worker isolation does not isolate the application/database they access.

## Affected-work selection: what can actually be skipped

Start with explicit Cabal ownership and downstream dependencies, not directory
names or a guessed list of related tests:

| Change, with all other inputs unchanged | Initial conservative selection |
|---|---|
| Ordinary documentation outside executable examples/build inputs | Existing light checks; verify classification |
| Hurl file/data only | Required acceptance group(s) and fixture/report checks; reuse unchanged executable |
| Test module owned by one executable | That executable and actual shared-helper consumers |
| nhintegrations implementation | Its build/tests and downstream testbed acceptance; nhcore is upstream |
| testbed implementation | Testbed build/acceptance and relevant examples/docs checks |
| nhcore implementation or shared core/testlib | Broad downstream closure, initially all Haskell suites |
| Lock/compiler/Cabal/Nix flags, planner, runner or unknown dependency | Full relevant platform verification |

Important repository-specific limits:

- `core/testlib` is part of the **nhcore library's exposed modules/source dirs**.
  Calling it test-only and skipping downstream work would be wrong.
- `core/test` is shared by the split test executables. Use declared `other-modules`
  and imports, not `core/test-service/` alone, to establish ownership.
- `codemap/capabilities.yaml` and `spec-check`'s `test_impact_globs` are useful
  localization hints, not a complete transitive dependency proof. For example,
  primitive/HTTP changes can affect downstream services/integrations beyond the
  directly listed core test globs. This research does not invoke a skill/pipeline.
- Public Core facade imports and Template Haskell complicate module-level precision;
  embedded files (`Text` exposes file embedding), generated code, orphan instances,
  rules, build flags, runtime fixtures and scripts also matter. Build a conservative
  graph from actual compiler/Cabal evidence; use both old/new graphs for deletions
  and renames. Unknown edges mean wider selection, never a green omission.
- An unchanged test executable alone is insufficient for skipping execution.
  Runtime data, PG image/configuration, scripts, environment and the test command
  must also be covered. Nix build reuse and reuse of a prior test result are distinct.

A proposed planner emits a versioned manifest: base/head, platform/toolchain,
changed inputs, selected targets, reasons, and explicit unaffected decisions.
The aggregate gate must require every selected job, reject missing/unknown/canceled
results, and allow an unaffected decision only from a successful validated planner.
No cache hit should be reported as fresh test execution. If reusing previous test
results later, require trusted successful provenance for the full execution-input
key; external/time-dependent integration tests stay outside that shortcut until
hermeticity is demonstrated. Full verification stays in place during shadow mode,
with periodic full audits after any eventual adoption.

Validate planning with throwaway mutations: implementation, test-only, fixture,
shared helper, dependency/flag, renamed/deleted module and unknown file; inject a
failure in each affected surface. Check both selection and build invalidation,
plus behavior when the graph/base/result evidence is missing. Keep the full Cabal
workspace and all required behavior; no silent policy change in this research.

## Targets and evidence standard

Track time to first failure, total required-check elapsed time, aggregate runner
seconds, per-example execution, build/evaluation/download/upload, and queue/setup
separately. Optimize critical elapsed time without hiding increased cost or flaky
retries. Three matched repetitions are the minimum for a performance decision.
A warm no-op/affected-plan result may approach sub-second on a persistent process;
ordinary hosted runner startup and tests that deliberately wait a second cannot.
The aim for CI is the smallest correctly selected work executed with useful
parallelism, rather than promising sub-second full fresh verification.

## Primary references checked

- [Hspec parallel semantics](https://hspec.github.io/parallel-spec-execution.html)
  and [runner/timing options](https://hspec.github.io/options.html); relevant flags
  independently verified on the repository's realized2.11.14 test binary.
- [Hurl execution options](https://hurl.dev/docs/manual.html); behavior also checked
  with the pinned7.0.0 binary, since current docs can differ from pinned defaults.
- [GitHub concurrency groups/cancellation](https://docs.github.com/en/actions/how-tos/write-workflows/choose-when-workflows-run/control-workflow-concurrency).
- [haskell.nix component model](https://input-output-hk.github.io/haskell.nix/reference/library.html);
  existing pinned component attributes are already exercised by this PR. No new
  unverified current-documentation API is introduced here.
