# Per-phase write-allowed paths

After each phase completes, the orchestrator runs `git diff --name-only HEAD` and refuses to advance the cursor if any path falls outside the phase's allow-list. This catches scope creep — e.g. a phase 10 (implementation) agent that touched a test file, or a phase 14 (fix findings) agent that fabricated an unrelated module.

The orchestrator's check is best-effort and based on globs. If a diff path is ambiguous, the orchestrator surfaces it to the maintainer rather than refusing silently.

## The table

| Phase | Write-allowed paths | Notes |
|-------|--------------------|-------|
| 1 Init | `.pipeline/` only | Pure state-machine init. |
| 2 Classify | `.pipeline/classification.json` only | |
| 3 ADR draft | `docs/decisions/NNNN-slug.md`, `.pipeline/` | Commits the ADR and opens a draft PR via `gh`; no source changes. |
| 4 Security review (ADR) | `.pipeline/findings-04.json` only | |
| 5 Performance review (ADR) | `.pipeline/findings-05.json` only | |
| 6 DevEx review | `.pipeline/devex-review-rubric.json`, `.pipeline/devex-review.md` | |
| 7 Architecture design | `docs/architecture/NNNN-slug.md`, `.pipeline/architecture-rubric.json` | |
| 8 Test spec design | `docs/architecture/NNNN-slug-tests.md`, `.pipeline/test-spec-rubric.json` | |
| 9 Test writing | `core/test/`, `core/test-core/`, `core/test-auth/`, `core/test-service/`, `core/test-integration/`, `testbed/`, `core/nhcore.cabal` (other-modules registration), plus the source-module **stubs** named in the architecture doc under `core/service/` or `core/concurrency/`. | nhcore-test-auth uses `hs-source-dirs: test-auth, test`; nhcore-test-core uses `test-core, test`; nhcore-test-integration uses `test-integration, test`. All listed roots are legitimate phase-9 destinations. The source-module stubs are limited to the exact paths the architecture doc names. Nothing else under `core/service/` or `core/concurrency/` may be touched. |
| 10 Implementation | Source modules under `core/service/` named in the architecture doc, `core/nhcore.cabal` (exposed-modules), `integrations/` (if the architecture doc names it). | **Never test files.** If the implementation needs a test fixture change, the agent must refuse and escalate to the maintainer. |
| 11 Build loop | Same paths as phase 10 (fix-iter is implementation work). | Includes `.pipeline/build.log`, `.pipeline/test.log`, `.pipeline/test-counts.json`, `.pipeline/hlint.log`. |
| 12 Security review (impl) | `.pipeline/findings-12.json` only | |
| 13 Performance review (impl) | `.pipeline/findings-13.json` only | |
| 14 Fix findings | Source modules under `core/service/`, `integrations/`, `core/nhcore.cabal`. Test files only on explicit maintainer authorization — default is forbidden. | The orchestrator checks for an authorization marker in the phase-14 agent prompt before allowing test-file changes. |
| 15 Final verify | `.pipeline/` only | |
| 16 Finalize PR | `.pipeline/pr-body.md`, `.pipeline/pr-title.txt`. Also commits the existing tree without modifying source, then marks the phase-3 draft PR ready. | The submit step's `git commit` includes whatever the tree contains; it does not write source. |
| 17 Opus PR review | Step 1: `.pipeline/findings-17.json` only. Step 2 (after approve): whatever paths the accepted findings name. | Per-finding fix scope is enforced by the `proposed_fix.file` field. |
| 18 CI cycle | Whatever fixes the CI/bot loop produces (typically small targeted edits to `core/service/` or `core/test*/`). | |

## How the orchestrator should consume this

After every `pipeline.py complete <N>`, the orchestrator runs:

```bash
git diff --name-only HEAD
```

…cross-references each path against the phase's row, and refuses to advance if any path is outside the allow-list. The refusal surfaces the offending paths and asks the maintainer to either (a) revert the out-of-scope changes, or (b) explicitly authorize the expansion (which the orchestrator should record in `.pipeline/state.json` under a `scope_overrides` key for future audit).

## Why this matters

The pipeline's audit history showed several cases of scope creep that landed silently:

- A phase 9 agent created `ReadinessBuilderSpec.hs` (a brand-new test file) without naming it in the test spec; the file shipped 8 non-behavioral tests that wasted reviewer cycles.
- A phase 11 agent that was supposed to drive the build loop instead added a `DROP TABLE` to production code via a "fix iter" path.
- A phase 14 agent rewrote public function signatures without surfacing the contract change to the maintainer.

The allow-list above is the smallest set of paths each phase legitimately needs. If a phase genuinely needs more, the agent surfaces the proposed expansion to the maintainer instead of silently widening its scope.
