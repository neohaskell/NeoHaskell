# Per-phase write-allowed paths

After each phase completes, the orchestrator runs `git diff --name-only HEAD` and refuses to advance the cursor if any path falls outside the phase's allow-list. This catches scope creep — e.g. a phase 10 (implementation) agent that touched a test file, or a phase 14 (fix findings) agent that fabricated an unrelated module.

This is the integration-pipeline-preview variant. Two invariants shape the table:

- **The state directory is `.integration-pipeline/`, and it is gitignored — it must never appear in a commit.** Every design artefact (design doc, architecture doc, test spec) lives there, not under `docs/`. If any `.integration-pipeline/` path shows up in `git diff --name-only HEAD` (i.e. staged/committed), the orchestrator refuses regardless of phase.
- **Integration code lives under `integrations/`, not `core/`.** The module tree is `integrations/Integration/<Name>/`, the façade shell is `integrations/Integration/<Name>.hs`, tests are under `integrations/test/`, and the package manifest is `integrations/nhintegrations.cabal`.

The orchestrator's check is best-effort and based on globs. If a diff path is ambiguous, the orchestrator surfaces it to the maintainer rather than refusing silently.

## The table

| Phase | Write-allowed paths | Notes |
|-------|--------------------|-------|
| 1 Init | `.integration-pipeline/` only (uncommitted) | Pure state-machine init. |
| 2 Classify | `.integration-pipeline/classification.json` only (uncommitted) | |
| 3 Design draft | **No committed source or doc changes** — only a single empty seed commit (`git commit --allow-empty`) plus the uncommitted `.integration-pipeline/integration-design.md` and `.integration-pipeline/draft-pr-body.md`. | Opens a draft PR via `gh` carrying the design in its **body** (the design doc is gitignored and never committed). `git diff --name-only HEAD` should be empty. |
| 4 Security review (design) | `.integration-pipeline/findings-04.json` only (uncommitted) | |
| 5 Performance review (design) | `.integration-pipeline/findings-05.json` only (uncommitted) | |
| 6 DevEx review | `.integration-pipeline/devex-review-rubric.json`, `.integration-pipeline/devex-review.md` (uncommitted) | |
| 7 Architecture design | `.integration-pipeline/integration-architecture.md`, `.integration-pipeline/architecture-rubric.json` (uncommitted) | **Never `docs/architecture/`** — the arch doc lives in `.integration-pipeline/` so the integration stays portable. |
| 8 Test spec design | `.integration-pipeline/integration-tests.md`, `.integration-pipeline/test-spec-rubric.json` (uncommitted) | **Never `docs/`.** |
| 9 Test writing | `integrations/test/` (the integration's spec files), `integrations/nhintegrations.cabal` (test-suite `other-modules` registration), plus the source-module **stubs** named in the architecture doc under `integrations/Integration/<Name>/`. | The source-module stubs are limited to the exact paths the architecture doc names. Nothing else under `integrations/Integration/` may be touched. |
| 10 Implementation | Source modules under `integrations/Integration/<Name>/` and the façade `integrations/Integration/<Name>.hs` named in the architecture doc, `integrations/nhintegrations.cabal` (`exposed-modules`). | **Never test files.** If the implementation needs a test fixture change, the agent must refuse and escalate to the maintainer. |
| 11 Build loop | Same paths as phase 10 (fix-iter is implementation work). | Includes the uncommitted `.integration-pipeline/build.log`, `.integration-pipeline/test.log`, `.integration-pipeline/test-counts.json`, `.integration-pipeline/hlint.log`. |
| 12 Security review (impl) | `.integration-pipeline/findings-12.json` only (uncommitted) | |
| 13 Performance review (impl) | `.integration-pipeline/findings-13.json` only (uncommitted) | |
| 14 Fix findings | Source modules under `integrations/Integration/<Name>/`, `integrations/nhintegrations.cabal`. Test files only on explicit maintainer authorization — default is forbidden. | The orchestrator checks for an authorization marker in the phase-14 agent prompt before allowing test-file changes. |
| 15 Final verify | `.integration-pipeline/` only (uncommitted) | |
| 16 Finalize PR | `.integration-pipeline/pr-body.md`, `.integration-pipeline/pr-title.txt` (uncommitted). Also commits the existing implementation tree without modifying source, then marks the phase-3 draft PR ready. | The submit step's `git commit` includes whatever the integration tree contains; it does not write source. |
| 17 CI cycle | Whatever fixes the CI/bot loop produces (typically small targeted edits under `integrations/Integration/<Name>/` or `integrations/test/`). | |

## How the orchestrator should consume this

After every `pipeline.py complete <N>`, the orchestrator runs:

```bash
git diff --name-only HEAD
```

…cross-references each path against the phase's row, and refuses to advance if any path is outside the allow-list. Because the design/architecture/test-spec artefacts are gitignored, most pre-implementation phases (2–8, 12–13, 15) should produce an **empty** `git diff --name-only HEAD` — anything committed there is itself a red flag. The refusal surfaces the offending paths and asks the maintainer to either (a) revert the out-of-scope changes, or (b) explicitly authorize the expansion (which the orchestrator should record in `.integration-pipeline/state.json` under a `scope_overrides` key for future audit).

## Why this matters

The pipeline's audit history showed several cases of scope creep that landed silently:

- A test-writing agent created a brand-new spec file without naming it in the test spec; the file shipped non-behavioral tests that wasted reviewer cycles.
- A build-loop agent that was supposed to drive the build loop instead added unrelated production code via a "fix iter" path.
- A fix-findings agent rewrote public function signatures without surfacing the contract change to the maintainer.
- A design-phase agent committed the design doc into the repo, defeating the portability invariant that keeps the integration extractable to a separate repo.

The allow-list above is the smallest set of paths each phase legitimately needs. If a phase genuinely needs more, the agent surfaces the proposed expansion to the maintainer instead of silently widening its scope.
