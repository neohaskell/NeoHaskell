> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: integration-pipeline-preview
description: Use when implementing a new outbound NeoHaskell integration end-to-end — from design draft through PR merge — and the work needs the structured 17-phase pipeline with design, security, and performance reviews (each grounded against integration complexity and the Jess persona), outside-in test design, build loop, and PR/CI handling. The integration design doc is intentionally NOT stored in `docs/decisions/` because outbound integrations are expected to move to separate repos. Triggers on 'run integration pipeline', 'integration pipeline', 'start integration pipeline', 'implement integration', 'new NeoHaskell outbound integration', 'add integration to nhintegrations', 'add outbound integration', 'wire up Stripe integration', 'integration pipeline init', 'next integration pipeline phase', 'approve integration phase N'.
kind: process
executor: opus
model: claude-opus-4-7
---

# NeoHaskell outbound-integration pipeline

Coordinates the 17-phase pipeline for landing a NeoHaskell outbound integration under `integrations/Integration/<Name>/`. State machine lives in `scripts/pipeline.py`; each phase dispatches to a child node.

This pipeline is the integration twin of `feature-pipeline-preview`. The structure is intentionally near-identical so the patterns are familiar — the differences are:

- **No ADR file, and the design is never committed.** Phase 3 writes the design doc to `.integration-pipeline/integration-design.md` (and later phases write `integration-architecture.md` and `integration-tests.md` to the same directory). Outbound integrations are scoped to be portable: the team may extract them to separate repos at any point, so an ADR slot in `docs/decisions/` would orphan. Repo-level ADRs are reserved for repo-level architecture. Because `.integration-pipeline/` is gitignored, phase 3's draft PR carries the design **in the PR body** (seeded by one empty commit a squash-merge erases) rather than as a committed file — the feature pipeline commits its ADR; this one does not.
- **Different code home.** Phase 10 places implementation under `integrations/Integration/<Name>/` (the `nhintegrations` cabal package), not `core/`. Phase 11 runs the same `cabal build all` / `cabal test` against that package.
- **Different state dir.** `.integration-pipeline/` instead of `.pipeline/`, so an integration pipeline can run alongside a feature pipeline without state collisions.

The two distinguishing characteristics from the feature pipeline carry over unchanged:

- **Grounding loop** — after every security and performance deep-audit (phases 4, 5, 12, 13), a grounding pass filters findings through the integration's complexity tier and Jess's 15-minute pocket so simple integrations do not accumulate cascades of unneeded caching, SHA pins, constant-time compares, or `INLINE` pragmas. See [`references/grounding-loop.md`](./references/grounding-loop.md).
- **Quality-rubric review-gates** — phases 6 (DevEx), 7 (architecture), 8 (test spec) each run a produce step followed by an independent review-quality step that validates the artefact against a per-phase rubric ([`devex-rubric.md`](./references/devex-rubric.md), [`architecture-rubric.md`](./references/architecture-rubric.md), [`test-spec-rubric.md`](./references/test-spec-rubric.md)). The pipeline advances automatically on `RUBRIC: pass` and halts with a structured rubric record on `RUBRIC: fail`.

## Shared invariants

- All state lives in `.integration-pipeline/state.json` and `.integration-pipeline/findings-*.json`. Drive via `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py <cmd>`.
- The pipeline must be initialised before any phase runs. Phase 1 calls `pipeline.py init`.
- PAUSE-gated phases (marked 🔒 below) stop the orchestrator until the maintainer runs `pipeline.py approve <N>`. PAUSE remains on phases 3 (design draft — opens a draft PR for review), 16 (PR finalize — marks the draft ready), and 17 (CI cycle / merge) — every other phase auto-gates on its rubric or findings.
- **No design artefact is ever written to `docs/decisions/` or `docs/architecture/`, and none is ever committed.** The design doc, architecture doc, and test spec all live in the gitignored `.integration-pipeline/`. Refuse any maintainer request to land them under `docs/` or to commit them into the tree — phase 3's design travels in the draft PR body, not in a commit.
- Every step reads [`references/jess-persona.md`](./references/jess-persona.md) and [`references/nhcore-context.md`](./references/nhcore-context.md) when delegating to a sonnet child.
- The grounding pass is mandatory — it cannot be skipped to "save time", and it cannot be merged with the deep-audit step.
- The review-quality steps on phases 6/7/8 are mandatory and must be run by a different agent invocation from the producer step (independence is what makes the gate trustworthy).
- **Trust-but-verify after every leaf.** When any leaf calls `pipeline.py complete <N>`, the orchestrator MUST independently re-verify the leaf's claimed outputs before advancing the cursor. For build/test-gated phases (11, 15), re-read `.integration-pipeline/test.log` and confirm `0 failures` across all suites. For findings-gated phases (4, 5, 12, 13), re-read the findings JSON and confirm the leaf's blocker-count claim. For fix-findings (14), confirm `blockers == 0` across both findings files. Canonical implementation: [`scripts/verify-leaf.py`](./scripts/verify-leaf.py) — invoke as `python3 .claude/skills/integration-pipeline-preview/scripts/verify-leaf.py <N>`. If the verify fails, refuse to advance and surface the mismatch.
- **Diff-shape policing.** After each phase, run `git diff --name-only HEAD` and check the changed paths against [`references/phase-allowed-paths.md`](./references/phase-allowed-paths.md). Any path outside the phase's allow-list refuses `complete <N>` and surfaces the offending paths. Because design/architecture/test-spec artefacts are gitignored, most pre-implementation phases should produce an **empty** diff — a committed `.integration-pipeline/` path is itself a red flag.

## Steps

1. **Init pipeline** — read `./01-init-pipeline/SKILL.md` and follow it with `{integration_name, issue_number, module_name, module_path, test_path, branch_name}`. Verify: `.integration-pipeline/state.json` exists and `pipeline.py status` lists phase 1 as completed.
2. **Classify integration** — spawn an Agent (model: haiku) and instruct it to read `./02-classify-integration/SKILL.md` and follow it. Verify: `.integration-pipeline/classification.json` exists with `tier ∈ {trivial, simple, moderate, complex, security-critical}`.
3. **Design draft 🔒** — spawn an Agent (model: opus) and instruct it to read `./03-design-draft/SKILL.md` and follow it. The leaf writes the design doc, opens a **draft PR** carrying the design in its body, then calls `pipeline.py complete 3`. Verify: `.integration-pipeline/integration-design.md` exists with Status: Proposed, `pipeline.py get pr_url` returns a URL, and `pipeline.py status` lists phase 3 as `needs approval`. Then stop until the maintainer reviews the design in the draft PR and runs `pipeline.py approve 3`.
4. **Security review (design)** — spawn an Agent (model: haiku) and instruct it to read `./04-security-design/SKILL.md` and follow it. Verify: `.integration-pipeline/findings-04.json` exists with `blockers >= 0`. If `blockers > 0`, stop and surface to maintainer.
5. **Performance review (design)** — spawn an Agent (model: haiku) and instruct it to read `./05-performance-design/SKILL.md` and follow it. Verify: `.integration-pipeline/findings-05.json` exists. May run in parallel with step 4.
6. **DevEx review** — spawn an Agent (model: haiku) and instruct it to read `./06-devex-review/SKILL.md` and follow it. Verify: `.integration-pipeline/devex-review-rubric.json` exists with `"verdict": "pass"`. On `fail`, the rubric record names the failing checks and the pipeline halts here.
7. **Architecture design** — spawn an Agent (model: haiku) and instruct it to read `./07-architecture-design/SKILL.md` and follow it. Verify: `.integration-pipeline/architecture-rubric.json` exists with `"verdict": "pass"`. On `fail`, the rubric record names the failing checks and the pipeline halts.
8. **Test spec design** — spawn an Agent (model: haiku) and instruct it to read `./08-test-spec-design/SKILL.md` and follow it. Verify: `.integration-pipeline/test-spec-rubric.json` exists with `"verdict": "pass"`. On `fail`, the rubric record names the failing checks and the pipeline halts.
9. **Test writing** — spawn an Agent (model: sonnet) and instruct it to read `./09-test-writing/SKILL.md` and follow it. Verify: tests compile and ALL fail.
10. **Implementation** — spawn an Agent (model: sonnet) and instruct it to read `./10-implementation/SKILL.md` and follow it. Verify: source files exist at the planned paths under `integrations/Integration/<Name>/`.
11. **Build loop** — spawn an Agent (model: haiku) and instruct it to read `./11-build-loop/SKILL.md` and follow it. Verify: `cabal build all` and `cabal test` pass; `.integration-pipeline/hlint.log` exists (warnings are captured for the PR body, not gated here).
12. **Security review (impl)** — spawn an Agent (model: haiku) and instruct it to read `./12-security-impl/SKILL.md` and follow it. Verify: `.integration-pipeline/findings-12.json` exists.
13. **Performance review (impl)** — spawn an Agent (model: haiku) and instruct it to read `./13-performance-impl/SKILL.md` and follow it. Verify: `.integration-pipeline/findings-13.json` exists. May run in parallel with step 12.
14. **Fix findings** — spawn an Agent (model: sonnet) and instruct it to read `./14-fix-findings/SKILL.md` and follow it. Verify: `blockers == 0` across findings-12 and findings-13 after fixes; `cabal test` still green.
15. **Final verify** — spawn an Agent (model: haiku) and instruct it to read `./15-final-verify/SKILL.md` and follow it. Verify: clean build, all tests pass; `.integration-pipeline/hlint.log` refreshed (hlint is captured for the PR body, not gated).
16. **Finalize PR (mark ready) 🔒** — spawn an Agent (model: haiku) and instruct it to read `./16-create-pr/SKILL.md` and follow it. It commits/pushes the implementation, sets the final title/body, and converts the **draft PR opened in phase 3** to ready for review (`gh pr ready`) — it does NOT create a second PR. Verify: `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py get pr_url` returns the draft PR URL and `gh pr view --json isDraft -q .isDraft` is `false`. The submit leaf calls `pipeline.py complete 16` itself; then stop until the maintainer runs `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py approve 16`.
17. **CI cycle** — spawn an Agent (model: haiku) and instruct it to read `./17-ci-cycle/SKILL.md` and follow it. Verify: CI green and PR merged.

Phase completion is owned by each phase's own skill — the orchestrator never calls `pipeline.py complete <N>` unconditionally. Rubric-gated phases (6, 7, 8) and the script-style record leaves (2, 4, 5, 12, 13) call `complete <N>` only when the gate passes; the test-writing, implementation, build-loop, fix-findings, final-verify, and create-PR phases call it from their last step on success. PAUSE-gated phases (3, 16, 17) then stop and wait for the maintainer's `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py approve <N>` before continuing.

## Refusals

- `.integration-pipeline/state.json` already exists with a different integration → refuse and ask the maintainer to run `pipeline.py reset` or pick the right state.
- A phase's verify check fails → stop, surface the failure, do not advance the cursor.
- A grounding pass is missing from a security/perf phase → refuse to mark the phase complete.
- The maintainer asks to skip the grounding pass → refuse; explain that the grounding pass is the load-bearing guard against cascade reviews.
- The maintainer asks to land any of `integration-design.md` / `integration-architecture.md` / `integration-tests.md` under `docs/decisions/` or `docs/architecture/` → refuse and explain: outbound integrations are scoped to be portable to separate repos, so a repo-level decision slot would orphan.
