---
name: feature-pipeline-preview
description: Use when implementing a new NeoHaskell feature end-to-end — from ADR draft through PR merge — and the work needs the structured 17-phase pipeline with ADR, security and performance reviews (each grounded against feature complexity and the Jess persona), outside-in test design, build loop, and PR/CI handling. Triggers on 'run feature pipeline', 'feature pipeline', 'start pipeline', 'implement feature', 'new NeoHaskell feature', 'implement issue #N', 'add type to nhcore', 'pipeline init', 'next pipeline phase', 'approve phase N'.
kind: process
executor: opus
model: claude-opus-4-7
---

# NeoHaskell feature pipeline

Coordinates the 17-phase pipeline for landing a NeoHaskell feature. State machine lives in `scripts/pipeline.py`; each phase dispatches to a child node. Replaces the older `neohaskell-feature-pipeline` skill.

The pipeline has two distinguishing characteristics:

- **Grounding loop** — after every security and performance deep-audit (phases 4, 5, 12, 13), a grounding pass filters findings through the feature's complexity tier and Jess's 15-minute pocket so simple features do not accumulate cascades of unneeded caching, SHA pins, constant-time compares, or `INLINE` pragmas. See [`references/grounding-loop.md`](./references/grounding-loop.md).
- **Quality-rubric review-gates** — phases 6 (DevEx), 7 (architecture), 8 (test spec) each run a produce step followed by an independent review-quality step that validates the artefact against a per-phase rubric ([`devex-rubric.md`](./references/devex-rubric.md), [`architecture-rubric.md`](./references/architecture-rubric.md), [`test-spec-rubric.md`](./references/test-spec-rubric.md)). These review steps replace the previous human PAUSE gates — the pipeline advances automatically on `RUBRIC: pass` and halts with a structured rubric record on `RUBRIC: fail`.

## Shared invariants

- All state lives in `.pipeline/state.json` and `.pipeline/findings-*.json`. Drive via `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py <cmd>`.
- The pipeline must be initialised before any phase runs. Phase 1 calls `pipeline.py init`.
- PAUSE-gated phases (marked 🔒 below) stop the orchestrator until the maintainer runs `pipeline.py approve <N>`. The PAUSE remains only on phases 3 (ADR draft), 16 (PR creation), and 17 (CI cycle / merge) — every other phase auto-gates on its rubric or findings.
- Every step reads [`references/jess-persona.md`](./references/jess-persona.md) and [`references/nhcore-context.md`](./references/nhcore-context.md) when delegating to a sonnet child.
- The grounding pass is mandatory — it cannot be skipped to "save time", and it cannot be merged with the deep-audit step.
- The review-quality steps on phases 6/7/8 are mandatory and must be run by a different agent invocation from the producer step (independence is what makes the gate trustworthy).

## Steps

1. **Init pipeline** — read `./01-init-pipeline/SKILL.md` and follow it with `{feature_name, issue_number, module_path, test_path, branch_name, adr_number}`. Verify: `.pipeline/state.json` exists and `pipeline.py status` lists phase 1 as completed.
2. **Classify feature** — spawn an Agent (model: haiku) and instruct it to read `./02-classify-feature/SKILL.md` and follow it. Verify: `.pipeline/classification.json` exists with `tier ∈ {trivial, simple, moderate, complex, security-critical}`.
3. **ADR draft 🔒** — spawn an Agent (model: opus) and instruct it to read `./03-adr-draft/SKILL.md` and follow it. Verify: `docs/decisions/NNNN-slug.md` exists with Status: Proposed. Then `pipeline.py complete 3` and stop until `pipeline.py approve 3`.
4. **Security review (ADR)** — spawn an Agent (model: haiku) and instruct it to read `./04-security-adr/SKILL.md` and follow it. Verify: `.pipeline/findings-04.json` exists with `blockers >= 0`. If `blockers > 0`, stop and surface to maintainer.
5. **Performance review (ADR)** — spawn an Agent (model: haiku) and instruct it to read `./05-performance-adr/SKILL.md` and follow it. Verify: `.pipeline/findings-05.json` exists. May run in parallel with step 4.
6. **DevEx review** — spawn an Agent (model: haiku) and instruct it to read `./06-devex-review/SKILL.md` and follow it. Verify: `.pipeline/devex-review-rubric.json` exists with `"verdict": "pass"`. On `fail`, the rubric record names the failing checks and the pipeline halts here.
7. **Architecture design** — spawn an Agent (model: haiku) and instruct it to read `./07-architecture-design/SKILL.md` and follow it. Verify: `.pipeline/architecture-rubric.json` exists with `"verdict": "pass"`. On `fail`, the rubric record names the failing checks and the pipeline halts.
8. **Test spec design** — spawn an Agent (model: haiku) and instruct it to read `./08-test-spec-design/SKILL.md` and follow it. Verify: `.pipeline/test-spec-rubric.json` exists with `"verdict": "pass"`. On `fail`, the rubric record names the failing checks and the pipeline halts.
9. **Test writing** — spawn an Agent (model: sonnet) and instruct it to read `./09-test-writing/SKILL.md` and follow it. Verify: tests compile and ALL fail.
10. **Implementation** — spawn an Agent (model: sonnet) and instruct it to read `./10-implementation/SKILL.md` and follow it. Verify: source files exist at the planned paths.
11. **Build loop** — spawn an Agent (model: haiku) and instruct it to read `./11-build-loop/SKILL.md` and follow it. Verify: `cabal build all` and `cabal test` pass; `.pipeline/hlint.log` exists (warnings are captured for the PR body, not gated here).
12. **Security review (impl)** — spawn an Agent (model: haiku) and instruct it to read `./12-security-impl/SKILL.md` and follow it. Verify: `.pipeline/findings-12.json` exists.
13. **Performance review (impl)** — spawn an Agent (model: haiku) and instruct it to read `./13-performance-impl/SKILL.md` and follow it. Verify: `.pipeline/findings-13.json` exists. May run in parallel with step 12.
14. **Fix findings** — spawn an Agent (model: sonnet) and instruct it to read `./14-fix-findings/SKILL.md` and follow it. Verify: `blockers == 0` across findings-12 and findings-13 after fixes; `cabal test` still green.
15. **Final verify** — spawn an Agent (model: haiku) and instruct it to read `./15-final-verify/SKILL.md` and follow it. Verify: clean build, all tests pass; `.pipeline/hlint.log` refreshed (hlint is captured for the PR body, not gated).
16. **Create PR 🔒** — spawn an Agent (model: haiku) and instruct it to read `./16-create-pr/SKILL.md` and follow it. Verify: `pipeline.py get pr_url` returns a URL. Then `complete 16`, stop until `approve 16`.
17. **CI cycle** — spawn an Agent (model: haiku) and instruct it to read `./17-ci-cycle/SKILL.md` and follow it. Verify: CI green and PR merged.

After each step the orchestrator runs `pipeline.py complete <N>`. PAUSE-gated steps then stop and wait for the maintainer's `pipeline.py approve <N>` before continuing.

## Refusals

- `.pipeline/state.json` already exists with a different feature → refuse and ask the maintainer to run `pipeline.py reset` or pick the right state.
- A phase's verify check fails → stop, surface the failure, do not advance the cursor.
- A grounding pass is missing from a security/perf phase → refuse to mark the phase complete.
- The maintainer asks to skip the grounding pass → refuse; explain that the grounding pass is the load-bearing guard against cascade reviews.
