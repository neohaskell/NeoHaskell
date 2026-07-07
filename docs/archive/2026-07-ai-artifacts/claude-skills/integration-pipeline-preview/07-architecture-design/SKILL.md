> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 07-architecture-design
description: Runs the architecture design phase (produce → review-quality) without a human pause; advances when the rubric verdict is pass.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# Architecture design

Produces the implementation-ready architecture document at `.integration-pipeline/integration-architecture.md`, then independently validates it against the [architecture quality rubric](../references/architecture-rubric.md). The pipeline advances only when the rubric verdict is `pass`.

## Steps

1. **Produce** — spawn an Agent (model: opus) and instruct it to read `./01-produce/SKILL.md` and follow it on the design draft + the DevEx review at `.integration-pipeline/devex-review.md`. Verify: `.integration-pipeline/integration-architecture.md` exists and contains the sections listed in the rubric.
2. **Review quality** — spawn an Agent (model: sonnet) and instruct it to read `./02-review-quality/SKILL.md` and follow it against the produced architecture doc plus the design draft. Verify: stdout contains `RUBRIC: pass` or `RUBRIC: fail`; the rubric record is written to `.integration-pipeline/architecture-rubric.json`.
3. After the review-quality step records `pass`, the orchestrator runs `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py complete 7`. If the rubric records `fail`, the agent has already refused — the orchestrator stops the pipeline at this step.

Walk these steps in order. After each, run the verify check before continuing. If a verify fails, stop and surface.

## Shared invariants

- The produce step writes the doc; the review-quality step is an independent reviewer that did not write the artefact.
- The review-quality step is the gate. The pipeline never advances past this phase without `RUBRIC: pass`.
- On `fail`, the rubric file names the failing checks and the producer step must be re-run after the issues are fixed.
