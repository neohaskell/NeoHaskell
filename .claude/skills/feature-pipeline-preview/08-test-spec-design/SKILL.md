---
name: 08-test-spec-design
description: Runs the test spec design phase (produce → review-quality) without a human pause; advances when the rubric verdict is pass.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# Test spec design

Produces the test specification at `docs/architecture/<adr-number>-<slug>-tests.md`, then independently validates it against the [test spec quality rubric](../references/test-spec-rubric.md). The pipeline advances only when the rubric verdict is `pass`.

## Steps

1. **Produce** — spawn an Agent (model: opus) and instruct it to read `./01-produce/SKILL.md` and follow it against the architecture doc at `docs/architecture/<adr-number>-<slug>.md`. Verify: `docs/architecture/<adr-number>-<slug>-tests.md` exists.
2. **Review quality** — spawn an Agent (model: sonnet) and instruct it to read `./02-review-quality/SKILL.md` and follow it against the produced spec plus the architecture doc. Verify: stdout contains `RUBRIC: pass` or `RUBRIC: fail`; the rubric record is written to `.pipeline/test-spec-rubric.json`.
3. After the review-quality step records `pass`, the orchestrator runs `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 8`. If the rubric records `fail`, the agent has already refused — the orchestrator stops the pipeline at this step.

Walk these steps in order. After each, run the verify check before continuing. If a verify fails, stop and surface.

## Shared invariants

- The produce step writes the spec; the review-quality step is an independent reviewer that did not write the artefact.
- The review-quality step is the gate. The pipeline never advances past this phase without `RUBRIC: pass`.
- On `fail`, the rubric file names the failing checks and the producer step must be re-run after the issues are fixed.
