---
name: 06-devex-review
description: Runs the DevEx review (produce → review-quality) without a human pause; advances automatically when the review passes the rubric.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# DevEx review

Produces a DevEx review of the ADR, then independently validates that review against the [DevEx quality rubric](../references/devex-rubric.md). The pipeline advances only when the rubric verdict is `pass`.

## Steps

1. **Produce** — spawn an Agent (model: opus) and instruct it to read `./01-produce/SKILL.md` and follow it on the ADR at `docs/decisions/<adr-number>-<slug>.md` and the findings at `.pipeline/findings-04.json` + `.pipeline/findings-05.json`. Verify: `.pipeline/devex-review.md` exists and contains a `## Per-criterion verdicts` section.
2. **Review quality** — spawn an Agent (model: sonnet) and instruct it to read `./02-review-quality/SKILL.md` and follow it against the produced review plus the ADR. Verify: stdout contains `RUBRIC: pass` or `RUBRIC: fail`; the rubric record is written to `.pipeline/devex-review-rubric.json`.
3. The review-quality leaf owns the `pipeline.py complete 6` call on `RUBRIC: pass`. If the rubric records `fail`, the leaf has already refused — the orchestrator stops the pipeline at this step.

Walk these steps in order. After each, run the verify check before continuing. If a verify fails, stop and surface.

## Shared invariants

- The produce step writes the review; the review-quality step is an independent reviewer that did not write the artefact.
- The review-quality step is the gate. The pipeline never advances past this phase without `RUBRIC: pass`.
- On `fail`, the rubric file names the failing checks and the producer step must be re-run after the issues are fixed.
