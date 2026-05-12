---
name: 05-performance-adr
description: Runs the performance review on the ADR, producing grounded findings against the 50k req/s target.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# Performance ADR Review

Produces `.pipeline/findings-05.json` with grounded performance findings against the ADR.

## Steps

1. **Hot-path analysis** — spawn an Agent (model: sonnet) and instruct it to read `./01-hot-path-analysis/SKILL.md` and follow it. Verify: stdout is a JSON array of raw findings.
2. **Ground** — spawn an Agent (model: sonnet) and instruct it to read `./02-ground/SKILL.md` and follow it, piping in the step 1 output. Verify: stdout is a JSON array with `grounding_outcome` on every entry.
3. **Record** — read `./03-record/SKILL.md` and follow it, piping in the step 2 output. Verify: `.pipeline/findings-05.json` exists and phase 5 is marked complete.

Walk these steps in order. After each, run the verify check before continuing. If a verify fails, stop and surface.

## Shared invariants

- Grounding is mandatory — even zero findings produce an audit record.
- The target throughput is 50k req/s; this informs every severity call.
- This phase is sequential internally; the orchestrator may run it in parallel with phase 4.
