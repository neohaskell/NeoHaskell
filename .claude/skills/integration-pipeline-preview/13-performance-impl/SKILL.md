---
name: 13-performance-impl
description: Runs the implementation-phase performance review with a static scan, deep audit, grounding, and record.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# Performance Review (Implementation)

Produces `.integration-pipeline/findings-13.json` with grounded performance findings against the actual implementation.

## Steps

1. **Static scan** — read `./01-static-scan/SKILL.md` and follow it. Verify: stdout is a JSON array.
2. **Deep audit** — spawn an Agent (model: sonnet) and instruct it to read `./02-deep-audit/SKILL.md` and follow it, piping in the step 1 output. Verify: stdout is a JSON array with `file:line` locations.
3. **Ground** — spawn an Agent (model: sonnet) and instruct it to read `./03-ground/SKILL.md` and follow it, piping in the step 2 output. Verify: stdout has `grounding_outcome` on every entry.
4. **Record** — read `./04-record/SKILL.md` and follow it, piping in the step 3 output. Verify: `.integration-pipeline/findings-13.json` exists and phase 13 is marked complete.

Walk these steps in order. After each, run the verify check before continuing. If a verify fails, stop and surface.

## Shared invariants

- Findings are tied to `file:line` references in the actual implementation.
- Grounding is mandatory — even zero findings produce an audit record.
- The 50k req/s target frames every severity call.
