> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 12-security-impl
description: Runs the implementation-phase security review with a static scan, deep audit, grounding, and record.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# Security Review (Implementation)

Produces `.pipeline/findings-12.json` with grounded security findings against the actual implementation.

## Steps

1. **Static scan** — read `./01-static-scan/SKILL.md` and follow it. Verify: stdout is a JSON array.
2. **Deep audit** — spawn an Agent (model: sonnet) and instruct it to read `./02-deep-audit/SKILL.md` and follow it, piping in the step 1 output. The leaf merges those static-scan findings with its own deep findings before emitting (dedup by `rule` + `location`); the static-scan list is never discarded. Verify: stdout is a JSON array with `file:line` locations.
3. **Ground** — spawn an Agent (model: sonnet) and instruct it to read `./03-ground/SKILL.md` and follow it, piping in the step 2 output. Verify: stdout has `grounding_outcome` on every entry.
4. **Record** — read `./04-record/SKILL.md` and follow it, piping in the step 3 output. Verify: `.pipeline/findings-12.json` exists and phase 12 is marked complete.

Walk these steps in order. After each, run the verify check before continuing. If a verify fails, stop and surface.

## Shared invariants

- Findings are tied to `file:line` references in the actual implementation.
- Grounding is mandatory — even zero findings produce an audit record.
- Static-scan output is folded into the deep-audit's emitted findings (union, dedup by `rule` + `location`); the static-scan list is never replaced or dropped silently.
