> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 14-fix-findings
description: Addresses every blocker finding from phases 12 and 13 and re-runs build and tests.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Fix Findings

Reads the implementation security and performance findings and resolves every blocker.

## Inputs

- `.integration-pipeline/findings-12.json`
- `.integration-pipeline/findings-13.json`
- `.integration-pipeline/integration-architecture.md`

## Plan

1. Read both findings files → verify: both load.
2. Filter to entries with `blocker: true` → verify: list captured.
3. Address each blocker with a code edit → verify: every blocker has a corresponding diff.
4. Re-run `nix develop --command cabal build all && nix develop --command cabal test` → verify: both pass.

Assumptions:
- Only blocker findings (`blocker: true`) must be fixed in this phase. Non-blockers are tracked but not blocking.
- If a fix significantly changes the architecture, escalate to the maintainer for re-review before continuing.
- Test files remain immutable.

If any assumption fails, refuse — do not guess.

## Steps

1. Load `.integration-pipeline/findings-12.json` and `.integration-pipeline/findings-13.json`.
2. Collect every entry where `blocker: true`.
3. For each blocker, locate the `file:line`, read the surrounding code, and apply a targeted fix.
4. Run `nix develop --command cabal build all`. If it fails, apply one more targeted fix and retry — bounded to a total of 3 attempts (1 initial + 2 retries). If still failing, refuse and surface the build log.
5. Run `nix develop --command cabal test`. If it fails, apply one more targeted fix and retry — bounded to a total of 3 attempts (1 initial + 2 retries). If still failing, refuse and surface the test log.
6. If any fix changes a public type or module boundary, stop and escalate to the maintainer for re-review.
7. Run `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py complete 14`.

## Output

All blockers resolved; build and tests green; phase 14 marked complete.

## Refusals

- Either findings file missing → refuse: "phase 12 or 13 output missing".
- Any blocker has no `file:line` reference → refuse: "blocker is unactionable".
- Build or test still failing after fixes → refuse and surface the failure.
