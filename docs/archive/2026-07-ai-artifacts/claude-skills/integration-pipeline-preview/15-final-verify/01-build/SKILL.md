> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 01-build
description: Runs cabal build all as a strict gate with no retry.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Final Build

Runs `nix develop --command cabal build all` and writes the output to `.integration-pipeline/final-build.log`. Non-zero exit aborts the phase.

## Inputs

- None — the working directory must be the repo root.

## Plan

1. Confirm `.integration-pipeline/` exists → verify: directory exists.
2. Run `nix develop --command cabal build all` redirecting to `.integration-pipeline/final-build.log` → verify: log written.
3. Capture exit code → verify: integer captured.
4. Propagate exit code → verify: non-zero surfaces to caller.

Assumptions:
- No retry. Failure means returning to phase 11.

If any assumption fails, refuse — do not guess.

## Steps

1. Run: `nix develop --command cabal build all > .integration-pipeline/final-build.log 2>&1`.
2. Capture the exit code.
3. If exit is non-zero, surface the tail of the log and exit non-zero.
4. Otherwise exit 0.

## Output

`.integration-pipeline/final-build.log` written; exit code propagated.

## Refusals

- `.integration-pipeline/` missing → refuse: "pipeline not initialised".
- `nix` not on PATH → refuse: "nix not found; the repo's dev shell is required for cabal".
