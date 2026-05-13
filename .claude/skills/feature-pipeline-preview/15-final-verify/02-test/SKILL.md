---
name: 02-test
description: Runs cabal test as a strict gate with no retry.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Final Test

Runs `nix develop --command cabal test` and writes the output to `.pipeline/final-test.log`. Non-zero exit aborts the phase.

## Inputs

- None — the working directory must be the repo root.

## Plan

1. Confirm `.pipeline/` exists → verify: directory exists.
2. Run `nix develop --command cabal test` redirecting to `.pipeline/final-test.log` → verify: log written.
3. Capture exit code → verify: integer captured.
4. Propagate exit code → verify: non-zero surfaces to caller.

Assumptions:
- No retry. Failure means returning to phase 11 or 14.

If any assumption fails, refuse — do not guess.

## Steps

1. Run: `nix develop --command cabal test > .pipeline/final-test.log 2>&1`.
2. Capture the exit code.
3. If exit is non-zero, surface the tail of the log and exit non-zero.
4. Otherwise exit 0.

## Output

`.pipeline/final-test.log` written; exit code propagated.

## Refusals

- `.pipeline/` missing → refuse: "pipeline not initialised".
- `nix` not on PATH → refuse: "nix not found; the repo's dev shell is required for cabal".
