---
name: 01-build
description: Runs cabal build all as a strict gate with no retry.
kind: leaf
executor: script
---

# Final Build

Runs `cabal build all` and writes the output to `.pipeline/final-build.log`. Non-zero exit aborts the phase.

## Inputs

- None — the working directory must be the repo root.

## Plan (Karpathy 1 + 4)

1. Confirm `.pipeline/` exists → verify: directory exists.
2. Run `cabal build all` redirecting to `.pipeline/final-build.log` → verify: log written.
3. Capture exit code → verify: integer captured.
4. Propagate exit code → verify: non-zero surfaces to caller.

Assumptions:
- No retry. Failure means returning to phase 11.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Run: `cabal build all > .pipeline/final-build.log 2>&1`.
2. Capture the exit code.
3. If exit is non-zero, surface the tail of the log and exit non-zero.
4. Otherwise exit 0.

## Output

`.pipeline/final-build.log` written; exit code propagated.

## Refusals

- `.pipeline/` missing → refuse: "pipeline not initialised".
- `cabal` not on PATH → refuse: "cabal not found".
