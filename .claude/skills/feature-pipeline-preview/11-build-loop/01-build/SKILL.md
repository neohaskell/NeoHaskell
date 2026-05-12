---
name: 01-build
description: Runs cabal build all and streams the output to the pipeline log.
kind: leaf
executor: script
---

# Build

Runs `cabal build all` and writes the output to `.pipeline/build.log`.

## Inputs

- None — the working directory must be the repo root.

## Plan (Karpathy 1 + 4)

1. Confirm `.pipeline/` exists → verify: directory exists.
2. Run `cabal build all` redirecting stdout+stderr to `.pipeline/build.log` → verify: log file exists.
3. Capture exit code → verify: integer captured.
4. On non-zero exit, stop the loop → verify: caller receives the non-zero exit.

Assumptions:
- The repo root is the current working directory.
- `.pipeline/` was created during phase 01.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Run: `cabal build all > .pipeline/build.log 2>&1`.
2. Capture the exit code.
3. If exit is non-zero, surface the tail of `.pipeline/build.log` and exit non-zero.
4. If exit is zero, exit 0.

## Output

`.pipeline/build.log` written; exit code propagated.

## Refusals

- `.pipeline/` missing → refuse: "pipeline not initialised; run phase 01 first".
- `cabal` not on PATH → refuse: "cabal not found on PATH".
