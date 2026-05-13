---
name: 02-test
description: Runs cabal test with streaming details and writes the output to the pipeline log.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Test

Runs `cabal test --test-show-details=streaming` and writes the output to `.pipeline/test.log`.

## Inputs

- None — the working directory must be the repo root.

## Plan

1. Confirm `.pipeline/` exists → verify: directory exists.
2. Run `cabal test --test-show-details=streaming` redirecting stdout+stderr to `.pipeline/test.log` → verify: log file exists.
3. Capture exit code → verify: integer captured.
4. Propagate exit code → verify: non-zero exits surface to the caller.

Assumptions:
- The repo root is the current working directory.
- The build step has already passed; otherwise tests cannot link.

If any assumption fails, refuse — do not guess.

## Steps

1. Run: `cabal test --test-show-details=streaming > .pipeline/test.log 2>&1`.
2. Capture the exit code.
3. If exit is non-zero, surface the tail of `.pipeline/test.log` and exit non-zero.
4. If exit is zero, exit 0.

## Output

`.pipeline/test.log` written; exit code propagated.

## Refusals

- `.pipeline/` missing → refuse: "pipeline not initialised; run phase 01 first".
- `cabal` not on PATH → refuse: "cabal not found on PATH".
