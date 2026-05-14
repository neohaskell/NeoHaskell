---
name: 02-test
description: Runs cabal test with streaming details and writes the output to the pipeline log.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Test

Runs `nix develop --command cabal test --test-show-details=streaming` and writes the output to `.integration-pipeline/test.log`.

## Inputs

- None — the working directory must be the repo root.

## Plan

1. Confirm `.integration-pipeline/` exists → verify: directory exists.
2. Run `nix develop --command cabal test --test-show-details=streaming` redirecting stdout+stderr to `.integration-pipeline/test.log` → verify: log file exists.
3. Capture exit code → verify: integer captured.
4. Propagate exit code → verify: non-zero exits surface to the caller.

Assumptions:
- The repo root is the current working directory.
- The build step has already passed; otherwise tests cannot link.

If any assumption fails, refuse — do not guess.

## Steps

1. If `.integration-pipeline/` does not exist, refuse: "pipeline not initialised; run phase 01 first" and exit non-zero.
2. If `nix` is not on PATH (`command -v nix`), refuse: "nix not found; the repo's dev shell is required for cabal" and exit non-zero.
3. Run: `nix develop --command cabal test --test-show-details=streaming > .integration-pipeline/test.log 2>&1`.
4. Capture the exit code.
5. If exit is non-zero, surface the tail of `.integration-pipeline/test.log` and exit non-zero.
6. If exit is zero, exit 0.

## Output

`.integration-pipeline/test.log` written; exit code propagated.

## Refusals

- `.integration-pipeline/` missing → refuse: "pipeline not initialised; run phase 01 first".
- `nix` not on PATH → refuse: "nix not found; the repo's dev shell is required for cabal".
