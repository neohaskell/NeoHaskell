---
name: 04-hlint
description: Runs hlint on changed files and writes the output to the pipeline log.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Hlint

Runs `hlint` on changed files and writes the output to `.pipeline/hlint.log`. Any output is treated as failure.

## Inputs

- The list of changed files from `git diff --name-only` (filtered to `.hs`).

## Plan

1. Compute the changed Haskell files list → verify: list captured (may be empty).
2. Run `hlint` on the list → verify: log file written.
3. Capture exit code → verify: integer captured.
4. Treat any non-empty output as failure → verify: caller sees the exit code.

Assumptions:
- CI treats hlint warnings as errors; any hlint output means the loop is not green.
- Empty change list is valid — hlint exits 0 immediately.

If any assumption fails, refuse — do not guess.

## Steps

1. Compute changed files: `git diff --name-only HEAD | grep '\.hs$'`.
2. If the list is empty, write an empty `.pipeline/hlint.log` and exit 0.
3. Otherwise run: `hlint <files...> > .pipeline/hlint.log 2>&1`.
4. Capture the exit code.
5. If exit is non-zero or the log is non-empty, surface the log tail and exit non-zero.
6. Otherwise exit 0.

## Output

`.pipeline/hlint.log` written; exit code propagated.

## Refusals

- `hlint` not on PATH → refuse: "hlint not found on PATH".
- Repo not a git working tree → refuse: "not in a git repo".
