---
name: 03-hlint
description: Runs hlint on changed files as a strict gate with no retry.
kind: leaf
executor: script
---

# Final Hlint

Runs `hlint` against changed files and writes the output to `.pipeline/final-hlint.log`. Any output is failure.

## Inputs

- The changed file list from `git diff --name-only HEAD` (filtered to `.hs`).

## Plan (Karpathy 1 + 4)

1. Compute the changed Haskell files list → verify: list captured.
2. Run `hlint` on the list → verify: log written.
3. Capture exit code → verify: integer captured.
4. Treat any non-empty output as failure → verify: caller sees the exit code.

Assumptions:
- CI treats hlint warnings as errors.
- Empty change list yields exit 0 with an empty log.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Compute changed files: `git diff --name-only HEAD | grep '\.hs$'`.
2. If the list is empty, write an empty `.pipeline/final-hlint.log` and exit 0.
3. Otherwise run: `hlint <files...> > .pipeline/final-hlint.log 2>&1`.
4. Capture the exit code.
5. If exit is non-zero or the log is non-empty, surface the log tail and exit non-zero.
6. Otherwise exit 0.

## Output

`.pipeline/final-hlint.log` written; exit code propagated.

## Refusals

- `hlint` not on PATH → refuse: "hlint not found".
- Repo not a git working tree → refuse: "not in a git repo".
