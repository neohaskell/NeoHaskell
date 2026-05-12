---
name: 01-static-scan
description: Runs the static security checks script across changed files and emits raw findings JSON.
kind: leaf
executor: script
---

# Static Security Scan

Runs `sec-static-checks.py` against the implementation's changed files and emits a raw findings JSON array.

## Inputs

- The changed file list from `git diff --name-only HEAD` (filtered to `.hs`).

## Plan (Karpathy 1 + 4)

1. Compute the changed Haskell files list → verify: list captured.
2. Invoke `sec-static-checks.py` with the list → verify: stdout is a JSON array.
3. Surface stderr on non-zero exit → verify: caller sees the failure.
4. Emit the JSON to stdout for the deep-audit step → verify: stdout parses.

Assumptions:
- Empty changed list yields an empty JSON array (`[]`) and exit 0.
- The script writes findings to stdout and diagnostics to stderr.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Compute changed files: `git diff --name-only HEAD | grep '\.hs$'`.
2. Run: `python3 .claude/skills/feature-pipeline/scripts/sec-static-checks.py <files...>`.
3. If exit is non-zero, surface stderr and exit non-zero.
4. Otherwise pass stdout through unchanged.

## Output

JSON array of static security findings on stdout.

## Refusals

- Script not found → refuse: "sec-static-checks.py missing".
- Repo not a git working tree → refuse: "not in a git repo".
