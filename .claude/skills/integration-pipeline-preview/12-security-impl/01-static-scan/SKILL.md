---
name: 01-static-scan
description: Runs the static security checks script across changed files and emits raw findings JSON.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Static Security Scan

Runs `sec-static-checks.py` against the implementation's changed files and emits a raw findings JSON array.

## Inputs

- The changed Haskell file list from a merge-base diff: `BASE=$(git merge-base HEAD origin/main 2>/dev/null || git merge-base HEAD main) && git diff --name-only "$BASE" HEAD -- '*.hs' '*.lhs'`. The pathspec keeps an empty match exit-code 0.

## Plan

1. Compute the changed Haskell files list → verify: list captured.
2. Invoke `sec-static-checks.py` with the list → verify: stdout is a JSON array.
3. Surface stderr on non-zero exit → verify: caller sees the failure.
4. Emit the JSON to stdout for the deep-audit step → verify: stdout parses.

Assumptions:
- Empty changed list yields an empty JSON array (`[]`) and exit 0.
- The script writes findings to stdout and diagnostics to stderr.

If any assumption fails, refuse — do not guess.

## Steps

1. Compute changed files against the merge-base so the PR delta is exact and empty matches stay exit-0: `BASE=$(git merge-base HEAD origin/main 2>/dev/null || git merge-base HEAD main); git diff --name-only "$BASE" HEAD -- '*.hs' '*.lhs'`.
2. Run: `python3 .claude/skills/integration-pipeline-preview/scripts/sec-static-checks.py <files...>`.
3. If exit is non-zero, surface stderr and exit non-zero.
4. Otherwise pass stdout through unchanged.

## Output

JSON array of static security findings on stdout.

## Refusals

- Script not found → refuse: "sec-static-checks.py missing".
- Repo not a git working tree → refuse: "not in a git repo".
