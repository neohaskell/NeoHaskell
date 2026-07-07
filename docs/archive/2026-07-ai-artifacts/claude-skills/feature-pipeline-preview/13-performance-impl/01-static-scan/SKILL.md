> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 01-static-scan
description: Runs the static performance checks script across changed files and emits raw findings JSON.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Static Performance Scan

Runs `perf-static-checks.py` against the implementation's changed files and emits a raw findings JSON array.

## Inputs

- The changed Haskell file list from `git diff --name-only HEAD -- '*.hs' '*.lhs'` (the pathspec keeps an empty match exit-code 0; `.lhs` is included because `perf-static-checks.py` accepts both extensions).

## Plan

1. Compute the changed Haskell files list → verify: list captured.
2. Invoke `perf-static-checks.py` with the list → verify: stdout is a JSON array.
3. Surface stderr on non-zero exit → verify: caller sees the failure.
4. Emit the JSON to stdout for the deep-audit step → verify: stdout parses.

Assumptions:
- Empty changed list yields `[]` and exit 0.
- The script writes findings to stdout and diagnostics to stderr.

If any assumption fails, refuse — do not guess.

## Steps

1. Compute changed files: `git diff --name-only HEAD -- '*.hs' '*.lhs'` (pathspec is empty-match safe — no `grep` pipe).
2. Run: `python3 .claude/skills/feature-pipeline-preview/scripts/perf-static-checks.py <files...>`.
3. If exit is non-zero, surface stderr and exit non-zero.
4. Otherwise pass stdout through unchanged.

## Output

JSON array of static performance findings on stdout.

## Refusals

- Script not found → refuse: "perf-static-checks.py missing".
- Repo not a git working tree → refuse: "not in a git repo".
