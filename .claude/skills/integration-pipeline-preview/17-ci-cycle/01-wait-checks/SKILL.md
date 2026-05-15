---
name: 01-wait-checks
description: Watches GitHub PR checks until they pass or fail.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Wait Checks

Runs `gh pr checks <pr_number> --watch` and exits 0 only when every check passes.

## Inputs

- `pipeline.py get pr_number` → the PR number from phase 16.

## Plan

1. Resolve the PR number via `pipeline.py get pr_number` → verify: integer captured.
2. Run `gh pr checks <NUM> --watch` → verify: command exits.
3. Capture exit code → verify: integer captured.
4. Propagate exit code → verify: non-zero falls through to fix-bot-comments.

Assumptions:
- `gh` is authenticated.
- `--watch` blocks until terminal state.

If any assumption fails, refuse — do not guess.

## Steps

1. Run: `NUM=$(python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py get pr_number)`.
2. If `NUM` is empty, refuse.
3. Run: `gh pr checks "$NUM" --watch`.
4. Capture the exit code.
5. If exit is zero, exit 0.
6. If exit is non-zero, exit non-zero so the caller falls through to bot-comments handling.

## Output

Exit code reflects CI state.

## Refusals

- `pr_number` not set → refuse: "no pr_number in pipeline state".
- `gh` not on PATH → refuse: "gh not found".
