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
2. Run: `python3 .claude/skills/feature-pipeline-preview/scripts/sec-static-checks.py <files...>`.
3. If exit is non-zero, surface stderr and exit non-zero.
4. Otherwise pass stdout through unchanged.

## Output

JSON array of static security findings on stdout.

## Additional static rules (ADR-0059 onwards)

In addition to whatever `sec-static-checks.py` produces, the leaf MUST also emit these findings (the orchestrator merges them into the deep-audit output):

- **schema-drop-table-production** (BLOCKER) — for every match of `DROP TABLE` in any file under `core/service/` that is NOT inside a test fixture (paths matching `*Spec.hs`, `*Test.hs`, `/testlib/`, `/test-service/`). Application startup code that drops tables wipes persistent state on every restart.
- **schema-migration-in-app-code** (non-blocker) — for every match of `CREATE TABLE` in any file that is NOT inside a path matching `*/Migrations/*.sql`. Schema initialisation belongs in a versioned migration file, not in app startup code (even `IF NOT EXISTS` is a sign the wiring is in the wrong place).
- **arch-doc-artifact-missing** (BLOCKER) — for every file path named in the architecture doc's **Module map** that does NOT exist in the tree. Parse `docs/architecture/<adr>-<slug>.md`'s table; for each row's path column, check `test -f <path>`. Missing files mean the implementation diverged silently from the architecture.

These three rules are encoded directly in this leaf's grep-and-emit pass; they do not require extending `sec-static-checks.py`.

## Refusals

- Script not found → refuse: "sec-static-checks.py missing".
- Repo not a git working tree → refuse: "not in a git repo".
