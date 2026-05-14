---
name: 02-submit
description: Stages, commits, pushes the branch, opens the PR, and records the PR URL and number.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Submit PR

Stages the changes, commits, pushes the branch, opens the PR via `gh`, and stores the resulting URL and number.

## Inputs

- `.integration-pipeline/pr-title.txt`
- `.integration-pipeline/pr-body.md`

## Plan

1. Confirm the current branch is not `main` → verify: `git branch --show-current` differs from `main`.
2. Stage and commit changed files → verify: `git status` is clean afterwards.
3. Push the branch with `-u` → verify: remote tracking branch set.
4. Run `gh pr create` and capture the URL → verify: URL stored via `pipeline.py set pr_url`.

Assumptions:
- The maintainer has not opted into `--no-verify` or signing bypasses; do not pass those flags.
- The branch hook will reject a commit on `main`.

If any assumption fails, refuse — do not guess.

## Steps

1. Run: `BRANCH=$(git branch --show-current)`. If `$BRANCH = main`, refuse: "cannot PR from main".
2. Run: `command -v gh >/dev/null 2>&1` — if non-zero, refuse: "gh not found" and exit non-zero.
3. Verify both input files exist: `test -f .integration-pipeline/pr-title.txt && test -f .integration-pipeline/pr-body.md`. Refuse with "PR body or title file missing" on failure.
4. Stage changed files. `git add -u` alone is unsafe here because new integration code is untracked. Resolve the integration's module + test paths from pipeline state:
   - `MODULE_PATH=$(python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py get module_path)`
   - `TEST_PATH=$(python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py get test_path)`
   - `MODULE_NAME=$(python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py get module_name)`
   - `git add -u` (catches edits to existing tracked files such as `integrations/nhintegrations.cabal`).
   - For each of `$MODULE_PATH`, `$TEST_PATH`, and `integrations/Integration/$MODULE_NAME/` (the integration's directory tree): if the path is non-empty AND exists on disk, run `git add -- "$PATH"`. Skip silently when empty/missing — pipeline state allows them to be unset until the architecture phase decides.
   - Confirm `.integration-pipeline/` is NOT staged: `git diff --cached --name-only | grep -q '^\.integration-pipeline/' && refuse "pipeline state must not be committed"`.
5. Read title from `.integration-pipeline/pr-title.txt`.
6. Commit: `git commit -m "$(cat .integration-pipeline/pr-title.txt)"`.
7. Push: `git push -u origin "$BRANCH"`.
8. Run: `URL=$(gh pr create --title "$(cat .integration-pipeline/pr-title.txt)" --body-file .integration-pipeline/pr-body.md)`.
9. Extract the PR number: `NUM=$(gh pr view --json number -q .number)`.
10. Run `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py set pr_url "$URL"`.
11. Run `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py set pr_number "$NUM"`.
12. Run `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py complete 16`.

## Output

PR opened; `pr_url` and `pr_number` stored in pipeline state; phase 16 marked complete.

## Refusals

- Current branch is `main` → refuse: "cannot PR from main".
- `gh` not on PATH → refuse: "gh not found".
- Either input file missing → refuse: "PR body or title file missing".
