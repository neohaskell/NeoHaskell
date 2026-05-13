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

- `.pipeline/pr-title.txt`
- `.pipeline/pr-body.md`

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
3. Stage changed files: `git add -u` plus any new files explicitly listed in the architecture doc.
4. Read title from `.pipeline/pr-title.txt`.
5. Commit: `git commit -m "$(cat .pipeline/pr-title.txt)"`.
6. Push: `git push -u origin "$BRANCH"`.
7. Run: `URL=$(gh pr create --title "$(cat .pipeline/pr-title.txt)" --body-file .pipeline/pr-body.md)`.
8. Extract the PR number: `NUM=$(gh pr view --json number -q .number)`.
9. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py set pr_url "$URL"`.
10. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py set pr_number "$NUM"`.
11. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 16`.

## Output

PR opened; `pr_url` and `pr_number` stored in pipeline state; phase 16 marked complete.

## Refusals

- Current branch is `main` → refuse: "cannot PR from main".
- `gh` not on PATH → refuse: "gh not found".
- Either input file missing → refuse: "PR body or title file missing".
