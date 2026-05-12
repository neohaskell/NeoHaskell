---
name: 02-submit
description: Stages, commits, pushes the branch, opens the PR, and records the PR URL and number.
kind: leaf
executor: script
---

# Submit PR

Stages the changes, commits, pushes the branch, opens the PR via `gh`, and stores the resulting URL and number.

## Inputs

- `.pipeline/pr-title.txt`
- `.pipeline/pr-body.md`

## Plan (Karpathy 1 + 4)

1. Confirm the current branch is not `main` → verify: `git branch --show-current` differs from `main`.
2. Stage and commit changed files → verify: `git status` is clean afterwards.
3. Push the branch with `-u` → verify: remote tracking branch set.
4. Run `gh pr create` and capture the URL → verify: URL stored via `pipeline.py set pr_url`.

Assumptions:
- The maintainer has not opted into `--no-verify` or signing bypasses; do not pass those flags.
- The branch hook will reject a commit on `main`.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. Run: `BRANCH=$(git branch --show-current)`. If `$BRANCH = main`, refuse.
2. Stage changed files: `git add -u` plus any new files explicitly listed in the architecture doc.
3. Read title from `.pipeline/pr-title.txt`.
4. Commit: `git commit -m "$(cat .pipeline/pr-title.txt)"`.
5. Push: `git push -u origin "$BRANCH"`.
6. Run: `URL=$(gh pr create --title "$(cat .pipeline/pr-title.txt)" --body-file .pipeline/pr-body.md)`.
7. Extract the PR number with `gh pr view --json number -q .number`.
8. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py set pr_url "$URL"`.
9. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py set pr_number "$NUM"`.
10. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 16`.

## Output

PR opened; `pr_url` and `pr_number` stored in pipeline state; phase 16 marked complete.

## Refusals

- Current branch is `main` → refuse: "cannot PR from main".
- `gh` not on PATH → refuse: "gh not found".
- Either input file missing → refuse: "PR body or title file missing".
