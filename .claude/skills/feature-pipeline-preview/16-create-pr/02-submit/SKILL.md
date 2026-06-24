---
name: 02-submit
description: Commits and pushes the implementation, updates the phase-3 draft PR's title/body, and marks it ready for review.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Submit PR (mark draft ready)

Commits and pushes the implementation accumulated since phase 3, updates the existing **draft PR** (opened in phase 3) with the final title and body, and marks it ready for review. It does not create a new PR.

## Inputs

- `.pipeline/pr-title.txt`
- `.pipeline/pr-body.md`
- Pipeline state `pr_number` and `pr_url` (set in phase 3 when the draft PR was opened).

## Plan

1. Confirm the current branch is not `main` → verify: `git branch --show-current` differs from `main`.
2. Confirm the phase-3 draft PR exists → verify: `pipeline.py get pr_number` is non-empty.
3. Commit and push the implementation → verify: branch pushed, `git status` clean.
4. Update the PR title/body and mark it ready → verify: `gh pr view --json isDraft` is `false`.

Assumptions:
- Phase 3 opened the draft PR and stored `pr_number`/`pr_url`.
- The maintainer has not opted into `--no-verify` or signing bypasses; do not pass those flags.
- The branch hook will reject a commit on `main`.

If any assumption fails, refuse — do not guess.

## Steps

1. Run: `BRANCH=$(git branch --show-current)`. If `$BRANCH = main`, refuse: "cannot finalize PR from main".
2. Run: `command -v gh >/dev/null 2>&1` — if non-zero, refuse: "gh not found" and exit non-zero.
3. Read the draft PR number: `NUM=$(python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py get pr_number)`. If empty, refuse: "no draft PR from phase 3 — phase 3 must open the draft PR first".
4. Stage changed files: `git add -u` plus any new files explicitly listed in the architecture doc.
5. Commit the implementation: `git commit -m "$(cat .pipeline/pr-title.txt)"`. If there is nothing to commit, continue (the implementation may already be committed).
6. Push: `git push -u origin "$BRANCH"`.
7. Update the existing PR's title and body: `gh pr edit "$NUM" --title "$(cat .pipeline/pr-title.txt)" --body-file .pipeline/pr-body.md`.
8. Convert the draft to ready for review: `gh pr ready "$NUM"`.
9. Run `python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py complete 16`.

## Output

Implementation committed and pushed; the phase-3 draft PR updated to its final title/body and marked ready for review; `pr_url`/`pr_number` unchanged; phase 16 marked complete.

## Refusals

- Current branch is `main` → refuse: "cannot finalize PR from main".
- `gh` not on PATH → refuse: "gh not found".
- `pr_number` unset → refuse: "no draft PR from phase 3 — open it in phase 3 first".
- Either input file missing → refuse: "PR body or title file missing".
