---
name: 02-submit
description: Commits and pushes the implementation, updates the phase-3 draft PR's title/body, and marks it ready for review.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Submit PR (mark draft ready)

Stages and commits the implementation accumulated since phase 3, pushes the branch, updates the existing **draft PR** (opened in phase 3) with the final title and body, and marks it ready for review. It does not create a new PR.

## Inputs

- `.integration-pipeline/pr-title.txt`
- `.integration-pipeline/pr-body.md`
- Pipeline state `pr_number` and `pr_url` (set in phase 3 when the draft PR was opened).

## Plan

1. Confirm the current branch is not `main` → verify: `git branch --show-current` differs from `main`.
2. Confirm the phase-3 draft PR exists → verify: `pipeline.py get pr_number` is non-empty.
3. Stage and commit the implementation → verify: `git status` is clean afterwards.
4. Push the branch with `-u` → verify: remote tracking branch set.
5. Update the PR title/body and mark it ready → verify: `gh pr view --json isDraft -q .isDraft` is `false`.

Assumptions:
- Phase 3 opened the draft PR and stored `pr_number`/`pr_url`.
- The maintainer has not opted into `--no-verify` or signing bypasses; do not pass those flags.
- The branch hook will reject a commit on `main`.

If any assumption fails, refuse — do not guess.

## Steps

1. Run: `BRANCH=$(git branch --show-current)`. If `$BRANCH = main`, refuse: "cannot finalize PR from main".
2. Run: `command -v gh >/dev/null 2>&1` — if non-zero, refuse: "gh not found" and exit non-zero.
3. Read the draft PR number: `NUM=$(python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py get pr_number)`. If empty, refuse: "no draft PR from phase 3 — phase 3 must open the draft PR first".
4. Verify both input files exist: `test -f .integration-pipeline/pr-title.txt && test -f .integration-pipeline/pr-body.md`. Refuse with "PR body or title file missing" on failure.
5. Stage changed files. `git add -u` alone is unsafe here because new integration code is untracked. Resolve the integration's module + test paths from pipeline state:
   - `MODULE_PATH=$(python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py get module_path)`
   - `TEST_PATH=$(python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py get test_path)`
   - `MODULE_NAME=$(python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py get module_name)`
   - `git add -u` (catches edits to existing tracked files such as `integrations/nhintegrations.cabal`).
   - For each of `$MODULE_PATH`, `$TEST_PATH`, and `integrations/Integration/$MODULE_NAME/` (the integration's directory tree): if the path is non-empty AND exists on disk, run `git add -- "$PATH"`. Skip silently when empty/missing — pipeline state allows them to be unset until the architecture phase decides.
   - Confirm `.integration-pipeline/` is NOT staged: `git diff --cached --name-only | grep -q '^\.integration-pipeline/' && refuse "pipeline state must not be committed"`.
6. Commit the implementation: `git commit -m "$(cat .integration-pipeline/pr-title.txt)"`. If there is nothing to commit (implementation already committed by an earlier phase), continue.
7. Push: `git push -u origin "$BRANCH"`.
8. Update the existing PR's title and body (replacing the phase-3 design-review body with the final PR body): `gh pr edit "$NUM" --title "$(cat .integration-pipeline/pr-title.txt)" --body-file .integration-pipeline/pr-body.md`.
9. Convert the draft to ready for review: `gh pr ready "$NUM"`.
10. Run `python3 .claude/skills/integration-pipeline-preview/scripts/pipeline.py complete 16`.

## Output

Implementation committed and pushed; the phase-3 draft PR updated to its final title/body and marked ready for review; `pr_url`/`pr_number` unchanged; phase 16 marked complete.

## Refusals

- Current branch is `main` → refuse: "cannot finalize PR from main".
- `gh` not on PATH → refuse: "gh not found".
- `pr_number` unset → refuse: "no draft PR from phase 3 — open it in phase 3 first".
- Either input file missing → refuse: "PR body or title file missing".
