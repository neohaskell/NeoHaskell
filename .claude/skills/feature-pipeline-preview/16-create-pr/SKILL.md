---
name: 16-create-pr
description: Finalizes the PR — writes the final body/title, commits and pushes the implementation, and marks the phase-3 draft PR ready for review.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# Finalize PR (mark ready)

Generates the final PR body and title, commits and pushes the implementation, then converts the **draft PR opened in phase 3** to ready for review. Phase 3 already opened the draft PR (for the ADR review); this phase updates its title/body and marks it ready — it never creates a second PR.

## Steps

1. **PR body** — spawn an Agent (model: sonnet) and instruct it to read `./01-pr-body/SKILL.md` and follow it. Verify: both `.pipeline/pr-body.md` and `.pipeline/pr-title.txt` exist — submit needs the title file.
2. **Submit** — read `./02-submit/SKILL.md` and follow it. Verify: `pipeline.py get pr_url` returns the phase-3 draft PR URL, the PR is no longer a draft (`gh pr view --json isDraft -q .isDraft` is `false`), and phase 16 marked complete.

Walk these steps in order. After step 2, PAUSE — wait for explicit maintainer approval before phase 17 runs (Opus PR review).

## Shared invariants

- The draft PR already exists (opened in phase 3); this phase updates its title/body and marks it ready — it never opens a second PR. Refuse if `pr_number`/`pr_url` is unset.
- PR body targets Jess as the reader (community-writer voice).
- Title follows conventional-commit format.
- Closes the originating issue with `Closes #N`.
- Branch is never `main`; the branch hook enforces this.
