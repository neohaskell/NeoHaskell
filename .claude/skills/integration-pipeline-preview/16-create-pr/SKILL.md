---
name: 16-create-pr
description: Drafts the PR body and submits the pull request, then pauses for maintainer approval.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# Create PR

Drafts a PR body and submits the pull request.

## Steps

1. **PR body** — spawn an Agent (model: sonnet) and instruct it to read `./01-pr-body/SKILL.md` and follow it. Verify: both `.integration-pipeline/pr-body.md` and `.integration-pipeline/pr-title.txt` exist — submit needs the title file.
2. **Submit** — read `./02-submit/SKILL.md` and follow it. Verify: PR URL captured in pipeline state; phase 16 marked complete.

Walk these steps in order. After step 2, PAUSE — wait for explicit maintainer approval before phase 17 runs.

## Shared invariants

- PR body targets Jess as the reader (community-writer voice).
- Title follows conventional-commit format.
- Closes the originating issue with `Closes #N`.
- Branch is never `main`; the branch hook enforces this.
