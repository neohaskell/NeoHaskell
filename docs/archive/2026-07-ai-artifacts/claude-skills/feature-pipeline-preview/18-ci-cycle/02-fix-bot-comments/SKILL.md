> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 02-fix-bot-comments
description: Reads CodeRabbit and other bot review comments, addresses them, and pushes fix-up commits.
kind: leaf
executor: sonnet
model: claude-sonnet-4-6
---

# Fix Bot Comments

Reads CodeRabbit / bot comments on the PR, addresses each, and pushes fix-up commits.

## Inputs

- `pipeline.py get pr_number` → PR number.

## Plan

1. Resolve the PR number → verify: integer captured.
2. Fetch both bot-comment surfaces and union them — review-thread comments (`gh api repos/neohaskell/NeoHaskell/pulls/<N>/comments`) AND general PR-timeline comments (`gh api repos/neohaskell/NeoHaskell/issues/<N>/comments`) → verify: both fetches return JSON arrays.
3. Address each bot comment with a code edit or a thread reply explaining false positive → verify: every bot comment has either a diff or a reply.
4. Commit and push fix-ups → verify: branch updated.

Assumptions:
- Both PR comment surfaces are enumerated each cycle (review-thread `pulls/<N>/comments` and general `issues/<N>/comments`) and merged into one bot list before filtering — a bot can post to either.
- Max 5 cycles of bot-comment fixing. After the 5th cycle, escalate to the maintainer.
- False positives must be explained on the PR thread, never silently dismissed.
- Test files remain immutable unless a bot specifically flags a broken test in spec terms — in that case escalate.

If any assumption fails, refuse — do not guess.

## Steps

1. `NUM=$(python3 .claude/skills/feature-pipeline-preview/scripts/pipeline.py get pr_number)`.
2. Fetch review-thread comments: `gh api repos/neohaskell/NeoHaskell/pulls/$NUM/comments`.
3. Fetch general PR-timeline comments: `gh api repos/neohaskell/NeoHaskell/issues/$NUM/comments`. Merge the two arrays (dedup by `id`) before the next step — bots can post to either surface.
4. Filter the merged list to bot authors (CodeRabbit, etc.).
5. For each comment, decide: real issue (fix with an edit) or false positive (reply on the thread explaining why).
6. After all edits, run `nix develop --command cabal build all` and `nix develop --command cabal test` to confirm nothing regressed.
7. Stage, commit with a `chore(review): address bot feedback` message, and push.
8. Increment the cycle counter via `pipeline.py iter 17`. If > 5, refuse and escalate.

## Output

Fix-up commit pushed; bot comments addressed; cycle counter advanced.

## Refusals

- Cycle counter > 5 → refuse: "bot-comment cycle exhausted; escalate to maintainer".
- `gh` not on PATH → refuse: "gh not found".
