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

## Plan (Karpathy 1 + 4)

1. Resolve the PR number → verify: integer captured.
2. Fetch comments via `gh api repos/neohaskell/NeoHaskell/pulls/<N>/comments` → verify: JSON array returned.
3. Address each bot comment with a code edit or a thread reply explaining false positive → verify: every bot comment has either a diff or a reply.
4. Commit and push fix-ups → verify: branch updated.

Assumptions:
- Max 5 cycles of bot-comment fixing. After the 5th cycle, escalate to the maintainer.
- False positives must be explained on the PR thread, never silently dismissed.
- Test files remain immutable unless a bot specifically flags a broken test in spec terms — in that case escalate.

If any assumption fails, refuse — do not guess.

## Steps (Karpathy 2 + 3)

1. `NUM=$(python3 .claude/skills/feature-pipeline/scripts/pipeline.py get pr_number)`.
2. Fetch: `gh api repos/neohaskell/NeoHaskell/pulls/$NUM/comments`.
3. Filter to bot authors (CodeRabbit, etc.).
4. For each comment, decide: real issue (fix with an edit) or false positive (reply on the thread explaining why).
5. After all edits, run `cabal build all` and `cabal test` to confirm nothing regressed.
6. Stage, commit with a `chore(review): address bot feedback` message, and push.
7. Increment the cycle counter via `pipeline.py iter 17`. If > 5, refuse and escalate.

## Output

Fix-up commit pushed; bot comments addressed; cycle counter advanced.

## Refusals

- Cycle counter > 5 → refuse: "bot-comment cycle exhausted; escalate to maintainer".
- `gh` not on PATH → refuse: "gh not found".
