---
name: 17-opus-pr-review
description: Opus reviews the open PR locally, proposes fixes, pauses for maintainer approval; then applies the accepted fixes.
kind: process
executor: opus
model: claude-opus-4-7
---

# Opus PR Review

After the PR is opened (phase 16), an Opus reviewer reads the diff against a strict NeoHaskell quality rubric and proposes fixes **locally** — NOT as inline GitHub comments. The phase PAUSES for the maintainer to accept/reject individual proposed fixes by editing `.pipeline/findings-17.json`. On `pipeline.py approve 17`, the orchestrator applies the accepted fixes, commits each one, and pushes.

## Steps

1. **Review** — spawn an Agent (model: opus) and instruct it to read `./01-review/SKILL.md` and follow it. Verify: `.pipeline/findings-17.json` exists with an array of findings; a markdown digest is surfaced in the orchestrator's conversation. The leaf calls `pipeline.py complete 17` and stops. The orchestrator PAUSES.
2. **Apply** — invoked AFTER the maintainer runs `pipeline.py approve 17`. Spawn an Agent (model: sonnet) and instruct it to read `./02-apply/SKILL.md` and follow it. Verify: each `maintainer_decision == "accept"` entry produced a corresponding commit on the branch; `cabal build all` + `cabal test all` are still green; `git push` updated the PR.

## Shared invariants

- Step 1 NEVER posts to GitHub. It writes locally and surfaces in the conversation.
- Step 2 NEVER commits findings where `maintainer_decision != "accept"`.
- Between step 1 and step 2 the maintainer is free to edit `.pipeline/findings-17.json` (typically to flip `pending` → `accept`/`reject`/`defer` per entry).
- Step 2 runs `cabal build all` + `cabal test all` AFTER each fix and refuses to commit if either breaks.
