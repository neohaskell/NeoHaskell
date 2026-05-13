---
name: 17-ci-cycle
description: Waits on CI, addresses bot comments, and waits for maintainer merge.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# CI Cycle

Waits on CI checks, addresses bot review comments, and waits for the maintainer to merge.

## Steps

1. **Wait checks** — read `./01-wait-checks/SKILL.md` and follow it. Step 1 is **successful** only when its exit code is 0 AND there are no unresolved bot/CodeRabbit comments on the PR. It is **failed** when the exit code is non-zero OR exit 0 but unresolved bot comments are detected.
2. **Fix bot comments** — spawn an Agent (model: sonnet) and instruct it to read `./02-fix-bot-comments/SKILL.md` and follow it. Trigger whenever step 1 was not successful (non-zero exit, or exit 0 with unresolved bot comments). Skip only when step 1 succeeded with no comments. Verify: no unaddressed CodeRabbit/bot comments remain.
3. **Await merge** — read `./03-await-merge/SKILL.md` and follow it. Verify: phase 17 marked complete and parked at `awaiting_approval` (the maintainer runs `pipeline.py approve 17` after the manual merge).

Walk these steps in order. After each, run the verify check before continuing. Whenever step 1 is not successful, run step 2 and then return to step 1.

## Shared invariants

- Max 5 iterations of the step 1 → step 2 loop. After 5 iterations, escalate to the maintainer.
- False-positive bot comments must be explained on the PR thread, not silently dismissed.
- Step 3 is the terminal step of the pipeline and does not auto-approve — the maintainer holds the merge gate.
