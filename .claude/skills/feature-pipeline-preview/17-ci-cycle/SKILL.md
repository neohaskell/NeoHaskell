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

1. **Wait checks** — read `./01-wait-checks/SKILL.md` and follow it. Verify: exit 0 means all checks green.
2. **Fix bot comments** — spawn an Agent (model: sonnet) and instruct it to read `./02-fix-bot-comments/SKILL.md` and follow it. Verify: no unaddressed CodeRabbit/bot comments remain. Skip if step 1 already exited 0 with no comments.
3. **Await merge** — read `./03-await-merge/SKILL.md` and follow it. Verify: phase 17 marked complete and approved.

Walk these steps in order. After each, run the verify check before continuing. If step 1 fails, fall through to step 2 and then loop back to step 1.

## Shared invariants

- Max 5 cycles of step 2 then step 1. After 5, escalate to the maintainer.
- False-positive bot comments must be explained on the PR thread, not silently dismissed.
- Step 3 is the terminal step of the pipeline.
