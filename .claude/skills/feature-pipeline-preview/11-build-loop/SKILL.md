---
name: 11-build-loop
description: Iterates build, test, and hlint until all are green or 10 fix iterations are exhausted.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# Build Loop

Drives the implementation to a state where `cabal build all`, `cabal test`, and `hlint .` are all green.

## Steps

1. **Build** — read `./01-build/SKILL.md` and follow it. Verify: `.pipeline/build.log` exists and exit was 0.
2. **Test** — read `./02-test/SKILL.md` and follow it. Verify: `.pipeline/test.log` exists and exit was 0.
3. **Fix iter** — spawn an Agent (model: sonnet) and instruct it to read `./03-fix-iter/SKILL.md` and follow it. Verify: at least one of build/test now passes that did not before, or the iteration counter advances.
4. **Hlint** — read `./04-hlint/SKILL.md` and follow it. Verify: `.pipeline/hlint.log` exists and exit was 0.

Walk these steps in order. If step 1 or 2 fails, jump to step 3 then back to step 1. After step 4 passes, mark phase 11 complete.

## Shared invariants

- Maximum 10 fix iterations across the entire loop. If still not green after 10, stop and escalate to the maintainer.
- CI treats hlint warnings as errors, so any hlint output is failure.
- Phase 11 is only complete when build, test, and hlint are all green simultaneously.
