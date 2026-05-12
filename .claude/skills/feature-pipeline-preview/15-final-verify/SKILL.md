---
name: 15-final-verify
description: Runs build, test, and hlint as a strict no-iteration gate before PR creation.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# Final Verify

Strict gate: `cabal build all`, `cabal test`, and `hlint .` must all be green with no iteration.

## Steps

1. **Build** — read `./01-build/SKILL.md` and follow it. Verify: exit 0.
2. **Test** — read `./02-test/SKILL.md` and follow it. Verify: exit 0.
3. **Hlint** — read `./03-hlint/SKILL.md` and follow it. Verify: exit 0.

Walk these steps in order. After each, run the verify check before continuing. If a verify fails, stop and surface — no iteration is allowed in this phase.

## Shared invariants

- No fix-loop here. Any failure means returning to phase 11 or 14.
- CI treats hlint warnings as errors, so any hlint output is failure.
- Phase 15 is only complete when all three steps are green.
