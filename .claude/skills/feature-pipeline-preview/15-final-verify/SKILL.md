---
name: 15-final-verify
description: Runs build and test as a strict no-iteration gate before PR creation; records final hlint warnings for the PR body.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# Final Verify

Strict gate on `cabal build all` and `cabal test`. Hlint runs as a warning collector — the canonical `.pipeline/hlint.log` is overwritten with the final state so phase 16 surfaces accurate warnings in the PR body.

## Steps

1. **Build** — read `./01-build/SKILL.md` and follow it. Verify: exit 0.
2. **Test** — read `./02-test/SKILL.md` and follow it. Verify: exit 0.
3. **Hlint (warning collector)** — read `./03-hlint/SKILL.md` and follow it. Verify: `.pipeline/hlint.log` exists; exit is 0 unless hlint is missing.

Walk these steps in order. After each, run the verify check before continuing. If step 1 or 2 fails, stop and surface — no iteration is allowed in this phase; the maintainer returns to phase 11 or 14. Step 3 never gates.

## Shared invariants

- No fix-loop here. Any build or test failure means returning to phase 11 or 14.
- Build and test are gates; hlint is informational and surfaced via the PR body at phase 16.
- Phase 15 is complete when build and test are green and `.pipeline/hlint.log` has been refreshed.
