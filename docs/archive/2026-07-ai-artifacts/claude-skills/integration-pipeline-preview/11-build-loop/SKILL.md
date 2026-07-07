> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 11-build-loop
description: Iterates build and test until both are green, then captures hlint warnings without gating.
kind: process
executor: haiku
model: claude-haiku-4-5-20251001
---

# Build Loop

Drives the implementation to a state where `cabal build all` and `cabal test` are green. Hlint runs at the end to collect warnings for the PR body — it does not gate the loop.

## Steps

1. **Build** — read `./01-build/SKILL.md` and follow it. Verify: `.integration-pipeline/build.log` exists and exit was 0.
2. **Test** — read `./02-test/SKILL.md` and follow it. Verify: `.integration-pipeline/test.log` exists and exit was 0.
3. **Fix iter** — spawn an Agent (model: sonnet) and instruct it to read `./03-fix-iter/SKILL.md` and follow it. Verify: at least one of build/test now passes that did not before, or the iteration counter advances.
4. **Hlint (warning collector)** — read `./04-hlint/SKILL.md` and follow it. Verify: `.integration-pipeline/hlint.log` exists (may be empty or non-empty); exit is 0 unless hlint is missing.

Walk these steps in order. If step 1 or 2 fails, jump to step 3 then back to step 1. After step 4 records the log, mark phase 11 complete regardless of how many warnings were captured.

## Shared invariants

- Maximum 10 fix iterations across the entire loop. If still not green after 10, stop and escalate to the maintainer.
- Build and test are the gates; hlint is informational and surfaced via the PR body at phase 16.
- Phase 11 is complete when build and test are simultaneously green and `.integration-pipeline/hlint.log` has been written.
