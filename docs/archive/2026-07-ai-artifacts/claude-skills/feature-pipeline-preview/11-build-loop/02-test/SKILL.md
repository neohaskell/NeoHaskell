> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 02-test
description: Runs cabal test with streaming details and writes the output to the pipeline log.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Test

Runs `nix develop --command cabal test --test-show-details=streaming` and writes the output to `.pipeline/test.log`.

## Inputs

- None — the working directory must be the repo root.

## Plan

1. Confirm `.pipeline/` exists → verify: directory exists.
2. Run `nix develop --command cabal test --test-show-details=streaming` redirecting stdout+stderr to `.pipeline/test.log` → verify: log file exists.
3. Capture exit code → verify: integer captured.
4. Propagate exit code → verify: non-zero exits surface to the caller.

Assumptions:
- The repo root is the current working directory.
- The build step has already passed; otherwise tests cannot link.

If any assumption fails, refuse — do not guess.

## Steps

1. If `.pipeline/` does not exist, refuse: "pipeline not initialised; run phase 01 first" and exit non-zero.
2. If `nix` is not on PATH (`command -v nix`), refuse: "nix not found; the repo's dev shell is required for cabal" and exit non-zero.
3. Run: `nix develop --command cabal test all --test-show-details=streaming > .pipeline/test.log 2>&1`.
4. Capture the exit code.
5. **Snapshot rollover (deterministic).** Before parsing the new run, if `.pipeline/test-counts.json` exists from the previous run, atomically rename it to `.pipeline/test-counts.prev.json` (overwriting any older snapshot). This guarantees the diff in step 7 always compares against a well-defined prior snapshot — no race conditions, no orphaned files.
6. Parse per-suite summary lines from `.pipeline/test.log` (the lines matching `^N examples, M failures(, K pending)?$` immediately above each `Test suite <name>: PASS/FAIL` marker). Emit a JSON object to `.pipeline/test-counts.json` keyed by suite name with fields `{examples, failures, pending}`.
7. If `.pipeline/test-counts.prev.json` exists (from step 5's rollover), diff against it and surface:
   - Any suite where `pending` increased → warning: "pending count grew for <suite>; review whether real tests were demoted to pending without a fixture being lifted"
   - Any suite where `pending` decreased without a corresponding test-file diff → finding: "pending count dropped for <suite> with no fixture work in this diff; suggests tests were deleted rather than satisfied"
   The check is a heuristic flag, not a refusal — the build-loop continues either way.
8. If exit (from step 4) is non-zero, surface the tail of `.pipeline/test.log` and exit non-zero.
9. If exit is zero, exit 0.

## Output

`.pipeline/test.log` and `.pipeline/test-counts.json` written; exit code propagated.

## Refusals

- `.pipeline/` missing → refuse: "pipeline not initialised; run phase 01 first".
- `nix` not on PATH → refuse: "nix not found; the repo's dev shell is required for cabal".
