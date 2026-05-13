---
name: 03-hlint
description: Runs hlint as a final pass that captures warnings for the PR body without gating the pipeline.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Final Hlint

Runs `hlint` against the changed Haskell files one last time and writes the output to `.pipeline/hlint.log`. Hlint warnings are **collected, not gated** — this leaf does not fail the final-verify phase on warnings. The PR-body step in phase 16 surfaces the log contents to the PR reviewer.

## Inputs

- The changed file list from `git diff --name-only HEAD` (filtered to `.hs`).

## Plan

1. Compute the changed Haskell files list → verify: list captured.
2. Run `hlint` on the list, redirecting stdout+stderr to `.pipeline/hlint.log` (overwriting any earlier build-loop log so the final state is canonical) → verify: log written.
3. Always exit 0 — the leaf records warnings, the PR body surfaces them, and CI's own hlint check is the canonical gate at merge time → verify: caller sees exit 0 unless hlint is missing or the working tree is corrupt.

Assumptions:
- Phase 15's earlier steps (`01-build`, `02-test`) are the strict gates here; hlint is informational for the PR reviewer.
- `.pipeline/hlint.log` is canonical at PR-body time — it reflects the final state after fix-findings (phase 14).

If any assumption fails, refuse — do not guess.

## Steps

1. Compute changed files: `git diff --name-only HEAD | grep '\.hs$'`.
2. If the list is empty, write an empty `.pipeline/hlint.log` and exit 0.
3. Otherwise run: `nix develop --command hlint <files...> > .pipeline/hlint.log 2>&1 || true` — the trailing `|| true` keeps the leaf at exit 0 regardless of warning count.
4. If the log is non-empty, print a one-line summary to stdout (`hlint produced N warning(s); see .pipeline/hlint.log and the PR body`). Do not surface as failure.
5. Exit 0.

## Output

`.pipeline/hlint.log` written (possibly empty), reflecting the final-verify pass. Exit code is always 0 unless a hard environmental failure occurred.

## Refusals

- `nix` not on PATH → refuse: "nix not found; the repo's dev shell is required for hlint" and exit non-zero. Environment failure, not a style warning.
- Repo not a git working tree → refuse: "not in a git repo" and exit non-zero.
