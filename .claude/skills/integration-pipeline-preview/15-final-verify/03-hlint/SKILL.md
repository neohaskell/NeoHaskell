---
name: 03-hlint
description: Runs hlint as a final pass that captures warnings for the PR body without gating the pipeline.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Final Hlint

Runs `hlint` against the changed Haskell files one last time and writes the output to `.integration-pipeline/hlint.log`. Hlint warnings are **collected, not gated** — this leaf does not fail the final-verify phase on warnings. The PR-body step in phase 16 surfaces the log contents to the PR reviewer.

## Inputs

- The changed Haskell file list from a merge-base diff: `BASE=$(git merge-base HEAD origin/main 2>/dev/null || git merge-base HEAD main) && git diff --name-only "$BASE"...HEAD -- '*.hs' '*.lhs'`. Three-dot range so committed branch deltas show up in CI; pathspec so an empty match stays exit-0.

## Plan

1. Compute the changed Haskell files list → verify: list captured.
2. Run `hlint` on the list, redirecting stdout+stderr to `.integration-pipeline/hlint.log` (overwriting any earlier build-loop log so the final state is canonical) → verify: log written.
3. Always exit 0 — the leaf records warnings, the PR body surfaces them, and CI's own hlint check is the canonical gate at merge time. When hlint is unavailable, write a placeholder line (`hlint: missing`) to `.integration-pipeline/hlint.log` and still exit 0 so the phase-complete check (`.integration-pipeline/hlint.log` refreshed) has a single canonical signal → verify: caller sees exit 0, `.integration-pipeline/hlint.log` always exists.

Assumptions:
- Phase 15's earlier steps (`01-build`, `02-test`) are the strict gates here; hlint is informational for the PR reviewer.
- `.integration-pipeline/hlint.log` is canonical at PR-body time — it reflects the final state after fix-findings (phase 14).

If any assumption fails, refuse — do not guess.

## Steps

1. Compute changed files against the PR merge-base so CI (where everything is committed) sees the same delta as a local checkout: `BASE=$(git merge-base HEAD origin/main 2>/dev/null || git merge-base HEAD main); git diff --name-only "$BASE"...HEAD -- '*.hs' '*.lhs'`.
2. If the list is empty, write an empty `.integration-pipeline/hlint.log` and exit 0.
3. Otherwise run: `nix develop --command hlint <files...> > .integration-pipeline/hlint.log 2>&1 || true` — the trailing `|| true` keeps the leaf at exit 0 regardless of warning count.
4. If the log is non-empty, print a one-line summary to stdout (`hlint produced N warning(s); see .integration-pipeline/hlint.log and the PR body`). Do not surface as failure.
5. Exit 0.

## Output

`.integration-pipeline/hlint.log` written (possibly empty), reflecting the final-verify pass. Exit code is always 0 unless a hard environmental failure occurred.

## Refusals

- Repo not a git working tree → refuse: "not in a git repo" and exit non-zero. Without a git tree we cannot compute the changed-file list at all.

Note: `nix` or `hlint` missing is **not** a refusal here. Write `hlint: missing` to `.integration-pipeline/hlint.log` and exit 0 — phase-complete only cares that the log was refreshed.
