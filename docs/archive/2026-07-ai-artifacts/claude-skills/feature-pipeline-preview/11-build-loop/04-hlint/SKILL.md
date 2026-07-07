> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

---
name: 04-hlint
description: Runs hlint on changed files and captures warnings for later surfacing in the PR body.
kind: leaf
executor: script
model: claude-haiku-4-5-20251001
---

# Hlint

Runs `hlint` on the changed Haskell files and writes the output to `.pipeline/hlint.log`. Hlint warnings are **collected, not gated** — this leaf does not fail the build loop on warnings. The PR-body step in phase 16 surfaces the log contents to the PR reviewer.

## Inputs

- The list of changed Haskell files from `git diff --name-only HEAD -- '*.hs' '*.lhs'` (the pathspec keeps an empty match exit-code 0).

## Plan

1. Compute the changed Haskell files list → verify: list captured (may be empty).
2. Run `hlint` on the list, redirecting stdout+stderr to `.pipeline/hlint.log` → verify: log written.
3. Always exit 0 — the leaf's job is to record the warnings, not to block on them → verify: caller sees exit 0 unless hlint is missing or the working tree is corrupt.

Assumptions:
- The build loop already gates on `cabal build all` and `cabal test`. Hlint is style guidance, not correctness, so it does not need to stop the loop.
- The PR-body step (phase 16.1) reads `.pipeline/hlint.log` and embeds any non-empty contents in the PR description so a human reviewer (or CI's hlint check) sees them.

If any assumption fails, refuse — do not guess.

## Steps

1. Compute changed files: `git diff --name-only HEAD -- '*.hs' '*.lhs'` (pathspec is empty-match safe — no `grep` pipe).
2. If the list is empty, write an empty `.pipeline/hlint.log` and exit 0.
3. Otherwise run: `nix develop --command hlint <files...> > .pipeline/hlint.log 2>&1 || true` — the trailing `|| true` keeps the leaf at exit 0 regardless of how many warnings hlint emitted.
4. If the log is non-empty, print a one-line summary to stdout (`hlint produced N warning(s); see .pipeline/hlint.log and the PR body`). Do not surface as failure.
5. Exit 0.

## Output

`.pipeline/hlint.log` written (possibly empty). Exit code is always 0 unless a hard environmental failure occurred (hlint missing, not in a git repo).

## Refusals

- `nix` not on PATH → refuse: "nix not found; the repo's dev shell is required for hlint" and exit non-zero. Environment failure, not a style warning.
- Repo not a git working tree → refuse: "not in a git repo" and exit non-zero.
