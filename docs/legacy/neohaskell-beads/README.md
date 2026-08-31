# Deprecated Beads change process

The Beads-backed change pipeline was superseded on 2026-08-31 by
[ADR-0076](../../decisions/0076-restore-resumable-change-pipeline.md). NeoHaskell
again uses the discoverable `neohaskell-pipeline` skill and local,
gitignored `.pipeline/state.json` as its authoritative resumable state.

This directory preserves the former formula, entry-point skills, and automatic
Beads hooks for historical diagnosis only. Nothing here is an active agent
entry point or runtime hook. The private `.beads` store and its remote pin are
retained in the repository so historical records remain inspectable; they do
not launch or govern changes.

## Archived layout

- `skills/neohaskell-change/` — five-step molecule playbook
- `skills/neohaskell-enqueue/` — dispatcher queue entry point
- `skills/beads/` — generic Beads agent skill
- `formulas/change.formula.toml` — five-step change formula
- `hooks/` — token accounting and private Dolt sync hooks
- `git-hooks/` — managed Beads Git hook wrappers
- `codex/` — Beads context hook configuration

Git history remains the source for the full cutover implementation. The last
behavioral reference before the cutover is commit
`2a3aaf242b4f24e442a51476aa890a1443f4fda2`; the `origin/main` audit boundary
is the parent of `eafc47c`, commit
`311ab50b560ee0f9f1b421c56a5ab376a050cef9`.
