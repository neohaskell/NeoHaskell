---
name: release-preparation-sweep
description: Reconcile and safely complete NeoHaskell's ordinary generated release preparation PRs when explicitly asked to run a release sweep.
---

# Release preparation PR sweep

Use this skill only for an explicit request to sweep ordinary generated release
preparations in `neohaskell/NeoHaskell`. It owns generated `platform` release
PRs. Recovery PRs and release repair belong to [NeoHaskell release](../neohaskell-release/SKILL.md).

An explicit request to run the sweep authorizes only the scoped actions in the
[operating procedure](references/operating-procedure.md): one reconciled `auto`
dispatch when needed, approval of a selected candidate's exact Action-required
run, a gated squash merge, and closure of individually proven stale PRs. Never
bypass repository rules, force-push, edit generated release files by hand,
create a bootstrap preparation, or mutate release history to make a candidate
fit.

Read the operating procedure before acting. It defines the live provenance
checks, repeatable gates, race handling, publication readback, and report. The
pure [decision model](scripts/sweep_model.py) and [behavior tests](tests/test_sweep_model.py)
document the expected reducer behavior; they do not query GitHub and cannot
replace live evidence.
