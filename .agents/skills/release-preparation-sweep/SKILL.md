---
name: release-preparation-sweep
description: Autonomously combine NeoHaskell's ordinary generated release preparations, repair failing CI, merge the verified candidate, and verify one published release when explicitly asked to run a release sweep.
---

# Release preparation PR sweep

Use this skill only for an explicit request to sweep ordinary generated release
preparations in `neohaskell/NeoHaskell`. It owns the run through public release
verification. Use [NeoHaskell release](../neohaskell-release/SKILL.md) for a
failed publication or recovery PR, then return to this run.

The result is one cumulative generated preparation from current `main`. Its
single release attempt and changelog entry must cover every verified older
preparation, including fragments revised on `main`. Prove that coverage before
closing older PRs. Close each proven older PR once the cumulative PR is open,
before waiting for its CI or merge. Do not merge old preparation branches
together. Each already contains an obsolete release plan, while the current
generator collects unreleased fragments from `main`.

An explicit request to run the sweep authorizes the scoped actions in the
[operating procedure](references/operating-procedure.md): preparation dispatch,
closure of proven older PRs, CI diagnosis and source repair in a separate PR,
one evidence-based CI rerun, gated merges, and publication recovery. Continue
until the public release passes verification or a concrete external blocker
prevents progress. Never bypass repository rules, force-push, edit generated
release files by hand, create a bootstrap preparation, or mutate release history
to make a candidate fit.

Read the operating procedure before acting. It defines the live provenance
checks, repeatable gates, race handling, publication readback, and report. The
pure [decision model](scripts/sweep_model.py) and [behavior tests](tests/test_sweep_model.py)
document the expected reducer behavior; they do not query GitHub and cannot
replace live evidence.
