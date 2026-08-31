# ADR-0076: Restore the resumable contract-delta change pipeline

> Supersedes [ADR-0075](0075-change-process-v2.md) and restores the operating
> model of [ADR-0067](0067-contract-delta-spec-gate.md), while retaining the
> later security and CI hardening.

## Status

Accepted

## Context

ADR-0075 replaced the local pipeline state machine with a Beads-backed queue,
a five-step formula, dispatcher-owned execution, and Beads-native telemetry.
That cutover made two different concerns inseparable: durable issue storage and
the execution contract for a change. It also moved the pipeline playbook out
of agent discovery and left the previously reliable `scripts/pipeline-state`
implementation present but explicitly deprecated.

The requested rollback is behavioral, not a repository reset. NeoHaskell has
continued to improve its CI, secret scanning, release checks, local-only
security-review policy, and expectation guard since the Beads cutover. Reverting
the whole tree to an old commit would discard those independent controls.

## Decision

Restore the contract-delta pipeline as the sole active change process:

1. `.claude/skills/neohaskell-pipeline/SKILL.md` is again the discoverable
   end-to-end playbook.
2. `.pipeline/state.json` is the authoritative, local, gitignored resume state,
   manipulated only through `./dev pipeline`.
3. The process again has two human touchpoints: contract approval at the draft
   PR and final maintainer review. A maintainer's explicit signal in a trusted
   channel supplies authorization; `./dev pipeline approve ... --by ... --via
   ...` records that authorization and is the machine-enforced gate. A GitHub
   comment can be evidence, but no remote service mutates local pipeline state.
4. Manual pipeline telemetry, failure labels, class fixes, and the weekly
   retrospective operate as defined by ADR-0068 and `telemetry/SCHEMA.md`.
5. Risk-tiered design reviews remain governed by ADR-0069: performance records
   are committed; security records stay local and gitignored.
6. The current expectation override remains
   `.claude/allow-expectation-edits`; restoring the pipeline does not roll back
   that later hardening.

All repository artifacts specific to the Beads process are deleted rather than
archived: the `.beads` store, formulas, skills, role agents, hooks, Codex
configuration, ignore rules, merge driver, and Nix input. Git history is the
single recovery source. The external NeoHaskell dispatcher project was paused
on 2026-08-31; that host-level state is outside this repository.

`scripts/process-check.sh` remains a deterministic CI gate, retargeted to prove
that exactly one pipeline is active: the `.pipeline` skill is discoverable,
superseded queue-backed execution artifacts are absent, and the governing
documents and ADR statuses agree.

## Preserved controls

This restoration does not weaken or rename the current repository gates:

- deterministic gitleaks scanning;
- spec, design-review, criterion-test, expectation, changelog, and spec-drift
  checks;
- full Haskell, Neo CLI, installer, testbed, and post-merge workflows;
- PR-comment allowlist filtering;
- Dependabot and required-check policy from ADR-0074;
- codemap, API-discovery, and dialect enforcement;
- telemetry union merge rules;
- generated-file source-of-truth boundaries.

## Consequences

### Positive

- A run resumes from one local validated state instead of rebuilding context
  from queue steps.
- Agents encounter one authoritative pipeline entry point.
- The draft contract gate and full risk-tiered review flow return without
  losing post-cutover hardening.
- No superseded queue or store artifacts remain in the current tree; Git
  history can recover them if ever needed.

### Negative

- Long-running orchestration again depends on preserving the local checkout and
  `.pipeline/state.json`.
- Maintainer authorization must be recorded honestly in local state; the state
  file proves the gate was recorded, not that the human signal was genuine.
- The repository cannot enforce the external dispatcher's paused state; host
  operators must keep it paused unless a future ADR deliberately restores it.

## References

- [ADR-0067: Contract-delta spec gate and resumable draft-PR flow](0067-contract-delta-spec-gate.md)
- [ADR-0068: Failure→asset-delta protocol and the learning loop](0068-failure-asset-delta-and-learning-loop.md)
- [ADR-0069: Security design-review records are local-only](0069-security-reviews-are-local.md)
- [ADR-0074: Dependabot auto-merge](0074-dependabot-auto-merge.md)
- [ADR-0075: Change process v2](0075-change-process-v2.md)
