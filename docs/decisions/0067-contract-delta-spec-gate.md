# ADR-0067: Contract-delta spec gate and resumable draft-PR flow

> Part of #715 — pipeline plan Phase 5 (spec gate + verification architecture).
>
> Restored as the active process by [ADR-0076](0076-restore-resumable-change-pipeline.md).
> ADR-0076 amends the approval transport: a trusted maintainer signal authorizes
> continuation, and the local `./dev pipeline approve` record is the
> machine-enforced gate.

## Status

Accepted

## Context

### Current State

Phases 0–4 built the generation substrate: a measured fast inner loop, a
three-layer dialect gate, lookup-based localization (`codemap/`), and API
knowledge delivery (`./dev api`, hot card, phrasebook). What was missing is
the *shape of a run*: baseline PRs took 2–3 hours and were wrong because the
first human checkpoint was a finished (wrong) diff. There was no mechanical
definition of "what this change promises", no cheap point for the maintainer
to redirect work, and no way to resume a half-done run without re-planning
(and re-diverging).

### Design Goals

1. **Exactly two human gates** — spec approval and PR review; every other
   checkpoint is mechanical (Nick's time is the scarce resource).
2. **The cheap gate comes first** — the maintainer approves a one-page
   contract before implementation exists, not a 40-file diff after.
3. **Machine-joinable specs** — the spec must mechanically derive the
   test-impact set, the design-review routing, and the drift check; prose
   promises rot, joins don't.
4. **Resume never re-plans** — a continued run picks up its recorded plan
   verbatim; divergence is parked visibly, never silently re-derived.

### GitHub Issue

- [#715: Continuous generation pipeline](https://github.com/neohaskell/NeoHaskell/issues/715)

## Decision

### 1. The spec is a contract delta, validated as data

`docs/changes/NNN-slug.md`, validated by `./dev spec-check` (CI: checks.yml
`spec` job). Machine-readable parts: a `yaml spec` header (`kind`, `touches:`
capability IDs from the closed ontology, ADR-trigger flags), a
`diff signatures` block promising the public-API delta in
`codemap/signatures/` vocabulary, and a criteria table (C1…Cn, each naming
its proving test and declaring `unit|integration|acceptance`).

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Free-form design doc | Rejected | not machine-joinable; rots; review cost scales with prose |
| Full event-model spec | Rejected | event models are for user apps (locked decision); internal changes are API/behavior contract deltas |
| Contract delta + criteria table | **Chosen** | joins mechanically to capabilities (reviews, test impact) and signatures (drift) |

Honesty is cross-checked, not trusted: a `-` line in the delta forces
`breaking: true`; trigger flags force a linked ADR; `TEMPLATE.md` is itself
a validated instance, so the template's machine-read parts (header keys, fence
infos, `## section` names, criteria cells) cannot drift from the validator.
Design-review records (`*-review.md`) share the directory but are not specs —
both validators skip them.

### 2. The gate is a draft PR; the continue signal is maintainer-only

The pipeline opens a draft PR containing only the spec (+ ADR, + red repro
test for bugs). Draft PRs run only the seconds-cheap checks (doctor, codemap,
lint, spec-check); the heavy build/test matrix is skipped until the PR
leaves draft (`ready_for_review` re-triggers it). A maintainer's explicit
signal in a trusted channel authorizes continuation. The orchestrator records
that signal with `./dev pipeline approve spec --by <who> --via <channel>`;
the resulting local state is the canonical, machine-enforced continuation
gate. A GitHub comment is optional communication only and cannot mutate the
persistent local pipeline state. The local operator environment is the trust
boundary: the approval record is an audit record, not authentication against a
process that already has write access to the checkout and gate implementation.

### 3. Resume state is a validated local contract

`.pipeline/state.json` (gitignored), manipulated only via `./dev pipeline`
(init/status/advance/set/approve/park/resume; schema-validated on every
save). Stage names are the telemetry schema v6 canon. Advancing past `spec`
without a recorded approval is mechanically refused. Parking requires a
label from the closed failure taxonomy.

### 4. Drift is checked at PR-ready, against the gated surface

`./dev spec-drift` verifies every promised `+` line exists in (and every
`-` line is absent from) `codemap/signatures/*.txt`. Because codemap-sync
(test.yml) proves committed signatures == code, checking the committed files
is checking the code. Runs in CI only for non-draft PRs — a draft is still
implementing its spec, drift there is expected.

### 5. Reviews are risk-tiered and design-time; measurement is nightly

`spec-check --plan` joins `touches:` with capability risk tags: intersecting
`security-sensitive`/`perf-sensitive` routes the run through the
corresponding design-review skill *after spec approval, before
implementation*; review records are committed next to the spec as
`NNN-slug.<kind>-review.md` (the compliance audit trail). Untagged specs skip
reviews entirely. That the record exists when routing fired is enforced at
PR-ready by `spec-check --reviews-pr` (checks.yml `spec` job), so the audit
artifact is gated, not merely prosed. Perf *measurement* is `./dev bench`
against `telemetry/bench-budgets.json`, nightly and never PR-blocking; budgets
start null (calibration) and are set from observed medians by the weekly review.

### 6. Expectations are protected in two layers

Changing recorded test expectations is exactly the action that must pass
through a human, so it is guarded twice. **Locally**, a PreToolUse hook
(`expectation-guard.py`, same self-test contract as the dialect guard) is the
fast teacher: it blocks edits that remove or reword existing expectation lines
unless the maintainer-authored, never-committed marker
`.claude/allow-expectation-edits` exists (no inline escape hatch), and it
fails loud-open on unparseable input rather than silently disabling itself.
**In CI** — the enforced backstop — the `expectations` job census-diffs the
committed test files against the merge base and blocks a net-removed/reworded
expectation unless a maintainer applied the `expectations-approved` PR label.
Operating on the committed result covers the rename / non-test-path /
Bash-mutation cases the local hook's path+payload matcher cannot, and the label
— unlike the local marker — is not agent-writable.

## Consequences

### Positive

- The maintainer redirects wrong work at the one-page stage, in minutes.
- Spec, reviews, tests, and drift check all join through the same two gated
  artifacts (capabilities.yaml, signatures) — no new vocabulary to rot.
- Parked runs carry labeled causes; every failure is trend-measurable.

### Negative

- Every pipeline change now pays the spec-writing overhead, even small ones
  (mitigated: empty contract delta is first-class; bug specs are one red test).
- Draft PRs get no heavy CI signal until ready — a spec whose implementation
  is doomed for build reasons is discovered later than it would be with
  always-on CI (accepted: that is what the fast local loop is for).

### Risks

- A process with local write access can forge the approval record or modify the
  gate itself. This pipeline does not defend against a compromised operator
  environment.
- Stage time-boxes are guesses until telemetry accumulates.

### Mitigations

- Only the trusted local orchestrator records approval after an explicit
  maintainer signal, including `--by` and `--via` audit fields. If hostile local
  processes enter the threat model, move authorization to an externally signed
  service under a new ADR; another local marker would not add security.
- Time-boxes live in the pipeline skill and are recalibrated at the weekly
  telemetry review (Phase 6).

## References

- [#715](https://github.com/neohaskell/NeoHaskell/issues/715)
- [Pipeline plan — Phase 5](../plans/2026-07-07-continuous-generation-pipeline-plan.md)
- [ADR-0066: Two-database API search](0066-two-database-api-search.md)
- [telemetry/SCHEMA.md](../../telemetry/SCHEMA.md)
