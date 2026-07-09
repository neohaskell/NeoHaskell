# ADR-0068: Failure→asset-delta protocol and the learning loop

> Part of #715 — pipeline plan Phase 6 (release tail + learning loop).

## Status

Accepted

## Context

Phases 1–5 built the generation substrate and the spec-gated change flow. What
Phase 6 adds is the *learning loop*: the plan's promise that "every failure makes
the pipeline better." Two failure modes have to be closed by construction:

1. **Instance-only fixes.** A run that failed because an alias was missing, an API
   was invented, or a localization was wrong could be retried and shipped without
   ever fixing the *class* — leaving the next run to hit the same wall. The plan
   names this the failure→asset-delta protocol (task 4): the fix for the instance
   and the fix for the class ship together.
2. **Unmeasured drift.** Recommendations for improving the pipeline (new aliases,
   phrasebook entries, hlint rules…) are cheap to assert and expensive to verify.
   Without a contract they accumulate as unvalidated opinion — the exact rot the
   constitution exists to prevent.

Today (pre-Phase-6) the protocol is **prose only**: `neohaskell-pipeline`'s
SKILL.md says "the asset fix ships with the retry" but nothing enforces it, and
there is no schema for recommendations or usage.

## Decision

### 1. A non-`ok` run must record an asset delta

`./dev telemetry finish --outcome {failed,parked}` now **requires**
`--asset-delta <type>:<destination>[:<ref>]`, validated against a closed
**delta-type taxonomy v1**: `alias | extension-point | phrasebook | hot-card |
hlint-rule | hook | cli-utility | skill-edit | telemetry-label | PRUNE | none`.
Each type maps to a real destination file (telemetry/SCHEMA.md). `none:<reason>`
is the justified escape — a failure with genuinely no class-fix must say so, not
stay silent. The `asset_delta` field lands in the run's `runs.jsonl` line, so the
protocol is measurable, not aspirational.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Prose in the skill (status quo) | Rejected | "a rule without a gate is a wish" — nothing counts instance-only fixes |
| Block the retry until an asset PR merges | Rejected | couples an in-flight run to a second PR's review latency; brittle |
| Record the delta at run close (chosen) | **Chosen** | the emitter is the seam every run passes through; the delta is data, trend-measurable, and the PR link lives in `ref` |

### 2. Usage accounting powers removal, not just addition

`./dev telemetry consult --asset <kind>:<name>` records what a run actually looked at
(`assets_consulted`, schema v3). An asset no run consults is a PRUNE candidate —
the loop can *shrink* the surface, not only grow it. Descriptive, not a gate.

### 3. Recommendations are a validated record, not free advice

The retrospective miner (`neohaskell-retrospective-miner` skill, run via
`./dev retrospect`) emits ≤5 recommendations/week to `telemetry/recommendations.jsonl`
under a closed contract (telemetry/SCHEMA.md): each needs a cited friction event,
evidence across **≥2 independent runs**, a measured `estimated_saving_s`, and a
`delta_type` from the taxonomy. `scripts/retrospect` validates the contract before
a line lands; out-of-taxonomy proposals (new asset kinds, new CLI utilities)
escalate to the maintainer — they are ontology decisions, never automatic. An
implemented delta is `validated` against its claimed metric over subsequent runs;
no movement makes it a PRUNE candidate in turn.

### 4. The kill switch is maintainer-triggered, notify-only

The definition-of-done gate (task 1) flags a post-merge suite failure on `main` as
a **revert-candidate** (label + notification); it never auto-reverts. The revert
itself (`./dev revert` + `.github/workflows/revert.yml`) fires only on a maintainer
label/comment, reusing `claude.yml`'s OWNER/MEMBER author-association check. A
flaky post-merge check must not churn good work; a human owns the revert decision.

## Consequences

### Positive

- Every failure leaves the pipeline measurably better or explicitly says why not.
- Recommendations are falsifiable: cited, measured, and validated over time.
- The asset surface can shrink (PRUNE), not just grow — rot has a removal path.

### Negative

- Every failed/parked run now pays the cost of naming a class-fix (mitigated:
  `none:<reason>` is first-class; the delta is one flag, not a second PR).

### Risks / Mitigations

- The miner and weekly review need **real runs**; there are none yet. The tooling
  ships now, self-tested on fixtures; activation (first weekly review on real data,
  first miner report, archive sunset) is a checklist gated on accumulated runs.

## Archive-sunset checklist (task 7, deferred)

`docs/archive/2026-07-ai-artifacts/` is deleted (dedicated PR, maintainer-approved)
only once **all** hold: Phase 5 + Phase 6 exit criteria met; one real feature and
one real bug shipped end-to-end through the pipeline; `runs.jsonl` carries those
real runs. `git mv` preserved history, so deletion loses nothing.

## Amendment (2026-07-09): successful runs also feed the loop (schema v4)

The original protocol captured a class-fix only on a **failed/parked** close
(`--asset-delta`, enforced). The first real runs exposed a blind spot: an `ok`
run can *itself improve the pipeline* — during #713 the run shipped a
changelog-generator fix, the security-reviews-local policy (ADR-0069), and a
telemetry-doc correction, none of which the loop recorded, because `asset_delta`
is null on success. "Every failure makes the pipeline better" missed "every
success can too."

Fix (schema **v4**): a new `improvements` field (list of `{type, destination,
ref}`, same taxonomy as `asset_delta` minus `none`) records class-fixes shipped
by any run, via optional repeatable `./dev telemetry finish … --improvement
<type>:<dest>`. `./dev retrospect` surfaces them ("Improvements shipped") — a
destination recurring there is a proven friction point for the miner. The
failure-path `--asset-delta` enforcement is unchanged.

## References

- [#715](https://github.com/neohaskell/NeoHaskell/issues/715)
- [Pipeline plan — Phase 6](../plans/2026-07-07-continuous-generation-pipeline-plan.md)
- [ADR-0067: Contract-delta spec gate](0067-contract-delta-spec-gate.md)
- [telemetry/SCHEMA.md](../../telemetry/SCHEMA.md)
