# ADR-0075: Change process v2 — five coarse steps, one human gate, verified auto-merge

> Supersedes the 20-step `change` formula (retired 2026-08-20). The full
> pre-reset beads state is preserved as a release asset on the private
> mirror: `NickSeagull/neohaskell-beads`, tag `beads-backup-2026-08-20`
> (sha256 `e02cdb3d1f5837cf4d5ab8481cfe6e8c0a5be7e8c8645196451dcd76c60b3d33`;
> restore by extracting over `.beads/` in a checkout and running
> `bd doctor`). Completes the arc of
> [ADR-0067](0067-contract-delta-spec-gate.md) →
> beads cutover → this rebuild. The retired pipeline skill remains at
> `docs/legacy/neohaskell-pipeline/SKILL.md` as historical reference.

> **Amended by
> [ADR-0076](0076-session-launched-change-process.md)** (2026-08-27): the
> steps, tiers, V1–V9 verdict and merge rule below all stand, but the
> execution substrate changed — runs are launched from a session via
> `/change`, not poured by a dispatcher daemon. The formula, the queue and the
> enqueue skill described here are retired. Note also that this process was
> never actually exercised: a missing `{{request}}` placeholder meant no
> molecule ever reached its spec agent with a request.

## Status

Accepted

## Context

The original `neohaskell-pipeline` skill was fast and reliable but required
the maintainer to attend every agent block. The first beads conversion fixed
the attendance problem and broke the speed: the 20-step formula
over-atomized the work (185 issues, 80 of them blocked, at reset time), and
every micro-step paid coordination overhead — queue latency, context
rebuilding, checker bounces — that the monolithic pipeline never paid.

Maximum verification per step is the right generalization for arbitrary
software. NeoHaskell is not arbitrary software: its bet is that correctness,
security and performance live in the **design of composable primitives**,
not in per-change review. And the project today has zero production users
and no user data — heavyweight security/perf design-review stages protect
data it does not have and load it does not serve. What the project needs
now is speed of iteration on primitives, with correctness verification
intact and security/perf reduced to a cheap concrete sanity pass rather
than a review ceremony.

## Decision

Wipe the beads state (backed up), and replace the 20-step formula with a
5-step molecule (`.beads/formulas/change.formula.toml`, playbook in
`.claude/skills/neohaskell-change/SKILL.md`):

1. **spec** (opus) — intake + binding localization + spec + draft PR, one pass.
2. **spec-approval** — the only unconditional human gate. Nick converses
   with Fable; Fable edits the spec with him and records the approval.
   Fable never executes pipeline work.
3. **build** (sonnet → opus on repeated failure) — plan + red-first tests +
   implementation in one continuous pass. Test discipline is unchanged:
   criteria tests red before code, expectation-guard enforced.
4. **verify** (opus, fresh agent) — the V1–V9 verdict: compile, tests
   red-on-base/green-on-head, full regression, spec-drift, dialect
   conformance, expectations intact, scope fence, lint, and V9: a
   security/perf **sanity pass** by a sonnet subagent with a fixed concrete
   checklist (trust-boundary validation, injection, resource cleanup,
   concurrency-stress tests where the contract is concurrent, unbounded
   growth) — max 2 iterations, never an ISO-style audit. Partition rule:
   whatever is deterministic runs as a deterministic tool in CI — secret
   scanning is the `secrets` gitleaks job in `checks.yml`, not an agent
   check — and agents verify only what needs judgment. Full correctness
   verification is retained deliberately — that was never the bar being
   lowered.
5. **pr** (haiku + sonnet) — CI-settle loop, then the merge rule below.

**Merge rule:** `breaking: false` and no public-API surface beyond the
approved delta → auto-merge on green CI + complete V1–V9 verdict
(`merged-by: auto` recorded). `breaking: true` or ADR attached → GATE 2,
only Nick merges. The independent opus verdict is what makes an unattended
merge defensible; [ADR-0074](0074-dependabot-auto-merge.md)'s
revert-candidate guard is what makes it recoverable.

**Failure policy:** per-step time-boxes (45/—/90/30/45 min), retry once →
escalate model → park with labeled report. Class-fix (asset-delta) is
required only when the same failure label occurs a second time — learn
from patterns, not from every stumble.

**Telemetry:** beads-native only. Stage timings come from bead status
transitions; per-stage token cost from the dispatcher's
`bd-token-tracking` hook. runs.jsonl, golden archives, consult-logging and
the weekly miner are retired.

**Sync:** beads data syncs to the private mirror
`NickSeagull/neohaskell-beads` (`refs/dolt/data`), never to the public
origin — agents write beads candidly (pasted logs, cut corners, memories)
and that channel publishes without human review.

## Consequences

- One human wait per non-breaking change (the spec conversation) instead
  of two; breaking changes keep both gates.
- Downscoped, explicitly and reversibly: the security and perf
  design-review stages become the V9 sanity checklist (restore the full
  reviews when the project holds real user data or serves real load).
  Dropped outright: the docs sub-pipeline (on hold until the docs system
  exists) and manual telemetry.
- The dialect-conformance check (V5) plus the V9 sanity pass inherit the
  load the review stages carried: if primitives stop being the
  correctness boundary, this ADR's premise fails and the full reviews
  come back.
- Agents gain merge authority on the auto-merge path — a first. The
  compensating controls are the fresh-agent verdict, the breaking flag
  honesty check at spec-approval (Nick sees the flag he is approving), and
  post-merge revert-candidate guard.
- Never split the 5 steps into finer beads; over-atomization is the
  failure mode this ADR exists to prevent.
