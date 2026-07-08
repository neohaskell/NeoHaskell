# ADR-0069: Security design-review records are local-only, never pushed

> Follows [ADR-0067](0067-contract-delta-spec-gate.md) (spec gate) and
> [ADR-0068](0068-failure-asset-delta-and-learning-loop.md) (Phase 6). Amends the
> Phase-5 design-review record policy.

## Status

Accepted

## Context

The risk-tiered design reviews (Phase 5) produce a committed record next to the
spec: `NNN-slug.security-review.md` and `NNN-slug.perf-review.md`. The original
policy committed **both** to the PR branch as the compliance audit trail, gated
at PR-ready by `./dev spec-check --reviews-pr` (a CI step in `checks.yml`).

`neohaskell/NeoHaskell` is a **public** repository. A security design review is,
by construction, a map of a change's attack surface: the trust boundaries it
touches, the STRIDE questions asked, which vectors were considered and *why they
were judged non-exploitable*. Committing that to public git history hands an
attacker a curated, per-change threat model — including, for any review that
does find a real issue, a description of the weakness before (or after) it is
fixed. The audit-trail benefit does not justify publishing the attack surface.

Perf reviews carry no such exposure — they discuss allocation, complexity, and
benchmark budgets, not how to break the system.

## Decision

**Security review records are local-only; perf review records stay committed.**

- `docs/changes/*.security-review.md` is **gitignored** — written on the
  developer's working tree, never committed or pushed. The audit trail lives on
  the maintainer's machine / secure store, outside public git history.
- The review is still **mandatory and enforced**, just not in CI:
  - `./dev spec-check --reviews-local` (dev machine) requires the gitignored
    security record to exist on disk (and backlink its spec) before the pipeline
    flips a PR to ready.
  - `./dev spec-check --reviews-pr` (CI) now gates only **committed** review
    kinds (perf). It can no longer see — and therefore no longer requires — a
    security record, which is intentionally absent from CI's checkout.
- `speclib.LOCAL_ONLY_REVIEW_KINDS = ("security",)` is the single source of the
  split; the two gates share one core (`missing_review_records(..., kinds=…)`).

## Consequences

- **Attack surface stays private.** The primary goal.
- **Weaker public audit trail for security.** CI can prove a *perf* review
  happened; it cannot prove a *security* review happened, because the artifact
  never reaches CI. The guarantee moves to the local gate + the pipeline state
  (`.pipeline/state.json` records the design-review stage) + the maintainer's
  local records. This is the accepted trade: privacy over public provability.
- **A pushed security review is a policy violation**, not just untidy. If one is
  ever committed, purge it from history (`git filter-branch`/`filter-repo` +
  force-push the unmerged branch) — the gitignore prevents recurrence.
- Migration: the first real security review (change 001, issue #713) was pushed
  before this ADR. It was untracked and gitignored, so it is absent from the
  PR's tip — a **squash-merge** collapses the branch to one commit whose diff has
  no security record, keeping `main` clean. (An intermediate branch commit still
  holds it until the branch is deleted; for a higher-sensitivity review, purge
  with `git filter-repo`/`filter-branch` + force-push before merge instead.)
