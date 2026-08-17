# ADR-0075: Darwin cache population and a requireable Darwin PR gate

> Builds on [ADR-0072](0072-neo-cutover-and-onboarding-slo.md) (the clean-machine
> onboarding SLO whose macOS leg this unblocks) and follows the credential
> isolation precedent set by [ADR-0070](0070-maintainer-codemap-regeneration.md)
> (no write credential coexists with untrusted code).

## Status

Accepted

## Context

`neohaskell.cachix.org` is populated from `ubuntu-latest` only. Both
token-holding jobs — `cachix-push.yml` (the dev shell) and `neo-ci.yml`'s
`cache-populate` (the released generated-project closure) — run on Linux, so no
`aarch64-darwin` path has ever been pushed. Two consequences, reported
separately and caused by the same absence:

1. `test-macos.yml` builds the entire aarch64-darwin dev-shell closure on every
   push to `main`, reads from the cache, and discards what it produced
   (issue #795). Every `flake.lock` bump invalidates the cache wholesale, so
   raising the flake-update cadence — the actual goal of #795 — would make
   Apple Silicon contributors rebuild GHC more often, not less.
2. The clean-machine onboarding SLO reaches the cache but substitutes nothing
   (`cachix.reachable=true`, `cachix.observed_use=false`) and exhausts its
   600-second deadline during `neo --ci test` on macOS ARM (issue #785).

`test-macos.yml` also runs on `push` to `main` only, so a toolchain bump that
breaks Darwin is discovered after it lands. That rules out auto-merging
flake-update PRs, which is the dependency #795 states explicitly.

Three forces shape the answer:

- **GitHub context rules.** `secrets` is not readable from a step-level `if:`,
  so the repo's honest-skip idiom (`env.CACHIX_AUTH_TOKEN == ''`) requires a
  **job-level** `env:`. On a same-repository pull request that would place a live
  cache-write credential in the same job as a contributor's build.
- **The repo already decided this once.** `scripts/workflow-check`'s
  `check_release_rehearsal` *fails* if the PR-reachable `consumer-contract` job
  so much as mentions `cachix`, and `cache-populate` exists as an isolated,
  trusted-push-only job for exactly this reason.
- **`AGENTS.md`: a CI gate that is not a required check is decoration.** A
  `paths:`-filtered job is *skipped, not reported*, on unrelated PRs, so requiring
  it in branch protection leaves the check permanently pending.

## Decision

**1. Darwin cache population lands as a separate trusted-push-only job, not as a
step inside the test matrix.** `test-macos.yml` gains `cachix-push-macos`, gated
at job level on `push` + exact `refs/heads/main` + same repository, holding the
job-level `CACHIX_AUTH_TOKEN`, skipping honestly when it is empty, and realizing
the closure itself so the `cachix-action` post-hook pushes it. The existing
`tests-macos` job stays token-free and gains pull-request coverage.

The cost is a duplicated build: separate jobs mean separate runners and separate
Nix stores, so the push job cannot push what a test leg built. We accept one
extra macOS build per qualifying push to `main` — the same shape `cachix-push.yml`
already pays on Linux — because the alternative is a live cache-write credential
in a job that executes pull-request-controlled code. It deliberately does not
`needs:` the test job: the pushed paths are content-addressed derivations of the
flake, identical whether or not the suites pass, so serializing would double
warm-up latency for no integrity gain.

**2. Darwin PR gating uses the `changes` + `if: always()` aggregate pattern, so
it can actually be required.** The `pull_request` trigger carries no
workflow-level `paths:`; a cheap `changes` job diff-scopes `flake.nix`,
`flake.lock`, `nix/**` and the workflow file itself; and `test-macos-gate` runs
`if: always()`, needs the matrix job, and accepts a skip only for a draft or an
untouched surface. This is the third instance of the pattern (`neo-ci-gate`,
`installer-ci-gate`). `core/**` is deliberately excluded from PR scope: a Darwin
matrix on every Haskell PR is a large recurring cost for a signal the Linux
`Test` gate already provides. Darwin PR coverage is scoped to what only Darwin
can catch.

Marking `test-macos-gate` required in branch protection remains a maintainer
repo-settings action, called out on the PR rather than assumed.

**3. The released-consumer closure is covered by extending `cache-populate` with
a matrix leg, not by a second workflow.** One job, one trust guard, one place
where the token lives. Both verbs are retained per leg: `./dev
neo-consumer-contract` (the exact-checkout closure) *and* `./dev cache-prime`
(the default released closure the SLO actually substitutes). `fail-fast: false`
keeps a Darwin failure from cancelling the Linux leg, and the job stays outside
`neo-ci-gate`'s `needs` — it is a best-effort warmer, never a merge blocker.

**4. The new credential surface is frozen mechanically.** `test-macos.yml` is
referenced nowhere else in the repo and is about to hold a secret; every other
token-bearing workflow already has a freeze check. `scripts/workflow-check` gains
`check_test_macos`, and `check_cache_populate` gains matrix-coverage assertions,
each with a positive fixture and a mutation fixture. Without this, the one
workflow holding a credential would be the one workflow nothing verifies.

## Consequences

**Positive.** A clean `nix develop` on Apple Silicon substitutes instead of
compiling GHC. A toolchain bump re-warms the Darwin cache instead of
invalidating it permanently (the push `paths:` gain `flake.nix` and `flake.lock`,
without which the one event that empties the cache is the one event that never
refills it). A Darwin regression surfaces on the PR that causes it, which is the
precondition nh-5dz / #810 needs before automating flake updates. The macOS ARM
onboarding SLO gets a populated cache to substitute from.

**Negative, accepted.** One extra macOS build per qualifying `main` push and one
macOS `cache-populate` leg per trusted push. Cachix storage grows by a full
aarch64-darwin closure per toolchain bump — retention is a maintainer operational
item. A flake-bump PR still builds against a cold Darwin cache, no worse than
today's push behaviour since `cache.iog.io` still substitutes most of GHC.

**Known residual.** `cache-prime` runs the `neo` built from the branch, which
pins the branch's compatibility revision, while the SLO measures a *released*
`neo` pinned to the revision embedded in its tag. If those differ, the primed
paths are not the paths the SLO asks for and `observed_use` stays `false` on
both platforms regardless of how much is pushed. That is a distinct defect with
its own fix (prime from the released artifact, or re-cut a release) and is
tracked separately rather than absorbed here.

**Routing gap.** `ci-cd` carries no `security-sensitive` tag in
`codemap/capabilities.yaml`, so `./dev spec-check --plan` routed no design review
for a change that moves a cache-write credential onto a new platform. The review
was requested manually (local-only record, [ADR-0069](0069-security-reviews-are-local.md));
tagging `ci-cd` so this routing is automatic is filed as a follow-up, since it
re-routes every future `ci-cd` spec.

## References

- Spec: [`docs/changes/007-darwin-cache-population-and-pr-gating.md`](../changes/007-darwin-cache-population-and-pr-gating.md)
- Issues: neohaskell/NeoHaskell#795, neohaskell/NeoHaskell#785
- Follow-up: nh-5dz / neohaskell/NeoHaskell#810 (scheduled flake input updates)
