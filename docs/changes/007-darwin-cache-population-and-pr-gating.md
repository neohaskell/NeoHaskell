# Change 007: Populate the aarch64-darwin caches and gate pull requests on Darwin

Nick wants the macOS ARM closure that CI already builds to stop being thrown
away, and a toolchain change to be validated on Darwin **before** it lands.
Today `neohaskell.cachix.org` is populated from `ubuntu-latest` only
(`cachix-push.yml` and `neo-ci.yml` are both Linux), while `test-macos.yml`
builds the whole aarch64-darwin dev-shell closure on every push to `main`,
reads from the cache, and discards everything it produced — and it never runs on
a pull request at all. The same Linux-only population is why the clean-machine
onboarding SLO reports `cachix.observed_use=false` and burns its 600-second
deadline on macOS ARM (#785). This change closes both halves of that gap in one
coherent caching move: the **dev-shell** closure (contributors running `nix
develop`) and the **released-`neo` consumer** closure (a user onboarding from a
published release), plus the Darwin PR signal that makes raising the flake-update
cadence safe (#795 steps 1–2). Scheduled flake updates themselves (#795 steps
3–4) stay with nh-5dz / #810.

```yaml spec
issue: issue#795
kind: feature
touches: [ci-cd, dev-pipeline]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

Infrastructure only. No Haskell surface is touched — no `core/`, `testbed/`,
`integrations/` or `neo/` source file changes, so no `codemap/signatures/` line
is added or removed and `./dev spec-drift` is trivially green. The promised
signature diff is therefore empty (a first-class spec per `TEMPLATE.md`, not a
degenerate one).

```diff signatures
```

The delta that *does* exist is a CI contract, stated here in the same
promise-shaped vocabulary:

- `+ .github/workflows/test-macos.yml`: a trusted-push-only `cachix-push-macos`
  job that pushes the aarch64-darwin dev-shell closure to
  `neohaskell.cachix.org`.
- `+ .github/workflows/test-macos.yml`: a `pull_request` trigger with **no
  workflow-level `paths:` filter**, per-PR diff scoping in a `changes` job, and
  an `if: always()` aggregate `test-macos-gate` job that is safe to require in
  branch protection.
- `+ .github/workflows/neo-ci.yml`: an `aarch64-apple-darwin` leg on the existing
  trusted `cache-populate` job (matrix), retaining **both** `./dev
  neo-consumer-contract` and `./dev cache-prime` per leg.
- `+ scripts/workflow-check`: `check_test_macos()` registered in `run()`, and an
  extended `check_cache_populate()` — the mechanical freeze for every clause
  above.
- `= .github/workflows/cachix-push.yml`, `neo-onboarding-slo.yml`,
  `scripts/onboarding-slo`, `scripts/cache-prime`, `scripts/neo-consumer-contract`,
  `scripts/neo-release`, `flake.nix`, `flake.lock`: unchanged.

## Root cause (#785 AC1)

No aarch64-darwin path has ever been pushed to `neohaskell.cachix.org`. Both
token-holding jobs are `runs-on: ubuntu-latest` — `cachix-push.yml` (dev shell)
and `neo-ci.yml`'s `cache-populate` (released consumer closure). A macOS ARM
runner therefore reaches the cache (`cachix.reachable=true`) and finds nothing
for its system (`cachix.observed_use=false`), so `neo --ci test` builds the whole
closure from source and blows the 600 s deadline. `test-macos.yml` produces the
missing dev-shell half of that closure on every push to `main` and has no
`cachix/cachix-action` step, so it is discarded. There is no second cause on the
macOS side: the population is simply absent.

One residual cause is **not** fixed by pushing more paths and is called out
under **F-9** below: the released `neo` generates a project pinned to *its*
embedded compatibility revision, while `cache-prime` runs the `neo` built from
the branch under test. If those revisions differ, the primed closure is not the
closure the SLO substitutes, on either platform.

## Design

### Surface 1 — `test-macos.yml`: push the dev-shell closure, and gate PRs

**Trust model (the crux).** The token must never be materialized in a job that
executes pull-request-controlled code. GitHub's context rules force the shape:
`secrets` is not readable from a step-level `if:`, so the repo's honest-skip
idiom (`env.CACHIX_AUTH_TOKEN == ''`) requires a **job-level** `env:` — which on
a same-repository PR would place a live cache-write credential next to a
contributor's `cabal build`. The repo already answers this exact question twice
(`neo-ci.yml`'s isolated `cache-populate`; `check_release_rehearsal` *errors* if
the PR-reachable `consumer-contract` so much as mentions `cachix`). This change
follows that precedent rather than inventing a third answer:

- **`tests-macos`** (the existing matrix job) runs on `push` **and**
  `pull_request`, holds **no secret**, and references neither `cachix` nor
  `CACHIX_AUTH_TOKEN`.
- **`cachix-push-macos`** (new) is gated at **job level** on
  `github.event_name == 'push' && github.ref == 'refs/heads/main' &&
  github.repository == 'neohaskell/NeoHaskell'`, holds the job-level
  `CACHIX_AUTH_TOKEN`, skips honestly when it is empty, sets up
  `cachix/cachix-action@v17` (`name: neohaskell`) **before** realizing the
  closure with the same command `cachix-push.yml` uses (`nix develop --command
  cabal build all --disable-documentation`), and lets the action's post-hook push
  what that built.

The accepted cost of the split: `cachix-push-macos` re-realizes the closure
rather than pushing exactly what a `tests-macos` leg built, because separate jobs
run on separate runners with separate Nix stores. That is one extra macOS build
per push to `main` — the same shape `cachix-push.yml` already pays on Linux —
in exchange for a credential that is *structurally* unreachable from PR code.
It deliberately does **not** `needs: tests-macos`: the store paths are
content-addressed derivations of the flake, identical whether or not the test
suites pass, so serializing behind the tests would double the warm-up latency
for no integrity gain.

**Push `paths:` gain `flake.nix` and `flake.lock`** alongside the existing
`core/**` and `nix/**`. Without this the one event that invalidates the whole
Darwin cache — a merged toolchain bump — is the one event that does not
repopulate it, which is precisely the failure mode #795 is about.

**PR gating that can actually be required** (`AGENTS.md`: "a CI gate that is not
a required check is decoration"). A `paths:`-filtered job is *skipped, not
reported*, on unrelated PRs, so requiring it in branch protection leaves the
check pending forever. The `pull_request` trigger therefore carries **no
workflow-level `paths:`**; scoping moves into a cheap `changes` job that diffs
against the actual base ref and sets `touched` for `flake.nix`, `flake.lock`,
`nix/**` and `.github/workflows/test-macos.yml`; `tests-macos` runs only when
`touched == 'true'` and the PR is not a draft; and `test-macos-gate` runs
`if: always()`, `needs: [changes, tests-macos]`, and accepts a skip **only**
when the PR is a draft or nothing relevant was touched. This is the
`neo-ci-gate` / `installer-ci-gate` pattern, third instance.

`core/**` is intentionally **not** in the PR scope: a Darwin matrix on every
Haskell PR is a large recurring cost for a signal the Linux `Test` gate already
gives. Darwin PR coverage is scoped to what only Darwin can catch — toolchain
and Nix changes.

**Substitution evidence (#795 AC2), report-only.** `tests-macos` gains a step
that runs `nix develop --command true`, tees the log, counts lines matching
`copying path … from https://neohaskell.cachix.org`, and writes the count plus
the matched store paths to `$GITHUB_STEP_SUMMARY`. It never fails the job: a
cold cache after a lock bump is legitimate, and a fail-closed evidence step would
turn a correct state into a red gate. It exists so AC2 is read off a summary
instead of eyeballed in a 40-minute log.

**Concurrency.** `group: test-macos-${{ github.workflow }}-${{ github.ref }}`,
`cancel-in-progress: ${{ github.event_name == 'pull_request' }}` — PR runs
supersede themselves, and a `main` push that is warming the cache is never
cancelled by the next push.

### Surface 2 — `neo-ci.yml`: an aarch64-darwin leg on `cache-populate`

The released-consumer half of #785. `cache-populate` becomes a matrix over
`{ubuntu-latest, x86_64-unknown-linux-gnu}` and `{macos-latest,
aarch64-apple-darwin}` with `fail-fast: false`, `runs-on: ${{ matrix.runner }}`,
`REHEARSAL_TARGET: ${{ matrix.target }}`, `timeout-minutes: ${{ matrix.timeout }}`
(180 on Darwin, 150 on Linux), and `dtolnay/rust-toolchain` receiving
`targets: ${{ matrix.target }}`.

Everything that makes the job trustworthy is untouched and must stay literally
matchable by `check_cache_populate`: push-only, exact-ref (`refs/heads/main`,
`refs/heads/integration/neo-monorepo`), same-repository, never `pull_request`,
honest skip on an empty token, every Cachix/contract step gated on
`env.CACHIX_AUTH_TOKEN != ''`. **Both** verbs are retained per leg —
`./dev neo-consumer-contract` (exact-checkout closure) *and* `./dev cache-prime`
(the default released closure the SLO actually substitutes). Dropping either on
the Darwin leg reproduces the bug being fixed.

The job stays **out of `neo-ci-gate`'s `needs`**: it is a best-effort warmer, not
a merge blocker, and `fail-fast: false` keeps a Darwin failure from cancelling
the Linux leg that works today.

The Darwin leg runs `./dev neo-consumer-contract --self-test` first as a cheap
canary, before the ~2-hour build, so a portability or shell-compatibility failure
surfaces in seconds rather than at the timeout.

### Surface 3 — `scripts/workflow-check`: freeze the new credential surface

`test-macos.yml` is referenced by nothing else in the repo and is about to hold a
credential; every other token-bearing workflow has a freeze check
(`check_cache_populate`, `check_neo_release`, `check_onboarding_slo`). Leaving it
unfrozen would make it the only unguarded credential surface, and the codemap's
`new-pipeline-asset` extension point routes exactly here. This change adds
`check_test_macos(name, text)`, registered in `run()` under
`TEST_MACOS_GATE = "test-macos.yml"`, with constants kept in lockstep with the
YAML: `TEST_MACOS_PUSH_JOB = "cachix-push-macos"`,
`TEST_MACOS_TEST_JOB = "tests-macos"`, `TEST_MACOS_GATE_JOB = "test-macos-gate"`,
`TEST_MACOS_TRUSTED_REF = "refs/heads/main"`, and
`TEST_MACOS_PR_SURFACES = ("flake.nix", "flake.lock", "nix/", ".github/workflows/test-macos.yml")`.
`check_cache_populate` is extended with `CACHE_POPULATE_TARGETS` /
`CACHE_POPULATE_RUNNERS` assertions. Every new assertion gets a positive fixture
**and** a mutation fixture in `--self-test`, matching the existing style.

## Edge cases and failure modes

Enumerated so the test-writer implements them rather than inferring them.

**Event space (exhaustive — this is the token-isolation argument).**

| Event | `tests-macos` | `cachix-push-macos` | Token materialized? |
|---|---|---|---|
| Fork PR | runs when touched | job `if` false | no (and forks get no secret anyway) |
| Same-repo PR | runs when touched | job `if` false | **no** — the guard, not the empty-secret accident, is what protects this case |
| Draft PR | skipped (gate accepts) | job `if` false | no |
| Push to `main` (paths hit) | runs | runs | yes, in the isolated job only |
| Push to another branch | not triggered | not triggered | no |
| `workflow_dispatch` | not configured | job `if` false | no |

- **E-1 — no token configured.** `cachix-push-macos` prints an honest skip and
  succeeds; nothing is pushed; no pretense of a warm cache.
- **E-2 — PR touches nothing relevant.** `changes` sets `touched=false`,
  `tests-macos` skips, `test-macos-gate` reports **success** (an honest skip).
  This is the case that makes the check requireable.
- **E-3 — PR touches `flake.lock` only.** Full Darwin matrix runs against a
  cold cache — no worse than today's push-to-`main` behaviour, because
  `cache.iog.io` still substitutes most of the GHC closure.
- **E-4 — first push after a lock bump.** The substitution-evidence step reports
  **0** copied paths (correct and expected); `cachix-push-macos` pushes the newly
  built closure; the *next* run reports a non-zero count. AC2 evidence is
  therefore a run **pair**, not a single run.
- **E-5 — redundant pushes.** All four `tests-macos` legs plus
  `cachix-push-macos` realize the same dev-shell derivations; only the push job
  holds the token, and Cachix pushes are content-addressed and idempotent, so a
  repeat push is a no-op.
- **F-1 — Cachix push fails** (revoked token, quota, network). The push job fails
  loudly (no `continue-on-error`): a silently broken cache warmer is how this
  gap survived unnoticed. It is a `main`-push job, so it never blocks a PR.
- **F-2 — Cachix storage growth.** Every toolchain bump adds a full
  aarch64-darwin GHC closure. Quota exhaustion presents as F-1. Retention/GC
  policy on the Cachix account is a maintainer operational item, named here, not
  automated by this change.
- **F-3 — macOS `bash` 3.2.** `test-macos.yml` already `brew install bash`es for
  this reason. `scripts/cache-prime`, `neo-consumer-contract` and `neo-release`
  must run under whatever `bash` the Darwin `cache-populate` leg has; the
  `--self-test` canary is the early detector, and installing a modern `bash` on
  the leg is the sanctioned fix. No script may be weakened to accommodate this.
- **F-4 — Darwin runner disk exhaustion.** A GHC closure plus a generated-project
  closure on a hosted macOS runner is the tightest resource in this change. It
  presents as an opaque build failure. Mitigation if it fires: free space before
  the build, or split `cache-prime` and `neo-consumer-contract` into separate
  Darwin legs. Named so it is diagnosed, not retried blindly.
- **F-5 — `./scripts/neo-release inspect` on Darwin.** The portability gate must
  accept a natively built `aarch64-apple-darwin` binary (its `TARGETS` already
  include the triple). A rejection here is a real defect in the leg, not a reason
  to skip the gate.
- **F-6 — `check_cache_populate` parsing.** `job_blocks` / `_steps_of` /
  `strip_comments` must keep matching once `cache-populate` gains
  `strategy:`/`matrix:` blocks and expression-valued `runs-on:` and
  `timeout-minutes:`. A silent parse miss would turn every existing trust
  assertion into a vacuous pass — so the self-test must include a mutation
  proving each assertion still **fails** on a matrixed job with the clause
  removed.
- **F-7 — over-broad guard drift.** Any later change to `startsWith`/`contains`
  on `github.ref`, a job-level `env:` token in `tests-macos`, or a `paths:` filter
  creeping back onto the `pull_request` trigger must fail `./dev workflow-check`.
- **F-8 — branch protection.** The aggregate job is only a gate once a maintainer
  marks `test-macos-gate` as a required check. Repo settings are outside an
  agent's reach; this ships as an explicit maintainer action item on the PR, not
  as a silent assumption (#795 AC3 is otherwise decoration).
- **F-9 — closure-identity drift (the residual #785 risk).** `cache-prime` runs
  the `neo` built from the branch, which pins the *branch's* compatibility
  revision; the SLO measures a *released* `neo` pinned to the revision embedded
  in its tag. If the two differ, the primed paths are not the paths the SLO asks
  for and `cachix.observed_use` stays `false` on both platforms no matter how much
  is pushed. Diagnosis: compare the generated project's flake input revision in
  the SLO log against the one `cache-prime` used. If that is the miss, it is a
  distinct defect (prime from the released artifact, or re-cut a release) and is
  filed as its own issue rather than absorbed here — see **Out of scope**.

**Concurrency-sensitive behaviour.**

- **X-1 — parallel pushers.** `cachix-push-macos`, `cachix-push.yml` and both
  `cache-populate` legs can push to `neohaskell.cachix.org` simultaneously.
  Cachix is content-addressed and handles concurrent uploads of overlapping paths;
  no ordering is assumed or required.
- **X-2 — cancelled warm-up.** `neo-ci.yml`'s existing concurrency group has
  `cancel-in-progress: true`, so a rapid second push to `main` can cancel a
  `cache-populate` leg mid-prime, leaving a *partial* (never corrupt) closure.
  Consequence: an SLO dispatched between those pushes can legitimately report
  `observed_use=false`. Evidence runs must follow a **completed**
  `cache-populate` run — stated as a precondition on C10/C11, not left to luck.
  `test-macos.yml` deliberately does not adopt that setting for pushes (see
  Concurrency above).
- **X-3 — matrix legs racing.** The Linux and Darwin `cache-populate` legs share
  no runner state; platform-independent paths (sources, fixed-output derivations)
  are pushed by both, idempotently.
- **X-4 — nothing is serialized for correctness.** No criterion depends on push
  ordering between workflows; every consumer either substitutes a path or builds
  it.

**Property-based criteria.** None apply, and that is a deliberate declaration
rather than an omission: the contract here is a finite set of static assertions
over a fixed YAML document, not an algebraic law over a value space. The nearest
equivalent — "no PR-reachable event materializes the token" — is a *quantified*
claim over a **closed, six-element** event space, so C2 discharges it
exhaustively (a table-driven check over every event shape) rather than by random
sampling.

## Criteria

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | `test-macos.yml` has a `cachix-push-macos` job that is the sole token holder: job-level guard `github.event_name == 'push'` + exact `github.ref == 'refs/heads/main'` + `github.repository == 'neohaskell/NeoHaskell'` + no `pull_request` reference and no `startsWith`/`endsWith`/`contains` on `github.ref`; job-level `CACHIX_AUTH_TOKEN`; an honest-skip step on `env.CACHIX_AUTH_TOKEN == ''`; every token-bearing step gated on `env.CACHIX_AUTH_TOKEN != ''`; `cachix/cachix-action` (`name: neohaskell`) set up before the realizing `nix develop … cabal build all` step | `./dev workflow-check --self-test` — new `check_test_macos` trust-guard fixtures: one positive plus one mutation per clause | unit |
| C2 | No pull-request-reachable job in `test-macos.yml` references `cachix`, `CACHIX_AUTH_TOKEN` or any `secrets.` expression, checked exhaustively over the closed event space {fork PR, same-repo PR, draft PR, push to `main`, push elsewhere, dispatch} as encoded in the job guards | `./dev workflow-check --self-test` — token-isolation fixtures, including a mutation that adds a Cachix step (and one that adds a job-level token) to `tests-macos` and must FAIL | unit |
| C3 | Darwin PR coverage is requireable: the `pull_request` trigger carries no workflow-level `paths:`; a `changes` job diff-scopes `flake.nix`, `flake.lock`, `nix/**` and `.github/workflows/test-macos.yml`; `tests-macos` runs only when `touched == 'true'` and the PR is not a draft; `test-macos-gate` is `if: always()`, `needs` the matrix job, and accepts a skip only for draft/untouched | `./dev workflow-check --self-test` — gate-shape fixtures, including mutations that re-add a `paths:` filter, drop a PR surface from the classifier, and drop `always()` from the aggregate | unit |
| C4 | The `push` trigger's `paths:` include `flake.nix` and `flake.lock` while retaining `core/**` and `nix/**`, so a merged toolchain bump re-warms the Darwin cache | `./dev workflow-check --self-test` — push-paths fixture plus a mutation dropping `flake.lock` | unit |
| C5 | `tests-macos` emits report-only substitution evidence: a `nix develop` step tees its log, counts `copying path … from https://neohaskell.cachix.org` matches, writes the count and matched store paths to `$GITHUB_STEP_SUMMARY`, and never fails the job | `./dev workflow-check --self-test` — evidence-step fixture (present, before the build, report-only) plus a mutation making it fail-closed | unit |
| C6 | `neo-ci.yml`'s `cache-populate` covers both targets: matrix `{ubuntu-latest, x86_64-unknown-linux-gnu}` and `{macos-latest, aarch64-apple-darwin}`, `fail-fast: false`, `REHEARSAL_TARGET` taken from the matrix (never a hardcoded triple), both `./dev neo-consumer-contract` and `./dev cache-prime` per leg, the trust guard unchanged, and the job still absent from `neo-ci-gate`'s `needs` | `./dev workflow-check --self-test` — extended `check_cache_populate` fixtures: both legs present passes; a dropped Darwin leg, a hardcoded `REHEARSAL_TARGET`, a dropped `cache-prime`, and a matrixed job with any trust clause removed each FAIL | unit |
| C7 | Every pre-existing workflow contract still holds on this branch — `check_release_rehearsal` (the PR-reachable `consumer-contract` holds no Cachix secret), `check_required_gate`, `check_component_gate`, `check_consumer_contract`, `check_onboarding_slo` — and `flake.lock` is byte-identical to `origin/main` | `./dev workflow-check` on the branch, plus an empty `git diff origin/main -- flake.lock flake.nix` | unit |
| C8 | On a pull request that touches `flake.lock`, the Darwin matrix runs and `test-macos-gate` reports success; on a pull request touching none of the scoped surfaces, `tests-macos` skips and `test-macos-gate` still reports **success** rather than leaving a pending check | this PR's own CI (it edits `.github/workflows/test-macos.yml`, so `touched=true`) plus any unrelated open PR after merge for the skip case; both run URLs recorded on the PR | integration |
| C9 | After merge, a `main`-push run of `test-macos.yml` pushes the aarch64-darwin dev-shell closure (#795 AC1), and the **next** run on a clean runner reports a non-zero count of paths copied from `https://neohaskell.cachix.org`, with GHC substituted rather than built (#795 AC2) | the post-merge run pair on `main`: `cachix-push-macos` post-hook push count, then the following run's substitution-evidence summary; both URLs recorded on the PR | integration |
| C10 | A trusted push to `main` runs the `cache-populate` Darwin leg to completion and its Cachix post-hook reports pushed paths for the released generated-project closure (#785: trusted population covers macOS ARM) | the post-merge `cache-populate (macos-latest)` job log — pushed-path count and both verbs executed; URL recorded on the PR | integration |
| C11 | A Neo Onboarding SLO dispatch reports, for `aarch64-apple-darwin`, `result=pass`, `elapsed_seconds <= 600` and `cachix.observed_use=true`, with a validating evidence sidecar (precondition: dispatched **after** a completed `cache-populate` run, at release tags whose embedded compatibility revision matches the primed closure — see F-9) | the dispatch's `onboarding-slo-aarch64-apple-darwin-*.json` evidence artifact and its `.sha256` sidecar | integration |
| C12 | The same dispatch reports `result=pass`, `elapsed_seconds <= 600` and `cachix.observed_use=true` for `x86_64-unknown-linux-gnu` — Linux must not regress and #785 requires both platforms | the same dispatch's `onboarding-slo-x86_64-unknown-linux-gnu-*.json` evidence artifact and its `.sha256` sidecar | integration |

C1–C7 are provable in CI on this PR. C8–C12 are operational evidence that can
only exist on a trusted `main` push or an explicit dispatch; they are named here
rather than hidden, and each names the exact artifact that discharges it.

## Primitives

No new primitive, and no new asset — the change lands entirely on existing ones.

- **No new `./dev` verb.** The work is carried by `./dev workflow-check`,
  `./dev cache-prime`, `./dev neo-consumer-contract` and `./dev onboarding-slo`,
  all of which already exist and already own this ground.
- **No new script.** `check_test_macos` extends `scripts/workflow-check`, the
  single mechanical consumer of `.github/workflows/*.yml`, rather than adding a
  parallel checker. Third instance of the freeze pattern in that file — well
  inside the rule of three, and consolidating rather than duplicating.
- **No new dependency.** Every action used is already pinned in this repo:
  `actions/checkout@v7.0.1`, `cachix/cachix-action@v17`,
  `DeterminateSystems/determinate-nix-action@v3.21.9`,
  `DeterminateSystems/magic-nix-cache-action@v14`, `dtolnay/rust-toolchain@1.94.0`,
  `Swatinem/rust-cache@v2`. No flake input, no `build-depends`, no hackage import,
  no new secret (`CACHIX_AUTH_TOKEN` already exists).
- **No new capability or extension point.** `ci-cd` and `dev-pipeline` already own
  every touched path; the `new-pipeline-asset` extension-point row already governs
  the `workflow-check` addition.
- **Reusable shape worth noticing.** This is the third `changes` + `if: always()`
  aggregate gate (after `neo-ci-gate` and `installer-ci-gate`). If a fourth
  appears, the aggregate-gate assertions in `workflow-check` should be promoted
  to one shared helper instead of a fourth copy — recorded here as a candidate,
  not done in this change.

## Security posture

`ci-cd` carries neither `security-sensitive` nor `perf-sensitive` in
`codemap/capabilities.yaml`, so `./dev spec-check --plan` routes **no** design
review for this spec. That empty routing is a gap in the tagging, **not a
clearance**: this change moves a cache-**write** credential onto a new runner
platform and adds a `pull_request` trigger to the workflow that will hold it —
the exact shape a security review exists for. A manual
`neohaskell-security-design-review` is therefore requested for this spec
(local-only record, gitignored, per ADR-0069), with these questions on the table:

1. Is job-level isolation (`cachix-push-macos` guarded on push + exact ref +
   same repo) sufficient, given that a same-repository PR *can* read secrets and
   that a PR may edit the workflow file itself? The counter-argument this design
   relies on: branch-push rights are already collaborator-level trust, and the
   guard means the token is not merely *unused* on a PR but *unreachable*.
2. Blast radius of `CACHIX_AUTH_TOKEN` (write access to the public cache →
   substituted-binary integrity for every contributor) and its rotation policy.
3. Whether `ci-cd` should be tagged `security-sensitive: true` so this routing
   is automatic next time. Recommendation: yes — filed as follow-up rather than
   slipped into this change, because it re-routes every future `ci-cd` spec.

## Out of scope

- **#795 AC4–AC6** — the scheduled `nix flake update` PR job, the cadence and
  per-input auto-merge policy, and bringing `haskellNix`/`nixpkgs`/`flake-utils`
  current. These are nh-5dz / #810, which depends on this change by the
  maintainer's explicit ordering: raising the update cadence before Darwin
  validates PRs would make macOS worse.
- **`flake.lock` is not bumped here.** Input currency is nh-5dz's contract.
- **`cachix-push.yml`'s missing `flake.nix`/`flake.lock` push paths** — the
  Linux dev-shell cache has the same "a lock bump never repopulates it" gap that
  C4 fixes for Darwin. That file is a read-only reference for this change; the
  gap is real and is filed as a follow-up issue rather than silently fixed.
- **#785 AC5–AC7** — evidence sidecar validation beyond C11/C12, the extended
  diagnostic-deadline override, and the standalone-repository archiving gate.
  Those are properties of `onboarding-slo` and the cutover, untouched here.
- **F-9's closure-identity drift**, if the evidence shows it. Priming from the
  released artifact instead of the branch build is a different change to
  `cache-prime` (a read-only reference here) and gets its own spec.
- **Branch protection** — marking `test-macos-gate` required is a maintainer
  repo-settings action (F-8).

## User impact

Not breaking. No public signature, wire format, or testbed behaviour changes.

**Contributors on Apple Silicon** get the payoff: after the first post-merge push
to `main`, a clean `nix develop` substitutes the GHC/haskell.nix closure from
`neohaskell.cachix.org` instead of compiling it, and a merged toolchain bump
re-warms that cache instead of invalidating it permanently.

**Pull-request authors** touching `flake.nix`, `flake.lock`, `nix/**` or
`test-macos.yml` get a macOS ARM signal before merge instead of after; everyone
else sees one extra cheap job (`changes`) and a green `test-macos-gate` that
honestly reports "nothing to test". Draft PRs skip the matrix, as elsewhere.

**Users onboarding from a release** are the point of the second surface: once the
Darwin `cache-populate` leg has run, `neo new` → `neo test` on a clean macOS ARM
machine substitutes the generated-project closure instead of building it, which
is what the 600-second onboarding SLO needs (#785).

**Maintainer actions required** (neither is automatable from an agent, both are
called out at PR time rather than assumed):

1. Mark `test-macos-gate` as a **required check** in branch protection — without
   it, #795 AC3 ships as decoration.
2. Dispatch **Neo Onboarding SLO** at the release tags after a completed
   post-merge `cache-populate` run, to produce the C11/C12 evidence.

Ongoing cost: one extra macOS build per qualifying push to `main`
(`cachix-push-macos`), one macOS `cache-populate` leg per trusted push, and
growth in Cachix storage per toolchain bump (F-2).

## ADR

[ADR-0075](../decisions/0075-darwin-cache-population-and-pr-gating.md) — the
trusted-push-only Darwin cache-push job (a cache-write credential never
coexisting with pull-request-controlled code), the `changes` + `if: always()`
aggregate as the only requireable shape for a diff-scoped gate, and extending
the existing `cache-populate` by matrix rather than adding a second population
workflow. Triggered by "adding significant new infrastructure"
(`docs/decisions/README.md`), the same trigger ADR-0070 fired on, even though no
`breaking` / `new-dependency` / `new-capability` / `new-extension-point` flag is
set.
