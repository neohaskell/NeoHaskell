# ADR-0071: Neo CLI cutover — archive-as-human-gate and the clean-machine onboarding SLO

> Closes the neo → NeoHaskell-monorepo migration (PR 7 in the project plan). The
> earlier PRs internalized the CLI, its starter, packaging, the consumer
> contract, and the native release/installer train. This ADR governs the final
> cutover: how the one user-facing SLO is proven, and how the standalone
> `neo`/`neo-starter` repositories are retired. Changes no library API.

## Status

Accepted

<!-- The GATE infrastructure (the onboarding SLO workflow + driver + wiring
     freeze, the doc/link re-routing, this ADR and the cutover checklist) is
     implemented by this PR. The cutover EXECUTION — obtaining real hosted SLO
     evidence, the final merge, and archiving the standalone repos — are HUMAN
     GATES that this PR deliberately does not automate. See
     docs/cutover-checklist.md for the operator runbook and the exact commands. -->

## Context

The migration must end with two facts durably true (project plan, "Completion"):

1. `NeoHaskell + neo + template + installer are mutually compatible`.
2. `clean supported machine → install → new → test → run ≤ 600 seconds`.

Fact 1 is already gated continuously: the generated-project consumer contract
(`neo-ci.yml` `consumer-contract`, [ADR-0068]-adjacent) proves a checksum-verified,
portable release binary generates and builds a project against **this** checkout.

Fact 2 — the onboarding SLO — is different in kind. It is a measurement of the
**real user path from published, immutable artifacts** on a **clean, supported,
GitHub-hosted machine**, not a test against the working tree. Proving it honestly
raises three hazards this ADR resolves:

- **Fabrication.** A source `cargo build` or `nix build .#neo` would measure the
  wrong thing (a dev binary / a `/nix/store`-linked binary) and could "pass" while
  the artifact a user actually downloads does not.
- **Mutable inputs.** Measuring `latest` records a number that pins to nothing —
  it cannot be reproduced or audited later.
- **A PR masquerading as proof.** PR CI runs on untrusted code and cannot be
  allowed to publish, hold secrets, or emit an SLO number that reads as evidence.

Separately, the standalone `neohaskell/neo` and `neohaskell/neo-starter`
repositories are still the historical homes of the code. Retiring them is
destructive-adjacent and irreversible-in-spirit, so it must be a reviewed human
action, not automation buried in a merge.

## Decision

### 1. The onboarding SLO is measured by one executable path, two modes

`scripts/onboarding-slo` is the single source of truth for the measured path
(`install → neo new → neo test → neo run → GET /health`, hard 600 s deadline). It
is wired by `.github/workflows/neo-onboarding-slo.yml` with a deliberate split:

- **Safe rehearsal (every PR + dispatch)** — the `self-test` job runs
  `./dev onboarding-slo --self-test` and `./dev workflow-check`. It downloads
  nothing mutable, holds no secret, and publishes nothing. It proves the gate is
  wired and the orchestration logic is correct — **not** an SLO number.
- **Real evidence (`workflow_dispatch` only)** — the `slo` job consumes
  **immutable** `installer-v*` and `neo-v*` release tags supplied as inputs,
  measures the path on the two supported clean-machine runners
  (`ubuntu-latest`/`x86_64-unknown-linux-gnu` and `macos-latest`/
  `aarch64-apple-darwin`), and uploads an **evidence JSON** artifact recording the
  source SHA, both release tags, target, elapsed seconds, verified checksums,
  peeled release-tag commit SHAs, Cachix reachability and separately observed
  substitution use, a SHA256 sidecar, and the pass/fail result. This is the
  ONLY producer of real SLO evidence; a PR event never reaches it.

### 2. The measured binary is a real, checksum-verified release artifact

The driver obtains `neo` through the installer's own semantics: it downloads the
immutable `installer-v*` asset + `SHA256SUMS`, **verifies before install**, then
runs the installer with `NEO_VERSION=<neo-v*>` so the installer performs its own
verified native `neo` download. No `cargo build`, no `nix build .#neo`. The gate
independently hashes the installed `neo` and requires it to match the pinned
`neo-v*` manifest.

### 3. Fail closed; never fabricate

A missing/mutable/foreign version tag, a missing or mismatched checksum, or an
unreachable public Cachix substituter aborts the run — it never emits a pass. The
600 s deadline starts before cache/API/download/install work and is enforced
against one monotonic global deadline. Every subprocess has the remaining budget,
a new process session, TERM→KILL process-group cleanup, and wait. The driver
publishes nothing and pushes nothing (self-test-enforced); failures still produce
valid JSON and a SHA256 sidecar, and cleanup never overwrites a pass.

### 4. Both layers are frozen executably

- `scripts/onboarding-slo --self-test` freezes the orchestration: strict reviewed
  SHA/tag validation, bounded timeout and descendant cleanup, checksum/hash
  mismatch handling, pass/failure evidence plus sidecars, and non-publication.
- `scripts/workflow-check` (`check_onboarding_slo`) freezes the workflow wiring:
  both supported runners+targets, the 600 s bound, immutable dispatch inputs (no
  `latest`), a real measured `run` (never a source/nix build), dispatch-only real
  evidence, least privilege (`contents: read`, no secret, no publication).

Both run in CI (the `self-test` job and `checks.yml`), so a regression to either
layer fails loudly on a normal PR.

### 5. Archiving the standalone repos is a human gate, not automation

This PR does **not** archive or delete anything. `docs/cutover-checklist.md` is
the operator runbook: it requires Nick's explicit approval before the final merge
and, separately, before archiving (never deleting) `neohaskell/neo` and
`neohaskell/neo-starter`. The standalone repos are **preserved** (read-only
archive, history intact) until post-cutover verification succeeds, and the
checklist records a rollback to the previous exact release.

## Consequences

- The SLO can only ever be claimed from a reviewed-SHA-bound, content-hashed hosted
  run — never from durable prose and never from a PR. GitHub release assets remain
  administratively mutable, so each run records the peeled tag SHAs and actual
  asset hashes rather than claiming tags alone make bytes immutable.
- Real SLO evidence cannot be produced until the `neo-v*`/`installer-v*` releases
  and the public Cachix paths exist. Until then the safe rehearsal is green and
  the checklist names the exact blocking human gate and commands. This is the
  intended state, not a gap.
- Retiring the standalone repos stays reversible-until-verified and reviewed.

## References

- Project plan: "neo CLI → NeoHaskell monorepo + starter", PR 7 and "Completion".
- `docs/cutover-checklist.md` — the human-gated cutover/archive runbook.
- `.github/workflows/neo-onboarding-slo.yml`, `scripts/onboarding-slo`,
  `scripts/workflow-check` (`check_onboarding_slo`).
- Release/installer contract: [ADR-adjacent] `installer/README.md`,
  `.github/workflows/neo-release.yml`, `scripts/neo-release`.
