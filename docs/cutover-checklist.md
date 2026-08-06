# Neo CLI cutover & archive checklist (PR 7)

The operator runbook for retiring the standalone `neohaskell/neo` and
`neohaskell/neo-starter` repositories in favour of the NeoHaskell monorepo.
Governed by [ADR-0071](decisions/0071-neo-cutover-and-onboarding-slo.md).

**Hard rules for this checklist**

- **Every destructive-adjacent step is a HUMAN GATE.** Nothing here is automated
  by this PR. Archiving is a manual GitHub action taken by a maintainer.
- **Archive, never delete.** The standalone repos are made read-only (history
  intact). They are **not** deleted, and they are **preserved until post-cutover
  verification passes**.
- **Nick approves twice, explicitly:** once before the final merge to `main`, and
  again before archiving the standalone repos. Neither approval is implied by the
  other.
- **No SLO claim without hosted evidence.** The onboarding SLO is "passed" only
  when an immutable, tag-pinned hosted run produced a green evidence artifact (see
  §2). Durable prose must never assert a pass.

---

## 0. Preconditions (green before starting)

- [ ] The final integration PR is green: `Test`, `Test macOS`, `neo-ci`,
      `installer-ci`, `checks` (incl. `./dev workflow-check`), `spec`.
- [ ] `./dev workflow-check` and `./dev workflow-check --self-test` pass locally.
- [ ] `./dev onboarding-slo --self-test` passes locally (orchestration frozen).
- [ ] `./dev neo-consumer-contract --self-test` passes; the blocking
      `consumer-contract` job is green (Fact 1: mutual compatibility).
- [ ] No durable doc, install instruction, issue/source link, bootstrap
      reference, Cargo metadata, or AGENTS guidance still treats standalone
      `neohaskell/neo` or `neohaskell/neo-starter` as authoritative. (Historical
      provenance in `neo/starter/IMPORT.md` and the installer's anti-pattern
      guard-rails are intentionally preserved.)

## 1. Publish the immutable release inputs (human gate — maintainer)

Real SLO evidence needs published, immutable artifacts. These are **not** created
by this PR.

- [ ] Tag and publish an installer release `installer-vX.Y.Z`
      (`.github/workflows/installer-ci.yml`, tag-gated). Produces
      `installer-neo-install-<target>` + `SHA256SUMS`.
- [ ] Tag and publish a Neo CLI release `neo-vA.B.C`
      (`.github/workflows/neo-release.yml`, tag-gated). Produces `neo-<target>` +
      `SHA256SUMS`, each portability-gated and native-smoked before publish.
- [ ] Confirm the public Cachix cache (`https://neohaskell.cachix.org`) is
      populated for the consumer path (trusted-push `cache-populate` job — it
      warms the cache from a push to an exact trusted ref: `main` or the
      migration `integration/neo-monorepo` branch, never from a PR). Without
      it the first project build cannot meet the 600 s deadline and the SLO run
      fails closed rather than fabricating a pass.

Record the exact tags: `installer-vX.Y.Z`, `neo-vA.B.C`.

## 2. Obtain hosted onboarding-SLO evidence (human gate — maintainer dispatch)

- [ ] Dispatch **Neo Onboarding SLO** (`workflow_dispatch`) with the immutable
      inputs from §1:
      - `installer_version = installer-vX.Y.Z`
      - `neo_version       = neo-vA.B.C`
      - `expected_source_sha = <the reviewed 40-hex cutover commit>`
- [ ] Both matrix legs pass (`ubuntu-latest`/`x86_64-unknown-linux-gnu` and
      `macos-latest`/`aarch64-apple-darwin`), each within 600 s.
- [ ] Download the evidence artifacts (`onboarding-slo-evidence-<target>`) and
      confirm each records: `source_sha`, both release tags, target,
      `elapsed_seconds ≤ 600`, both peeled tag SHAs equal the reviewed source SHA,
      the installed Neo hash equals the release manifest, `cachix.reachable = true`,
      `cachix.observed_use = true`, and `result = "pass"`.
- [ ] Attach the evidence artifacts (or their URLs) to the final PR as the SLO
      proof. **Do not** transcribe the number into durable prose as the source of
      truth — the artifact is the evidence.

> If §1 is not yet done, this step is BLOCKED. That is expected: finish all safe
> preparation, keep the PR draft, and state this exact gate. The SLO is not
> "passed" until this step is green.

## 3. Final review & merge (human gate — Nick approval #1)

- [ ] Nick reviews the full diff, green CI, and the §2 evidence artifacts.
- [ ] Nick gives **explicit** approval to merge (e.g. a maintainer `@claude`
      approval comment / recorded sign-off), per the plan's two-touchpoint model.
- [ ] Squash-merge the integration PR to `main` (one merge; `main` receives only
      the reviewed result).
- [ ] Post-merge, confirm `main` is green (`Test`/`Test macOS`); `dod.yml` flags a
      failure as a revert-candidate (notify-only).

## 4. Archive the standalone repos (human gate — Nick approval #2)

Only after §3 is merged AND post-cutover verification (§5) has begun succeeding.

- [ ] Nick gives **explicit, separate** approval to archive.
- [ ] In `neohaskell/neo`: replace the README top with a deprecation banner
      pointing at `neohaskell/NeoHaskell` (CLI at `neo/`, installer at
      `installer/`, releases under `neo-v*`/`installer-v*`, install via the
      verified installer). Then **Settings → Archive this repository** (read-only;
      history preserved). **Do not delete.**
- [ ] In `neohaskell/neo-starter`: same deprecation banner pointing at the
      internalized starter (`neo/starter/`, provenance in `neo/starter/IMPORT.md`).
      Then archive read-only. **Do not delete.**
- [ ] Redirect any external references (org README, website, docs) to the
      monorepo. (In-repo durable docs are already routed — verified in §0.)

## 5. Post-cutover verification & rollback readiness

- [ ] From a clean machine, run the published `curl … bootstrap.sh | sh` path and
      confirm `neo new → test → run → /health` works end-to-end against the
      published releases (mirrors the §2 automated evidence, by hand).
- [ ] Keep the standalone repos **archived, not deleted**, through the
      verification window so nothing is lost if a rollback is needed.
- [ ] **Rollback plan (to the previous exact release):** if a regression surfaces,
      re-pin consumers to the previous immutable tags (`NEO_VERSION=neo-v<prev>`,
      `NEO_INSTALLER_VERSION=installer-v<prev>`) and, if a merged change is at
      fault, use the kill switch: a maintainer comments `/revert` on the merged PR
      → `revert.yml` opens a revert PR (`./dev revert <sha>`; never auto-merged).
      Un-archiving is not required for rollback — the archived repos stay readable
      and their releases remain downloadable.

## Human gates at a glance

| Gate | Owner | Blocks |
|------|-------|--------|
| Publish `installer-v*` + `neo-v*`, populate Cachix (§1) | maintainer | §2 |
| Dispatch + collect hosted SLO evidence (§2) | maintainer | §3 |
| Approve final merge (§3) | **Nick** | merge to `main` |
| Approve archiving standalone repos (§4) | **Nick** | archive |
| Post-cutover verification before letting archives settle (§5) | maintainer | closing the cutover |
