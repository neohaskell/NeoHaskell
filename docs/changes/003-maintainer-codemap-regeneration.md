# Change 003: Maintainer-triggered codemap regeneration onto a contributor PR

A contributor opens a PR against `main` but cannot run `./dev codemap`, so the
generated codemap artifacts (`codemap/signatures/*.txt`, `codemap/MAP.md`,
`codemap/.doc-ratchet`) drift from their API changes and the `codemap` CI gate
fails. Nick wants to fix this **from the GitHub Actions panel on `main`**: enter
the contributor PR number, approve a protected publish stage, and have the
regenerated codemap committed **directly onto the contributor's existing PR
branch** as one fast-forward commit — with no fallback PR and no path for the
contributor's *code* to run alongside a write credential.

The trust model is pragmatic: Nick **reviews the contributor PR** before
triggering regeneration and approves the publish Environment. So the essential
control is a **job split** — the contributor's `./dev codemap` runs only in an
untrusted `generate` job (`contents: read`, no secret); a protected `publish`
job holds the credential and runs **no contributor script, hook, or build tool**,
checking out the exact reviewed SHA as data only. This, plus head-race pinning,
a symlink-under-`codemap/` guard, a manifest for deletions, and a final
allowlisted staged-diff gate, is the decision recorded in
[ADR-0070](../decisions/0070-maintainer-codemap-regeneration.md).

```yaml spec
issue: adhoc:maintainer-codemap-regeneration
kind: feature
touches: [ci-cd, dev-pipeline]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

Infrastructure only — a new `workflow_dispatch` workflow
(`.github/workflows/codemap-regen.yml`), a new trusted validation helper
(`scripts/codemap-regen-guard.py`, registered as `./dev codemap-regen-guard`),
and static wiring assertions in `scripts/workflow-check`. No `nhcore` /
`nhintegrations` public API changes: no `codemap/signatures/` line is added or
removed by this change. The promised diff is therefore empty.

```diff signatures
```

**Trust boundary (the crux — why two jobs, not one).** The accepted threat model
is a **maintainer-reviewed** PR (Nick reviews the contributor PR and approves the
publish Environment), so the line is pragmatic, not a bespoke hostile-archive
subsystem: **no write credential or secret may coexist with executing
contributor code, and `publish` must run no contributor script/hook/build tool.**
The workflow is therefore two jobs:

- **`generate`** — checks out the *exact untrusted PR head SHA* (pinned via the
  GitHub API in a prior trusted `resolve` step), holds `contents: read`,
  `persist-credentials: false`, and **no secrets/write credential**, runs `./dev
  codemap`, collects **only** the enumerated generated outputs (the allowlist
  below) **plus a generated `manifest.txt`** (the complete expected generated
  path set, so deletions/renames are represented), and uploads them via pinned
  `actions/upload-artifact`. It cannot push and holds no secret.
- **`publish`** — gated by a GitHub **Environment** (`codemap-publish`, required
  reviewer Nick, `main`-only deployment branches) and the **sole** holder of the
  maintainer credential. It re-verifies PR metadata (SHA/repo/branch/state/base
  unchanged; still open against `main`; maintainer edits still enabled for a
  fork), then **checks out the exact pinned contributor SHA as data** with
  `persist-credentials: false`, **git hooks and credential helpers disabled**,
  and **runs no contributor code** (no `./dev`, no build, no hook). It downloads
  the artifact (pinned `actions/download-artifact`; **no custom archive parser**),
  **rejects any symlink at/under `codemap/`**, copies and `git add`s **only** the
  allowlist, `git rm`s allowlisted tracked signature files **absent from
  `manifest.txt`** (rejecting any manifest entry outside the allowlist), gates on
  `git diff --cached --name-status` being **codemap-allowlist-only** (empty diff =
  successful no-op), commits **once** (`chore: regenerate codemap`, deterministic
  identity), **re-checks the remote head**, and pushes **fast-forward only, never
  `--force`**.

**Generated-output allowlist** (exactly what `./dev codemap` writes and git
tracks; the hoogle `.hoogle-*` DBs are gitignored and never collected):

```
codemap/MAP.md
codemap/.doc-ratchet
codemap/signatures/*.txt
```

**Control-plane / secret guard (load-bearing).** `workflow_dispatch` is **not
intrinsically `main`-only** — the Actions UI/API can dispatch any ref, and a
workflow modified on another ref could drop a YAML ref check. The protection that
actually keeps the secret from a modified workflow on a non-`main` ref is a
**mandatory GitHub configuration**: the `codemap-publish` Environment MUST set
**Deployment branches = `main` only** (alongside required reviewer Nick), so
GitHub refuses the environment+secret to any job whose ref isn't `main`,
regardless of that ref's workflow contents. A runtime `github.ref` check remains
as **defense in depth only** and is not claimed to protect the secret alone.

**Auth is a feasibility gate, not a settled fact.** The upstream `GITHUB_TOKEN`
cannot push to a contributor fork, and a **fine-grained PAT scoped to the upstream
repo is likely insufficient** (a third party's fork cannot normally be selected
in its scope, so `maintainer_can_modify` does not obviously grant it push); a
GitHub App installed only upstream also cannot reach the fork. The **hypothesis**
is a maintainer-owned **classic PAT** with the narrowest scope that still honors
"Allow edits by maintainers" (e.g. `public_repo`). This **must be proven by a
disposable real user-owned fork spike** (criterion C8, `integration`) **before
implementation is accepted** — recording no secret value and cleaning up the
disposable branch. The spike fixes the exact minimum-viable credential type and
its rotation/revocation guidance; it is stored as the `codemap-publish`
Environment secret `CODEMAP_PUBLISH_TOKEN`, exposed **solely** to `publish`, and
**no secret is committed**. If the spike cannot prove a safe path, the
implementation is **parked**, not invented. ADR-0070 records this gate.

**Fail-closed, no fallback PR.** Maintainer edits disabled on a fork, insufficient
permissions, an org-owned/unsupported fork, a metadata race (head SHA/repo/branch/
state/base changed after generation), a symlink under `codemap/`, a manifest entry
outside the allowlist, a staged path outside the codemap allowlist, a remote-head
race, or a non-fast-forward push each abort with an actionable
`$GITHUB_STEP_SUMMARY` and **zero repository mutation**. There is deliberately no
`gh pr create` / create-pull-request step anywhere in the workflow.

**No injection surface.** `pull_request_target` is not used; there is no automatic
PR trigger — the workflow is manually initiated from the trusted default branch
only. PR-controlled values reach `run:` steps through `env:` indirection, never
inline `${{ }}`, matching the repo's existing template-injection defense.

## Criteria

C1–C7 are `unit` — metadata/manifest/diff validation in a harness self-test plus
static-wiring assertions on the workflow. C8 is `integration` and cannot be
satisfied by a mock: it is the credential **feasibility gate** — a real
authenticated fast-forward push to a disposable user-owned fork. The end-to-end
production push against a real contributor PR still cannot run in CI (needs a live
fork + the maintainer secret), so its constituent guarantees are proven at the
seams the runner *can* check (the trusted guard's decisions and the workflow's
static shape) plus the one-off C8 spike that proves the credential.

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | The metadata guard pins then re-checks exact head SHA/repo/branch/state/base and fails on any change; it requires the PR still `open` against this repo's `main` and (for a fork) `maintainer_can_modify` true, failing when maintainer edits are disabled or the fork is org-owned/unsupported; an unchanged editable user-fork/same-repo PR passes | `./dev codemap-regen-guard --self-test` metadata-pin/recheck + maintainer-editability cases | unit |
| C2 | The workflow's job contract holds statically: `generate` has `contents: read`, `persist-credentials: false`, and no secrets/write credential; `publish` is the **sole** holder of `CODEMAP_PUBLISH_TOKEN`; every third-party action is pinned by full commit SHA; no `pull_request_target`, no automatic PR trigger; `workflow_dispatch` takes the PR-number input; no fallback-PR step exists | `./dev workflow-check` (and its `--self-test` codemap-regen job-contract assertions) | unit |
| C3 | The mandatory `main`-only `codemap-publish` Environment policy (+ required reviewer Nick) is documented as the load-bearing secret guard, and a runtime non-`main` dispatch is rejected by the ref guard; `workflow-check` asserts the `environment:` gating, the runtime ref guard, and the documented-policy marker | `./dev workflow-check` (`--self-test`) environment-policy + ref-guard assertions | unit |
| C4 | A symlink at or under `codemap/` (including a `codemap` / `codemap/signatures` path component that is a symlink) is rejected before any staging | `./dev codemap-regen-guard --self-test` symlink-rejection cases | unit |
| C5 | The manifest permits allowlisted **additions**, encodes allowlisted **deletions/renames** (a tracked `codemap/signatures/*.txt` absent from the manifest is removed), and **rejects any manifest entry outside the allowlist**; only allowlisted paths are ever copied/staged | `./dev codemap-regen-guard --self-test` manifest add / delete / out-of-allowlist cases | unit |
| C6 | The final `git diff --cached --name-status` gate passes only when every staged path is in the codemap allowlist: a real allowlisted change commits once (`chore: regenerate codemap`, deterministic identity), an empty staged diff is a **successful no-op** (no commit), any non-allowlisted staged path fails | `./dev codemap-regen-guard --self-test` staged-diff-allowlist / no-op / unexpected-path cases | unit |
| C7 | `publish` runs **no contributor script, hook, or build command**: it checks out the pinned SHA with `persist-credentials: false`, git hooks disabled, and credential helpers disabled, invoking no `./dev`/build/hook from the contributor tree; the remote head is re-checked immediately before a **fast-forward-only, never-force** push, which fails clearly on a race/non-fast-forward | `./dev workflow-check` (`--self-test`) no-contributor-exec + fast-forward-push assertions | unit |
| C8 | **Auth feasibility gate:** a real authenticated fast-forward push to a disposable user-owned fork with "Allow edits by maintainers" enabled succeeds using the selected minimum-viable credential, proving the credential type end to end and documenting its blast radius / rotation / revocation; it records no secret value and cleans up the disposable branch. If unprovable, the run parks rather than shipping an invented credential | disposable user-owned-fork push spike, recorded in the PR (no mock) | integration |

## User impact

Not breaking. No public signature or wire-format change. New capability for
maintainers only: a manually-dispatched workflow on `main`. Contributors see
their PR branch receive one `chore: regenerate codemap` fast-forward commit after
Nick approves the protected environment; a no-op (codemap already current) leaves
the branch untouched and the run succeeds. Every unsupported or unsafe condition
(maintainer edits disabled, org-owned fork, metadata race, bad artifact,
unexpected diff, non-fast-forward) fails with an actionable Actions summary and
mutates nothing — **no fallback PR is ever created**. Testbed: no acceptance-test
change — this is CI/tooling with no HTTP-observable behavior. One-time maintainer
setup is **mandatory and load-bearing**: the `codemap-publish` Environment with
**required reviewer Nick** AND **deployment branches = `main` only**, plus the
`CODEMAP_PUBLISH_TOKEN` secret whose exact type is fixed by the C8 feasibility
spike — documented in ADR-0070 and the workflow header. Without any of these the
workflow fails closed at `publish`.

## ADR

[ADR-0070](../decisions/0070-maintainer-codemap-regeneration.md) — the two-job
trust boundary (data-only artifact seam), the Environment-gated publish with the
maintainer credential isolated from contributor code, the fail-closed
no-fallback-PR policy, and the fine-grained-PAT auth (with the GitHub-App
migration path). Triggered by "significant new infrastructure" (the
`docs/decisions/README.md` ADR-required list) even though no
breaking/new-dependency/new-capability/new-extension-point flag fires.
