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

**Auth (decided; validated by the first run).** The Actions `GITHUB_TOKEN` and a
fine-grained PAT/GitHub-App scoped to the upstream repo cannot reach a contributor
fork; the credential is a maintainer-owned **classic PAT** with the minimum
practical scope for the current public base+fork case — **`public_repo`** — which
acts with the maintainer's identity that "Allow edits by maintainers" grants push
to the fork PR head. It is stored **only** as the `main`-restricted
`codemap-publish` Environment secret `CODEMAP_PUBLISH_TOKEN`, exposed solely to
`publish`, used ephemerally (never baked into `origin`/committed). Its **broad
public-repo blast radius is documented and accepted**, with a **dedicated
low-privilege bot identity** recommended when practical, expiry **≤90 days**, and
revoke-on-exposure. Per the maintainer decision of 2026-08-04, the disposable-fork
credential spike is **no longer a pre-implementation blocker** (it needs two
identities the harness lacks); instead the **first Environment-approved production
run on the reviewed PR #724 is the end-to-end credential validation** — a GitHub
denial fails clearly and safely with **no fallback PR and no remote mutation**.
ADR-0070 §5 records this.

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
static-wiring assertions on the workflow, covering **all pre-push safety**. C8 is
an **operational** criterion (level `integration`): the maintainer-approved
**first production run** on the reviewed PR #724 validates the credential's
PR-specific fork-write grant end to end. It is **not** a pre-implementation or
PR-ready blocker (maintainer decision 2026-08-04 — the real spike needs two
identities the CI harness lacks); a push denial fails safe with no mutation and no
fallback. Its proof is that first run against a committed runbook, not a CI test.

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | The metadata guard pins then re-checks exact head SHA/repo/branch/state/base and fails on any change; it requires the PR still `open` against this repo's `main` and (for a fork) `maintainer_can_modify` true, failing when maintainer edits are disabled or the fork is org-owned/unsupported; an unchanged editable user-fork/same-repo PR passes | `./dev codemap-regen-guard --self-test` metadata-pin/recheck + maintainer-editability cases | unit |
| C2 | The workflow's job contract holds statically: `generate` has `contents: read`, `persist-credentials: false`, and no secrets/write credential; `publish` is the **sole** holder of `CODEMAP_PUBLISH_TOKEN`; every third-party action is pinned by full commit SHA; no `pull_request_target`, no automatic PR trigger; `workflow_dispatch` takes the PR-number input; no fallback-PR step exists | `./dev workflow-check` (and its `--self-test` codemap-regen job-contract assertions) | unit |
| C3 | The mandatory `main`-only `codemap-publish` Environment policy (+ required reviewer Nick) is documented as the load-bearing secret guard, and a runtime non-`main` dispatch is rejected by the ref guard; `workflow-check` asserts the `environment:` gating, the runtime ref guard, and the documented-policy marker | `./dev workflow-check` (`--self-test`) environment-policy + ref-guard assertions | unit |
| C4 | A symlink at or under `codemap/` (including a `codemap` / `codemap/signatures` path component that is a symlink) is rejected before any staging | `./dev codemap-regen-guard --self-test` symlink-rejection cases | unit |
| C5 | The manifest permits allowlisted **additions**, encodes allowlisted **deletions/renames** (a tracked `codemap/signatures/*.txt` absent from the manifest is removed), and **rejects any manifest entry outside the allowlist**; only allowlisted paths are ever copied/staged | `./dev codemap-regen-guard --self-test` manifest add / delete / out-of-allowlist cases | unit |
| C6 | The final `git diff --cached --name-status` gate passes only when every staged path is in the codemap allowlist: a real allowlisted change commits once (`chore: regenerate codemap`, deterministic identity), an empty staged diff is a **successful no-op** (no commit), any non-allowlisted staged path fails | `./dev codemap-regen-guard --self-test` staged-diff-allowlist / no-op / unexpected-path cases | unit |
| C7 | `publish` runs **no contributor script, hook, or build command**: it checks out the pinned SHA with `persist-credentials: false`, git hooks disabled, and credential helpers disabled, invoking no `./dev`/build/hook from the contributor tree; the remote head is re-checked immediately before a **fast-forward-only, never-force** push, which fails clearly on a race/non-fast-forward | `./dev workflow-check` (`--self-test`) no-contributor-exec + fast-forward-push assertions | unit |
| C8 | **Auth operational validation (not a pre-push blocker):** the first maintainer-approved production run on the reviewed contributor PR #724 fast-forward-commits the regenerated codemap using the classic `public_repo` PAT, validating the PR-specific fork-write grant end to end; a GitHub push denial fails clearly and safely with no fallback and no remote mutation. Blast radius / rotation / revocation are documented in the workflow runbook | first Environment-approved production run on PR #724, per the workflow-header runbook | integration |

## User impact

Not breaking. No public signature or wire-format change. New capability for
maintainers only: a manually-dispatched workflow on `main`. Contributors see
their PR branch receive one `chore: regenerate codemap` fast-forward commit after
Nick approves the protected environment; a no-op (codemap already current) leaves
the branch untouched and the run succeeds. Every unsupported or unsafe condition
(maintainer edits disabled, org-owned fork, metadata race, symlink under
`codemap/`, out-of-allowlist manifest/diff, non-fast-forward) fails with an
actionable Actions summary and mutates nothing — **no fallback PR is ever
created**. Testbed: no acceptance-test change — this is CI/tooling with no
HTTP-observable behavior. One-time maintainer setup is **mandatory and
load-bearing**: the `codemap-publish` Environment with **required reviewer Nick**
AND **deployment branches = `main` only**, plus the `CODEMAP_PUBLISH_TOKEN` secret
— a maintainer classic `public_repo` PAT (broad public-repo blast radius
documented; dedicated bot identity recommended; expiry ≤90d; revoke-on-exposure)
— documented in ADR-0070 and the workflow header. Without any of these the
workflow fails closed at `publish`.

## ADR

[ADR-0070](../decisions/0070-maintainer-codemap-regeneration.md) — the two-job
trust boundary (data-only artifact seam), the Environment-gated publish with the
maintainer credential isolated from contributor code, the fail-closed
no-fallback-PR policy, and the fine-grained-PAT auth (with the GitHub-App
migration path). Triggered by "significant new infrastructure" (the
`docs/decisions/README.md` ADR-required list) even though no
breaking/new-dependency/new-capability/new-extension-point flag fires.
