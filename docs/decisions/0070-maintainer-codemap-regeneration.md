# ADR-0070: Maintainer-triggered codemap regeneration onto a contributor PR

> Companion to the pipeline ADRs [ADR-0067](0067-contract-delta-spec-gate.md),
> [ADR-0068](0068-failure-asset-delta-and-learning-loop.md),
> [ADR-0069](0069-security-reviews-are-local.md). Adds a maintainer-only CI
> capability; changes no library API.

## Status

Proposed

<!-- Becomes `Accepted` only after Nick's explicit spec approval is recorded via
     the pipeline (`./dev pipeline approve spec`). At Gate 1 this is a proposal
     under review, and two load-bearing questions are still open: the credential
     feasibility gate (§5) and the mandatory Environment branch policy (§4a). -->

## Context

The codemap's generated artifacts — `codemap/signatures/*.txt`,
`codemap/MAP.md`, `codemap/.doc-ratchet` — are produced by `./dev codemap` from
the built API surface and are checked in. A CI gate (`checks.yml` `codemap` job,
via `codemap-sync`) fails a PR whose committed codemap does not match its code.

A contributor who changes an exported API but **cannot run `./dev codemap`**
(no nix, no toolchain, Windows, etc.) is therefore stuck: the gate is red and
they cannot make it green. The maintainer (Nick) wants to unstick them **from
the GitHub Actions panel on `main`**: enter the PR number, approve a protected
stage, and have the regenerated codemap land **directly on the contributor's PR
branch** so the *existing* PR turns green — no second "codemap" PR to chase, no
fallback.

The regeneration must *run the contributor's code* (`./dev codemap` builds
`nhcore`/`nhintegrations` from the PR head). That is the entire difficulty:

- The contributor controls the checked-out tree — `dev`, `scripts/**`, build
  hooks, `.gitattributes`, `.git/hooks`, repo config, **and the bytes of the
  generated files themselves** (filenames, contents, symlinks, sizes).
- Committing onto their branch requires a credential that can push to a
  **fork** — the upstream `GITHUB_TOKEN` cannot be assumed to have that, and
  even if it did, exposing *any* write credential to a job that executes
  contributor code is an arbitrary-code-execution-to-credential-theft pipeline.

Prior art in this repo: `revert.yml` is maintainer-triggered
(OWNER/MEMBER-gated) and opens (never merges) a PR with `persist-credentials:
false` + an explicit tokenized remote; `checks.yml` uses `env:` indirection for
PR-controlled values and pins actions by full SHA. This ADR extends those
conventions to a **write-onto-a-fork** flow, which none of them do.

## Decision

**Split generation and publication into two jobs separated by a data-only
artifact boundary; gate publication behind a maintainer-approved GitHub
Environment that is the sole holder of the write credential; fail closed with no
fallback PR.**

### 1. Manual trigger from the trusted default branch only

`on: workflow_dispatch` with a single `pr` input (the open PR number). No
`pull_request`, no `pull_request_target`, no automatic trigger. `workflow_dispatch`
already requires write access to the repo, so only maintainers can start it; the
Environment approval below is the second, explicit gate.

**Dispatch is not intrinsically `main`-only.** The Actions UI/API can select
another ref, and a workflow modified on that ref would run *its* definition — so
the workflow file's own contents cannot be trusted to gate the secret. The
load-bearing control is the `codemap-publish` Environment's `main`-only
deployment-branch policy (§4a), which GitHub enforces server-side regardless of
the running ref's workflow contents. A runtime `github.ref` check is kept as
defense in depth only.

### 2. `resolve` (trusted) pins the target by API

A trusted job resolves the PR via the GitHub API and **pins** head repo,
head ref, head SHA, PR state, and base ref. It validates: PR is `open`, base is
this repo's `main`, head is a user-owned fork or the same repo (org-owned forks
are unsupported and rejected), and — when the head is a fork —
`maintainer_can_modify` (a.k.a. "Allow edits by maintainers") is `true`. The
pinned metadata is passed forward as job outputs / an artifact.

### 3. `generate` (untrusted) — `contents: read`, no secrets

Checks out the **exact pinned head SHA**, `persist-credentials: false`,
`permissions: contents: read`, **no `secrets` in scope**. Runs `./dev codemap`.
Collects **only** the enumerated allowlist —

```
codemap/MAP.md
codemap/.doc-ratchet
codemap/signatures/*.txt
```

— plus a **generated manifest** (`manifest.txt`) listing the *complete* expected
generated path set after regeneration (every allowlisted file that should exist).
The manifest is what lets `publish` represent **deletions/renames**: an
allowlisted `codemap/signatures/*.txt` that is tracked on the head but absent
from the manifest is a stale signature file to remove. Everything is uploaded via
pinned `actions/upload-artifact` (`include-hidden-files: true`, because
`.doc-ratchet` is a dotfile — the `workflow-check` hidden-artifact rule). The job
holds no credential and can push nothing; its only output is the artifact
(consumed in `publish` via pinned `actions/download-artifact` — **no custom
archive parser**).

### 4. `publish` (trusted) — Environment-gated, credential-isolated, runs no contributor code

The accepted threat model is a **maintainer-reviewed** PR: Nick reviews the
outside contributor's PR before triggering regeneration, and approves the publish
Environment. So a bespoke hostile-archive / Git-object security subsystem is out
of scope. The non-negotiable line is narrower and clear: **no contributor script,
hook, build tool, or executable may run in `publish`, and `publish` is the only
job that holds the write credential.** A normal checkout of the *exact, reviewed,
pinned* SHA is allowed — it is data, not execution.

- `environment: codemap-publish` — a GitHub Environment with **required reviewer
  = Nick** and **`main`-only deployment branches** (§4a). The job does not start
  until Nick approves. The maintainer credential (`CODEMAP_PUBLISH_TOKEN`, §5) is
  referenced **only in this job**.
- **Re-fetch PR metadata independently** and fail if head SHA, head repo, head
  branch, state, or base changed since `resolve`/`generate`; require the PR still
  `open` against `main` and (for a fork) `maintainer_can_modify` still true.
- **Checkout the exact pinned contributor head SHA** with `persist-credentials:
  false`, **git hooks disabled** (`core.hooksPath=/dev/null`) and **credential
  helpers disabled** (`core.credentialHelper=""`). Nothing from the contributor
  tree is executed: no `./dev`, no build, no hook, no script — the checkout is
  read as data only. Download the generated artifact with pinned
  `actions/download-artifact`.
- **Symlink rejection:** reject any symlink at or under `codemap/` (and any
  `codemap` / `codemap/signatures` path component that is a symlink) — the one
  path-shape defense retained, cheap and sufficient for a reviewed tree.
- **Apply the allowlist + manifest:** copy **only** the explicit generated-output
  allowlist from the artifact over the checked-out tree, and `git add` **only**
  those paths. Then reconcile deletions via the manifest: any tracked, allowlisted
  `codemap/signatures/*.txt` **absent from `manifest.txt`** is `git rm`'d (stale
  signature removal/rename). **Manifest entries outside the allowlist are
  rejected** (fail closed).
- **Staged-diff gate:** inspect `git diff --cached --name-status`; **fail if any
  path is outside the codemap allowlist**. An **empty staged diff is a successful
  no-op** (exit 0, no commit, no push). Otherwise commit **once** with a
  deterministic bot identity and message `chore: regenerate codemap`.
- **Re-check the remote head immediately before push** and push **fast-forward
  only, never `--force`**, to the fork's exact head ref. A race (remote head
  moved) or any non-fast-forward **fails clearly** — no force, no fallback.

### 4a. The Environment's branch policy is the load-bearing secret guard

`workflow_dispatch` is **not intrinsically `main`-only**: the Actions UI/API can
dispatch a workflow against any selected ref, and a workflow modified on another
ref could delete a YAML `github.ref == 'refs/heads/main'` check. Therefore the
protection that actually keeps `CODEMAP_PUBLISH_TOKEN` away from a modified
workflow on a non-`main` ref is a **mandatory GitHub configuration**, not YAML:

- The `codemap-publish` Environment MUST set **Deployment branches and tags →
  Selected branches → `main` only** (in addition to required reviewer Nick).
  GitHub then refuses to grant the environment (and its secret) to any job whose
  `github.ref` is not `main`, *regardless of the workflow file's contents on
  that ref*.
- A runtime `github.ref`/`github.workflow_ref` check is kept in the workflow as
  **defense in depth only** — it fails a mis-dispatched run early with a clear
  message, but it is explicitly **not** claimed to protect the secret on its own.

This one-time setup is documented in the workflow header and here; without it the
capability is unsafe and the ADR's guarantees do not hold.

### 5. Auth is a **feasibility gate**, not a settled fact (spike required)

The credential that can push to a *contributor-owned fork* is an **open question
that must be proven by a real integration spike before implementation is
accepted** — it is deliberately not encoded here as known-working:

- A **fine-grained PAT** selected for the *upstream* repository is **likely
  insufficient**: fine-grained PATs are scoped to repositories the token owner
  owns or is granted, and a third party's fork cannot normally be selected in
  that scope — so "Allow edits by maintainers" does **not** obviously make an
  upstream-scoped fine-grained PAT able to push to the fork branch.
- A **GitHub App** installed only on the upstream repo **cannot access the fork**
  either (installations are per-repository); it qualifies *only* if it can
  demonstrably reach the contributor fork.
- The **hypothesis to prove** is that the minimum viable credential is a
  maintainer-owned **classic PAT** with the narrowest scope that still honors
  `maintainer_can_modify` (e.g. `public_repo` for public forks) — the same
  mechanism `git`/`gh` use when a maintainer pushes to a PR branch locally.

**Feasibility gate (blocks implementation acceptance):** a disposable, real
**user-owned fork** with "Allow edits by maintainers" enabled must be used to
prove the exact credential type and the authenticated **fast-forward push path**
end to end (criterion C8, `integration`). The spike records **no secret value**,
and cleans up the disposable branch/state. Based on the spike, the ADR/spec name
the exact minimum-viable credential and document its real **scope / blast radius,
rotation, and revocation**. The secret is stored as the `codemap-publish`
Environment secret **`CODEMAP_PUBLISH_TOKEN`**; **no secret value is ever
committed**, and it is **not persisted in Git config or a remote URL** where
avoidable — publish uses a **trusted ephemeral authentication mechanism**
appropriate to the credential the spike proves (e.g. an `http.extraheader`
scoped to the single push, or a short-lived tokened remote removed after use),
not a token baked into `origin`. **If the spike cannot prove a safe credential
path, the implementation is parked** (`./dev pipeline park`), not shipped with an
invented credential.

### 6. Fail closed, never a fallback PR

Every unsupported/unsafe condition — maintainer edits disabled, insufficient
permissions, org-owned/unsupported fork, metadata race, a symlink under
`codemap/`, a manifest entry outside the allowlist, a staged path outside the
codemap allowlist, remote-head race, non-fast-forward push — aborts with an
actionable `$GITHUB_STEP_SUMMARY` and **zero** repository mutation. There is no
PR-creation step anywhere in the workflow: a maintainer-triggered regenerate
either updates the contributor's branch or fails loudly.

### 7. The guard is trusted, testable, and self-checked

`scripts/codemap-regen-guard.py` holds the security-relevant decisions
(metadata-race / editability comparison, symlink rejection under `codemap/`,
allowlist + manifest reconciliation, staged-diff allowlist gate) as pure
functions with a `--self-test`; `./dev doctor` runs that self-test and `./dev
workflow-check` statically asserts the workflow's security contract (job
permissions, secret isolation, environment gating, no contributor-code
execution, action SHA-pinning, no `pull_request_target`, no auto trigger, no
fallback PR). The guarantees live in code the runner exercises, not in YAML
nobody re-checks.

## Consequences

- **Contributor code never touches a write credential.** The job split is the
  security argument: `generate` runs the contributor's `./dev codemap` under
  `contents:read` with no secret and produces the artifact; `publish` holds the
  credential but runs **no contributor script, hook, or build tool** — it checks
  out the reviewed pinned SHA as *data* (hooks + credential helpers disabled),
  copies only the allowlist, and pushes. Pragmatic for a maintainer-reviewed PR,
  not a bespoke hostile-archive subsystem.
- **Two human-meaningful controls:** only a maintainer can dispatch the workflow
  (write access), and publication waits on an explicit Environment approval.
- **The Environment branch policy is load-bearing, not cosmetic.** `main`-only
  deployment branches are what actually deny the secret to a modified workflow on
  another ref; the runtime ref check is only defense in depth (§4a).
- **Authentication is an unresolved feasibility gate (§5).** Whether *any*
  credential can fast-forward-push to a contributor fork must be proven by a
  disposable-fork spike (C8) before implementation is accepted; if it cannot, the
  implementation is parked rather than shipped with an invented credential.
- **One-time setup cost:** the maintainer must create the `codemap-publish`
  Environment with **required reviewer = Nick** AND **deployment branches =
  `main` only**, plus the `CODEMAP_PUBLISH_TOKEN` secret (type fixed by the C8
  spike). Documented here and in the workflow header. Without any of them the
  workflow fails closed at `publish`.
- **A stuck contributor is unblocked in-place** — their existing PR goes green
  after one fast-forward commit, with no second PR to reconcile.
- **Scope is deliberately narrow:** only the codemap allowlist is ever written,
  only fast-forward, only from `main`'s workflow definition. Anything outside
  that surface is a failure, not a fallback.
- **Not covered:** org-owned forks (GitHub does not honor `maintainer_can_modify`
  for them) are rejected, not worked around — the maintainer pushes manually or
  the contributor grants access. This is the accepted limitation.
