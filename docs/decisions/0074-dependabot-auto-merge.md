# ADR-0074: Dependabot patch/minor auto-merges; majors stop for a human

> Builds on [ADR-0068](0068-failure-asset-delta-and-learning-loop.md) (the
> post-merge revert-candidate guard and the `/revert` kill switch), which is
> what makes an unattended merge recoverable.

## Status

Accepted

## Context

Dependency PRs are the highest-volume, lowest-judgement work arriving at this
repository. Dependabot opens them weekly for `github-actions` and on demand for
security advisories; each one costs a maintainer a context switch to read a
version number and press a button that CI already told them was safe.

The repository is unusually well set up to delegate that button. `main` is
protected by required status checks that already encode "the project builds,
the dialect holds, the specs are honoured, the acceptance suite is green", and
the `Main` ruleset requires **zero** approving reviews — the human in the loop
today is a formality on exactly this class of PR, and a genuine gate on every
other class.

Two things make blanket auto-merge wrong, though:

1. **A green suite is not proof for a major bump.** Commit `8c5ab0b` is the
   evidence: a grouped `npm_and_yarn` bump passed everything and still had to
   be reverted. Breaking changes are, by construction, the ones the existing
   tests were never written to catch.
2. **Required checks are the definition of "everything passed", and that
   definition drifts.** At the time of writing, `neo-ci-gate`,
   `installer-ci-gate` and `adr index` run on every PR but are *not* required —
   so a Rust or installer regression could not block a merge. Any automation
   that trusts "CI is green" inherits whatever that set happens to be.

## Decision

**Patch and minor updates enable GitHub's native auto-merge; majors never do.**

- The workflow (`.github/workflows/dependabot-auto-merge.yml`) does not decide
  whether CI passed. It calls `gh pr merge --auto --squash` and stops. GitHub
  holds the PR until every required status check is green, and holds it
  indefinitely if one fails. The guarantee therefore lives in branch
  protection, in one place, where adding a gate automatically tightens the
  automation instead of silently bypassing it.
- Grouped updates are classified by the **highest** bump in the group, so a
  group containing one major is a major.
- A major is labelled `dependency-major` and handed to a Claude run that reads
  the release notes Dependabot embeds in the PR body, greps this repository for
  real usages of the affected APIs, and comments with the breaking changes, the
  blast radius (`file:line` citations), the migration, and a verdict. The
  analysis is advice; the merge decision stays with the maintainer.
- Claude receives the PR body and diff as **files on disk**, with only
  `Read/Grep/Glob/Write` available. Upstream release notes are third-party text
  and are treated as data; the comment is posted by a plain bash step under the
  job's own permissions.
- **The Claude half runs in a second workflow**
  (`dependabot-major-review.yml`), triggered by `workflow_run` on the first.
  This is not decomposition for its own sake: a Dependabot-triggered run
  resolves `secrets.*` against the **Dependabot** secret store, where an
  Actions secret like `CLAUDE_CODE_OAUTH_TOKEN` does not exist. A `workflow_run`
  executes in base-repo context and sees the normal Actions secrets, so the
  credential stays in exactly one store instead of being mirrored into a second
  one that has to be rotated in lockstep. The price is `workflow_run`'s own
  rule: GitHub always runs the copy of that file on the default branch, so it
  cannot be exercised from a PR — the first proof is the next real major.
- The set of required checks is widened to cover every component gate, so that
  "everything passed" means what it says.

Recovery is unchanged and already exists: `post-merge-guard.yml` flags a
post-merge suite failure on `main` as a revert-candidate, and `/revert` opens
the revert PR (ADR-0068). Auto-merge is safe to adopt precisely because that
tail was built first.

## Consequences

- Routine dependency PRs merge unattended, usually within the CI window, and
  the maintainer sees only the majors — the ones where judgement is the work.
- A red check on a patch bump no longer produces a notification anyone must
  act on; the PR simply never merges. Stale auto-merge-pending PRs become the
  signal to look at, and they are visible in the PR list.
- Every future CI gate must be added to branch protection to be load-bearing.
  A workflow that runs but is not required is now, explicitly, decoration.
- No new secret and no duplicated one: the analysis reads the existing Actions
  secret from base-repo context. If it is ever missing or the Claude step fails,
  the review workflow still posts a "not auto-merged, review by hand" comment
  rather than failing silently.
- Editing `dependabot-major-review.yml` has no feedback loop in CI. It is
  reviewed by reading, and validated in production on the next major.

## Alternatives considered

- **Auto-merge everything, rely on `/revert`.** Rejected: the revert path costs
  a broken `main` plus two PRs, and `8c5ab0b` shows majors reach it. The cost
  of reading a major bump is far lower than the cost of reverting one.
- **A workflow that polls check runs and merges when they are all green.**
  Rejected: it re-implements branch protection in bash, and its notion of "all
  checks" is a hardcoded list that rots the day a gate is added.
- **Requiring an approving review from a bot account.** Rejected: it adds an
  identity and a token to manage for a signal branch protection already gives
  for free.
- **Mirroring `CLAUDE_CODE_OAUTH_TOKEN` into the Dependabot secret store** so
  one workflow could do everything. Rejected: two copies of one credential,
  rotated by hand, in stores that no check compares. The `workflow_run` hop
  costs one file and keeps the credential singular.
- **OIDC workload-identity federation** (`anthropic_federation_rule_id`), which
  would remove the stored credential entirely. Deferred, not rejected: it is the
  better end state, but it needs a federation rule configured outside this
  repository, and it can replace the secret later without touching the split
  above.
- **Patch-only.** Rejected as too conservative for this repository's dependency
  mix, where minors are dominated by GitHub Actions and Haskell tooling that the
  required suite genuinely exercises.
