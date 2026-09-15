# ADR-0077: Local release notes and coordinated publication from main

## Status

Proposed

## Context

The maintainer wants semantic release automation without paying for model API
calls in CI. Local Codex already has the change context needed to write useful
release notes and migration instructions. Publication should use that reviewed
text without asking a model to reconstruct intent from commit titles.

The agreed public policy is `0.BREAKING.COMPATIBLE`: a breaking release increments
the second number and resets the third; a compatible feature or fix increments
only the third. Reaching version 1 is a separate explicit decision.

Framework, integrations, CLI, and starter form one user-facing platform. The
installer has a different job and can ship repairs independently. Existing
release tags are heterogeneous: framework `v0.*` and bare `0.*`, plus `neo-v*`
and `installer-v*`. Current manifests also differ. Matching numbers alone would
not prove compatibility: the CLI embeds an immutable framework revision in its
starter, verified by the executable compatibility contract.

Main requires pull requests and required checks, with no ruleset bypass actors.
A workflow cannot simply commit a changelog/version bump straight to main.
The existing implementation specs and their generated Unreleased changelog
contain engineering detail; they are not the desired user-facing release notes.

## Decision

### 1. Two release groups

- **Platform:** `nhcore`, `nhintegrations`, the reference application's package
  metadata, Neo CLI, and bundled starter share the platform release version.
  `CHANGELOG.md` is the public platform history.
- **Installer:** retains its independent `installer-vX.Y.Z` releases and gets
  `installer/CHANGELOG.md`. A platform release need not rebuild the installer.

Use the existing `neo-vX.Y.Z` downloadable release namespace, with the public
release title `NeoHaskell X.Y.Z`. The corresponding `vX.Y.Z` framework tag is an
alias at the same main commit; there is one platform GitHub Release with the
native assets and complete notes. This preserves the installer's existing
`neo-v*` resolution contract. Never move old tags or rewrite old releases.

Each affected group's highest impact determines its next version. A breaking
change in either a fragment or its merged Conventional Commit (`!`,
`BREAKING CHANGE:`, or `BREAKING-CHANGE:`) must not be silently downgraded.
The pipeline's commit validator must accept `!` syntax. Feature/fix/performance
changes need release notes; other changes may declare an explicit no-release
impact with a reason. Internal housekeeping does not release itself.
Classification uses the merged first-parent history and reviewed fragments,
not every transient feature-branch commit. Fragment groups disambiguate a
cross-component commit; an ambiguous breaking marker fails for local correction.

### 2. Temporary user-facing fragments

The local `neohaskell-release` skill prepares `.changes/<unique-slug>.md` in the
feature PR. One fragment describes one coherent user-facing change in one group;
a cross-group change can have two fragments. No version or date is assigned on
the feature branch. Files are consumed when the generated release-preparation
PR merges. Git history retains the reviewed originals.

Proposed fragment contract (the example is illustrative, not a real migration):

````markdown
---
group: platform
component: Framework
impact: breaking
category: Breaking changes
---

## Summary

Explain the observable change and who needs to act.

## Migration

Explain affected uses, exact edits, before/after examples, and verification.
State explicitly when persisted data or configuration also needs migration.

## Agent prompt

```text
Provide self-contained instructions: identify affected usages, apply the actual
API/configuration changes, and run the relevant checks. Include the concrete
examples and verification steps here; do not depend on "the example above".
Report unresolved cases instead of inventing a mapping or weakening tests.
```
````

Groups are `platform` and `installer`. Components are `Framework`, `Integrations`,
`CLI`, `IDE`, and `Installer`; the installer component belongs only to its group.
Impacts are `breaking`, `compatible`, and `none`. Categories are `Breaking changes`,
`Added`, `Improved`, `Fixed`, `Deprecated`, and `Removed`, in that order.
Breaking impact requires the breaking category, substantive migration guidance,
and an agent prompt. A non-breaking removal must explain why supported users are
unaffected. A no-release record explains its reason and is excluded from public
notes. Validation checks structure and required content; human review is what
establishes that the migration advice is correct.

Implementation specs remain separate. Haskell PRs still carry their existing
specs, while CLI-only PRs may carry a release fragment without a Haskell spec.
The skill can be invoked directly and is also called before final substantive
review in the feature pipeline. It uses the local signed-in Codex session.

The shared `neohaskell-pr` skill owns PR names, stack management, and descriptions
that lead with outcomes Jess understands. Its
[Jess writing guide](../../.agents/skills/neohaskell-pr/references/jess-writing.md)
is the common prose standard for PR introductions and all release notes. Jess is
the time-constrained junior application developer, not a framework maintainer.
Before publication, every release entry must explain what changed, whether Jess
is affected, what action she needs to take, and how to verify it, without assuming
knowledge of implementation internals. Migration prompts must also be understandable
to Jess so she can judge what she is asking her agent to do. Unclear prose is not
ready to publish, even if technically accurate; rewrite it locally. CI can check
structure and references but cannot certify comprehension. Final prose review
belongs with the local authoring and maintainer review, not a hosted model call.

### 3. Deterministic changelog format

Each release section contains:

1. `## X.Y.Z — YYYY-MM-DD`, newest release first.
2. Category subsections in the fixed order above, omitting empty categories.
3. Concise component-labelled bullets with PR links (commit links when there is
   no PR association); ordering within a category follows merge order and slug.
4. For breaking releases, `Migration from <previous release>` with per-change
   instructions and collapsible copy-paste agent prompts.
5. A compare link between the actual prior and current tags.

The release body is the same generated Markdown section as the changelog. User
prose and code fences are preserved; the generator never rewrites or summarizes
them. There is no Unreleased section: `.changes/` is the pending material.
Migration prompts must work when copied alone, including when several unrelated
changes appear in the same release. The renderer adds a deterministic from/to
version preamble inside each copyable prompt and preserves the authored
instructions following it; the local fragment need not guess a future version.

The date is the planned UTC release date captured once in the release manifest;
retries do not recompute it. GitHub's actual publication timestamp is separate.
Historical changelog content is retained with a clearly labelled legacy boundary
and is not silently reclassified as a new release.

### 4. Main-based release preparation, then publication

The workflow runs on main pushes, with a main-only dispatch path for retries.
Feature PR CI validates fragments and previews output but cannot publish or
change versions. Preparation captures an exact main source SHA and examines the
merged change set since the last completed release for each group.

Preparation creates or updates a dedicated release PR containing only derived
changes: version metadata/lockfiles, changelog sections, a machine-readable
release manifest, and removal of the consumed fragments. The manifest records
the source range, prior and target versions, fragment identities and hashes,
planned date, and note hashes. New main changes invalidate a stale preparation
until it is regenerated; they cannot disappear behind already-consumed notes.
An in-progress or partially published release must be resolved before planning
the next version. The maintainer merges the release PR after required checks.

The release manifest is provenance, not an authorization token. The publisher
independently reconstructs the expected preparation and checks the merged diff,
source history, and allowed paths. A release-looking title, label, or bot author
is insufficient to bypass spec/fragment checks or authorize publication.
Generated release PRs have a narrowly verified bookkeeping path through the
existing spec and changelog gates; normal feature PRs keep their contracts.

After the preparation PR's merge reaches main, the workflow builds the exact
release revision and publishes only after native build, checksum, portability,
and consumer verification succeed. Failed builds leave a retriable release,
not an apparently complete public release. The first rollout does not cut a
production release merely as a validation exercise.

Use the built-in `GITHUB_TOKEN`; no model API keys, personal Codex credentials,
or new hosted AI bots. Main protection remains unchanged. Repository settings
must permit Actions to create PRs. GitHub may require a maintainer to approve CI
runs on a PR created by `GITHUB_TOKEN`; the workflow must explain that state
instead of waiting silently or claiming success. This is an operational release
step, outside the feature pipeline's two approval gates.

Call build/publication jobs explicitly through the orchestrating workflow or
reusable workflows. Do not assume tags created using `GITHUB_TOKEN` will trigger
the old tag workflows. Existing manual rehearsal paths remain non-publishing
unless the same main-release provenance checks pass.

### 5. Released starter provenance

A commit cannot embed its own SHA. Therefore the release build, after the
preparation PR has merged as revision R, deterministically prepares the embedded
starter to pin R before compiling the CLI. It updates all starter pins together
and obtains a valid Nix lock for R; editing only a lock's revision while retaining
an old content hash is forbidden.

This preparation occurs in the isolated release workspace and is part of the
reproducible build recipe. The source tag remains R, which contains the correct
platform package versions. Release assets include the generated starter inputs
and provenance/checksums so the release can be reconstructed from R. Every
native target uses the same prepared inputs. The generated compatibility
manifest and consumer verification read those actual prepared inputs.

Ordinary development builds continue using the committed starter pin. Their
compatibility manifest must remain truthful; sharing the release version does
not make an arbitrary local build the published artifact. Local tooling must be
able to rehearse the exact release preparation explicitly.

### 6. Recovery and bootstrap

Serialize release mutations; repeated events and skipped/coalesced pending runs
must not lose merged fragments. Work from the current recorded main snapshot,
not the assumption that one event equals one unreleased commit. Existing tags
and completed release assets are immutable inputs: verify and reuse an identical
result, otherwise stop with the conflicting tag/SHA/hash. Never force-retag or
silently overwrite a completed release. Publication failures resume the same
manifest/version; they do not calculate another bump.

Bootstrap explicitly records the existing release baselines, accepting both
historical bare and v-prefixed framework tags and rejecting conflicting aliases.
The first coordinated platform version is prepared as an explicit local release
plan (the charter targets 0.10.0), with notes covering its declared range. The
new automation does not invent migration prose for the historical backlog or
pretend old independent CLI/framework versions were already in lockstep.

## Validation

[Change 009](../changes/009-semantic-releases.md) owns the proving-test contract.
Fixtures exercise version rules, fragment parsing, exact Markdown rendering,
temporary Git histories, generated-diff validation, fake GitHub failures and
retries, and workflow permission/trigger wiring. Existing native release and
consumer gates continue proving the delivered assets. These checks cannot prove
live GitHub publication before the workflow exists on main; report that limit.

## Consequences

Users get one platform release story and explicit migrations; installer fixes
can ship independently. Unchanged platform components may receive a new shared
version, and a platform release waits for all of its artifact gates.

Release prose is reviewed with implementation and is available without an AI
service during publication. Temporary fragments avoid shared-changelog conflicts.
Generated metadata is committed through the repository's normal protections.
The tradeoff is a release-preparation PR and potentially an Actions approval,
rather than an unattended direct push into protected main.

## Sources

- [GitHub workflow triggering and GITHUB_TOKEN](https://docs.github.com/en/actions/how-tos/write-workflows/choose-when-workflows-run/trigger-a-workflow)
- [GitHub ruleset rules](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/managing-rulesets/available-rules-for-rulesets)
- [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/)
- [SemVer initial development](https://semver.org/)
- [Changesets](https://github.com/changesets/changesets)
- [Towncrier](https://towncrier.readthedocs.io/en/stable/)
