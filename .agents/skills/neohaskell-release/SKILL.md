---
name: neohaskell-release
description: Prepare NeoHaskell release notes and breaking-change migrations, validate release fragments and generated release bookkeeping, and investigate or recover a failed release. Use before final PR review and when explicitly preparing or repairing a release.
---

# NeoHaskell release workflow

Use this skill for release notes, breaking-change migrations, local release
validation, or an explicitly requested release recovery. CI performs
deterministic planning and publication. Read [Writing for Jess](references/jess-writing.md)
before drafting notes.

Generated application behavior and the embedded IDE bundle are owned by
[NeoHaskell consumer verification](../neohaskell-consumer-verification/SKILL.md).
Use that skill for `neo new` outcome checks or committed IDE bundle fidelity.
Framework export, TH marker and generated-API changes carry their starter and
testbed migrations in the same PR under that skill's verification contract.

## Local entrypoints

Run tools from the repository root, or set `NEOHASKELL_ROOT` to the checked-out
repository. The skill owns its scripts, configuration, tests and release
history. No root `./dev` dispatcher is required.

```sh
./.agents/skills/neohaskell-release/scripts/semantic-release check --base <owning-base>
./.agents/skills/neohaskell-release/scripts/semantic-release --self-test
./.agents/skills/neohaskell-release/scripts/neo-release --self-test
./.agents/skills/neohaskell-release/scripts/cache-prime --self-test
```

`semantic-release --self-test` runs the release engine's local fixture suite. It
uses an in-memory GitHub API double and makes no GitHub calls. The full generated
bookkeeping exception remains in the release check. It reconstructs the complete
expected diff from the immutable source revision and rejects any unaccounted
file or mode change. Archived release retries load immutable source and
preparation commits, so CI must fetch complete Git history before running it.

## Activate the CI migration

The [required-check configuration](references/required-checks.json) prepares seven main-branch contexts: `hlint (dialect gate)`, `ci-gate`, `adr index (decisions README faithful)`, `neo-ci-gate`, `secret scan (gitleaks, deterministic)`, `owned tooling checks`, and `release fragment check`. Every entry uses Actions app ID 15368, and `strict` remains false. This is source configuration only; it does not alter live protection settings.

Coordinate the protection update with landing the workflow migration. Requiring the new check names before main produces them blocks other pull requests. Capture the prior required-check configuration and compare fresh settings immediately before an authorized landing. If landing cannot proceed after the update, restore that captured configuration. Contributors use the automatically maintained local codemap without committing generated output.

## During a change PR

1. Inspect the approved specification and the actual diff against the owning
   stack base. For changed exported or internal symbols, complete the
   [application-boundary audit](references/app-boundary.md) before selecting impact
   or drafting a migration. Keep implementation guidance separate from user
   release prose.
2. Copy [the fragment template](assets/change.md) to
   `.changes/<unique-slug>.md`. Use one fragment per affected component and do
   not reuse a consumed filename.
3. Use `group: platform` for Framework, Integrations, CLI or IDE. Installer
   publication is retired, and installer release fragments are rejected.
4. Declare `impact: compatible`, `breaking` or `none`. Features, fixes and
   performance improvements normally mean compatible. Internal refactors with
   unchanged supported app contracts, CI work, and docs work use none with a
   concrete reason in Summary. Apply the audit to distinguish app contracts from
   direct-import compatibility caveats.
   A nonbreaking `fix(deps)` whose entire Git diff matches the release engine's
   verified dependency-maintenance shapes needs no platform fragment. The
   engine applies the same rule during PR validation and main release planning.
   A mixed diff or breaking declaration keeps the normal fragment requirement.
5. Write a substantive Summary. Breaking fragments require category
   `Breaking changes`, a Migration section with `### Verify`, and a fenced,
   self-contained Agent prompt. Ground edits and checks in the maintained API.
6. Run the local fragment check against the owning base and include the release
   note review in the final substantive PR review. Feature PRs do not assign
   versions or dates and do not edit changelogs.

Versions use `0.BREAKING.COMPATIBLE`. Breaking changes increment the second
number and reset the third. Features and fixes increment only the third. A
generated release preparation records the proposed release in the canonical
manifest at `references/manifest.json` and proposes package versions, changelog
sections and consumed fragments.

## Durable release history

The canonical release ledger is
`references/manifest.json`. Its initial contents preserve the existing records
byte for byte. When reading a historical revision, the engine falls back to the
live root `scripts/releases/manifest.json` path if the canonical path is absent.
If both canonical and live root ledgers exist, they must contain the same
records or validation stops with an inconsistency error. Release history is
versioned with the owning skill and remains available without local archives.

Never replace an existing ledger with an empty bootstrap ledger. Bootstrap is
reserved for a repository with no release history. This checkout already has
release records, so its normal operation reads that history and does not use
bootstrap.

## Releases on main

After a change with release impact reaches main, the complete unreleased
first-parent history and reviewed fragments determine the platform impact.
Squash, two-parent merge and rebase merges are supported. Notes may be committed
after the code. Planning considers the whole range and does not require a
fragment in every implementation commit.

A preparation freezes the source revision, version, date and rendered notes.
The merged preparation builds from that exact revision. The starter input is
frozen with a Nix content hash and embedded in each current CLI target. The
source configuration declares `native_policy: native-v2`, and each new plan
records `target_policy: native-v2` before its identity is calculated. This
policy selects Linux x86-64 and Apple silicon macOS. Historical plans without
that field keep their frozen four-target contract so a pending attempt can
resume unchanged. `neo-release targets` lists current build targets; its
inspect, verify and install commands also accept the historical target
formats. Two native builds, portability and install checks,
and the generated-app contract must pass before publication. The release body
must equal the committed changelog section. Platform tags `neo-vX` and `vX`
share the same commit. Retries reuse verified inputs and assets; any mismatch
stops publication.

If main advances before a preparation merges, its provenance check fails.
Dispatch `auto` on main to prepare a fresh PR, then close the stale PR after
verifying its replacement. Do not edit generated files or bypass provenance.
The workflow does not merge PRs or change branch protection.

## First release for a repository without history

The workflow and CLI do not create new bootstrap preparations. Automatic
preparation remains inactive when the release ledger has no attempts. Preserve
that state and coordinate a separately reviewed release-history migration; do
not infer a baseline from tags or reset the ledger.

## Retry or recover a failed release

- For a transient failure, dispatch `operation=auto` on main. The recorded
  version, source, date, notes and assets remain immutable. Matching uploads are
  reused, and mismatches stop the retry.
- For a source correction, merge the fix with its own reviewed fragment, then
  dispatch `operation=recover` with the full attempt ID and a concrete reason.
  Review and merge that recovery PR. It restores consumed notes once and removes
  only the unpublished changelog section.
- A failed version remains reserved. A replacement breaking release advances
  beyond it and migrates from the last published baseline.
- To cancel a recovery before merging, close its PR and explicitly dispatch
  resume with the attempt ID, then auto. Never delete the pause branch manually.

Validation fixtures do not claim a live two-target build or production
publication. The complete generated-app result is reported only by the
consumer-verification contract.
