---
name: neohaskell-release
description: Prepare NeoHaskell release notes and breaking-change migrations locally with Codex, validate pending fragments, and manage explicit manual bootstrap or failed-release recovery through the main release workflow. Use before final PR review and when preparing or repairing a release.
---

# Release notes and publication

Use the local Codex session for writing and judgment. CI performs deterministic
versioning, builds, and publication with GitHub's built-in token; it calls no AI
provider. Read [Writing for Jess](../neohaskell-pr/references/jess-writing.md)
before drafting. If Jess cannot tell what changed, whether her app is affected,
what to do, and how to check the result, rewrite the notes.

## During a change PR

1. Inspect the approved spec and actual diff against the owning stack base.
   Separate implementation guidance in `docs/changes/` from user release prose.
2. Copy [the fragment template](assets/change.md) to `.changes/<unique-slug>.md`.
   Never reuse a consumed filename. Use one fragment per affected component;
   avoid parallel edits to one shared pending document.
3. Use `group: platform` for Framework, Integrations, CLI, or IDE. The framework,
   integrations, CLI and bundled starter share one platform version. Installer
   publication is retired; installer release fragments are rejected.
4. Declare `impact: compatible`, `breaking`, or `none`. Features, fixes and
   performance improvements normally mean compatible. Internal CI/docs work
   uses none with a concrete reason in Summary. A breaking Conventional Commit
   (`!` or `BREAKING CHANGE:`) requires real breaking notes. Do not hide a user
   change behind an internal commit type.
5. Write a concise Summary. Categories are Breaking changes, Added, Improved,
   Fixed, Deprecated, Removed. Omit migration sections for compatible/none
   changes. Breaking fragments require category Breaking changes, Migration
   with a `### Verify` subsection, and a fenced Agent prompt.
6. For a breaking change, explain affected apps, old and new behavior, exact
   edits, and verification commands grounded in the actual API. The prompt must
   work when pasted alone: include detection, edits, preserved behavior, tests,
   and what to report. Never refer to “the guide above.” Do not invent commands.
   Publication adds the actual old/new version preamble automatically.
7. Run `./dev semantic-release check --base <owning-base>` and apply the Jess
   review. Include these notes in the final substantive PR review. Feature PRs
   do not assign versions/dates or edit CHANGELOG.md. The existing
   pipeline still has two human gates; note authoring adds no approval gate.

## What happens on main

After activation, main merges are classified from their Conventional Commit and
reviewed fragments. Versions remain `0.BREAKING.COMPATIBLE`: breaking increments
the second number and resets the third; features/fixes increment only the third.
A generated PR proposes package versions, dated changelog sections, fragment
consumption, and an immutable record in `scripts/releases/manifest.json`.
Generated bookkeeping gets a spec exception only after its complete diff is
reconstructed and verified. It still needs normal required CI and merge review.
A title, actor, label, or manually edited manifest cannot grant that exception.

Merging a fresh preparation builds the exact merged revision. Its starter inputs
are frozen once with a real Nix content hash and embedded in every CLI target.
Four native builds, portability/install checks and the generated-app consumer
check must pass before publication. The release body equals its committed
changelog section. Platform tags `neo-vX` and `vX` share the same commit;
The planned date is the UTC date of the main source snapshot and stays fixed
across retries. The first draft asset, `release-bundle.zip`, freezes all verified
inputs and binaries; retries restore it and skip rebuilding.

If main advances before a preparation merges, its provenance check fails.
Dispatch `auto` on main to prepare a fresh PR; close the stale PR after verifying
the replacement. Never edit generated files, update-branch, or bypass the check
on the stale PR. The workflow does not merge PRs or change branch protection.
GitHub may require approving workflow runs created by its built-in token.

## First release: explicitly manual

Installing this workflow ships an empty ledger. It performs **no preparation PR,
tag, or release write** on main until a maintainer explicitly starts a bootstrap.
Automation activates only when that manual first release is public and its source,
notes, aliases, assets and checksums verify. A draft/failed bootstrap cannot
activate it. Historical tags and changelog prose are preserved.

When the user asks to make the first release **after this CI PR is merged**:

1. Check `gh api repos/neohaskell/NeoHaskell/actions/permissions/workflow`.
   Preparation PRs require `can_approve_pull_request_reviews: true`, controlled by
   **Settings → Actions → General → Workflow permissions → Allow GitHub Actions
   to create and approve pull requests**. Include enabling that setting in the
   authorized first-release setup if disabled; keep default token access read-only
   and retain main protection. Do not enable it merely to install or test this PR.
   See [GitHub's setting documentation](https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/enabling-features-for-your-repository/managing-github-actions-settings-for-a-repository).
2. Prepare and merge reviewed fragments for the desired initial release using
   the normal change workflow. Do not synthesize the entire historical backlog.
3. Inspect existing immutable tags. Agree on the existing baseline tag and target
   version; the target must exceed both the baseline and all reserved attempts.
   Do not infer a baseline from whichever unrelated tag sorts highest.
4. Dispatch from main, substituting the reviewed values:

   ```sh
   gh workflow run semantic-release.yml --ref main \
     -f operation=bootstrap \
     -f baseline=<existing-v-or-neo-v-tag> -f version=<0.Y.Z>
   ```

5. Review the resulting preparation PR, required checks, rendered notes and
   migration prompt. Merge only within the user's release/merge authorization.
   Watch the main run and verify the public release and all assets. This public
   first release activates automatic platform releases.

Do not run bootstrap merely to test or install the automation. This first CI PR
must not cut a release; the manual first release is a separate requested task.

## Retry or recover a failed release

- Transient failure: dispatch `operation=auto` on main. The recorded version,
  source, date, notes and assets stay immutable. Matching uploads are reused;
  existing mismatches stop publication. Never delete or overwrite assets/tags
  to make a retry pass.
- Source correction: merge the needed fix with its own reviewed fragment, then
  dispatch `operation=recover`, `attempt=<full ID from manifest>`, and a concrete
  reason. This first creates a durable pause branch for that ID while holding
  the publication lock, then opens a recovery PR. Review/merge that exact PR.
  It restores consumed notes once and removes only the unpublished changelog
  section. Published history, draft assets and tags remain unchanged.
- The failed version remains reserved. For example, published 0.4.2 followed by
  failed breaking 0.5.0 produces replacement breaking 0.6.0, migrating from 0.4.2.
  A failed first bootstrap needs a new explicit bootstrap; it cannot self-activate.
- Cancel a recovery before merging: close its PR and explicitly dispatch resume
  with the attempt ID, then auto. Once the recovery PR merges, resume is refused.
  Never delete the pause branch manually. Published releases cannot be abandoned.

Validation: `./dev semantic-release --self-test`, `./dev workflow-check`, and
`./dev neo-skills-check`. When changing platform tags, assets, or release
workflows, also run `cargo test --manifest-path installer/Cargo.toml --locked
--all-features`: `installer/tests/consistency.rs` binds existing installer
downloads to the current platform publisher. Retiring installer publication
does not retire this compatibility gate. Fixtures prove contracts; they do not claim a live
four-target build or production publication. For PR titles and status updates,
use [neohaskell-pr](../neohaskell-pr/SKILL.md).
