# Change 009: Prepare release notes locally and publish coordinated releases from main

Give NeoHaskell one user-facing platform release version for the framework,
integrations, Neo CLI, and bundled starter; keep the installer independently
versioned. Codex prepares reviewed user-facing notes in temporary `.changes/`
files. Deterministic GitHub Actions assign versions from changes merged to
`main`, generate changelogs, and publish releases without model API credentials.
Implementation specifications remain in `docs/changes/` and are not release prose.

```yaml spec
issue: adhoc:semantic-releases
kind: feature
touches: [dev-pipeline, ci-cd, governance-docs, installer, website]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

No Haskell application API change. This changes repository release tooling,
metadata, agent skills, and artifact publication. Release commands are registered
through `dev`; their behavioral contracts are proved below.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Version planning recognizes Conventional Commit breaking markers and explicit fragment impact; for each affected group, any breaking change maps 0.Y.Z to 0.(Y+1).0 and other releasable changes map to 0.Y.(Z+1), never automatically to 1.0.0; unrelated tags, malformed versions, duplicate aliases, and no-release changes cannot select a wrong baseline or bump | `script:scripts/semantic-release#--self-test versions` | unit | none |
| C2 | Fragment validation accepts platform and installer records independently, rejects invalid or duplicate metadata, traversal and symlinks, empty or placeholder summaries, and breaking changes without migration steps, verification, and a self-contained agent prompt; authoring needs no final version | `script:scripts/semantic-release#--self-test fragments` | unit | none |
| C3 | Changelog generation is deterministic, newest-first, omits Unreleased and empty categories, includes component labels and source links, and preserves nested Markdown/code fences; breaking entries carry user migration guidance and collapsible agent prompts; the GitHub release body is the corresponding changelog section | `script:scripts/semantic-release#--self-test render` | unit | none |
| C4 | Preparation reads an immutable merged main snapshot, maps fragments to their introducing squash/merge commits, handles accumulated changes exactly once, generates only permitted version/changelog/manifest/fragment-consumption edits, and excludes release housekeeping from future bumps; no-release and invalid input perform no writes | `script:scripts/semantic-release#--self-test prepare` | unit | none |
| C5 | Publication accepts only a merged, verified release-preparation result on main; feature branches, spoofed release subjects, stale plans, foreign SHAs, unaccounted source changes, and manually altered generated metadata cannot publish | `script:scripts/semantic-release#--self-test provenance` | unit | none |
| C6 | Retries preserve the planned version, release date, notes, source, and checksums; identical completed releases are no-ops, mismatching existing tags/assets fail, partial publication can resume, and concurrent main merges cannot lose fragments, downgrade a version, or move an existing tag | `script:scripts/semantic-release#--self-test recovery` | unit | none |
| C7 | A platform release synchronizes its package versions, emits a released CLI whose starter pins the exact main release revision coherently, and retains checksum/portability/consumer checks; the compatibility manifest describes the actual shipped starter and generated release inputs | `script:scripts/semantic-release#--self-test artifacts`<br>`script:scripts/neo-release#--self-test`<br>`script:scripts/neo-consumer-contract#--self-test` | unit | none |
| C8 | The workflow retains required PR checks and main protection, explicitly connects preparation/build/publication without relying on GITHUB_TOKEN-created tag events, grants write permissions only to necessary jobs, and never invokes an AI provider; installer and platform releases stay independently selectable | `script:scripts/semantic-release#--self-test workflow`<br>`script:scripts/workflow-check#--self-test` | unit | none |
| C9 | The pipeline routes release-note authoring to a discoverable local skill before final substantive review, accepts Conventional Commit exclamation-mark syntax, gates fragments instead of feature-branch changelog edits, and recognizes generated release bookkeeping only after checking its exact derivation rather than trusting an actor, title, or label | `script:scripts/neo-skills-check#--self-test`<br>`script:scripts/pipeline-state#--self-test`<br>`script:scripts/spec-check#--self-test` | unit | none |
| C10 | Bootstrap preserves historical changelog content and all existing tags/releases, requires an explicit reviewed baseline and initial platform release plan, and does not classify or publish the entire historical backlog merely because the new workflow was installed | `script:scripts/semantic-release#--self-test bootstrap` | unit | none |

These are deterministic unit/fixture contracts, including temporary Git histories
and mocked GitHub responses; they do not claim a live publication or a successful
native release build. PR-ready validation must also run the existing native build,
checksum, portability, and generated-project consumer gates for changed release
paths. Live publication is exercised only after the workflow reaches `main` and a
release-preparation PR is merged. No production release is cut to test this PR.

## User impact

The approved version and changelog design is specified in ADR-0077. Users get one
platform version, component-labelled release notes, and actionable migration
guidance with a copy-paste prompt for their coding agent. The installer keeps its
own version and changelog. There is no application migration caused by this
tooling change itself; later breaking releases supply their own real migrations.

Two operational details accompany the agreed format:

- Main currently requires a PR with no bypass actors. Version updates, fragment
  consumption, and committed changelogs therefore land through a generated
  release-preparation PR, not a direct workflow push to main. Its merge triggers
  builds and publication. This is a separate release decision after the feature
  pipeline's existing two gates, not a third feature-implementation gate.
- The date in the generated section is the planned release date, fixed in the
  reviewed release manifest. GitHub records the actual publication timestamp
  separately; a delayed or retried publication does not rewrite committed history.

## ADR

[ADR-0077](../decisions/0077-local-release-notes-and-main-publication.md) records
the release grouping, temporary note format, protected-main publication flow,
bootstrap policy, and reproducible starter pinning. This is significant release
infrastructure even though it adds no public Haskell API or build dependency.
