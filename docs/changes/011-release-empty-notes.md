# Change 011: Allow an empty pending release-note queue

Unblock the first coordinated release without weakening normal codemap checks.
Release preparation consumes every pending `.changes/` fragment, so a complete
release legitimately leaves no tracked files there. PR #874 currently fails
because the codemap treats that expected state as a stale ownership rule.

```yaml spec
issue: adhoc:release-empty-notes
kind: bug
touches: [dev-pipeline, governance-docs, ci-cd]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

No application API changes. Allow only the existing `dev-pipeline` ownership
glob `.changes/**` to be empty, with an explicit explanation that publication
consumes its files. Continue checking all populated paths for duplicate owners
and reject every other unmatched ownership glob. Run the regression coverage
from the existing codemap CI job so the empty state remains protected.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Codemap validation succeeds after all pending release fragments have been consumed | `script:scripts/test-codemap-empty-changes#ReleaseNotesOwnership.test_consumed_release_notes` | unit | none |
| C2 | Codemap validation still accepts the populated pending-note queue | `script:scripts/test-codemap-empty-changes#ReleaseNotesOwnership.test_pending_release_notes` | unit | none |
| C3 | An unrelated missing owned path still fails the ghost-entry check | `script:scripts/test-codemap-empty-changes#ReleaseNotesOwnership.test_other_missing_owned_paths_still_fail` | unit | none |

## User impact

Release preparation can pass CI after consuming its notes. Existing apps and
their migration instructions do not change. This is internal CI maintenance
with no release impact. Keep the generated preparation PR immutable: after
this fix merges, dispatch a fresh 0.9.0-to-0.10.0 bootstrap and retire the stale
preparation only after its replacement is verified.

## ADR

Not required — preserves the existing release design and codemap ownership.
