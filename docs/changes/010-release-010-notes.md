# Change 010: Prepare the 0.10.0 release notes and upgrade guides

Explain what app developers gain when upgrading from NeoHaskell 0.9.0, what
existing code needs to change, and how to verify the upgrade. Audit the Git
history and both endpoint trees with parallel local agents, then consolidate
the verified findings into reviewed release fragments. Prepare a draft PR;
publication remains a later, explicit manual bootstrap on main.

```yaml spec
issue: adhoc:release-010-notes
kind: feature
touches: [dev-pipeline, governance-docs]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

This PR documents existing changes; it introduces no application API changes.
Its fragments describe historical breaking changes against the published tag
`0.9.0` (`b80204df3586cb071554c986699997201795c6e1`). The audited target is
`d2201cc555d55cc558d751ff67d04569a42eda18`, the merged release automation.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Pending fragments pass the existing metadata, migration, verification and standalone prompt contract | `script:scripts/semantic-release#check --base origin/main` | unit | none |
| C2 | The renderer preserves migration examples and collapsible prompts in the generated changelog/release section | `script:scripts/semantic-release#--self-test render` | unit | none |
| C3 | Bootstrap requires explicit baseline and version selection and installation remains inactive until verified publication | `script:scripts/semantic-release#--self-test bootstrap` | unit | none |

In addition to these mechanical checks, review every entry against endpoint code
and the Jess writing guide. A local bootstrap preview must select 0.10.0 from
0.9.0 and include all releasable fragments. Record the audit coverage and actual
verification in the draft PR. Fixture tests do not prove the prose or build a
published binary.

## User impact

The prepared notes cover Framework, Integrations, CLI and IDE. They explain
affected applications, exact old-to-new edits and verification, with copy-paste
agent prompts for breaking changes. Changes to APIs introduced after 0.9.0 are
described as additions in their final form, rather than unnecessary migrations.
Neo CLI features already published in the independent `neo-v0.1.x` series are
identified as joining the coordinated platform release.

Installer publication, internal automation history, dependency-only updates,
unimplemented designs and reverted changes are excluded from user release prose.
No package versions, permanent changelog, release ledger, repository settings,
tags or published releases change in this PR. After review and merge, a separate
manual bootstrap prepares the generated 0.10.0 release PR on main.

## ADR

Not required — no new API or architecture decision. This applies
[ADR-0077](../decisions/0077-local-release-notes-and-main-publication.md).
