# Change NNN: <imperative title>

<!-- This template is ITSELF a valid spec instance: CI validates it with
     `./dev spec-check` (checks.yml `spec` job), so its machine-read parts
     (header keys, fence infos, `## section` names, criteria cells) can't drift
     from the validator. Copy it to docs/changes/NNN-slug.md (next 3-digit
     number) as the FIRST commit of a draft PR — the spec is the contract the
     maintainer approves before implementation starts. -->

One paragraph of intent: what the requester asked for, in their vocabulary.

```yaml spec
issue: adhoc:example            # issue#NNN or adhoc:<slug>
kind: feature                   # feature | bug | refactor
touches: [core-primitives]      # capability IDs from codemap/capabilities.yaml (closed list)
breaking: false                 # MUST be true if the contract delta has any `-` line
new-dependency: false           # any new build-depends / flake input
new-capability: false           # this change adds a row to codemap/capabilities.yaml
new-extension-point: false      # this change adds a row to codemap/extension-points.yaml
```

## Contract delta

The promised public-API diff, in `codemap/signatures/` vocabulary:
`<+|-> <Module>: <signature line>`. At PR-ready, `./dev spec-drift` verifies
the regenerated signatures honor every line. Internal-only changes promise an
empty block — that is a first-class spec, not a degenerate one.

```diff signatures
+ Text: exampleNewFunction :: Text -> Text
```

## Criteria

Every numbered criterion contains only typed proving-test locators and declares
its level plus real boundary. Supported forms are
`hspec:<suite>:<repo-path>#<exact-match>`, `script:<repo-path>#<arguments>`, and
`hurl:<repo-path>`. Separate multiple independent locators with `<br>`; prose,
duplicates, unresolved paths, zero/ambiguous matches, and unknown suites fail.
`unit` uses boundary `none`; `integration` uses an attested boundary from
`test-surfaces.json`; `acceptance` uses an exact Hurl path and `http:real`.
For `kind: bug`, C1 is the failing reproduction test, committed red in the
draft PR: the repro **is** the spec and Gate 1 requires its current red receipt.

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | example: slugifies unicode titles | `hspec:nhcore-test-core:core/test/TextSpec.hs#slugifies unicode` | unit | none |

## User impact

Breaking? Testbed effect? Migration note? "None" is an acceptable answer;
silence is not.

## ADR

Not required — no trigger (breaking / new-dependency / new-capability /
new-extension-point all false).

<!-- When any trigger flag is true, this section MUST link the decision,
     e.g. [ADR-00NN](../decisions/00NN-slug.md) — the ADR is part of the
     spec the maintainer reviews at the gate, and lands with the merge. -->
