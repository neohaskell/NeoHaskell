# Change NNN: <imperative title>

<!-- This template is ITSELF a valid spec instance: CI validates it with
     `./dev spec-check` (checks.yml `spec` job), so the template can never
     drift from the validator. Copy it to docs/changes/NNN-slug.md (next
     3-digit number) as the FIRST commit of a draft PR — the spec is the
     contract the maintainer approves before implementation starts. -->

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

Every numbered criterion names the test that proves it and declares its level.
A behavior that crosses a real boundary (filesystem, Postgres, HTTP) must
declare `integration` or `acceptance` — a mocked unit test cannot satisfy it.
For `kind: bug`, C1 is the failing reproduction test, committed red in the
draft PR: the repro **is** the spec.

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | example: slugifies unicode titles | `TextSpec` "slugifies unicode" | unit |

## User impact

Breaking? Testbed effect? Migration note? "None" is an acceptable answer;
silence is not.

## ADR

Not required — no trigger (breaking / new-dependency / new-capability /
new-extension-point all false).

<!-- When any trigger flag is true, this section MUST link the decision,
     e.g. [ADR-0067](../decisions/0067-slug.md) — the ADR is part of the
     spec the maintainer reviews at the gate, and lands with the merge. -->
