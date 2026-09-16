# Change 013: Teach NeoHaskell with progressive depth and a practice project

Publish the approved human documentation methodology and a source-grounded learning
journey from evaluating NeoHaskell through building, connecting, operating, and
evolving applications with an AI coding partner. Use ecommerce as the continuing
example and practice project, with general-purpose concepts leading the topic pages.
Preserve direct consultation and an accessible evaluator path; no existing company's
story is required. Agent-facing documentation is separate.

The introduction develops the philosophy before code through longer conceptual
chapters and editable visual explanations. History, correction, feature slices,
shared modeling, and human-agent trust form the accessible foundation. Diagram
sources, exports, text alternatives, and review evidence join the maintenance gate.
Worked Event Models use the canonical command/event/read-model colours and trace
concrete values across a timeline. Actual IDE screenshots connect the notation to
the tools; binary image hashes and text alternatives join the documentation checks.

```yaml spec
issue: adhoc:human-documentation
kind: feature
touches: [website, ci-cd, governance-docs]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

No Haskell or Rust public APIs change.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Every planned human page has source evidence, review coverage, and valid local links | `script:website/scripts/check-docs.mjs#--check` | unit | none |
| C2 | The documentation gate rejects missing pages, source drift, invalid links, and absent review evidence | `script:website/scripts/check-docs.mjs#--self-test` | unit | none |

## User impact

Jess can learn NeoHaskell through a cohesive ecommerce practice project, apply the
concepts to her own domain, evaluate business tradeoffs without reading every
implementation section, and consult individual topics. Existing
getting-started and deployment routes remain usable. The source examples distinguish
implemented public capabilities from shop-specific designs. The authoring plan is
`website/DOCUMENTATION_PLAN.md`; the review record explicitly separates automated
verification from a future human reader trial and live external-provider testing.

## ADR

Not required — no breaking API, new dependency, capability, or extension point.
