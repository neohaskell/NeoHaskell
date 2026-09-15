# Change 012: Teach the human ecommerce journey with progressive depth

Publish the approved human documentation methodology and a source-grounded learning
journey from evaluating NeoHaskell through building, connecting, operating, and
evolving a fictional shop with an AI coding partner. Preserve direct consultation
and an accessible evaluator path. Agent-facing documentation is separate.

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

Jess can follow one ecommerce learning journey, evaluate business tradeoffs without
reading every implementation section, and consult individual topics. Existing
getting-started and deployment routes remain usable. The source examples distinguish
implemented public capabilities from shop-specific designs. The authoring plan is
`website/DOCUMENTATION_PLAN.md`; the review record explicitly separates automated
verification from a future human reader trial and live external-provider testing.

## ADR

Not required — no breaking API, new dependency, capability, or extension point.
