# Change 013: Teach NeoHaskell with progressive depth and a practice project

Publish the approved human documentation methodology and a source-grounded learning
journey from evaluating NeoHaskell through building, connecting, operating, and
evolving applications with an AI coding partner. Use ecommerce as the continuing
example and practice project, with general-purpose concepts leading the topic pages.
Preserve direct consultation and an accessible evaluator path; no existing company's
story is required. Agent-facing documentation is separate.

The reader installs the released CLI, creates one project with `neo new`, and
continues it through the cart, IDE, tests, integrations, and deployment. Complete
tutorial checkpoints support source and runtime verification. Framework-repository
commands belong only in the contribution branch.

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
new-dependency: true
new-capability: false
new-extension-point: false
```

## Contract delta

No Haskell or Rust public APIs change. The Neo CLI's generated project preset
adds `DerivingStrategies`, so framework declaration helpers work without per-file
language pragmas. A regression checks the real rendered template and inheritance
by application, library, and test components. The published 0.10.0 binary lacks
this default; pragma-free tutorial verification uses the corrected local CLI,
and the installation page identifies the next-release requirement.

```diff signatures
```

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | The monorepo delegates public website ownership to `neohaskell/website-v2` and retains only a migration pointer | `script:scripts/website-migration-check#--check` | unit | none |
| C2 | The migration boundary rejects obsolete in-repository website payload, workflows, and ADR integration | `script:scripts/website-migration-check#--self-test` | unit | none |

## User impact

Jess can learn NeoHaskell through a cohesive ecommerce practice project, apply the
concepts to her own domain, evaluate business tradeoffs without reading every
implementation section, and consult individual topics. Existing
getting-started and deployment routes remain usable. The source examples distinguish
implemented public capabilities from shop-specific designs. The authoring plan and review record remain in the standalone
[`neohaskell/website-v2`](https://github.com/neohaskell/website-v2) repository;
they explicitly separate automated verification from a future human reader trial
and live external-provider testing.

## ADR

[ADR-0078](../decisions/0078-documentation-image-zoom.md) records the Starlight
image-zoom dependency. Diagrams and screenshots enlarge in a dialog on the
current page, with keyboard controls and captions. Built-page verification
requires an accessible zoom trigger for every registered illustration.
