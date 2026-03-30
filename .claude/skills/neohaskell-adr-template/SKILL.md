---
name: neohaskell-adr-template
description: ADR (Architecture Decision Record) template for NeoHaskell. Use when creating a new ADR, drafting a decision document, or starting a new feature proposal. Triggers on 'new ADR', 'create ADR', 'architecture decision', 'decision record', 'feature proposal', 'ADR draft'.
---

# NeoHaskell ADR Template

Use this skill when creating Architecture Decision Records for the NeoHaskell project. ADRs document significant architectural decisions and live in `docs/decisions/`.

## When to Write an ADR

**Write an ADR when:**
1. Renaming or restructuring modules
2. Changing the public API of `Core`
3. Adding new EventStore implementations
4. Modifying the command execution flow
5. Changing code style conventions
6. Adding significant new infrastructure (new types in nhcore, new service patterns)

**Do NOT write an ADR for:**
- Bug fixes
- Performance improvements without API changes
- Documentation updates
- Adding new commands or entities (domain logic)

## Numbering Convention

ADRs are numbered sequentially with 4-digit zero-padding: `NNNN-short-description.md`

**To determine the next number:**

```bash
ls docs/decisions/*.md | tail -1
```

**File naming**: Use lowercase kebab-case for the description slug.

Examples:
- `0041-decimal-arithmetic-operations.md`
- `0042-websocket-transport-adapter.md`

## ADR Template

```markdown
# ADR-NNNN: [Descriptive Title]

## Status

Proposed

## Context

[Why we needed to make this decision. Include:]

### Current State

[What exists today. What problem or gap this addresses.]

### Use Cases

- [Concrete use case 1]
- [Concrete use case 2]
- [Concrete use case 3]

### Design Goals

1. [Goal 1 with rationale]
2. [Goal 2 with rationale]
3. [Goal 3 with rationale]

### GitHub Issue

- [#NNN: Issue Title](https://github.com/neohaskell/NeoHaskell/issues/NNN)

## Decision

### 1. [First Decision Point]

[Explanation of what was decided and why]

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Option A  | Rejected | [Why] |
| Option B  | **Chosen** | [Why] |

### 2. [Second Decision Point]

[More decisions...]

### N. Type Definitions

```haskell
-- Core type definition
data MyType = MyType
  { field1 :: !Text
  , field2 :: !Int
  }
  deriving (Eq, Show, Generic)
```

### N+1. Public API

```haskell
-- Key functions
new :: Config -> Task CreateError MyType
transform :: MyType -> TransformResult
```

## Consequences

### Positive

- [Benefit 1]
- [Benefit 2]

### Negative

- [Drawback 1]
- [Drawback 2]

### Risks

- [Risk 1]
- [Risk 2]

### Mitigations

- [Mitigation for risk 1]
- [Mitigation for risk 2]

## References

- [#NNN: Related Issue](https://github.com/neohaskell/NeoHaskell/issues/NNN)
- [ADR-XXXX: Related Decision](XXXX-related-decision.md)
- [relevant/source/file.hs](../../relevant/source/file.hs)
```

## Field-by-Field Guidance

### Title (`# ADR-NNNN: [Title]`)

- Be specific and action-oriented
- Include the concept name, not just the category

| Good | Bad |
|------|-----|
| `Decimal Type for Financial Calculations` | `New Type` |
| `OAuth2 Provider Integration Architecture` | `OAuth2 Support` |
| `WebTransport Request Body Size Coordination` | `Request Body Changes` |
| `Redacted Type for Sensitive Data` | `Security Improvement` |

### Status

Always start as `Proposed`. Only the maintainer changes status.

| Status | Meaning |
|--------|---------|
| `Proposed` | Under discussion, not yet approved |
| `Accepted` | Approved and implemented |
| `Deprecated` | No longer relevant |
| `Superseded by ADR-XXXX` | Replaced by a newer decision |

### Context Section

This is the "why" — explain the problem, not the solution.

**Must include:**
- Current state (what exists today)
- The gap or problem being addressed
- At least 2-3 concrete use cases
- Design goals (numbered, with rationale)
- GitHub issue link (if one exists)

**Good context writing:**
> "NeoHaskell lacks a decimal or fixed-point number type for precise financial calculations. This gap forces downstream applications to use workarounds that are error-prone and verbose."

**Bad context writing:**
> "We need a decimal type."

### Decision Section

This is the "what" — document each decision point separately.

**Structure each sub-decision as:**
1. A clear heading (`### N. Decision Point Name`)
2. What was decided
3. Why — with alternatives considered in a comparison table
4. Code examples showing the chosen approach

**Always include:**
- Type definitions with NeoHaskell conventions (strict fields, descriptive type params)
- Module placement (`core/[directory]/Module.hs`)
- Public API signatures
- Comparison tables for rejected alternatives

### Consequences Section

Split into **Positive**, **Negative**, **Risks**, and **Mitigations** subsections.

**Good consequence writing:**
> "**Positive**: Jess can use `Decimal.fromFloat 12.50` without understanding fixed-point arithmetic internals."

**Bad consequence writing:**
> "**Positive**: Better."

### References Section

Link to:
- GitHub issues (`[#NNN: Title](https://github.com/neohaskell/NeoHaskell/issues/NNN)`)
- Related ADRs (`[ADR-XXXX: Title](XXXX-slug.md)`)
- Source files (`[path/to/file.hs](../../path/to/file.hs)`)
- External RFCs or standards (if relevant)

## Updating the README Index

After creating an ADR, add it to the index table in `docs/decisions/README.md`:

```markdown
| [NNNN](NNNN-slug.md) | Title | Proposed |
```

Insert it in numerical order at the end of the table.

## NeoHaskell-Specific ADR Considerations

When writing ADRs for NeoHaskell features:

1. **Jess persona**: Consider how the decision affects Jess (junior dev, 15-30 min/day). Every API choice must pass the "Jess at 10 PM" test.

2. **Three principles**: Evaluate against Least Astonishment, Developer Happiness, and Least Effort.

3. **Module placement**: Specify which `hs-source-dir` the new module belongs in (core, service, json, schema, etc.).

4. **Re-export from Core**: If the type is foundational, note that it must be re-exported from `Core.hs`.

5. **Type families**: If adding entity/event types, specify the type family instances (`EntityOf`, `EventOf`, `NameOf`).

6. **Serialization**: Specify JSON representation. Prefer `toEncoding` over `toJSON` for performance.

7. **Code style**: All code examples in the ADR MUST follow NeoHaskell style (pipes, do-blocks, case, qualified imports, descriptive type params). Load the `neohaskell-style-guide` skill if unsure.
