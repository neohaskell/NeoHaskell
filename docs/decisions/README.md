# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records for the NeoHaskell project.

## What are ADRs?

ADRs document significant architectural decisions made during the development of NeoHaskell. Each ADR captures:

- **Context**: Why we needed to make this decision
- **Decision**: What we decided and the reasoning
- **Consequences**: What changes as a result (both positive and negative)

## ADR Index

| ADR                                           | Title                         | Status   |
| --------------------------------------------- | ----------------------------- | -------- |
| [0001](0001-initial-architecture-baseline.md) | Initial Architecture Baseline | Accepted |

## Creating New ADRs

When making a significant architectural decision, create a new ADR following this format:

```markdown
# ADR-NNNN: [Title]

## Status

[Proposed | Accepted | Deprecated | Superseded by ADR-XXXX]

## Context

[Why we needed to make this decision]

## Decision

[What we decided and why]

## Consequences

[What changes as a result]
```

### Naming Convention

ADRs are numbered sequentially: `NNNN-short-description.md`

Examples:

- `0002-flatten-postgres-modules.md`
- `0003-rename-decision-to-service-decision.md`

### When to Write an ADR

Write an ADR when:

1. Renaming or restructuring modules
2. Changing the public API of `Core`
3. Adding new EventStore implementations
4. Modifying the command execution flow
5. Changing code style conventions
6. Adding significant new infrastructure

You do not need an ADR for:

- Bug fixes
- Performance improvements without API changes
- Documentation updates
- Adding new commands or entities (domain logic)

## Status Lifecycle

1. **Proposed**: ADR is under discussion
2. **Accepted**: Decision has been made and implemented
3. **Deprecated**: Decision is no longer relevant
4. **Superseded**: Replaced by a newer ADR (link to replacement)
