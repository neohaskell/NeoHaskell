# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records for the NeoHaskell project.

## What are ADRs?

ADRs document significant architectural decisions made during the development of NeoHaskell. Each ADR captures:

- **Context**: Why we needed to make this decision
- **Decision**: What we decided and the reasoning
- **Consequences**: What changes as a result (both positive and negative)

## ADR Index

| ADR                                           | Title                           | Status   |
| --------------------------------------------- | ------------------------------- | -------- |
| [0001](0001-initial-architecture-baseline.md) | Initial Architecture Baseline   | Accepted |
| [0002](0002-webapi-adapter-architecture.md)   | WebAPI Adapter Architecture     | Accepted |
| [0003](0003-command-abstraction-and-flow.md)  | Command Abstraction and Flow    | Accepted |
| [0004](0004-eventstore-abstraction.md)        | EventStore Abstraction          | Accepted |
| [0005](0005-service-module-reorganization.md) | Service Module Reorganization   | Accepted |
| [0006](0006-entity-snapshot-cache.md)         | Entity Snapshot Cache           | Accepted |
| [0007](0007-queries-read-models.md)           | Queries (Read Models)           | Proposed |
| [0008](0008-integration-pattern.md)           | Integration Pattern             | Proposed |
| [0009](0009-jwt-authentication-middleware.md) | JWT Authentication Middleware   | Accepted |
| [0010](0010-oauth2-provider-architecture.md)  | OAuth2 Provider Integration     | Accepted |
| [0011](0011-file-upload-architecture.md)      | File Upload Architecture        | Proposed |
| [0012](0012-postgres-file-state-store.md)     | PostgreSQL FileStateStore       | Proposed |
| [0013](0013-automatic-schema-generation.md)   | Automatic Schema Generation     | Proposed |
| [0014](0014-webtransport-openapi-integration.md) | WebTransport OpenAPI Integration | Proposed |
| [0015](0015-http-outbound-integration.md) | HTTP Outbound Integration | Proposed |
| [0016](0016-redacted-type-for-sensitive-data.md) | Redacted Type for Sensitive Data | Proposed |
| [0017](0017-toschema-auto-derivation-in-command-th.md) | ToSchema Auto-Derivation in Command TH | Proposed |
| [0018](0018-http-localhost-exception-for-oauth2-discovery.md) | HTTP Localhost Exception for OAuth2 Discovery | Proposed |
| [0019](0019-webtransport-request-body-size-coordination.md) | WebTransport Request Body Size Coordination | Proposed |
| [0020](0020-openapi-grouping-and-complete-endpoints.md) | OpenAPI Grouping and Complete Endpoint Coverage | Proposed |
| [0021](0021-declarative-config-dsl.md) | Declarative Config DSL | Proposed |
| [0022](0022-decimal-type.md) | Decimal Type for Financial Calculations | Proposed |

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
