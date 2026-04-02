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
| [0023](0023-ai-pdf-transcription.md) | AI-Powered OCR via Multimodal Models | Proposed |
| [0024](0024-cors-support.md) | CORS Support for WebTransport | Proposed |
| [0025](0025-auto-health-endpoint.md) | Auto Health Endpoint for WebTransport Apps | Proposed |
| [0026](0026-line-buffering-for-containerized-deployments.md) | Line Buffering for Containerized Deployments | Proposed |
| [0027](0027-postgres-pool-health.md) | PostgreSQL Pool Health for Serverless Databases | Proposed |
| [0028](0028-structured-logging.md) | Structured Logging with fast-logger | Proposed |
| [0029](0029-worker-crash-recovery.md) | Worker Crash Recovery for Outbound Integrations | Proposed |
| [0030](0030-configurable-dispatcher-config.md) | Configurable Dispatcher Config | Proposed |
| [0031](0031-token-query-param-oauth2.md) | Token Query Parameter for OAuth2 Browser Redirects | Proposed |
| [0032](0032-simple-event-store.md) | SimpleEventStore with Optional JSONL Persistence | Proposed |
| [0033](0033-config-dependent-oauth2-provider.md) | Config-Dependent OAuth2 Provider Factory | Proposed |
| [0034](0034-cli-transport.md) | CliTransport — Command-Line Interface Transport | Proposed |
| [0035](0035-config-dependent-application-builders.md) | Config-Dependent Application Builder Functions | Proposed |
| [0036](0036-wave1-security-hardening.md) | Wave 1 Security Hardening | Accepted |
| [0037](0037-postgres-listen-keepalive-reconnect.md) | PostgreSQL LISTEN Connection Keepalive and Reconnection | Proposed |
| [0038](0038-fix-flaky-subscription-test.md) | Fix Flaky Subscription Test Ordering Assertion | Accepted |
| [0039](0039-fix-listen-notify-connection-leak.md) | Fix LISTEN/NOTIFY Connection Leak in Test Teardown | Accepted |
| [0040](0040-neoql-mvp.md) | NeoQL MVP — Field Access and Equality Filtering | Proposed |
| [0041](0041-audio-transcription-integration.md) | Audio Transcription Integration via Multimodal Models | Proposed |
| [0042](0042-parser-library.md) | Parser Library — Beginner-Friendly megaparsec Wrapper | Proposed |
| [0043](0043-neohaskell-comment-syntax-parsing.md) | NeoHaskell Comment Syntax Parsing | Proposed |
| [0044](0044-layout-library.md) | Layout Library — Beginner-Friendly prettyprinter Wrapper | Proposed |
| [0045](0045-integration-agent.md) | Integration.Agent — Provider-Agnostic AI Agent via Tool Calling | Proposed |
| [0046](0046-function-syntax-fun-keyword.md) | Function Syntax — `fun` Keyword Parsing and Transpilation | Proposed |
| [0047](0047-eventvariantof-typeclass.md) | EventVariantOf Typeclass for Type-Safe Event Variant Wrapping | Proposed |
| [0048](0048-file-upload-content-deduplication.md) | File Upload Content Deduplication via SHA-256 | Proposed |
| [0050](0050-internal-command-transport.md) | Internal Command Transport | Proposed |
| [0051](0051-query-pagination-result-limits.md) | Query Pagination and Result Limits | Proposed |
| [0052](0052-mcp-stdio-transport.md) | McpTransport — MCP STDIO Transport for AI Assistant Integration | Proposed |

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
