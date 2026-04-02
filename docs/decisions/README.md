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
| [0007](0007-queries-read-models.md)           | Queries (Read Models)           | Accepted |
| [0008](0008-integration-pattern.md)           | Integration Pattern             | Accepted |
| [0009](0009-jwt-authentication-middleware.md) | JWT Authentication Middleware   | Accepted |
| [0010](0010-oauth2-provider-architecture.md)  | OAuth2 Provider Integration     | Accepted |
| [0011](0011-file-upload-architecture.md)      | File Upload Architecture        | Accepted |
| [0012](0012-postgres-file-state-store.md)     | PostgreSQL FileStateStore       | Accepted |
| [0013](0013-automatic-schema-generation.md)   | Automatic Schema Generation     | Accepted |
| [0014](0014-webtransport-openapi-integration.md) | WebTransport OpenAPI Integration | Accepted |
| [0015](0015-http-outbound-integration.md) | HTTP Outbound Integration | Accepted |
| [0016](0016-redacted-type-for-sensitive-data.md) | Redacted Type for Sensitive Data | Accepted |
| [0017](0017-toschema-auto-derivation-in-command-th.md) | ToSchema Auto-Derivation in Command TH | Accepted |
| [0018](0018-http-localhost-exception-for-oauth2-discovery.md) | HTTP Localhost Exception for OAuth2 Discovery | Accepted |
| [0019](0019-webtransport-request-body-size-coordination.md) | WebTransport Request Body Size Coordination | Accepted |
| [0020](0020-openapi-grouping-and-complete-endpoints.md) | OpenAPI Grouping and Complete Endpoint Coverage | Accepted |
| [0021](0021-declarative-config-dsl.md) | Declarative Config DSL | Accepted |
| [0022](0022-decimal-type.md) | Decimal Type for Financial Calculations | Accepted |
| [0023](0023-ai-pdf-transcription.md) | AI-Powered OCR via Multimodal Models | Accepted |
| [0024](0024-cors-support.md) | CORS Support for WebTransport | Accepted |
| [0025](0025-auto-health-endpoint.md) | Auto Health Endpoint for WebTransport Apps | Accepted |
| [0026](0026-line-buffering-for-containerized-deployments.md) | Line Buffering for Containerized Deployments | Accepted |
| [0027](0027-postgres-pool-health.md) | PostgreSQL Pool Health for Serverless Databases | Accepted |
| [0028](0028-structured-logging.md) | Structured Logging with fast-logger | Accepted |
| [0029](0029-worker-crash-recovery.md) | Worker Crash Recovery for Outbound Integrations | Accepted |
| [0030](0030-configurable-dispatcher-config.md) | Configurable Dispatcher Config | Accepted |
| [0031](0031-token-query-param-oauth2.md) | Token Query Parameter for OAuth2 Browser Redirects | Accepted |
| [0032](0032-simple-event-store.md) | SimpleEventStore with Optional JSONL Persistence | Accepted |
| [0033](0033-config-dependent-oauth2-provider.md) | Config-Dependent OAuth2 Provider Factory | Accepted |
| [0034](0034-cli-transport.md) | CliTransport — Command-Line Interface Transport | Accepted |
| [0035](0035-config-dependent-application-builders.md) | Config-Dependent Application Builder Functions | Accepted |
| [0036](0036-wave1-security-hardening.md) | Wave 1 Security Hardening | Accepted |
| [0037](0037-postgres-listen-keepalive-reconnect.md) | PostgreSQL LISTEN Connection Keepalive and Reconnection | Accepted |
| [0038](0038-fix-flaky-subscription-test.md) | Fix Flaky Subscription Test Ordering Assertion | Accepted |
| [0039](0039-fix-listen-notify-connection-leak.md) | Fix LISTEN/NOTIFY Connection Leak in Test Teardown | Accepted |
| [0040](0040-neoql-mvp.md) | NeoQL MVP — Field Access and Equality Filtering | Accepted |
| [0041](0041-audio-transcription-integration.md) | Audio Transcription Integration via Multimodal Models | Accepted |
| [0042](0042-parser-library.md) | Parser Library — Beginner-Friendly megaparsec Wrapper | Accepted |
| [0043](0043-neohaskell-comment-syntax-parsing.md) | NeoHaskell Comment Syntax Parsing | Accepted |
| [0044](0044-layout-library.md) | Layout Library — Beginner-Friendly prettyprinter Wrapper | Accepted |
| [0045](0045-integration-agent.md) | Integration.Agent — Provider-Agnostic AI Agent via Tool Calling | Accepted |
| [0046](0046-function-syntax-fun-keyword.md) | Function Syntax — `fun` Keyword Parsing and Transpilation | Accepted |
| [0047](0047-eventvariantof-typeclass.md) | EventVariantOf Typeclass for Type-Safe Event Variant Wrapping | Accepted |
| [0048](0048-file-upload-content-deduplication.md) | File Upload Content Deduplication via SHA-256 | Accepted |
| [0049](0049-outboundintegration-typeclass-dispatch-generation.md) | OutboundIntegration Typeclass with Typed Event Dispatch | Accepted |
| [0050](0050-internal-command-transport.md) | Internal Command Transport | Accepted |
| [0051](0051-query-pagination-result-limits.md) | Query Pagination and Result Limits | Accepted |
| [0052](0052-mcp-stdio-transport.md) | McpTransport — MCP STDIO Transport for AI Assistant Integration | Accepted |

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
