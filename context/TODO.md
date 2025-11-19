# NeoHaskell Event Sourcing Framework - Implementation TODO

This document outlines the roadmap for completing the NeoHaskell Event Sourcing Framework implementation. The framework follows established patterns from Martin Fowler, Greg Young, and Adam Dymitruk.

## Current Status

### ✅ Completed Foundation (Excellent Base)

**Event Store Core** - Production-ready implementation

- Append-only storage with optimistic concurrency control
- Global and stream-level position tracking
- Forward/backward reading (streams and global)
- Entity-filtered reading
- Multiple subscription types (all, from position, entity-specific, stream-specific)
- Stream truncation (GDPR compliance)
- InMemory and Postgres implementations
- Comprehensive error handling
- Robust test coverage with property-based testing

**Files:**

- `core/service/Service/EventStore.hs` - Core interface
- `core/service/Service/EventStore/InMemory.hs` - Testing implementation
- `core/service/Service/EventStore/Postgres.hs` - Production implementation
- `core/service/Service/Event.hs` - Event types and metadata

---

## Phase 1: Core CQRS Components (High Priority)

These components enable developers to actually build applications using the event store.

### 1.1 Entity Pattern Implementation ✅ COMPLETED

**Priority:** CRITICAL
**Estimated Effort:** Medium
**Dependencies:** None (builds on existing Event Store)

**Status:** Completed in PR #226 - "Add Entity Fetcher Service for Event Sourcing"

**Completed Tasks:**

- [x] Create `Service.EntityFetcher` module (implements entity fetching pattern)
- [x] Implement entity state reconstruction via event replay using reduction function
- [x] Add version tracking to entity state (via custom state types)
- [x] Create helper functions for entity loading from event store (`fetch` method)
- [x] Document entity design patterns for NeoHaskell style (via examples and tests)

**Note:** The implementation uses `EntityFetcher` with a reduction function pattern instead of a typeclass-based `Entity` pattern. This provides:

- Clean separation: `applyEvent` is provided as a reduction function
- Command handling is intentionally separated (will be in CommandHandler module)
- Pure entity state reconstruction via event replay
- Efficient streaming-based event consumption

**Deliverables Completed:**

- ✅ [core/service/Service/EntityFetcher.hs](core/service/Service/EntityFetcher.hs)
- ✅ [core/service/Service/EntityFetcher/Core.hs](core/service/Service/EntityFetcher/Core.hs)
- ✅ Example entity implementation: `BankAccount` with state and events in test helpers
- ✅ Comprehensive unit tests covering:
  - Basic entity fetching (empty, single event, multiple events)
  - Independent entity streams
  - Large event counts (100+ events with performance boundaries)
  - Concurrent fetch operations
  - Error scenarios and edge cases
  - Performance testing with 10, 100, and 1000 events
- ✅ Tests follow Given-When-Then pattern
- ✅ Both InMemory and Postgres event store implementations tested

**Design Decisions:**

- ✅ Follows NeoHaskell coding style (explicit imports, no point-free, `do` blocks)
- ✅ Uses reduction function pattern: `(event -> state -> state)`
- ✅ Entities are pure (no IO in business logic - reduction function is pure)
- ✅ State type is explicit (not hidden)
- ✅ Streaming-based event consumption for memory efficiency
- ✅ Version tracking is responsibility of entity state (not enforced by framework)

---

### 1.2 Command Infrastructure

**Priority:** CRITICAL
**Estimated Effort:** Medium
**Dependencies:** 1.1 Entity Pattern (EntityFetcher)

**Tasks:**

**Core Command Types:**

- [ ] Create `Service.Command` module with base types
- [ ] Define `Command` typeclass with methods:
  - `validate :: command -> Array Text` - Returns validation errors
  - `decide :: command -> Option entity -> Option UserSecurity -> Result (Array Event) ValidationError` - Business logic decision
  - `getEntityId :: command -> Option UserSecurity -> Option StrongId` - Extract target entity ID
- [ ] Define `TenantCommand` typeclass (extends Command) with:
  - `decide :: command -> Option entity -> Option UserSecurity -> Uuid -> Result (Array Event) ValidationError` - Includes tenant ID
  - `getEntityId :: command -> Option UserSecurity -> Uuid -> Option StrongId` - Tenant-scoped entity ID
- [ ] Create command result types:
  - `CommandAccepted` with entity ID
  - `ValidationError` with error messages
  - `ConflictError` for concurrency conflicts
  - `ForbiddenError` for authorization failures
  - `NotFoundError` for missing entities

**Command Metadata & Context:**

- [ ] Implement `CommandContext` type with:
  - Correlation ID (UUID for tracing)
  - Causation ID (what triggered this command)
  - User/actor security context (`Option UserSecurity`)
  - Tenant ID (`Option Uuid` for multi-tenancy)
  - Timestamp
- [ ] Create `UserSecurity` type for authentication:
  - User ID/Subject
  - Roles/Claims
  - Tenant memberships
- [ ] Add command metadata helpers for context propagation

**Validation Infrastructure:**

- [ ] Create validation helper functions:
  - Nullability/required field validation
  - Format validation utilities
  - Business rule validation helpers
- [ ] Implement validation result aggregation
- [ ] Add custom validation support (JsonLogic or similar rules engine)
- [ ] Create validation error message builders

**Authorization Infrastructure:**

- [ ] Define authorization types:
  - `AuthOptions`: Everyone, Authenticated, RequireRoles, Custom
  - Custom authorization function type: `(Option UserSecurity -> Option entity -> command -> Result Unit AuthError)`
- [ ] Implement authorization checking before command execution
- [ ] Add tenant-scoped authorization (ensure user belongs to tenant)
- [ ] Create authorization error types

**Idempotency Support:**

- [ ] Create `IdempotencyKey` type (newtype wrapper for Text/UUID)
- [ ] Define idempotency cache entity/state:
  - Key (IdempotencyKey)
  - State (Pending/Accepted/Rejected)
  - Stored result (success or error)
  - Lock expiration timestamp
- [ ] Implement idempotency events:
  - `IdempotentCommandStarted` - Lock acquired
  - `IdempotentCommandAccepted` - Stored success result
  - `IdempotentCommandRejected` - Stored error result
- [ ] Add idempotency key validation (reject invalid keys like "undefined", "null", etc.)

**File Attachment Support:**

- [ ] Create `AttachedFile` type with ID and optional tags
- [ ] Implement file validation:
  - Check file exists in system
  - Verify file not deleted
  - Confirm file ownership/permissions
- [ ] Add file confirmation events:
  - `FileConfirmed` - Mark file as used by command
  - `FileTagged` - Add metadata tags to file
- [ ] Create helpers to extract files from command properties

**Testing Infrastructure:**

- [ ] Create command testing helpers:
  - Mock entity state builders
  - Mock user security contexts
  - Command execution test harness
- [ ] Add validation testing utilities
- [ ] Create authorization testing helpers
- [ ] Implement idempotency testing scenarios

**Deliverables:**

- `core/service/Service/Command.hs` - Base command types and typeclass
- `core/service/Service/Command/TenantCommand.hs` - Tenant-scoped commands
- `core/service/Service/Command/Context.hs` - Command context and metadata
- `core/service/Service/Command/Validation.hs` - Validation utilities
- `core/service/Service/Command/Authorization.hs` - Auth types and helpers
- `core/service/Service/Command/Idempotency.hs` - Idempotency support
- `core/service/Service/Command/FileAttachment.hs` - File handling
- Example command implementations:
  - Regular command (e.g., `OpenBankAccount`, `DepositMoney`)
  - Tenant command (e.g., `CreateOrder`, `AssignTask`)
- Comprehensive unit tests
- Documentation with examples

**Design Considerations:**

- Commands are data structures with behavior via typeclass instances
- Separate `Command` and `TenantCommand` for single-tenant vs multi-tenant scenarios
- Commands express intent and contain validation logic
- Decision logic (`decide`) is pure - takes state, returns events or errors
- Support both simple validation (nullability, format) and complex rules (JsonLogic)
- Idempotency prevents duplicate command execution
- File attachments are first-class citizens with lifecycle management
- Authorization happens before command execution
- Tenant isolation enforced at framework level
- Must follow NeoHaskell style (explicit imports, no point-free, `Result` not `Either`)
- Use `StrongId` types for entity IDs (type-safe identifiers)
- Validation errors are `Array Text` for multiple error messages
- Commands should not directly access event store - that's CommandHandler's job

---

### 1.3 Command Handlers

**Priority:** CRITICAL
**Estimated Effort:** Medium-Large
**Dependencies:** 1.1 Entities, 1.2 Commands

**Tasks:**

- [ ] Create `Service.CommandHandler` module
- [ ] Implement command handler pattern:
  - Load entity from event store
  - Reconstruct current state via event replay
  - Pass command to entity
  - Handle validation errors
  - Persist emitted events atomically
  - Handle optimistic concurrency conflicts with retry
- [ ] Create command handler registration/routing
- [ ] Implement retry logic with exponential backoff
- [ ] Add command execution result types (Success/Failure)
- [ ] Create helper functions for common patterns

**Deliverables:**

- `core/service/Service/CommandHandler.hs`
- Retry policies and strategies
- Integration tests (command → events → persistence)
- Performance benchmarks

**Design Considerations:**

- Transaction boundary = single entity
- Handle `ConsistencyCheckFailed` errors with retries
- Return new entity version on success
- Distinguish validation errors from system errors

---

### 1.4 Projection Engine

**Priority:** CRITICAL
**Estimated Effort:** Large
**Dependencies:** None (works with existing Event Store)

**Tasks:**

- [ ] Create `Service.Projection` module
- [ ] Define `Projection` typeclass with methods:
  - `projectEvent` - Handle single event
  - `getPosition` - Get current checkpoint position
  - `setPosition` - Update checkpoint
- [ ] Implement projection runner that:
  - Subscribes to event store
  - Maintains checkpoint/position
  - Processes events in order
  - Handles idempotency (deduplication)
  - Supports rebuild from scratch
- [ ] Create projection manager for multiple concurrent projections
- [ ] Implement checkpoint storage (in read model DB)
- [ ] Add projection lag monitoring
- [ ] Support filtered projections (specific event types)

**Deliverables:**

- `core/service/Service/Projection.hs`
- Projection runner with subscription management
- Checkpoint persistence
- Example projections
- Rebuild tools
- Monitoring metrics

**Design Considerations:**

- At-least-once delivery (projections must be idempotent)
- Support catch-up mode (historical events) then live mode
- Graceful shutdown with checkpoint save
- Error handling with retry and dead letter queue

---

### 1.5 Read Model Infrastructure

**Priority:** HIGH
**Estimated Effort:** Medium
**Dependencies:** 1.4 Projection Engine

**Tasks:**

- [ ] Create `Service.ReadModel` module
- [ ] Define read model storage abstraction
- [ ] Implement common read model patterns:
  - Single entity views
  - List/collection views
  - Aggregated statistics
  - Search indexes
- [ ] Create helpers for read model updates in projections
- [ ] Support multiple storage backends (Postgres, in-memory)
- [ ] Add query helper functions
- [ ] Document read model design patterns

**Deliverables:**

- `core/service/Service/ReadModel.hs`
- Storage abstraction
- Example read models
- Query utilities
- Documentation on eventual consistency

**Design Considerations:**

- Separate database from event store (typically)
- Denormalized for query performance
- Accept eventual consistency
- Optional inline projections for strong consistency

---

### 1.6 Query Handlers (CQRS Queries)

**Priority:** HIGH
**Estimated Effort:** Small
**Dependencies:** 1.5 Read Model

**Tasks:**

- [ ] Create `Service.Query` module
- [ ] Define query types and handlers
- [ ] Implement direct read model access
- [ ] Add common query patterns:
  - Get by ID
  - List with pagination
  - Filter and sort
  - Search
- [ ] Create query result types
- [ ] Add caching support (optional)
- [ ] Document query design patterns

**Deliverables:**

- `core/service/Service/Query.hs`
- Query handler utilities
- Pagination helpers
- Example queries
- Documentation

**Design Considerations:**

- Queries are pure data retrieval (no business logic)
- No shared types between commands and queries
- Cache when appropriate
- Handle stale reads gracefully

---

## Phase 2: Event Evolution & Performance (Medium Priority)

These features are needed for production systems that evolve over time.

### 2.1 Event Versioning & Upcasting

**Priority:** MEDIUM-HIGH
**Estimated Effort:** Medium
**Dependencies:** Phase 1 complete

**Tasks:**

- [ ] Create `Service.EventVersion` module
- [ ] Add version metadata to events
- [ ] Implement upcasting infrastructure:
  - Upcaster registry
  - Transformation pipeline
  - Version-specific deserializers
- [ ] Create upcaster middleware (between store and business logic)
- [ ] Support multiple event versions simultaneously
- [ ] Add schema evolution patterns:
  - Additive changes (new optional fields)
  - Event splitting/merging
  - Field deprecation
- [ ] Create migration tools for copy-and-replace
- [ ] Document versioning best practices

**Deliverables:**

- `core/service/Service/EventVersion.hs`
- Upcaster framework
- Migration utilities
- Version compatibility tests
- Documentation on schema evolution

**Design Considerations:**

- Never modify existing events
- Transform on read (lazy upcasting)
- Keep only latest version in business logic
- Test transformations don't lose data

---

### 2.2 Snapshot Support

**Priority:** MEDIUM
**Estimated Effort:** Medium
**Dependencies:** 1.1 Entities

**Tasks:**

- [ ] Create `Service.Snapshot` module
- [ ] Define snapshot storage interface
- [ ] Implement snapshot strategies:
  - Lazy creation (after N events, e.g., 1000)
  - Periodic snapshots (every N events, e.g., 100)
  - On-demand snapshots
- [ ] Add snapshot metadata (version, timestamp)
- [ ] Implement snapshot loading with event replay delta
- [ ] Create snapshot storage (separate from events)
- [ ] Add snapshot invalidation on schema changes
- [ ] Implement snapshot eviction policies
- [ ] Add benchmarks comparing with/without snapshots

**Deliverables:**

- `core/service/Service/Snapshot.hs`
- Snapshot storage implementations
- Configuration for snapshot policies
- Performance benchmarks
- Documentation

**Design Considerations:**

- Snapshots are optimization only (not required for correctness)
- Can always rebuild from events
- Snapshot versioning like event versioning
- Only snapshot high-volume entities

---

### 2.3 Enhanced Event Metadata

**Priority:** MEDIUM
**Estimated Effort:** Small
**Dependencies:** None (enhancement to existing Event type)

**Tasks:**

- [ ] Extend `Event` type with additional metadata:
  - Correlation ID (track related events)
  - Causation ID (what caused this event)
  - User/actor who triggered event
  - Tenant ID (for multi-tenancy)
  - Source application/service
- [ ] Add metadata to event creation helpers
- [ ] Update serialization to include new fields
- [ ] Create metadata extraction utilities
- [ ] Document metadata usage patterns

**Deliverables:**

- Enhanced `Service.Event` module
- Migration for existing events (nullable fields)
- Metadata utilities
- Documentation

**Design Considerations:**

- Make new fields optional for backward compatibility
- Use UUIDs for correlation/causation IDs
- Capture metadata at command handling time

---

## Phase 3: Advanced Patterns (Medium Priority)

These enable complex business workflows and cross-aggregate coordination.

### 3.1 Saga / Process Manager

**Priority:** MEDIUM
**Estimated Effort:** Large
**Dependencies:** Phase 1 complete

**Tasks:**

- [ ] Create `Service.Saga` module
- [ ] Define saga state management (event-sourced)
- [ ] Implement saga event handling:
  - Listen to events from multiple entities
  - Dispatch commands to entities
  - Track saga state transitions
- [ ] Support both patterns:
  - Choreography (decentralized, event-driven)
  - Orchestration (centralized coordinator)
- [ ] Add timeout handling for incomplete sagas
- [ ] Implement compensation logic for failures
- [ ] Create saga state persistence
- [ ] Add saga monitoring and debugging tools

**Deliverables:**

- `core/service/Service/Saga.hs`
- Example saga implementations
- State machine utilities
- Timeout handling
- Documentation on saga patterns

**Design Considerations:**

- Sagas coordinate multiple entities
- Use events for choreography
- Use commands for orchestration
- Handle partial failures gracefully
- Idempotent saga handlers

---

### 3.2 Integration Events & Outbox Pattern

**Priority:** MEDIUM
**Estimated Effort:** Medium
**Dependencies:** Phase 1 complete

**Tasks:**

- [ ] Create `Service.IntegrationEvent` module
- [ ] Separate domain events from integration events
- [ ] Implement Outbox pattern:
  - Outbox table in same database as entities
  - Store integration events in same transaction as domain events
  - Background worker polls outbox and publishes
  - Mark events as published
- [ ] Create event transformation (domain → integration)
- [ ] Add message broker integration interface:
  - Kafka
  - RabbitMQ
  - Azure Service Bus
- [ ] Implement at-least-once delivery guarantees
- [ ] Add dead letter queue for failed publishing

**Deliverables:**

- `core/service/Service/IntegrationEvent.hs`
- `core/service/Service/Outbox.hs`
- Outbox worker process
- Message broker adapters
- Documentation on bounded context integration

**Design Considerations:**

- Transactional outbox ensures reliability
- Integration events cross bounded contexts
- Transform to hide internal details
- Subscribers must be idempotent

---

### 3.3 Temporal Queries

**Priority:** MEDIUM
**Estimated Effort:** Medium
**Dependencies:** 1.1 Entities, 1.4 Projections

**Tasks:**

- [ ] Create `Service.TemporalQuery` module
- [ ] Implement point-in-time state reconstruction:
  - Replay events up to specific version
  - Replay events up to specific timestamp
- [ ] Add historical projection building
- [ ] Implement bi-temporal tracking:
  - As-at: When fact occurred
  - As-of: When we learned about fact
- [ ] Create audit trail query helpers
- [ ] Add "time-travel" projection builder
- [ ] Document use cases (audit, compliance, analysis)

**Deliverables:**

- `core/service/Service/TemporalQuery.hs`
- Point-in-time query utilities
- Historical projection tools
- Example audit queries
- Documentation

**Design Considerations:**

- Events are immutable timeline
- Can reconstruct any historical state
- Useful for compliance and debugging
- Performance: may need caching for common queries

---

## Phase 4: Production Infrastructure (Lower Priority)

These features are needed for operating production systems at scale.

### 4.1 Monitoring & Observability

**Priority:** MEDIUM
**Estimated Effort:** Medium
**Dependencies:** Phase 1 complete

**Tasks:**

- [ ] Create `Service.Metrics` module
- [ ] Implement key metrics:
  - Event append rate (events/second)
  - Event store size (GB, event count)
  - Projection lag (seconds behind live)
  - Command success/failure rates
  - Query response times
  - Subscription backpressure
  - Entity cache hit rate
- [ ] Add structured logging with context:
  - Correlation IDs in all logs
  - Command execution logs
  - Event append logs
  - Projection processing logs
  - Error logs with stack traces
- [ ] Implement distributed tracing support:
  - Correlation ID propagation
  - Causation tracking
  - Span creation for operations
- [ ] Create health check endpoints
- [ ] Add alerting thresholds
- [ ] Build dashboards (Grafana templates)

**Deliverables:**

- `core/service/Service/Metrics.hs`
- Metrics collection and export
- Structured logging utilities
- Tracing integration
- Dashboard templates
- Runbook documentation

**Design Considerations:**

- Use existing Haskell metrics libraries
- Export to Prometheus/StatsD
- Low overhead (don't slow down hot path)
- Correlation IDs everywhere

---

### 4.2 Performance Optimization

**Priority:** MEDIUM-LOW
**Estimated Effort:** Medium
**Dependencies:** Phase 1 complete, 2.2 Snapshots

**Tasks:**

- [ ] Implement entity caching:
  - In-memory cache for hot entities
  - TTL and eviction policies
  - Cache invalidation on updates
- [ ] Add projection batching:
  - Process events in batches
  - Bulk updates to read models
  - Configurable batch size
- [ ] Optimize event store queries:
  - Database indexes review
  - Query plan analysis
  - Partitioning strategy for large stores
- [ ] Implement parallel projection processing:
  - Independent projections run concurrently
  - Resource management
- [ ] Add connection pooling tuning
- [ ] Create performance benchmarks
- [ ] Profile and optimize hot paths

**Deliverables:**

- Entity caching layer
- Batch processing utilities
- Database optimization guide
- Parallel projection runner
- Performance benchmarks
- Tuning documentation

**Design Considerations:**

- Measure before optimizing
- Focus on append performance (most critical)
- Cache invalidation is hard
- Benchmark realistic workloads

---

### 4.3 Enhanced Resilience

**Priority:** MEDIUM-LOW
**Estimated Effort:** Medium
**Dependencies:** Phase 1 complete

**Tasks:**

- [ ] Create `Service.Resilience` module
- [ ] Implement retry policies:
  - Exponential backoff with jitter
  - Maximum retry attempts
  - Retry on specific errors only
- [ ] Add circuit breaker pattern:
  - Track failure rates
  - Open circuit after threshold
  - Half-open state for recovery testing
  - Closed state when recovered
- [ ] Implement fallback strategies:
  - Graceful degradation
  - Default responses for queries
  - Cached responses when available
- [ ] Add poison message handling:
  - Dead letter queue for bad events
  - Logging and alerting
  - Manual replay capability
- [ ] Create timeout handling
- [ ] Implement compensating actions framework

**Deliverables:**

- `core/service/Service/Resilience.hs`
- Retry policy configurations
- Circuit breaker implementation
- Dead letter queue
- Documentation on error handling

**Design Considerations:**

- Distinguish transient from permanent errors
- Don't retry validation failures
- Circuit breakers prevent cascading failures
- Log all resilience actions for debugging

---

### 4.4 Security Enhancements

**Priority:** MEDIUM-LOW
**Estimated Effort:** Medium
**Dependencies:** Phase 1 complete

**Tasks:**

- [ ] Create `Service.Security` module
- [ ] Implement authentication integration:
  - User context in commands
  - User ID in event metadata
- [ ] Add authorization checks:
  - Permission verification before command execution
  - Role-based access control
  - Entity-level permissions
- [ ] Implement event encryption:
  - Encrypt sensitive event data
  - Key management integration
  - Transparent decryption on read
- [ ] Add data masking for projections:
  - Redact sensitive fields in read models
  - Role-based field visibility
- [ ] Implement secure transport (TLS):
  - Database connections
  - Message broker connections
- [ ] Add audit logging enhancements:
  - Track all access (read and write)
  - Security event logging
  - Compliance reporting
- [ ] Create access control for event store operations

**Deliverables:**

- `core/service/Service/Security.hs`
- Authentication/authorization framework
- Encryption utilities
- Data masking helpers
- Audit logging
- Security documentation

**Design Considerations:**

- Defense in depth
- Encrypt data at rest and in transit
- Audit all security-relevant events
- Follow principle of least privilege

---

### 4.5 Enhanced Subscription Infrastructure

**Priority:** MEDIUM-LOW
**Estimated Effort:** Small-Medium
**Dependencies:** None (enhancement to existing subscriptions)

**Tasks:**

- [ ] Add persistent subscription state:
  - Store subscription positions in database
  - Resume from last position after restart
- [ ] Implement competing consumers:
  - Distribute events across multiple instances
  - Load balancing
  - Ensure each event processed by one consumer
- [ ] Add subscription groups:
  - Named subscription with shared position
  - Multiple consumers in same group
- [ ] Implement back-pressure handling:
  - Slow down when consumers can't keep up
  - Buffer management
  - Flow control
- [ ] Add subscription monitoring:
  - Track subscription lag
  - Alert on falling behind
  - Consumer health checks
- [ ] Create subscription management API:
  - List subscriptions
  - Reset subscription position
  - Pause/resume subscriptions

**Deliverables:**

- Enhanced subscription infrastructure
- Persistent subscription storage
- Competing consumer support
- Subscription admin tools
- Documentation

**Design Considerations:**

- Backward compatible with existing subscriptions
- At-least-once delivery
- Consumer idempotency required
- PostgreSQL LISTEN/NOTIFY for push notifications

---

## Phase 5: Developer Experience (Ongoing)

These improvements make the framework easier to use and understand.

### 5.1 Documentation

**Priority:** HIGH (ongoing)
**Estimated Effort:** Ongoing

**Tasks:**

- [ ] Create comprehensive framework documentation:
  - Getting started guide
  - Core concepts explanation
  - Design patterns and best practices
  - API reference
- [ ] Write example applications:
  - Simple e-commerce (orders, inventory, shipping)
  - Bank account (transfers, balance)
  - Task management
- [ ] Document common patterns:
  - Entity design
  - Command validation
  - Projection patterns
  - Saga coordination
- [ ] Create architecture decision records (ADRs)
- [ ] Write troubleshooting guide
- [ ] Create migration guides (version upgrades)
- [ ] Document performance tuning
- [ ] Write runbooks for operations

**Deliverables:**

- `docs/event-sourcing/` directory with comprehensive docs
- Example applications in `examples/` directory
- ADRs in `docs/adr/`
- Operational runbooks

**Design Considerations:**

- Examples should follow NeoHaskell style
- Progressive learning path (simple to complex)
- Real-world scenarios
- Clear explanations of "why" not just "how"

---

### 5.2 Testing Infrastructure Enhancements

**Priority:** MEDIUM
**Estimated Effort:** Small-Medium
**Dependencies:** Phase 1 complete

**Tasks:**

- [ ] Create entity testing helpers:
  - Given-When-Then DSL
  - Event builder utilities
  - Assertion helpers for events
- [ ] Add projection testing utilities:
  - Event replay test harness
  - Idempotency verification
  - State assertion helpers
- [ ] Create integration test framework:
  - End-to-end test scenarios
  - Test data builders
  - Cleanup utilities
- [ ] Add property-based testing examples:
  - Entity invariants
  - Projection correctness
  - Concurrency properties
- [ ] Create performance test suite:
  - Throughput benchmarks
  - Latency measurements
  - Load testing scenarios

**Deliverables:**

- `core/service/Service/Testing.hs`
- Test utilities and helpers
- Example test suites
- Testing documentation

**Design Considerations:**

- Make testing easy and intuitive
- Clear test failure messages
- Fast test execution
- Realistic test scenarios

---

### 5.3 Code Generation & Templates

**Priority:** LOW
**Estimated Effort:** Medium
**Dependencies:** Phase 1 complete

**Tasks:**

- [ ] Create CLI command for generating:
  - Entity boilerplate
  - Command types
  - Event types
  - Projection skeleton
  - Read model templates
- [ ] Add NeoHaskell style templates
- [ ] Create project scaffolding
- [ ] Add code snippets for common patterns
- [ ] Generate test templates

**Deliverables:**

- CLI commands: `neo generate entity`, `neo generate projection`, etc.
- Template files
- Documentation on code generation

**Design Considerations:**

- Follow NeoHaskell conventions
- Customizable templates
- Generate compilable code
- Include TODO comments for customization

---

## Phase 6: Advanced Infrastructure (Future)

These are nice-to-have features for specific use cases.

### 6.1 Multi-Tenancy Support

**Priority:** LOW
**Estimated Effort:** Medium

**Tasks:**

- [ ] Add tenant ID to events and commands
- [ ] Implement tenant isolation:
  - Separate event streams per tenant
  - Tenant-specific read models
  - Cross-tenant query prevention
- [ ] Add tenant-aware subscriptions
- [ ] Create tenant management utilities

---

### 6.2 Event Store Partitioning

**Priority:** LOW
**Estimated Effort:** Large

**Tasks:**

- [ ] Implement horizontal partitioning:
  - Partition by entity ID
  - Partition by time range
  - Partition routing
- [ ] Add partition management tools
- [ ] Create partition migration utilities

---

### 6.3 Event Archival

**Priority:** LOW
**Estimated Effort:** Medium

**Tasks:**

- [ ] Implement event archival:
  - Move old events to cold storage
  - Archive policy configuration
  - Archived event retrieval
- [ ] Add data retention policies
- [ ] Create compliance reporting

---

### 6.4 GraphQL Query Layer

**Priority:** LOW
**Estimated Effort:** Medium
**Dependencies:** 1.6 Query Handlers

**Tasks:**

- [ ] Create GraphQL schema for read models
- [ ] Implement GraphQL resolvers using query handlers
- [ ] Add real-time subscriptions (via GraphQL subscriptions)
- [ ] Create GraphQL playground

---

## Implementation Guidelines

### Code Quality Standards

- **Follow NeoHaskell style guide** (CLAUDE.md)

  - Explicit imports with qualified modules
  - No point-free style
  - Use `do` blocks for bindings
  - Use `Result` instead of `Either`
  - Use `|>` for composition
  - Use `[fmt|...|]` for string interpolation

- **Testing requirements**

  - Unit tests for all business logic
  - Integration tests for all components
  - Property-based tests for invariants
  - Performance benchmarks for critical paths
  - Minimum 80% code coverage

- **Documentation requirements**
  - Haddock comments for all public APIs
  - Module-level documentation explaining purpose
  - Example usage in documentation
  - Doctest examples where appropriate

### Development Process

1. **For each feature:**

   - Create design document (if complex)
   - Write tests first (TDD)
   - Implement feature
   - Update documentation
   - Add example usage
   - Performance testing
   - Code review

2. **Quality gates:**

   - All tests pass
   - Code follows style guide
   - Documentation complete
   - Performance acceptable
   - Security reviewed

3. **Release process:**
   - Version bump (semantic versioning)
   - Changelog update
   - Migration guide (if breaking changes)
   - Release notes

### Priority Definitions

- **CRITICAL**: Blocks all other work, must be done first
- **HIGH**: Core functionality needed for MVP
- **MEDIUM**: Important features for production use
- **MEDIUM-LOW**: Nice to have, improves framework
- **LOW**: Future enhancements, specific use cases

### Effort Estimates

- **Small**: 1-3 days
- **Small-Medium**: 3-5 days
- **Medium**: 1-2 weeks
- **Medium-Large**: 2-3 weeks
- **Large**: 3-4+ weeks

---

## Recommended Implementation Order

### Milestone 1: Basic CQRS (4-6 weeks)

1. Entities (1.1)
2. Commands (1.2)
3. Command Handlers (1.3)
4. Enhanced Event Metadata (2.3)
5. Testing Infrastructure (5.2)
6. Documentation (5.1)

**Outcome**: Developers can build simple event-sourced entities with command handling.

### Milestone 2: Read Side (3-4 weeks)

1. Projection Engine (1.4)
2. Read Model Infrastructure (1.5)
3. Query Handlers (1.6)
4. Documentation updates (5.1)

**Outcome**: Complete CQRS with queryable read models.

### Milestone 3: Production Readiness (4-6 weeks)

1. Event Versioning (2.1)
2. Snapshots (2.2)
3. Monitoring & Observability (4.1)
4. Enhanced Resilience (4.3)
5. Documentation (5.1)

**Outcome**: Production-ready framework for evolving systems.

### Milestone 4: Advanced Features (6-8 weeks)

1. Sagas (3.1)
2. Integration Events & Outbox (3.2)
3. Temporal Queries (3.3)
4. Performance Optimization (4.2)
5. Security Enhancements (4.4)
6. Documentation (5.1)

**Outcome**: Enterprise-ready framework with advanced patterns.

### Milestone 5: Developer Experience (Ongoing)

1. Code Generation (5.3)
2. Example Applications (5.1)
3. Enhanced Subscriptions (4.5)
4. Additional features as needed

**Outcome**: Easy-to-use framework with great DX.

---

## Success Metrics

### Technical Metrics

- Event append latency: < 10ms (p99)
- Query response time: < 100ms (p99)
- Projection lag: < 1 second (normal load)
- Test coverage: > 80%
- Zero data loss guarantees

### Developer Experience Metrics

- Time to first working entity: < 30 minutes
- Documentation completeness: All public APIs documented
- Example coverage: All major patterns demonstrated
- Community feedback: Positive reception

### Production Readiness Metrics

- Monitoring coverage: All critical operations monitored
- Error handling: All error paths covered
- Performance: Scales to 10,000+ events/second
- Reliability: 99.9%+ uptime in production use

---

## Notes

- This TODO represents a comprehensive event sourcing framework
- NeoHaskell already has an excellent foundation with the Event Store implementation
- Prioritization should focus on Phase 1 (Core CQRS) to enable real applications
- Phases 2-4 can be implemented iteratively based on real-world needs
- Community feedback should guide feature prioritization
- Performance optimization should be data-driven (measure first)
- Security should be considered from the start, not bolted on later

---

**Last Updated**: 2025-11-17
**Status**: Planning phase - awaiting implementation
**Maintainer**: NeoHaskell Core Team
