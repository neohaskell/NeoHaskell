# Transport Protocol Architecture for NeoHaskell Services

## Overview

This document outlines the architecture for implementing a type-safe, high-performance transport protocol system for NeoHaskell's event-sourced services. The system enables commands to be exposed through multiple transport protocols (REST, GraphQL, gRPC) with compile-time guarantees and correctness under high load.

## Core Concepts

### Transport vs Serialization

The current `SerializationProtocols` associated type family in the `Command` class should be renamed to `TransportProtocols` as it represents the complete transport layer (REST, GraphQL, gRPC), not just data serialization formats.

### Type-Level Protocol Tracking

The system uses type-level programming to track:

1. Which protocols each command supports (via `TransportProtocols` type family)
2. Which adapters are registered in the service (via `Record.Row` type)
3. Compile-time verification that all required protocols have corresponding adapters

## Architecture Components

### 1. Core Protocol Abstractions

#### 1.1 Command Class Enhancement

```haskell
class Command command where
  type IsMultiTenant command :: Bool
  type IsMultiTenant command = False

  type EntityIdType command :: Type
  type EntityIdType command = Uuid

  -- Renamed from SerializationProtocols
  type TransportProtocols command :: [Symbol]
  type TransportProtocols command = '["REST"]  -- Default to REST

  getEntityIdImpl :: GetEntityIdFunction (IsMultiTenant command) command (EntityIdType command)
  decideImpl :: DecideFunction (IsMultiTenant command) command (EntityOf command) (EventOf (EntityOf command))
```

#### 1.2 Transport Protocol Type Class

```haskell
class TransportProtocol (protocol :: Symbol) where
  -- Constraint for types that can be decoded from requests
  type RequestFormat protocol :: Type -> Constraint

  -- Constraint for types that can be encoded to responses
  type ResponseFormat protocol :: Type -> Constraint

  -- Protocol-specific endpoint specification
  type EndpointSpec protocol command :: Type

  -- Parse incoming request into command
  parseRequest ::
    (RequestFormat protocol command) =>
    proxy protocol ->
    ByteString ->
    Task Text command

  -- Format command result for response
  formatResponse ::
    (ResponseFormat protocol result) =>
    proxy protocol ->
    result ->
    ByteString

  -- Generate endpoint specification for a command
  generateEndpoint ::
    proxy protocol ->
    TypeRep command ->
    EndpointSpec protocol command
```

#### 1.3 Protocol Implementations

**REST Protocol:**

```haskell
instance TransportProtocol "REST" where
  type RequestFormat "REST" = FromJSON
  type ResponseFormat "REST" = ToJSON
  type EndpointSpec "REST" command = RestEndpoint

data RestEndpoint = RestEndpoint
  { method :: HttpMethod
  , path :: Text
  , contentTypes :: Array MediaType
  }
```

### 2. Service Definition Enhancement

#### 2.1 Extended ServiceDefinition Type

```haskell
data ServiceDefinition
    (commands :: Record.Row Type)        -- Commands registered in the service
    (protocols :: [Symbol])               -- Union of all command protocols
    (adapters :: Record.Row Type)        -- Adapters for protocols
    (value :: Type)                      -- Value carried through DSL
  = ServiceDefinition
  { commandNames :: Record commands
  , protocolAdapters :: Record adapters
  , value :: value
  }
```

#### 2.2 Type-Level Protocol Collection

When adding a command to the service, extract and union its protocols:

```haskell
-- Type family to extract protocols from a command
type family ExtractProtocols command :: [Symbol] where
  ExtractProtocols command = TransportProtocols command

-- Type family to union protocol lists
type family UnionProtocols (ps1 :: [Symbol]) (ps2 :: [Symbol]) :: [Symbol]

-- Enhanced command registration
command ::
  forall commandType commandName protocols.
  ( Command commandType
  , commandName ~ NameOf commandType
  , protocols ~ TransportProtocols commandType
  , IsLabel commandName (Record.Field commandName)
  ) =>
  ServiceDefinition
    '[commandName Record.:= CommandDefinition commandType]
    protocols
    '[]  -- No adapters yet
    Unit
```

#### 2.3 Adapter Registration

```haskell
-- Register an adapter for a specific protocol
withAdapter ::
  forall protocol adapter currentCommands currentProtocols currentAdapters value.
  ( TransportProtocol protocol
  , ServiceAdapter adapter
  , AdapterProtocol adapter ~ protocol
  , Member protocol currentProtocols ~ True  -- Compile-time check
  ) =>
  adapter ->
  ServiceDefinition currentCommands currentProtocols currentAdapters value ->
  ServiceDefinition currentCommands currentProtocols
    (currentAdapters Record.++ '[protocol Record.:= adapter]) value
```

### 3. Service Adapter System

#### 3.1 Service Adapter Type Class

```haskell
class ServiceAdapter adapter where
  type AdapterProtocol adapter :: Symbol
  type AdapterConfig adapter :: Type

  -- Initialize the adapter with service definition
  initializeAdapter ::
    adapter ->
    AdapterConfig adapter ->
    Task Text AdapterState

  -- Start serving requests
  startServing ::
    adapter ->
    AdapterState ->
    ServiceRuntime commands ->
    Task Text (AsyncTask Unit)

  -- Graceful shutdown
  stopServing ::
    adapter ->
    AdapterState ->
    Task Text Unit

data AdapterState = AdapterState
  { serverId :: ServerId
  , connectionPool :: ConnectionPool
  , metrics :: MetricsCollector
  , healthStatus :: HealthStatus
  }
```

#### 3.2 Service Runtime

```haskell
-- Runtime representation of the service
data ServiceRuntime commands = ServiceRuntime
  { commandHandlers :: Record commands
  , eventStore :: EventStore
  , entityFetcher :: EntityFetcher
  , loadStrategy :: LoadStrategy
  , monitoring :: MonitoringSystem
  }
```

### 4. REST Adapter Implementation

#### 4.1 REST Adapter Type

```haskell
data RestAdapter = RestAdapter
  { port :: Int
  , basePath :: Text
  , middleware :: Array WarpMiddleware
  }

instance ServiceAdapter RestAdapter where
  type AdapterProtocol RestAdapter = "REST"
  type AdapterConfig RestAdapter = RestConfig

  initializeAdapter adapter config = do
    pool <- ConnectionPool.create config.poolSize
    metrics <- Metrics.initialize
    RestAdapterState.yield
      { connectionPool = pool
      , metrics = metrics
      , warpSettings = buildWarpSettings config
      }
```

#### 4.2 Request Processing Pipeline

```haskell
processRestRequest ::
  forall command.
  ( Command command
  , Member "REST" (TransportProtocols command)
  ) =>
  Request ->
  ServiceRuntime commands ->
  Task Text Response

-- Pipeline stages:
-- 1. Route matching (command name from URL)
-- 2. Content-Type validation
-- 3. Request body parsing (JSON → Command)
-- 4. Command validation
-- 5. Command execution via CommandHandler
-- 6. Result serialization (CommandHandlerResult → JSON)
-- 7. Response construction with appropriate status code
```

#### 4.3 Endpoint Generation

```haskell
generateRestEndpoints ::
  forall commands.
  Record commands ->
  Array RestEndpoint

-- For each command in the service:
-- - Generate path: /api/v1/{command-name}
-- - Determine HTTP method (POST for commands, GET for queries)
-- - Set accepted content types from command's formats
-- - Configure request/response schemas
```

### 5. Error Handling

#### 5.1 Error Classification

```haskell
data ServiceError
  = ValidationError ValidationErrorDetails
  | AuthenticationError AuthErrorDetails
  | AuthorizationError AuthzErrorDetails
  | CommandRejection Text
  | EventStoreError EventStoreErrorDetails
  | ProtocolError ProtocolErrorDetails
  | ResourceExhausted ResourceErrorDetails
  | InternalError Text

data ErrorClassification
  = Retryable RetryConfig      -- Can be retried
  | NonRetryable               -- Should not retry
  | RateLimited Duration       -- Retry after delay
```

#### 5.2 Error Response Format

```haskell
data ErrorResponse = ErrorResponse
  { error :: ErrorDetails
  , requestId :: RequestId
  , timestamp :: Timestamp
  , suggestions :: Array Text
  }

data ErrorDetails = ErrorDetails
  { code :: ErrorCode
  , message :: Text
  , details :: Maybe (HashMap Text Value)
  , retryable :: Bool
  , retryAfter :: Maybe Duration
  }
```

### 6. Type-Level Compile-Time Guarantees

#### 6.1 Protocol Coverage Verification

```haskell
-- Type family to check all command protocols have adapters
type family CheckProtocolCoverage
  (commandProtocols :: [Symbol])
  (adapterProtocols :: [Symbol]) :: Constraint where
  CheckProtocolCoverage '[] adapters = ()
  CheckProtocolCoverage (p ': ps) adapters =
    If (Member p adapters)
       (CheckProtocolCoverage ps adapters)
       (TypeError ('Text "Missing adapter for protocol: " ':<>: ShowType p))
```

#### 6.2 Command Registration Safety

```haskell
-- Ensure command names are unique
type family UniqueCommandNames (commands :: Record.Row Type) :: Constraint

-- Ensure all commands have required instances
type family ValidateCommands (commands :: Record.Row Type) :: Constraint where
  ValidateCommands commands =
    ( AllCommandsHaveDecide commands
    , AllCommandsHaveEntityId commands
    , AllCommandsSerializable commands
    )
```

#### 6.3 Service Composition Safety

```haskell
-- Safe service composition ensuring protocol compatibility
composeServices ::
  forall cmds1 cmds2 protocols1 protocols2 adapters1 adapters2.
  ( DisjointLabels cmds1 cmds2  -- No command name conflicts
  , protocols1 ~ ExtractAllProtocols cmds1
  , protocols2 ~ ExtractAllProtocols cmds2
  , CheckProtocolCoverage (Union protocols1 protocols2) (Union adapters1 adapters2)
  ) =>
  ServiceDefinition cmds1 protocols1 adapters1 () ->
  ServiceDefinition cmds2 protocols2 adapters2 () ->
  ServiceDefinition
    (cmds1 Record.++ cmds2)
    (Union protocols1 protocols2)
    (Union adapters1 adapters2)
    ()
```

## Implementation Strategy

### Core Components Build Order

1. **Transport Protocol Abstraction Layer**

   - Define `TransportProtocol` type class
   - Rename `SerializationProtocols` to `TransportProtocols`
   - Implement type-level protocol tracking

2. **Service Definition Enhancement**

   - Add protocol and adapter tracking to `ServiceDefinition`
   - Implement type-level protocol extraction
   - Create adapter registration DSL

3. **REST Adapter**

   - Implement `ServiceAdapter` for REST
   - Integrate Warp server
   - Build request/response pipeline

4. **Basic Error Handling**

   - Implement error classification
   - Create standard error response format
   - Add basic retry logic (leveraging existing CommandHandler retry)

5. **Type-Level Safety**
   - Implement protocol coverage verification
   - Add command registration safety checks
   - Ensure service composition safety

### Testing Strategy

1. **Type-Level Testing**

   - Compile-time tests for protocol coverage
   - Type family unit tests
   - Service composition safety tests

2. **Integration Testing**

   - REST endpoint generation
   - End-to-end request flows
   - Error propagation
   - Command execution via HTTP

3. **Correctness Testing**
   - Concurrent command execution
   - Event ordering verification
   - Existing optimistic concurrency handling

## Key Design Principles

1. **Type Safety First**: All protocol and adapter relationships verified at compile time
2. **Zero Runtime Overhead**: Type-level computations have no runtime cost
3. **Composability**: Services, protocols, and adapters compose cleanly
4. **Extensibility**: New protocols can be added without modifying core
5. **Performance**: Designed for millions of requests with bounded resources
6. **Correctness**: Event sourcing guarantees maintained under all conditions
7. **Observability**: Complete visibility into system behavior
8. **Graceful Degradation**: System degrades predictably under load

## Future Enhancements

This section covers planned enhancements that will be implemented after the core REST transport system is operational.

### Additional Protocol Support

#### GraphQL Protocol

```haskell
instance TransportProtocol "GraphQL" where
  type RequestFormat "GraphQL" = GraphQLInput
  type ResponseFormat "GraphQL" = GraphQLOutput
  type EndpointSpec "GraphQL" command = GraphQLMutation

data GraphQLMutation = GraphQLMutation
  { mutationName :: Text
  , inputType :: GraphQLType
  , outputType :: GraphQLType
  }
```

#### gRPC Protocol

```haskell
instance TransportProtocol "gRPC" where
  type RequestFormat "gRPC" = ProtoBufMessage
  type ResponseFormat "gRPC" = ProtoBufMessage
  type EndpointSpec "gRPC" command = GrpcMethod

data GrpcMethod = GrpcMethod
  { serviceName :: Text
  , methodName :: Text
  , requestType :: ProtoType
  , responseType :: ProtoType
  , streaming :: StreamingType
  }
```

### High-Load Architecture

#### Load Management Strategy

```haskell
data LoadStrategy = LoadStrategy
  { maxConcurrentRequests :: Int        -- Maximum concurrent requests
  , requestQueueSize :: Int             -- Bounded queue for incoming requests
  , connectionPoolSize :: Int           -- Database/EventStore connections
  , eventBatchSize :: Int               -- Events per batch write
  , backpressureThreshold :: Double     -- When to apply backpressure (0-1)
  , circuitBreakerConfig :: CircuitBreakerConfig
  }

data CircuitBreakerConfig = CircuitBreakerConfig
  { failureThreshold :: Int            -- Failures before opening
  , successThreshold :: Int            -- Successes to close
  , timeout :: Duration                -- Time in open state
  , halfOpenRequests :: Int            -- Requests in half-open state
  }
```

#### Connection Pool Management

```haskell
data ConnectionPool = ConnectionPool
  { activeConnections :: ConcurrentVar (Array Connection)
  , waitingQueue :: BoundedQueue ConnectionRequest
  , poolMetrics :: PoolMetrics
  , poolConfig :: PoolConfig
  }

data PoolConfig = PoolConfig
  { minConnections :: Int
  , maxConnections :: Int
  , connectionTimeout :: Duration
  , idleTimeout :: Duration
  , validationInterval :: Duration
  }
```

#### Request Queue with Backpressure

```haskell
data RequestQueue = RequestQueue
  { queue :: BoundedQueue Request
  , rejectionPolicy :: RejectionPolicy
  , prioritization :: PriorityFunction
  }

data RejectionPolicy
  = RejectOldest         -- Drop oldest request
  | RejectNewest         -- Drop new request
  | RejectLowestPriority -- Drop based on priority
  | BlockUntilSpace      -- Block producer
```

#### Event Store Optimizations

```haskell
data EventStoreOptimizations = EventStoreOptimizations
  { batchWriting :: BatchWriteConfig
  , readCache :: CacheConfig
  , partitioning :: PartitionStrategy
  , asyncPublishing :: PublishingConfig
  }

data BatchWriteConfig = BatchWriteConfig
  { maxBatchSize :: Int           -- Maximum events per batch
  , maxBatchDelay :: Duration     -- Maximum wait before flush
  , parallelWriters :: Int        -- Concurrent batch writers
  }

data CacheConfig = CacheConfig
  { cacheSize :: Int              -- Number of entities to cache
  , ttl :: Duration               -- Time to live
  , warmupEntities :: Array EntityId  -- Pre-load on startup
  }
```

### Correctness Guarantees

#### Concurrency Control

```haskell
data ConcurrencyControl = ConcurrencyControl
  { optimisticLocking :: OptimisticLockConfig
  , retryStrategy :: RetryStrategy
  , conflictResolution :: ConflictResolutionPolicy
  }

data RetryStrategy = RetryStrategy
  { maxRetries :: Int
  , backoffStrategy :: BackoffStrategy
  , jitterFunction :: JitterFunction
  }

data BackoffStrategy
  = ConstantBackoff Duration
  | LinearBackoff Duration Duration
  | ExponentialBackoff Duration Double Int  -- base, multiplier, maxDelay
  | FibonacciBackoff Duration Int
```

#### Idempotency Support

```haskell
data IdempotencyConfig = IdempotencyConfig
  { idempotencyKeyHeader :: Text
  , deduplicationWindow :: Duration
  , storage :: IdempotencyStorage
  }

data IdempotencyStorage
  = InMemoryStorage (ConcurrentVar (HashMap IdempotencyKey Response))
  | RedisStorage RedisConnection
  | EventStoreStorage  -- Use event store for deduplication
```

#### Ordering Guarantees

```haskell
data OrderingGuarantee
  = NoOrdering           -- Best effort, no guarantees
  | PerEntityOrdering    -- Orders preserved per entity
  | GlobalOrdering       -- Total order across all entities
  | CausalOrdering       -- Respects causal dependencies

-- Enforcement mechanism
data OrderingEnforcement = OrderingEnforcement
  { sequencer :: EventSequencer
  , conflictDetection :: ConflictDetector
  , reorderBuffer :: ReorderBuffer
  }
```

### Monitoring and Observability

#### Metrics Collection

```haskell
data ServiceMetrics = ServiceMetrics
  { requestMetrics :: RequestMetrics
  , commandMetrics :: CommandMetrics
  , eventStoreMetrics :: EventStoreMetrics
  , resourceMetrics :: ResourceMetrics
  }

data RequestMetrics = RequestMetrics
  { totalRequests :: Counter
  , requestDuration :: Histogram
  , requestsInFlight :: Gauge
  , requestsByProtocol :: LabeledCounter Protocol
  , requestsByCommand :: LabeledCounter CommandName
  }

data CommandMetrics = CommandMetrics
  { commandsAccepted :: Counter
  , commandsRejected :: Counter
  , commandsFailed :: Counter
  , commandRetries :: Histogram
  , decisionDuration :: Histogram
  }
```

#### Health Check System

```haskell
data HealthCheckSystem = HealthCheckSystem
  { livenessCheck :: HealthCheck      -- Is service running?
  , readinessCheck :: HealthCheck     -- Can accept traffic?
  , startupCheck :: HealthCheck       -- Initialization complete?
  , customChecks :: Array HealthCheck
  }

data HealthCheck = HealthCheck
  { name :: Text
  , check :: Task Text HealthStatus
  , timeout :: Duration
  , critical :: Bool  -- Affects overall health
  }

data HealthStatus
  = Healthy
  | Degraded Text  -- Partial functionality
  | Unhealthy Text -- Not functioning
```

#### Distributed Tracing

```haskell
data TracingConfig = TracingConfig
  { enabled :: Bool
  , sampler :: TraceSampler
  , exporter :: TraceExporter
  , propagation :: PropagationFormat
  }

data TraceContext = TraceContext
  { traceId :: TraceId
  , spanId :: SpanId
  , parentSpanId :: Maybe SpanId
  , baggage :: HashMap Text Text
  }
```

### Security Considerations

#### Input Validation

```haskell
data ValidationConfig = ValidationConfig
  { schemaValidation :: SchemaValidator
  , sizeLimit :: Int
  , complexityLimit :: ComplexityLimit
  , sanitization :: SanitizationRules
  }

data ComplexityLimit = ComplexityLimit
  { maxDepth :: Int           -- Nesting depth
  , maxFields :: Int          -- Total fields
  , maxArrayLength :: Int     -- Array size
  }
```

#### Rate Limiting

```haskell
data RateLimitConfig = RateLimitConfig
  { globalLimit :: RateLimit
  , perClientLimit :: RateLimit
  , perCommandLimit :: LabeledRateLimit CommandName
  , rateLimitStorage :: RateLimitStorage
  }

data RateLimit = RateLimit
  { requestsPerWindow :: Int
  , windowDuration :: Duration
  , burstAllowance :: Int
  }
```

#### Authentication and Authorization

```haskell
data AuthConfig = AuthConfig
  { authenticationMiddleware :: AuthenticationMiddleware
  , authorizationPolicy :: AuthorizationPolicy
  , tokenValidation :: TokenValidator
  , sessionManagement :: SessionManager
  }

data AuthorizationPolicy = AuthorizationPolicy
  { commandPermissions :: HashMap CommandName Permission
  , roleHierarchy :: RoleHierarchy
  , dynamicRules :: Array DynamicAuthRule
  }
```

### Performance Targets

These targets will guide optimization efforts once the system is operational:

#### Throughput

- Single node: 100,000+ requests/second
- Horizontal scaling: Linear up to 10 nodes
- Event persistence: 10,000+ events/second

#### Latency

- p50: < 10ms
- p95: < 50ms
- p99: < 100ms
- p99.9: < 500ms

#### Resource Usage

- Memory: < 4GB RSS under normal load
- CPU: < 80% utilization at peak
- Connections: < 10,000 concurrent
- File descriptors: < 20,000

#### Reliability

- Availability: 99.99% uptime
- Error rate: < 0.1% under normal conditions
- Recovery time: < 30 seconds after failure
- Data loss: Zero (event sourcing guarantee)

## Open Questions

1. **Protocol Negotiation**: How should clients negotiate which protocol to use?
2. **Protocol Versioning**: How to handle protocol version compatibility?
3. **Cross-Protocol Communication**: Should commands be callable across protocols?
4. **Streaming Support**: How to handle streaming commands/events?
5. **Federation**: How to federate services across multiple nodes?
6. **Schema Evolution**: How to handle command schema changes over time?
7. **Multi-Region**: Considerations for geo-distributed deployments?

## Conclusion

This architecture provides a robust, type-safe foundation for exposing event-sourced commands through multiple transport protocols. By leveraging NeoHaskell's type system and the existing event sourcing infrastructure, we achieve both compile-time safety and runtime performance suitable for production systems handling millions of requests.
