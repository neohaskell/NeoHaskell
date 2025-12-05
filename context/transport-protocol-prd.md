# PRD: Transport Protocol Architecture for NeoHaskell Services

**Document Version:** 1.1
**Status:** Ready for Implementation

---

## 1. Overview

### 1.1 Problem Statement

NeoHaskell services currently support executing individual commands directly. We need a way to define complete services that expose commands through transport protocols (REST, GraphQL, gRPC), with compile-time guarantees that all required protocols have corresponding adapters.

### 1.2 Goal

Implement a type-safe, monadic DSL for defining services that:

1. Accumulates protocol requirements from commands
2. Accumulates protocol adapters registered in scope
3. Validates at the deployment boundary that all required protocols are provided
4. Produces clear, actionable compile-time error messages when protocols are missing

### 1.3 Success Criteria

- Developers can define services using a `do`-notation DSL
- Commands can declare which protocols they support via the `TransportProtocols` type family
- Missing adapters result in compile-time errors with helpful messages
- A `DirectAdapter` allows running commands directly as `Task` without network transport
- All command execution flows through a uniform `Bytes` interface
- All runtime code is covered by tests following TDD methodology

---

## 2. User-Facing API

### 2.1 Service Definition DSL

The service definition DSL allows developers to declaratively specify which protocols their service exposes and which commands it handles. The syntax uses `do`-notation for familiarity:

```haskell
cartService = do
  expose (DirectAdapter defaultConfig)
  command @CreateCart
  command @AddItemToCart
  command @RemoveItemFromCart
```

The `expose` keyword declares that the service makes its commands available through a particular transport protocol. This reads naturally as "this service exposes commands via the Direct protocol."

### 2.2 Command Execution Interface

All command execution operates on `Bytes`. The `ServiceRuntime` provides a single execution function that accepts the command name and a `Bytes` payload. This uniform interface means:

- All adapters work with the same data format
- Commands are decoded from `Bytes` via Aeson at execution time
- The runtime routes to the appropriate adapter based on the command's declared protocols

### 2.3 Deployment

Deployment converts a `ServiceDefinition` into a runnable `ServiceRuntime`. This is the boundary where protocol validation occurs — if any required protocol lacks an adapter, compilation fails with a descriptive error.

---

## 3. Technical Specification

### 3.1 Core Types

#### 3.1.1 ServiceDefinition

The `ServiceDefinition` type carries four type-level parameters that track the service's configuration:

| Parameter           | Purpose                                                              |
| ------------------- | -------------------------------------------------------------------- |
| `commands`          | Row type of all registered commands and their definitions            |
| `requiredProtocols` | Type-level list of protocols needed by the registered commands       |
| `providedProtocols` | Type-level list of protocols for which adapters have been registered |
| `adapters`          | Row type of adapter instances, keyed by protocol name                |

The monadic `value` parameter enables `do`-notation composition.

#### 3.1.2 ServiceRuntime

The `ServiceRuntime` is the deployed, runnable form of a service. It provides:

- **Command execution**: A function that takes a command name (`Text`) and payload (`Bytes`), routes to the appropriate adapter, and returns the result as `Bytes`
- **Shutdown**: Graceful shutdown of all adapters

The execution interface is intentionally `Bytes`-in, `Bytes`-out to maintain uniformity across all transport protocols. Each adapter is responsible for understanding the `Bytes` format it receives.

#### 3.1.3 DirectAdapter

The `DirectAdapter` is a special adapter that executes commands in-process without network transport. Despite being "direct," it still operates on the `Bytes` interface:

1. Receives command payload as `Bytes`
2. Decodes the command using Aeson's `FromJSON` instance
3. Executes via the existing `CommandHandler` infrastructure
4. Encodes the result using Aeson's `ToJSON` instance
5. Returns result as `Bytes`

This design ensures that the `DirectAdapter` exercises the same serialization path as network adapters, making it suitable for testing and local development.

Configuration options include command timeout duration and retry policy.

### 3.2 Type Classes

#### 3.2.1 TransportProtocol

The `TransportProtocol` class defines the contract for a protocol. Each protocol specifies its configuration type and runtime state type. This is primarily a marker class that enables type-level protocol tracking.

#### 3.2.2 ServiceAdapter

The `ServiceAdapter` class defines how an adapter integrates with the service:

- **Protocol association**: Each adapter declares which protocol it implements via an associated type
- **Initialization**: How to set up the adapter's runtime state
- **Execution**: How to execute a command given `Bytes` input, producing `Bytes` output
- **Shutdown**: How to cleanly terminate the adapter

The execution function receives `Bytes` and must handle decoding internally. For the `DirectAdapter`, this means Aeson decoding. For a future `RestAdapter`, the `Bytes` might already be JSON from an HTTP request body.

### 3.3 Type Families

#### 3.3.1 Protocol Set Operations

Three type families provide set operations on type-level protocol lists:

**Member**: Checks whether a symbol exists in a type-level list. Returns `'True` or `'False`.

**Union**: Combines two type-level lists, eliminating duplicates. Used when registering commands to accumulate their protocol requirements.

**Difference**: Computes elements present in the first list but absent from the second. This is the key operation at deployment time — it reveals which required protocols lack adapters.

#### 3.3.2 Protocol Validation

The `ValidateProtocols` type family is a constraint that either passes silently (when no protocols are missing) or produces a compile-time error with a helpful message.

The error message should include:

- A clear statement that adapters are missing
- The complete list of required protocols
- The complete list of provided protocols
- The specific protocols that are missing
- Guidance to use `expose` to add the missing adapters

### 3.4 DSL Functions

#### 3.4.1 Service Initialization

The `service` function creates an empty service definition with no commands, no required protocols, no provided protocols, and no adapters. This is the starting point for building a service.

#### 3.4.2 Protocol Exposure

The `expose` function registers an adapter in the service definition. Semantically, it declares "this service exposes its commands via this protocol."

**Type-level behavior:**

- Adds the adapter's protocol to the `providedProtocols` list (via union)
- Stores the adapter instance in the `adapters` row

**Important:** `expose` does NOT check whether any commands require this protocol. Validation is deferred to deployment, enabling order-independent declaration.

#### 3.4.3 Command Registration

The `command` function registers a command type in the service definition.

**Type-level behavior:**

- Adds the command to the `commands` row
- Unions the command's `TransportProtocols` into `requiredProtocols`

**Important:** `command` does NOT check whether adapters exist for the command's protocols. Validation is deferred to deployment.

#### 3.4.4 Deployment

The `deploy` function converts a `ServiceDefinition` into a `ServiceRuntime`. This is the validation boundary.

**Type-level behavior:**

1. Computes `missing = Difference requiredProtocols providedProtocols`
2. Applies the `ValidateProtocols` constraint
3. If `missing` is non-empty, compilation fails with a descriptive error
4. If `missing` is empty, the constraint passes

**Runtime behavior:**

1. Initializes each registered adapter
2. Builds the command routing table
3. Returns the `ServiceRuntime`

If any adapter fails to initialize, deployment fails with a runtime error.

### 3.5 Command Execution Flow

When a command is executed through the `ServiceRuntime`:

1. **Routing**: The runtime receives a command name and `Bytes` payload
2. **Adapter selection**: Based on the command's declared protocols and available adapters, the runtime selects an appropriate adapter
3. **Execution**: The adapter's execute function is called with the `Bytes` payload
4. **Decoding**: The adapter decodes the `Bytes` (for `DirectAdapter`, via Aeson)
5. **Command handling**: The decoded command is processed via `CommandHandler`
6. **Encoding**: The result is encoded back to `Bytes`
7. **Response**: The `Bytes` result is returned

### 3.6 Monadic Interface

The `ServiceDefinition` type must support `do`-notation. This requires implementing `Functor`, `Applicative`, and `Monad` (or an indexed monad variant).

The key challenge is that `expose` and `command` modify type-level parameters. The implementation team should evaluate:

1. **Standard Monad with phantom threading**: May work if type inference cooperates
2. **Indexed Monad (IxMonad)**: Explicitly threads type-level state changes
3. **RebindableSyntax**: Custom bind operators that handle type changes

The choice should prioritize ergonomics — the DSL should feel natural to use.

---

## 4. Detailed Requirements

### 4.1 Functional Requirements

| ID    | Requirement                                                                    |
| ----- | ------------------------------------------------------------------------------ |
| FR-01 | Service definitions use `do`-notation syntax                                   |
| FR-02 | `expose` adds protocol to provided list at type level                          |
| FR-03 | `command` adds command's protocols to required list at type level              |
| FR-04 | `deploy` computes difference of required vs provided protocols                 |
| FR-05 | Non-empty difference produces compile-time error                               |
| FR-06 | Error message lists ALL missing protocols, not just the first                  |
| FR-07 | Order of `expose`/`command` calls does not affect validation                   |
| FR-08 | Command execution interface operates on `Bytes`                                |
| FR-09 | `DirectAdapter` decodes commands from `Bytes` via Aeson                        |
| FR-10 | `DirectAdapter` encodes results to `Bytes` via Aeson                           |
| FR-11 | `DirectAdapter` executes commands via existing `CommandHandler` infrastructure |
| FR-12 | `ServiceRuntime` routes commands to appropriate adapters                       |
| FR-13 | `ServiceRuntime` supports graceful shutdown                                    |

### 4.2 Non-Functional Requirements

| ID     | Requirement                                                        |
| ------ | ------------------------------------------------------------------ |
| NFR-01 | Type-level computation adds zero runtime overhead                  |
| NFR-02 | Compile-time error messages are actionable and complete            |
| NFR-03 | DSL syntax is intuitive for developers familiar with `do`-notation |
| NFR-04 | `Bytes` interface enables uniform adapter implementation           |

---

## 5. Testing Strategy

### 5.1 Testing Principles

1. **Type-level code is validated by the compiler** — no unit tests needed for type families
2. **Runtime code follows TDD** — tests written before implementation
3. **Compile-fail tests verify error cases** — separate files that must NOT compile

### 5.2 Compile-Time Validation Tests

Create test files that exercise the type-level validation. These are not traditional tests but rather files whose compilation status verifies correctness.

**Files that must NOT compile:**

- A service with a command requiring REST but no REST adapter exposed
- A service with commands requiring multiple protocols but only some adapters exposed
- A service with no adapters but commands that require protocols

**Files that MUST compile:**

- A service where all required protocols have corresponding adapters
- A service with more adapters than required (extra adapters are allowed)
- An empty service with no commands and no adapters

CI should verify that the "must not compile" files produce the expected error messages.

### 5.3 Unit Tests — ServiceDefinition Construction

Test the construction and introspection of service definitions:

- Empty service creation produces correct initial state
- `expose` correctly registers adapters and can be called multiple times
- `command` correctly registers commands and can be called multiple times
- The monadic interface composes correctly via `do`-notation

### 5.4 Unit Tests — DirectAdapter

Test the `DirectAdapter` in isolation:

**Initialization:**

- Initializes successfully with default configuration
- Initializes successfully with custom timeout and retry settings
- Produces correct runtime state

**Command Execution:**

- Successfully decodes valid `Bytes` payload via Aeson
- Returns appropriate error for malformed `Bytes`
- Returns appropriate error for `Bytes` that don't match expected command schema
- Encodes successful results to `Bytes` correctly
- Encodes error results to `Bytes` correctly

**Shutdown:**

- Shuts down cleanly
- Rejects further execution after shutdown

### 5.5 Integration Tests — Command Execution

Test end-to-end command execution through a deployed service:

**Happy Path:**

- Deploy a service, execute a command, receive correct result
- Execute multiple different commands through the same runtime
- Commands that produce events do so correctly

**Error Handling:**

- Command rejection is properly encoded in response `Bytes`
- Adapter errors are surfaced appropriately
- Timeout behavior works as configured
- Commands against a shutdown runtime fail appropriately

### 5.6 Integration Tests — Service Lifecycle

Test the full lifecycle of a service:

**Deployment:**

- Successful deployment initializes all adapters
- Failed adapter initialization fails deployment with clear error

**Runtime:**

- Service handles concurrent command execution
- Service maintains correct state across multiple commands

**Shutdown:**

- Shutdown terminates all adapters
- Shutdown is idempotent (can be called multiple times)
- Post-shutdown execution attempts fail gracefully

### 5.7 Test Fixtures

Create reusable test fixtures:

- Simple test commands with `TransportProtocols = '["Direct"]`
- A test command that always rejects (for testing rejection flow)
- A slow test command (for testing timeout behavior)
- A test command with complex payload (for testing serialization)
- Pre-configured test service definitions

---

## 6. Implementation Checklist

### Phase 1: Type-Level Foundation

Define the type families that power protocol tracking:

- `Member` for checking list membership
- `Union` for combining protocol lists
- `Difference` for computing missing protocols
- `ValidateProtocols` for producing compile errors

Create the compile-fail test files and verify they behave as expected.

### Phase 2: Core Types

Define the fundamental types:

- `ServiceDefinition` with its type parameters
- `ServiceRuntime` with `Bytes`-based interface
- `ServiceError` for error representation
- `TransportProtocol` type class
- `ServiceAdapter` type class

### Phase 3: DSL Implementation

Following TDD, implement the DSL functions:

- `service` for creating empty definitions
- `expose` for registering adapters
- `command` for registering commands
- Monadic interface enabling `do`-notation

Verify that the compile-success test files compile correctly.

### Phase 4: DirectAdapter

Following TDD, implement the DirectAdapter:

- Configuration types
- `TransportProtocol "Direct"` instance
- `ServiceAdapter DirectAdapter` instance
- Aeson-based encoding/decoding of `Bytes`
- Integration with existing `CommandHandler`

### Phase 5: Deployment and Runtime

Following TDD, implement deployment and execution:

- `deploy` function with adapter initialization
- Command routing based on protocol matching
- `Bytes`-in, `Bytes`-out execution flow
- Graceful shutdown

### Phase 6: Integration and Polish

- Run complete test suite
- Verify all compile-fail tests produce expected errors
- Verify all compile-success tests compile
- Add edge case coverage as needed

---

## 7. File Structure

```text
core/service/
├── Service/
│   ├── Definition.hs              -- ServiceDefinition type and DSL
│   ├── Definition/
│   │   ├── TypeLevel.hs           -- Type families (Member, Union, Difference)
│   │   └── Validation.hs          -- ValidateProtocols and error formatting
│   ├── Runtime.hs                 -- ServiceRuntime type and execution
│   ├── Adapter.hs                 -- ServiceAdapter class
│   ├── Adapter/
│   │   └── Direct.hs              -- DirectAdapter implementation
│   ├── Protocol.hs                -- TransportProtocol class
│   └── Error.hs                   -- ServiceError types

test/
├── compile-fail/                  -- Files that must NOT compile
├── compile-success/               -- Files that MUST compile
└── Test/Service/                  -- Runtime tests
```

---

## 8. Open Design Decisions

| Decision                    | Notes                                                                                             |
| --------------------------- | ------------------------------------------------------------------------------------------------- |
| Monadic interface approach  | Evaluate standard Monad vs IxMonad vs RebindableSyntax based on ergonomics                        |
| Adapter runtime storage     | Determine how to store heterogeneous adapter runtimes for command routing                         |
| Protocol selection strategy | When a command supports multiple protocols and multiple adapters exist, define selection priority |

---

## 9. Implementation Progress

**Last Updated:** December 5, 2024, 3:45 PM

### 9.1 Completed Components ✅

#### Type-Level Foundation (Phase 1) - 100% Complete
- ✅ `Member` type family for checking list membership
- ✅ `Union` type family for combining protocol lists
- ✅ `Difference` type family for computing missing protocols
- ✅ `ValidateProtocols` constraint with clear error messages
- **Location:** [Service/Definition/TypeLevel.hs](core/service/Service/Definition/TypeLevel.hs), [Service/Definition/Validation.hs](core/service/Service/Definition/Validation.hs)
- **Tests:** Comprehensive type-level tests in TransportProtocolSpec.hs

#### Core Types (Phase 2) - 90% Complete
- ✅ `ServiceDefinition` with 5 type parameters (commands, requiredProtocols, providedProtocols, adapters, value)
- ✅ `ServiceRuntime` type definition with execute/shutdown interface
- ✅ `ServiceError` basic type
- ✅ `TransportProtocol` type class
- ✅ `ServiceAdapter` type class
- **Location:** [Service/ServiceDefinition/Core.hs](core/service/Service/ServiceDefinition/Core.hs), [Service/Runtime.hs](core/service/Service/Runtime.hs)

#### DSL Implementation (Phase 3) - 90% Complete
- ✅ `service` function for empty definitions
- ✅ `expose` function for registering adapters
- ✅ `command` function for registering commands
- ✅ Monadic interface with QualifiedDo support (avoids RebindableSyntax issues)
- ⚠️ `deploy` function exists but returns placeholder
- **Location:** [Service/ServiceDefinition/Core.hs](core/service/Service/ServiceDefinition/Core.hs)
- **Tests:** DSL composition tests working

#### DirectAdapter (Phase 4) - 50% Complete
- ✅ DirectConfig type with configuration
- ✅ `TransportProtocol "Direct"` instance
- ✅ `ServiceAdapter DirectAdapter` instance
- ✅ Basic initialization and shutdown
- ❌ Aeson-based encoding/decoding of Bytes (stub implementation)
- ❌ Integration with CommandHandler (TODO)
- **Location:** [Service/Adapter/Direct.hs](core/service/Service/Adapter/Direct.hs)

#### Testing Infrastructure - 100% Complete
- ✅ NeoHaskell wrappers for should-not-typecheck
- ✅ Compile-time validation tests (shouldNotTypecheck)
- ✅ Type-level operation tests
- ✅ DSL composition tests
- **Location:** [Test/CompileTime.hs](core/testlib/Test/CompileTime.hs), [Service/TransportProtocolSpec.hs](core/test/Service/TransportProtocolSpec.hs)

### 9.2 Pending Components ❌

#### Deployment and Runtime (Phase 5) - 10% Complete
- ❌ `deploy` function actual implementation (currently returns placeholder)
- ❌ Adapter initialization during deployment
- ❌ Command routing table construction
- ❌ Bytes-in, Bytes-out execution flow
- ❌ Graceful shutdown implementation

#### Integration Components - 0% Complete
- ❌ CommandHandler integration
- ❌ EventStore integration
- ❌ EntityFetcher integration
- ❌ Command deserialization from Bytes
- ❌ Result serialization to Bytes

#### Additional Adapters - 0% Complete
- ❌ REST adapter implementation
- ❌ GraphQL adapter implementation
- ❌ gRPC adapter implementation

### 9.3 Key Technical Achievements

1. **Type-safe compile-time validation** - Missing protocols produce clear, actionable error messages at compile time
2. **Clean DSL syntax** - QualifiedDo enables `Service.do` notation without RebindableSyntax complications
3. **Comprehensive test coverage** - Type-level operations thoroughly tested including compile-fail scenarios
4. **Modular architecture** - Clear separation between compile-time (ServiceDefinition) and runtime (ServiceRuntime)

### 9.4 Critical Next Steps

1. **Implement `deploy` function** - Initialize adapters and build command routing
2. **Complete DirectAdapter.executeCommand** - Implement Bytes → Command → Decision → Events → Bytes pipeline
3. **Create ServiceRuntime implementation** - Command routing and execution orchestration
4. **Integrate CommandHandler** - Connect to existing event sourcing infrastructure
5. **Add integration tests** - End-to-end command execution validation

### 9.5 Usage Example (Current State)

```haskell
{-# LANGUAGE QualifiedDo #-}

-- Service definition compiles and validates protocols
cartService :: ServiceDefinition cmds req prov adp Unit
cartService = Service.do
  Service.service
  Service.expose (DirectAdapter defaultConfig)
  Service.command @CreateCartCommand

-- Deploy currently returns placeholder but validates at compile-time
runtime <- Service.deploy cartService  -- Compile error if protocols missing!
```

## 10. Glossary

| Term                   | Definition                                                                                   |
| ---------------------- | -------------------------------------------------------------------------------------------- |
| **Transport Protocol** | A method of exposing commands externally (REST, GraphQL, gRPC, Direct)                       |
| **Adapter**            | Runtime component that handles command execution for a specific protocol                     |
| **Required Protocols** | Union of all protocols declared by registered commands                                       |
| **Provided Protocols** | Protocols for which adapters have been registered via `expose`                               |
| **DirectAdapter**      | Adapter that executes commands in-process, using Aeson for `Bytes` serialization             |
| **Deploy**             | Convert a `ServiceDefinition` into a runnable `ServiceRuntime`                               |
| **Bytes**              | NeoHaskell wrapper over `ByteString`, the uniform data format for command execution          |
| **expose**             | DSL keyword to register an adapter, declaring the service exposes commands via that protocol |
