# ADR-0002: WebAPI Adapter Architecture

## Status

Accepted

## Context

The NeoHaskell project implements an Event Sourcing / CQRS architecture where commands are the primary entry point for mutations. To expose these commands over HTTP, we need an adapter layer that:

1. Receives HTTP requests and routes them to the appropriate command handlers
2. Deserializes JSON payloads into typed command values
3. Executes commands through the decision and event persistence pipeline
4. Returns appropriate HTTP responses based on command outcomes
5. Protects against denial-of-service attacks via large request payloads

This ADR documents the architecture of the WebAPI adapter and its integration with the command execution infrastructure.

## Decision

### 1. The ApiBuilder Abstraction

We define a transport-agnostic `ApiBuilder` typeclass in `Service.Api.ApiBuilder` that allows different transport mechanisms (HTTP, CLI, WebSocket, etc.) to expose commands uniformly:

```haskell
class ApiBuilder api where
  type Request api
  type Response api
  type RunnableApi api

  assembleApi :: ApiEndpoints api -> RunnableApi api
  runApi :: api -> RunnableApi api -> Task Text Unit
  buildCommandHandler :: ... -> ApiEndpointHandler
```

Key types:
- `ApiEndpointHandler`: A function `Bytes -> (Bytes -> Task Text Unit) -> Task Text Unit` that receives raw request bytes and a response callback
- `ApiEndpoints api`: A record containing the API configuration and a `Map Text ApiEndpointHandler` mapping command names to their handlers

This abstraction separates:
- **Transport concerns** (HTTP routing, serialization format) in the `ApiBuilder` instance
- **Business logic** (command execution, event persistence) in the `CommandHandler`

### 2. WebAPI Implementation

The `WebApi` type (`Service.Api.WebApi`) implements `ApiBuilder` for HTTP transport using the Warp web server:

```haskell
data WebApi = WebApi
  { port :: Int,
    maxBodySize :: Int
  }

server :: WebApi
server = WebApi
  { port = 8080,
    maxBodySize = 1048576  -- 1MB default
  }
```

The implementation provides:

#### 2.1 Associated Types

```haskell
type Request WebApi = Wai.Request
type Response WebApi = Wai.Response
type RunnableApi WebApi = Wai.Request -> (Wai.Response -> Task Text Wai.ResponseReceived) -> Task Text Wai.ResponseReceived
```

The `RunnableApi` type is a Task-based WAI application that gets converted to a standard WAI `Application` for Warp.

#### 2.2 URL Routing

Commands are exposed at `/commands/<CommandName>` endpoints. The routing logic in `assembleApi`:

1. Parses the request path to extract `["commands", commandName]`
2. Looks up the command handler in the `commandEndpoints` map
3. Returns 404 if the command is not found

#### 2.3 Request Body Handling with DoS Protection

The `readBodyWithLimit` function reads request bodies with a configurable size limit:

```haskell
readBodyWithLimit :: Int -> Wai.Request -> Task Text (Result Text Bytes)
```

Key behaviors:
- Reads body chunks incrementally, tracking total size
- Returns `Result.Err` if the body exceeds `maxBodySize`
- Drains remaining body on rejection to avoid connection issues
- Default limit is 1MB (1,048,576 bytes)

This prevents attackers from exhausting server memory with large payloads.

#### 2.4 Response Status Mapping

HTTP status codes are derived from the `CommandResponse` type:

| CommandResponse | HTTP Status |
|-----------------|-------------|
| `Accepted`      | 200 OK      |
| `Rejected`      | 400 Bad Request |
| `Failed`        | 500 Internal Server Error |

### 3. Command Handler Building

The `buildCommandHandler` method constructs an `ApiEndpointHandler` for a specific command type:

```haskell
buildCommandHandler ::
  forall command name.
  ( Command command,
    Json.FromJSON command,
    name ~ NameOf command,
    Record.KnownSymbol name
  ) =>
  WebApi ->
  Record.Proxy command ->
  (command -> Task Text CommandResponse) ->
  ApiEndpointHandler
```

This function:
1. Deserializes the request body as JSON into the typed `command` value
2. On parse failure, returns a `Failed` response with error details
3. On success, invokes the command handler and serializes the response

### 4. Service Definition Integration

The `Service.ServiceDefinition.Core` module wires commands to APIs through type-level tracking:

```haskell
service :: Service _ _ _ _
service =
  Service.new
    |> Service.command @AddItem
    |> Service.command @CreateCart
    |> Service.useServer WebApi.server
    |> Service.useEventStore postgresConfig
```

When `Service.command @SomeCommand` is called:
1. The `ApiOf SomeCommand` type family determines which API serves this command
2. The command is registered in a type-level row with its API name
3. At startup, commands are grouped by API and endpoint handlers are built

The `runService` function:
1. Creates a single `EventStore` instance shared across all commands
2. Groups commands by their API (via `ApiOf` type family)
3. Builds endpoint handlers for each command using `buildCmdEP`
4. Assembles and runs each API with its command endpoints

### 5. Request/Response Flow

Complete flow for an HTTP command request:

```
HTTP POST /commands/CreateCart
    |
    v
Warp Server (runApi)
    |
    v
assembleApi (WebApi instance)
    |-- Parse path: ["commands", "CreateCart"]
    |-- Read body with size limit
    |-- Look up handler in commandEndpoints Map
    v
ApiEndpointHandler (buildCmdEP)
    |-- buildCommandHandler deserializes JSON
    |-- Create EntityFetcher for entity reconstruction
    v
CommandHandler.execute
    |-- Extract entity ID from command
    |-- Fetch current entity state (if exists)
    |-- Run Decision monad (decide function)
    |-- Handle Accept/Reject outcomes
    v
EventStore.insert (on Accept)
    |-- Persist events with consistency checks
    |-- Retry with exponential backoff on conflicts
    v
CommandResponse
    |-- Convert CommandHandlerResult to response
    |-- Map to HTTP status code
    v
HTTP Response (JSON)
```

### 6. Command Definition Requirements

For a command to be exposed via WebAPI, it must:

1. Define `type instance EntityOf MyCommand = MyEntity`
2. Define `type instance ApiOf MyCommand = WebApi`
3. Implement `Json.FromJSON` for deserialization
4. Call the `command ''MyCommand` Template Haskell splice
5. Provide `getEntityId` and `decide` functions

Example:

```haskell
data CreateCart = CreateCart
  deriving (Generic, Typeable, Show)

instance Json.FromJSON CreateCart

type instance EntityOf CreateCart = CartEntity
type instance ApiOf CreateCart = WebApi

getEntityId :: CreateCart -> Maybe Uuid
getEntityId _ = Nothing

decide :: CreateCart -> Maybe CartEntity -> Decision CartEvent
decide _ entity = case entity of
  Just _ -> Decision.reject "Cart already exists!"
  Nothing -> do
    id <- Decision.generateUuid
    Decision.acceptNew [CartCreated {entityId = id}]

command ''CreateCart
```

## Consequences

### Positive

1. **Transport agnosticism**: The `ApiBuilder` abstraction allows adding new transports (CLI, WebSocket) without changing command logic
2. **Type-safe routing**: The `ApiOf` type family ensures commands are registered with the correct API at compile time
3. **DoS protection**: Built-in request body size limiting prevents memory exhaustion attacks
4. **Clean separation**: Transport concerns (HTTP status codes, JSON parsing) are isolated from business logic
5. **Configurable defaults**: `WebApi.server` provides sensible defaults that can be overridden

### Negative

1. **Fixed URL structure**: Commands are always at `/commands/<name>` with no customization
2. **JSON only**: No support for other serialization formats (Protobuf, MessagePack)
3. **POST only**: All commands use POST; no support for other HTTP methods
4. **No middleware**: No built-in support for authentication, rate limiting, or logging middleware

### Trade-offs

1. **Simplicity over flexibility**: The fixed `/commands/<name>` routing prioritizes convention over configuration
2. **Type coercion**: `unsafeCoerce` is used in `ServiceDefinition` to bridge type-level and value-level API handling; this is sound but requires careful maintenance

### Future Considerations

1. Add middleware support for cross-cutting concerns (auth, logging, metrics)
2. Consider REST-style resource routing as an alternative adapter
3. Add support for query endpoints alongside command endpoints
4. Consider WebSocket adapter for real-time event subscriptions
5. Add OpenAPI specification generation from registered commands

## References

- `/Users/nick/Source/NeoHaskell/core/service/Service/Api/WebApi.hs` - WebAPI implementation
- `/Users/nick/Source/NeoHaskell/core/service/Service/Api/ApiBuilder.hs` - ApiBuilder typeclass
- `/Users/nick/Source/NeoHaskell/core/service/Service/ServiceDefinition/Core.hs` - Service builder
- `/Users/nick/Source/NeoHaskell/core/service/Service/CommandHandler/Core.hs` - Command execution
- `/Users/nick/Source/NeoHaskell/core/service/Service/CommandResponse.hs` - Response types
- `/Users/nick/Source/NeoHaskell/testbed/src/Testbed/Cart/Commands/CreateCart.hs` - Example command
- `/Users/nick/Source/NeoHaskell/docs/decisions/0001-initial-architecture-baseline.md` - Baseline ADR
