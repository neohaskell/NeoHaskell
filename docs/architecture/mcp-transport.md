# Architecture: MCP STDIO Transport

**ADR**: [0052-mcp-stdio-transport.md](../decisions/0052-mcp-stdio-transport.md)
**Issue**: [#598](https://github.com/neohaskell/NeoHaskell/issues/598)

## Module Map

```text
core/service/Service/Transport/
  Mcp.hs                       -- Public API: McpTransport type, Transport instance
  Mcp/
    JsonRpc.hs                 -- JSON-RPC 2.0 message types, parsing, serialization
    Protocol.hs                -- MCP method handlers (initialize, tools/*, resources/*)
    Response.hs                -- CommandResponse → MCP CallToolResult mapping

core/test/Service/Transport/
  McpSpec.hs                   -- Transport instance tests
  Mcp/
    JsonRpcSpec.hs             -- JSON-RPC parsing/serialization tests
    ProtocolSpec.hs            -- MCP protocol handler tests
    ResponseSpec.hs            -- Response mapping tests
```

## Cabal Changes

In `nhcore.cabal`, add to the `exposed-modules` list (after `Service.Transport.Cli.Output`):

```
    Service.Transport.Mcp
    Service.Transport.Mcp.JsonRpc
    Service.Transport.Mcp.Protocol
    Service.Transport.Mcp.Response
```

In the `nhcore-test-service` test suite `other-modules` (after `Service.Transport.InternalSpec`):

```
    Service.Transport.McpSpec
    Service.Transport.Mcp.JsonRpcSpec
    Service.Transport.Mcp.ProtocolSpec
    Service.Transport.Mcp.ResponseSpec
```

## Type Definitions

### Service.Transport.Mcp

```haskell
module Service.Transport.Mcp (
  McpTransport (..),
) where

import Core
import Json qualified
import Map (Map)
import Map qualified
import Record qualified
import Service.Auth (RequestContext)
import Service.Auth qualified as Auth
import Service.CommandExecutor.TH (deriveKnownHash)
import Service.Response (CommandResponse)
import Service.Transport (EndpointHandler, EndpointSchema (..), Endpoints (..), QueryEndpointHandler, Transport (..))
import Service.Transport.Mcp.JsonRpc qualified as JsonRpc
import Service.Transport.Mcp.Protocol qualified as Protocol
import Task qualified

data McpTransport = McpTransport
  { serverName :: Text
  , serverVersion :: Text
  }

type instance NameOf McpTransport = "McpTransport"

deriveKnownHash "McpTransport"

instance Transport McpTransport where
  type Request McpTransport = Bytes
  type Response McpTransport = Bytes
  type RunnableTransport McpTransport = Task Text Unit

  buildHandler ::
    forall command name.
    ( Command command,
      Json.FromJSON command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    McpTransport ->
    Record.Proxy command ->
    (RequestContext -> command -> Task Text CommandResponse) ->
    EndpointHandler
  -- Deserializes JSON bytes into command, calls handler, serializes response.
  -- Identical pattern to CliTransport.buildHandler.

  assembleTransport ::
    Endpoints McpTransport ->
    Task Text Unit
  -- Builds the STDIO event loop:
  -- 1. Pre-compute cached tools/list and resources/list responses
  -- 2. Create Protocol.ServerState with endpoints and cached responses
  -- 3. Return a Task that runs the STDIO read-dispatch-write loop

  runTransport :: McpTransport -> Task Text Unit -> Task Text Unit
  -- Identity: runTransport _transport task = task
  -- (Same pattern as CliTransport)
```

### Service.Transport.Mcp.JsonRpc

```haskell
module Service.Transport.Mcp.JsonRpc (
  -- * Request types
  JsonRpcRequest (..),
  parseRequest,
  -- * Response types
  JsonRpcResponse,
  successResponse,
  errorResponse,
  notificationResponse,
  -- * Error types
  JsonRpcError (..),
  -- * Standard error codes
  parseError,
  invalidRequest,
  methodNotFound,
  invalidParams,
  internalError,
  -- * Serialization
  encodeResponse,
) where

import Basics
import Bytes (Bytes)
import Json qualified
import Maybe (Maybe (..))
import Text (Text)

-- | A parsed JSON-RPC 2.0 request.
data JsonRpcRequest = JsonRpcRequest
  { method :: Text
  , params :: Maybe Json.Value
  , id :: Maybe Json.Value
    -- ^ Nothing for notifications (no response expected)
  }
  deriving (Show, Eq, Generic)

-- | Parse a raw Bytes line into a JsonRpcRequest.
-- Returns Left with a JsonRpcResponse (parse error) if the input is malformed.
parseRequest :: Bytes -> Result JsonRpcResponse JsonRpcRequest

-- | A JSON-RPC 2.0 response (opaque, constructed via smart constructors).
-- Serialized with toEncoding for performance.
data JsonRpcResponse
  deriving (Show, Eq, Generic)

-- | Construct a success response with a result value.
successResponse :: Maybe Json.Value -> Json.Value -> JsonRpcResponse
-- ^ Takes request id and result value.

-- | Construct an error response.
errorResponse :: Maybe Json.Value -> JsonRpcError -> JsonRpcResponse
-- ^ Takes request id and error.

-- | Construct a notification acknowledgment (no response sent).
notificationResponse :: JsonRpcResponse

-- | A JSON-RPC 2.0 error object.
data JsonRpcError = JsonRpcError
  { code :: {-# UNPACK #-} Int
  , message :: Text
  , errorData :: Maybe Json.Value
  }
  deriving (Show, Eq, Generic)

-- Standard error constructors:

-- | -32700: Malformed JSON
parseError :: Text -> JsonRpcError

-- | -32600: Invalid request (missing jsonrpc field, pre-init)
invalidRequest :: Text -> JsonRpcError

-- | -32601: Unknown method
methodNotFound :: Text -> JsonRpcError

-- | -32602: Invalid params (unknown tool/resource)
invalidParams :: Text -> JsonRpcError

-- | -32603: Internal server error (generic message, never leak details)
internalError :: JsonRpcError

-- | Serialize a JsonRpcResponse to Bytes for writing to stdout.
-- Uses toEncoding (no intermediate Value).
encodeResponse :: JsonRpcResponse -> Bytes
```

**Implementation notes:**
- `JsonRpcResponse` must implement `Json.ToJSON` with `toEncoding` using `Json.pairs`/`Json.pair` for zero-allocation serialization.
- `parseRequest` validates the `"jsonrpc": "2.0"` field and extracts `method`, `params`, `id`.
- Notifications have no `id` field — `parseRequest` sets `id = Nothing`.
- `encodeResponse` appends a newline (`\n`) after the JSON for line-delimited output.

### Service.Transport.Mcp.Protocol

```haskell
module Service.Transport.Mcp.Protocol (
  -- * Server state
  ServerState (..),
  newServerState,
  -- * Request processing
  handleRequest,
  -- * STDIO loop
  runStdioLoop,
) where

import Basics
import Bytes (Bytes)
import ConcurrentVar (ConcurrentVar)
import Json qualified
import Map (Map)
import Service.Transport (EndpointHandler, EndpointSchema, QueryEndpointHandler)
import Service.Transport.Mcp.JsonRpc (JsonRpcRequest, JsonRpcResponse)
import Task (Task)
import Text (Text)

-- | Maximum allowed message size in bytes (1 MB).
maxMessageSize :: Int
maxMessageSize = 1_048_576

-- | Mutable server state tracking initialization and cached responses.
data ServerState = ServerState
  { initialized :: ConcurrentVar Bool
  , serverName :: Text
  , serverVersion :: Text
  , commandEndpoints :: Map Text EndpointHandler
  , queryEndpoints :: Map Text QueryEndpointHandler
  , cachedToolsList :: Bytes
    -- ^ Pre-serialized tools/list response (computed once at assembly time)
  , cachedResourcesList :: Bytes
    -- ^ Pre-serialized resources/list response (computed once at assembly time)
  }

-- | Create initial server state from endpoints and transport config.
newServerState ::
  Text ->
  -- ^ serverName
  Text ->
  -- ^ serverVersion
  Map Text EndpointHandler ->
  Map Text QueryEndpointHandler ->
  Map Text EndpointSchema ->
  -- ^ commandSchemas (for tools/list)
  Map Text EndpointSchema ->
  -- ^ querySchemas (for resources/list)
  Task Text ServerState

-- | Process a single JSON-RPC request and produce a response.
-- Returns Nothing for notifications (no response expected).
handleRequest :: ServerState -> JsonRpcRequest -> Task Text (Maybe JsonRpcResponse)

-- | Run the STDIO event loop.
-- Reads lines from stdin, dispatches via handleRequest, writes responses to stdout.
-- Exits when stdin is closed (EOF).
-- All diagnostic output goes to stderr.
runStdioLoop :: ServerState -> Task Text Unit
```

**Implementation notes:**
- `newServerState` pre-computes `cachedToolsList` and `cachedResourcesList` by converting `EndpointSchema` maps to MCP tool/resource JSON using `Schema.JsonSchema.toJsonSchema` for `inputSchema` conversion.
- `handleRequest` dispatches by `request.method`:
  - `"initialize"` → return capabilities JSON, store init state
  - `"notifications/initialized"` → set `initialized` to `True`, return `Nothing`
  - `"ping"` → return `successResponse id (Json.object [])`
  - `"notifications/cancelled"` → return `Nothing`
  - `"tools/list"` → return pre-cached tools list
  - `"tools/call"` → extract tool name from params, look up in `commandEndpoints`, create `Auth.anonymousContext`, encode arguments to Bytes, call handler, map response via `Response.toCallToolResult`
  - `"resources/list"` → return pre-cached resources list
  - `"resources/read"` → extract URI from params, parse query name, look up in `queryEndpoints`, call handler, wrap as resource content
  - `"prompts/list"` → return empty list
  - `"prompts/get"` → return error (not found)
  - Unknown → return `methodNotFound` error
- Pre-init guard: if `initialized` is `False` and method is not `"initialize"`, return `invalidRequest`.
- `runStdioLoop` uses `Data.ByteString.hGetLine System.IO.stdin` for raw byte reading (no Text decode), checks `maxMessageSize`, and writes responses via `Data.ByteString.hPut System.IO.stdout` followed by a newline and `System.IO.hFlush System.IO.stdout`.
- All `Log` output and unhandled errors go to `System.IO.stderr`.

### Service.Transport.Mcp.Response

```haskell
module Service.Transport.Mcp.Response (
  toCallToolResult,
  toResourceContent,
) where

import Basics
import Json qualified
import Service.Response (CommandResponse (..))
import Text (Text)

-- | Convert a CommandResponse to an MCP CallToolResult JSON value.
--
-- Accepted: two content blocks — entity ID confirmation + assistant-audience CQRS guidance.
-- Rejected: single content block with isError = true.
-- Failed: single content block with isError = true, generic message (no internal details).
toCallToolResult :: CommandResponse -> Json.Value

-- | Wrap query result text as an MCP resource content JSON value.
-- Takes the resource URI and the JSON text returned by the query handler.
toResourceContent :: Text -> Text -> Json.Value
-- ^ resourceUri -> queryResultJson -> MCP resources/read response
```

**Implementation notes:**
- `toCallToolResult` for `Accepted` includes two `text` content blocks:
  1. `"Command accepted. Entity ID: {entityId}"` with `annotations: { audience: ["user", "assistant"] }`
  2. CQRS guidance with `annotations: { audience: ["assistant"], priority: 0.9 }`
- `toCallToolResult` for `Rejected` uses `isError: true` with `"Command rejected: {reason}"`.
- `toCallToolResult` for `Failed` uses `isError: true` with `"Command failed: Internal error"` (never expose the raw error string to prevent information leakage — per security review).
- `toResourceContent` wraps the query JSON as `{ contents: [{ uri, mimeType: "application/json", text }] }`.
- All JSON construction uses `Json.object` and `Json..=` (Aeson builders, no string interpolation for JSON values).

## Integration Points

### Existing nhcore utilities to use

| Utility | Module | Purpose |
|---------|--------|---------|
| `Json.decodeBytes` | `Json` | Parse request JSON from Bytes |
| `Json.object`, `Json..=` | `Json` | Construct response JSON values |
| `Json.encodeText` | `Json` | Serialize JSON to Text |
| `Schema.JsonSchema.toJsonSchema` | `Schema.JsonSchema` | Convert `Schema` to JSON Schema for MCP `inputSchema` |
| `Auth.anonymousContext` | `Service.Auth` | Create RequestContext (no auth for MCP) |
| `ConcurrentVar.new`, `.get`, `.set` | `ConcurrentVar` | Track initialization state |
| `Task.fromIO` | `Task` | Wrap IO operations (stdin/stdout) |
| `Task.yield` | `Task` | Return pure values in Task |
| `Task.mapError` | `Task` | Transform error types |
| `Map.get`, `Map.entries` | `Map` | Endpoint lookups and iteration |
| `Text.toKebabCase` | `Text` | Convert query names for resource URIs |
| `Text.toBytes`, `Text.fromBytes` | `Text` | Bytes ↔ Text conversion |
| `deriveKnownHash` | `Service.CommandExecutor.TH` | Generate KnownHash instance for McpTransport |

### GHC base modules needed (qualified)

| Module | Purpose |
|--------|---------|
| `System.IO qualified` | `stdin`, `stdout`, `stderr` handles, `hSetBuffering`, `hFlush`, `hIsEOF` |
| `Data.ByteString qualified` | `hGetLine`, `hPut` for raw byte I/O |
| `Data.ByteString.Lazy qualified` | `toStrict` for Aeson encoding output |

### How it plugs into the runtime

`McpTransport` uses the **generic transport path** in `Service.Application.Transports.runGenericTransport`. No changes needed to `Transports.hs` — when `transportName` is `"McpTransport"`, it falls through to `runGenericTransport` which:
1. Creates `Endpoints McpTransport` with the transport instance, command endpoints, query endpoints, and schemas.
2. Calls `assembleTransport` → returns `Task Text Unit` (the STDIO loop).
3. Calls `runTransport` → identity, just runs the task.

### NameOf instance

`McpTransport` needs `type instance NameOf McpTransport = "McpTransport"` and `deriveKnownHash "McpTransport"` — these are required for the transport to register in `ServiceDefinition.Core.buildEndpointsByTransport`.

## Protocol Constants

```haskell
-- MCP protocol version
mcpProtocolVersion :: Text
mcpProtocolVersion = "2025-03-26"

-- Resource URI scheme
resourceUriPrefix :: Text
resourceUriPrefix = "neohaskell://queries/"
```

## Data Flow

```
AI Client (stdin) → [raw bytes line]
  → JsonRpc.parseRequest → JsonRpcRequest
  → Protocol.handleRequest → dispatches by method:
      tools/call → EndpointHandler → CommandResponse
        → Response.toCallToolResult → Json.Value
        → JsonRpc.successResponse → JsonRpcResponse
      resources/read → QueryEndpointHandler → Text (JSON)
        → Response.toResourceContent → Json.Value
        → JsonRpc.successResponse → JsonRpcResponse
  → JsonRpc.encodeResponse → [raw bytes + newline]
→ AI Client (stdout)
```
