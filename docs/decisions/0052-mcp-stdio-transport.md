# ADR-0052: McpTransport — MCP STDIO Transport for AI Assistant Integration

## Status

Proposed

## Context

### Current State

NeoHaskell services can be exposed over HTTP (`WebTransport`, ADR-0002/0014) and CLI (`CliTransport`, ADR-0034). The `Transport` typeclass (`Service/Transport.hs`) supports pluggable transport adapters, and the `EndpointSchema` type captures request schemas, response schemas, descriptions, and grouping metadata at registration time. Today, `WebTransport` converts these into OpenAPI paths and `CliTransport` converts them into CLI subcommands.

AI assistants (Claude Desktop, VS Code Copilot, Cursor) use the [Model Context Protocol](https://modelcontextprotocol.io) (MCP) to discover and invoke external tools and data. MCP is JSON-RPC 2.0 over STDIO — the AI client spawns the MCP server as a subprocess, sends requests on stdin, and reads responses from stdout. The protocol provides a natural mapping to NeoHaskell's existing architecture:

| MCP Concept | What It Does | NeoHaskell Equivalent |
|---|---|---|
| **Tools** | Functions the AI can call | **Commands** |
| **Resources** | Read-only data the AI can browse | **Queries** |
| **Prompts** | Reusable message templates | **Queries** (user-designated) |

The `EndpointSchema` data that powers OpenAPI generation can also power MCP capability discovery, and the same `EndpointHandler` / `QueryEndpointHandler` abstractions can dispatch MCP requests.

### Use Cases

- A NeoHaskell service is exposed as an MCP server so Claude Desktop can call its commands and read its query results with zero changes to service definitions.
- A developer building a local-first tool adds `McpTransport` alongside `CliTransport` so the same service is usable from both the terminal and AI assistants.
- An existing service that already has `WebTransport` adds `McpTransport` to let AI agents interact with it locally during development.

### Design Goals

1. **Minimal ceremony**: Adding MCP support should be one call — `Application.withTransport McpTransport { serverName = "...", serverVersion = "..." }` — with no changes to service definitions, commands, or queries.
2. **Reuse existing infrastructure**: Use the same `EndpointSchema`, `EndpointHandler`, and `QueryEndpointHandler` that power `WebTransport` and `CliTransport`.
3. **Correct CQRS signaling**: MCP tool responses should teach the AI the CQRS pattern — commands confirm and return entity IDs, queries retrieve state.
4. **Spec compliance**: Implement the MCP protocol handshake, capability negotiation, and error model faithfully.

### GitHub Issue

- [#598: feat(service): Add MCP (Model Context Protocol) STDIO Transport](https://github.com/neohaskell/NeoHaskell/issues/598)

## Decision

### 1. McpTransport Data Type

```haskell
data McpTransport = McpTransport
  { serverName :: Text
  , serverVersion :: Text
  }
```

The type is minimal — MCP STDIO servers have no network configuration, no auth, and no CORS. The `serverName` and `serverVersion` are returned during the MCP handshake.

There is no default constructor. Jess must provide both `serverName` and `serverVersion` explicitly — these identify the service to AI clients and should always match the application's actual name and version.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Default constructor with placeholder values | Rejected | Placeholder names like `"neohaskell-mcp"` leak into AI client UIs and make services indistinguishable. Forcing explicit values ensures every MCP server is identifiable. |
| Single `McpTransport` for STDIO + HTTP | Rejected | STDIO and Streamable HTTP have fundamentally different I/O models; mixing them creates unnecessary complexity. Streamable HTTP is out of scope (issue #598). |
| `McpTransport` with configurable I/O handles | Rejected | Over-engineered for the STDIO-only scope. Testability is achieved by injecting handle pairs in tests, not by configuration. |
| `McpTransport` with required `serverName` and `serverVersion` | **Chosen** | Matches the minimal information required by the MCP `initialize` handshake. Explicit construction prevents anonymous services. Future fields (logging level, capability toggles) can be added without breaking changes. |

### 2. Transport Instance

```haskell
instance Transport McpTransport where
  type Request McpTransport = Bytes
  type Response McpTransport = Bytes
  type RunnableTransport McpTransport = Task Text Unit
```

This mirrors `CliTransport`'s type instance pattern. The `RunnableTransport` is a `Task` that runs the STDIO event loop.

### 3. Module Placement

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| `core/service/Service/Transport/Mcp.hs` (flat) | Rejected | The MCP transport has enough internal structure (JSON-RPC parsing, protocol handlers, response builders) to warrant a sub-module. |
| `core/service/Service/Transport/Mcp.hs` + `Mcp/` sub-modules | **Chosen** | Main module exports the public API (`McpTransport`, `stdio`). Sub-modules handle protocol internals. Matches `Transport/Cli.hs` + `Cli/Output.hs` pattern. |

```text
core/service/Service/Transport/
  Mcp.hs                    -- McpTransport type, Transport instance, stdio constructor
  Mcp/
    JsonRpc.hs              -- JSON-RPC 2.0 message types and parsing
    Protocol.hs             -- MCP protocol handlers (initialize, tools/*, resources/*, prompts/*)
    Response.hs             -- CommandResponse → MCP CallToolResult mapping
```

### 4. STDIO Event Loop

`runTransport` starts a loop that reads newline-delimited JSON-RPC 2.0 messages from stdin and writes responses to stdout.

```haskell
runTransport :: McpTransport -> Task Text Unit -> Task Text Unit
runTransport _transport task = task
```

The loop itself is assembled by `assembleTransport`:

1. Read one line from stdin (UTF-8, newline-delimited).
2. Parse as JSON-RPC 2.0 request.
3. Route by method name to the appropriate handler.
4. Serialize the response and write to stdout followed by a newline.
5. Repeat until stdin is closed (EOF).

All diagnostic output (logs, errors) goes to stderr, never stdout. Stdout is reserved exclusively for JSON-RPC responses.

### 5. MCP Protocol Handshake

The MCP protocol requires a 3-step initialization:

1. **Client sends `initialize`** with `protocolVersion` and `capabilities`.
2. **Server responds** with its `protocolVersion`, `capabilities`, and `serverInfo`.
3. **Client sends `notifications/initialized`** (no response expected).

Server capabilities declared:

```json
{
  "capabilities": {
    "tools": {},
    "resources": {},
    "prompts": {},
    "logging": {}
  },
  "protocolVersion": "2025-03-26",
  "serverInfo": {
    "name": "neohaskell-mcp",
    "version": "0.1.0"
  }
}
```

The transport tracks initialization state. Requests received before `initialize` completes (except `initialize` itself) return a JSON-RPC error with code `-32600` (invalid request).

### 6. Method Routing

| MCP Method | Handler | Source |
|---|---|---|
| `initialize` | Return capabilities and server info | `McpTransport` config |
| `notifications/initialized` | Mark session as initialized | Internal state |
| `ping` | Return empty result `{}` | Built-in |
| `notifications/cancelled` | No-op (acknowledge) | Built-in |
| `tools/list` | List all command schemas as MCP tools | `commandSchemas` |
| `tools/call` | Dispatch command via `EndpointHandler` | `commandEndpoints` |
| `resources/list` | List all query schemas as MCP resources | `querySchemas` |
| `resources/read` | Dispatch query via `QueryEndpointHandler` | `queryEndpoints` |
| `prompts/list` | Empty list (no prompt support in MVP) | Built-in |
| `prompts/get` | JSON-RPC error: prompt not found | Built-in |

Unknown methods return JSON-RPC error code `-32601` (method not found).

### 7. Command → Tool Mapping

`tools/list` converts each entry in `commandSchemas` to an MCP tool definition:

```haskell
commandSchemaToTool :: Text -> EndpointSchema -> Json.Value
```

| EndpointSchema field | MCP Tool field | Mapping |
|---|---|---|
| Map key (command name) | `name` | Direct |
| `description` | `description` | Direct |
| `requestSchema` | `inputSchema` | JSON Schema (already compatible) |

`tools/call` dispatches to the matching `EndpointHandler`:

1. Look up the tool name in `commandEndpoints`.
2. If not found, return JSON-RPC error `-32602` (invalid params) with message "Unknown tool: {name}".
3. Create an anonymous `RequestContext` (MCP STDIO has no auth).
4. Encode the `arguments` object as JSON `Bytes`.
5. Call the `EndpointHandler` with the context, bytes, and a callback.
6. Map the `CommandResponse` to a `CallToolResult`.

### 8. CommandResponse → CallToolResult Mapping

**Accepted command:**

```json
{
  "content": [
    {
      "type": "text",
      "text": "Command accepted. Entity ID: 550e8400-e29b-41d4-a716-446655440000",
      "annotations": { "audience": ["user", "assistant"] }
    },
    {
      "type": "text",
      "text": "This command registered an event but does not return entity state. To see the current state, read the appropriate resource (e.g. resources/read with URI neohaskell://queries/{query-name}?id={entity-id}).",
      "annotations": { "audience": ["assistant"], "priority": 0.9 }
    }
  ],
  "isError": false
}
```

The second content block uses `audience: ["assistant"]` to teach the AI the CQRS pattern: commands confirm, queries retrieve state.

**Rejected command:**

```json
{
  "content": [
    { "type": "text", "text": "Command rejected: {reason}" }
  ],
  "isError": true
}
```

**Failed command:**

```json
{
  "content": [
    { "type": "text", "text": "Command failed: {error}" }
  ],
  "isError": true
}
```

### 9. Query → Resource Mapping

`resources/list` converts each entry in `querySchemas` to an MCP resource definition:

| EndpointSchema field | MCP Resource field | Mapping |
|---|---|---|
| Map key (query name) | `uri` | `neohaskell://queries/{name}` |
| `description` | `description` | Direct |
| `responseSchema` | Not exposed | MCP resources don't declare response schemas |
| Map key (query name) | `name` | Kebab-cased query name |

`resources/read` dispatches to the matching `QueryEndpointHandler`:

1. Parse the resource URI to extract the query name from `neohaskell://queries/{name}`.
2. Parse optional query parameters from the URI (e.g., `?id=...`) as a NeoQL `Expr` filter.
3. Look up the query name in `queryEndpoints`.
4. If not found, return JSON-RPC error `-32602` with message "Unknown resource: {uri}".
5. Call the handler with `Nothing` (no user claims), the optional filter, and a default `QueryPageRequest`.
6. Return the result as a resource content block.

Response format:

```json
{
  "contents": [
    {
      "uri": "neohaskell://queries/cart-summary",
      "mimeType": "application/json",
      "text": "{\"items\": [...], \"total\": 42.50}"
    }
  ]
}
```

### 10. Error Model

MCP distinguishes two error tiers:

| Error Type | When | Response |
|---|---|---|
| **Tool execution error** | Command rejected or failed | Normal `result` with `isError: true` in `CallToolResult` |
| **Protocol error** | Unknown method, malformed params, pre-init request | JSON-RPC `error` object (no `result` field) |

Standard JSON-RPC error codes used:

| Code | Meaning | When |
|---|---|---|
| `-32700` | Parse error | Malformed JSON |
| `-32600` | Invalid request | Missing `jsonrpc` field, pre-init request |
| `-32601` | Method not found | Unknown MCP method |
| `-32602` | Invalid params | Unknown tool/resource name, malformed arguments |
| `-32603` | Internal error | Unexpected server failure |

### 11. Authentication Model

MCP STDIO servers run as local subprocesses of the AI client. There is no network boundary, no JWT, no OAuth2. `McpTransport` uses `Auth.anonymousContext` for all requests — identical to `CliTransport`.

### 12. Logging

The transport uses `notifications/message` to send log messages to the MCP client. Log levels map directly:

| NeoHaskell | MCP |
|---|---|
| `Log.debug` | `"debug"` |
| `Log.info` | `"info"` |
| `Log.warn` | `"warning"` |
| `Log.error` | `"error"` |

Log messages are written to stderr as well, for visibility when debugging without an MCP client.

### 13. JSON-RPC 2.0 Message Types

```haskell
data JsonRpcRequest = JsonRpcRequest
  { jsonrpc :: Text
  , method :: Text
  , params :: Maybe Json.Value
  , id :: Maybe Json.Value
  }

data JsonRpcResponse = JsonRpcResponse
  { jsonrpc :: Text
  , result :: Maybe Json.Value
  , error :: Maybe JsonRpcError
  , id :: Maybe Json.Value
  }

data JsonRpcError = JsonRpcError
  { code :: Int
  , message :: Text
  , errorData :: Maybe Json.Value
  }
```

Notifications (methods starting with `notifications/`) have no `id` field and receive no response.

### 14. Prompt Support

In the MVP, `prompts/list` returns an empty list and `prompts/get` returns a JSON-RPC error (prompt not found). Full prompt support (mapping designated queries to MCP prompts) is deferred to a follow-up.

### 15. Usage Example

```haskell
-- App.hs
app :: Application
app =
  Application.new
    |> Application.withEventStore simpleStore
    |> Application.withTransport WebTransport.server
    |> Application.withTransport McpTransport
        { serverName = "my-service"
        , serverVersion = "1.0.0"
        }
    |> Application.withService Cart.service
    |> Application.withQuery @CartSummary
    |> Application.run
```

Commands serve both transports:

```haskell
type instance TransportsOf AddItem = '[WebTransport, McpTransport]
```

The service is now usable from both HTTP clients and AI assistants. Claude Desktop configuration:

```json
{
  "mcpServers": {
    "my-service": {
      "command": "my-service-exe",
      "args": []
    }
  }
}
```

### 16. Dependencies

**New dependency:** `aeson` (already in use). No new external dependencies required.

The JSON-RPC message types and STDIO loop are implemented directly — no JSON-RPC library is needed. The protocol surface is small enough (request/response/notification) that a library dependency would add more weight than value.

### 17. Out of Scope

- **Streamable HTTP transport**: For remote/cloud MCP servers (future ADR).
- **MCP Sampling**: Server asking the client for LLM completions.
- **Resource subscriptions**: Change notifications when query results update.
- **Completions/autocomplete**: MCP autocomplete support for tool arguments.
- **Full prompt support**: Mapping user-designated queries to MCP prompts (follow-up).

## Consequences

### Positive

- Jess can expose any NeoHaskell service to AI assistants by adding `Application.withTransport McpTransport { serverName = "...", serverVersion = "..." }` with no changes to existing service definitions.
- The same `EndpointSchema` data that powers OpenAPI docs and CLI parsers also powers MCP tool/resource discovery — no duplication.
- The CQRS guidance in tool responses teaches AI assistants the command/query separation pattern, reducing hallucinated "return values" from commands.
- The `runGenericTransport` path in `Service.Application.Transports` handles `McpTransport` with no special-casing — it's just another transport.

### Negative

- The STDIO event loop occupies stdin/stdout, making `McpTransport` incompatible with `CliTransport` in the same process (both want stdin).
- No authentication means `McpTransport` is only suitable for local, trusted environments.
- MVP skips prompt support, which limits the richness of AI assistant interactions.

### Risks

- A bug in the STDIO loop (e.g., partial writes, buffering issues) could corrupt the JSON-RPC stream and break the AI client connection.
- The MCP spec is still evolving; breaking changes in future spec versions could require transport updates.
- Mixing `McpTransport` and `CliTransport` on the same command could cause confusion about which transport is active.

### Mitigations

- Line-buffered stdout and explicit newline delimiters prevent partial writes.
- Pin to `protocolVersion: "2025-03-26"` and version-gate any future spec changes.
- Document that `McpTransport` and `CliTransport` should not be combined in the same process; if both are needed, run separate executables.

## References

- [#598: feat(service): Add MCP (Model Context Protocol) STDIO Transport](https://github.com/neohaskell/NeoHaskell/issues/598)
- [Model Context Protocol Specification](https://modelcontextprotocol.io)
- [ADR-0002: WebAPI Adapter Architecture](0002-webapi-adapter-architecture.md) — Transport abstraction origin
- [ADR-0034: CliTransport — Command-Line Interface Transport](0034-cli-transport.md) — CLI transport pattern
- [ADR-0050: Internal Command Transport](0050-internal-command-transport.md) — Transport marker pattern
- [core/service/Service/Transport.hs](../../core/service/Service/Transport.hs) — Transport typeclass
- [core/service/Service/Transport/Cli.hs](../../core/service/Service/Transport/Cli.hs) — CliTransport reference
- [core/service/Service/Application/Transports.hs](../../core/service/Service/Application/Transports.hs) — Transport runtime
- [core/service/Service/Response.hs](../../core/service/Service/Response.hs) — CommandResponse type
