# Test Specification: MCP STDIO Transport

**Architecture**: [mcp-transport.md](mcp-transport.md)
**Test files**: `core/test/Service/Transport/McpSpec.hs`, `Mcp/JsonRpcSpec.hs`, `Mcp/ProtocolSpec.hs`, `Mcp/ResponseSpec.hs`

## Test Registration

Add to `nhcore-test-service` `other-modules` in `nhcore.cabal` (after `Service.Transport.InternalSpec`):

```
    Service.Transport.McpSpec
    Service.Transport.Mcp.JsonRpcSpec
    Service.Transport.Mcp.ProtocolSpec
    Service.Transport.Mcp.ResponseSpec
```

All test modules export `spec :: Spec Unit` and are discovered by hspec-discover.

## Shared Test Fixtures

Reuse `Test.Service.Command.Core.AddItemToCart` (already available in test suite) with:
```haskell
type instance NameOf AddItemToCart = "AddItemToCart"
```

For protocol tests, create mock `EndpointHandler` and `QueryEndpointHandler` values that return predictable responses.

---

## McpSpec.hs — Transport Instance Tests

| # | Describe | It | Input | Expected | Type |
|---|----------|----|-------|----------|------|
| 1 | McpTransport constructor | stores serverName | `McpTransport { serverName = "test-svc", serverVersion = "1.0" }` | `.serverName == "test-svc"` | Happy |
| 2 | McpTransport constructor | stores serverVersion | `McpTransport { serverName = "test-svc", serverVersion = "2.3.1" }` | `.serverVersion == "2.3.1"` | Happy |
| 3 | runTransport | returns the task unchanged (identity) | `runTransport transport (Task.yield unit)` | `result == unit` | Happy |
| 4 | buildHandler | decodes valid JSON and calls handler with parsed command | Valid `AddItemToCart` JSON bytes | Callback receives `Accepted { entityId = "test-id" }` | Happy |
| 5 | buildHandler | returns Failed response for invalid JSON | `"not-valid-json"` as bytes | Callback receives `Failed {}` | Error |
| 6 | buildHandler | returns Failed response for empty bytes | `""` as bytes | Callback receives `Failed {}` | Error |
| 7 | buildHandler | returns Failed response for valid JSON with wrong shape | `"{}"` as bytes (missing required fields) | Callback receives `Failed {}` | Error |
| 8 | buildHandler | passes anonymous RequestContext to handler | Valid command, handler captures `ctx.user` | `ctx.user == Nothing` | Edge |

---

## JsonRpcSpec.hs — JSON-RPC Parsing and Serialization

### parseRequest

| # | Describe | It | Input | Expected | Type |
|---|----------|----|-------|----------|------|
| 1 | parseRequest | parses valid request with id | `{"jsonrpc":"2.0","method":"tools/list","id":1}` | `Ok JsonRpcRequest { method = "tools/list", params = Nothing, id = Just (Number 1) }` | Happy |
| 2 | parseRequest | parses request with params | `{"jsonrpc":"2.0","method":"tools/call","params":{"name":"AddItem"},"id":2}` | `Ok` with `params = Just (object ...)` | Happy |
| 3 | parseRequest | parses notification (no id) | `{"jsonrpc":"2.0","method":"notifications/initialized"}` | `Ok` with `id = Nothing` | Happy |
| 4 | parseRequest | parses request with string id | `{"jsonrpc":"2.0","method":"ping","id":"abc"}` | `Ok` with `id = Just (String "abc")` | Edge |
| 5 | parseRequest | parses request with null id | `{"jsonrpc":"2.0","method":"ping","id":null}` | `Ok` with `id = Just Null` | Edge |
| 6 | parseRequest | rejects missing jsonrpc field | `{"method":"ping","id":1}` | `Err` (parse error response) | Error |
| 7 | parseRequest | rejects wrong jsonrpc version | `{"jsonrpc":"1.0","method":"ping","id":1}` | `Err` (invalid request response) | Error |
| 8 | parseRequest | rejects missing method field | `{"jsonrpc":"2.0","id":1}` | `Err` (invalid request response) | Error |
| 9 | parseRequest | rejects malformed JSON | `"not json at all"` | `Err` (parse error response) | Error |
| 10 | parseRequest | rejects empty input | `""` | `Err` (parse error response) | Error |
| 11 | parseRequest | handles unicode in method name | `{"jsonrpc":"2.0","method":"tst/\u00e9","id":1}` | `Ok` with method containing unicode | Edge |

### Error constructors

| # | Describe | It | Input | Expected | Type |
|---|----------|----|-------|----------|------|
| 12 | parseError | has code -32700 | `parseError "bad"` | `.code == -32700` | Happy |
| 13 | invalidRequest | has code -32600 | `invalidRequest "msg"` | `.code == -32600` | Happy |
| 14 | methodNotFound | has code -32601 | `methodNotFound "x"` | `.code == -32601` | Happy |
| 15 | invalidParams | has code -32602 | `invalidParams "y"` | `.code == -32602` | Happy |
| 16 | internalError | has code -32603 | `internalError` | `.code == -32603` | Happy |
| 17 | internalError | uses generic message | `internalError` | `.message` does not contain implementation details | Edge |

### Response construction and serialization

| # | Describe | It | Input | Expected | Type |
|---|----------|----|-------|----------|------|
| 18 | successResponse | includes jsonrpc 2.0 and id | `successResponse (Just (Number 1)) (object [])` | Serialized JSON has `"jsonrpc":"2.0"`, `"id":1`, `"result":{}` | Happy |
| 19 | successResponse | includes result value | `successResponse (Just (Number 5)) (String "hello")` | `"result":"hello"` in output | Happy |
| 20 | errorResponse | includes error object | `errorResponse (Just (Number 1)) (methodNotFound "foo")` | Has `"error":{"code":-32601,...}`, no `"result"` key | Happy |
| 21 | errorResponse | preserves request id | `errorResponse (Just (String "req-1")) err` | `"id":"req-1"` in output | Happy |
| 22 | encodeResponse | produces valid JSON bytes | Any response | `Json.decodeBytes` on result succeeds | Happy |
| 23 | encodeResponse | ends with newline | Any response | Last byte is `0x0A` | Edge |
| 24 | successResponse | handles null id | `successResponse (Just Null) result` | `"id":null` in output | Edge |

---

## ResponseSpec.hs — CommandResponse to MCP Mapping

### toCallToolResult

| # | Describe | It | Input | Expected | Type |
|---|----------|----|-------|----------|------|
| 1 | toCallToolResult | maps Accepted to two content blocks | `Accepted { entityId = "abc-123" }` | JSON has `isError: false`, 2 content blocks, first contains "abc-123" | Happy |
| 2 | toCallToolResult | first block has user+assistant audience | `Accepted { entityId = "xyz" }` | First block annotations: `audience: ["user", "assistant"]` | Happy |
| 3 | toCallToolResult | second block has assistant-only audience | `Accepted { entityId = "xyz" }` | Second block annotations: `audience: ["assistant"], priority: 0.9` | Happy |
| 4 | toCallToolResult | second block mentions resources/read | `Accepted { entityId = "xyz" }` | Second block text contains "resources/read" | Happy |
| 5 | toCallToolResult | maps Rejected to isError true | `Rejected { reason = "Out of stock" }` | `isError: true`, content text contains "Out of stock" | Happy |
| 6 | toCallToolResult | maps Failed to isError true with generic message | `Failed { error = "DB connection lost" }` | `isError: true`, content text is generic (does NOT contain "DB connection lost") | Security |
| 7 | toCallToolResult | Accepted with empty entityId | `Accepted { entityId = "" }` | Still produces valid JSON with empty string in content | Edge |
| 8 | toCallToolResult | Rejected with empty reason | `Rejected { reason = "" }` | `isError: true`, text contains "Command rejected" | Edge |
| 9 | toCallToolResult | content blocks have type "text" | `Accepted { entityId = "id" }` | All content blocks have `"type": "text"` | Happy |

### toResourceContent

| # | Describe | It | Input | Expected | Type |
|---|----------|----|-------|----------|------|
| 10 | toResourceContent | wraps query result with URI and mimeType | `"neohaskell://queries/cart-summary"`, `"{\"items\":[]}"` | Has `contents` array with single entry, correct URI, `mimeType: "application/json"` | Happy |
| 11 | toResourceContent | preserves query JSON verbatim | URI, `"{\"a\":1}"` | `text` field is exactly `"{\"a\":1}"` | Happy |
| 12 | toResourceContent | handles empty query result | URI, `"[]"` | Valid JSON with `text: "[]"` | Edge |
| 13 | toResourceContent | handles unicode in query result | URI, `"{\"name\":\"caf\\u00e9\"}"` | Preserves unicode in text field | Edge |

---

## ProtocolSpec.hs — MCP Protocol Handler Tests

### handleRequest — initialization

| # | Describe | It | Input | Expected | Type |
|---|----------|----|-------|----------|------|
| 1 | handleRequest | responds to initialize with capabilities | `initialize` request | Response has `capabilities` with `tools`, `resources`, `prompts`, `logging` | Happy |
| 2 | handleRequest | includes serverInfo in initialize response | `initialize` request | `serverInfo.name` and `serverInfo.version` match config | Happy |
| 3 | handleRequest | includes protocolVersion in initialize response | `initialize` request | `protocolVersion == "2025-03-26"` | Happy |
| 4 | handleRequest | returns Nothing for notifications/initialized | `notifications/initialized` notification | Returns `Nothing` (no response) | Happy |
| 5 | handleRequest | rejects non-init requests before initialization | `tools/list` before init | Returns error response with code `-32600` | Error |
| 6 | handleRequest | allows initialize before initialized notification | `initialize` request (fresh state) | Returns success (not rejected) | Happy |

### handleRequest — tools

| # | Describe | It | Input | Expected | Type |
|---|----------|----|-------|----------|------|
| 7 | handleRequest | tools/list returns registered commands as tools | State with 2 command schemas | Response `tools` array has 2 entries | Happy |
| 8 | handleRequest | tools/list includes tool name from schema key | Schema key "AddItem" | Tool entry has `name: "AddItem"` | Happy |
| 9 | handleRequest | tools/list includes description from EndpointSchema | Schema with description "Add item to cart" | Tool entry has matching description | Happy |
| 10 | handleRequest | tools/list includes inputSchema from requestSchema | Schema with `SObject` request schema | Tool entry has `inputSchema` as JSON Schema object | Happy |
| 11 | handleRequest | tools/list returns empty array when no commands | State with empty commandSchemas | `tools: []` | Edge |
| 12 | handleRequest | tools/call dispatches to correct handler | `tools/call` with `name: "AddItem"`, valid arguments | Handler called, returns Accepted response wrapped as CallToolResult | Happy |
| 13 | handleRequest | tools/call returns error for unknown tool | `tools/call` with `name: "NonExistent"` | JSON-RPC error with code `-32602`, message contains "Unknown tool" | Error |
| 14 | handleRequest | tools/call returns error for missing name param | `tools/call` with no `name` in params | JSON-RPC error with code `-32602` | Error |
| 15 | handleRequest | tools/call passes anonymous context to handler | `tools/call` with valid tool, handler captures ctx | `ctx.user == Nothing` | Edge |

### handleRequest — resources

| # | Describe | It | Input | Expected | Type |
|---|----------|----|-------|----------|------|
| 16 | handleRequest | resources/list returns registered queries as resources | State with 2 query schemas | Response `resources` array has 2 entries | Happy |
| 17 | handleRequest | resources/list URIs use neohaskell:// scheme | Schema key "cart-summary" | URI is `neohaskell://queries/cart-summary` | Happy |
| 18 | handleRequest | resources/list returns empty array when no queries | State with empty querySchemas | `resources: []` | Edge |
| 19 | handleRequest | resources/read dispatches to correct query handler | `resources/read` with `uri: "neohaskell://queries/cart-summary"` | Query handler called, result wrapped as resource content | Happy |
| 20 | handleRequest | resources/read returns error for unknown resource | `resources/read` with `uri: "neohaskell://queries/nonexistent"` | JSON-RPC error with code `-32602`, message contains "Unknown resource" | Error |
| 21 | handleRequest | resources/read returns error for malformed URI | `resources/read` with `uri: "not-a-valid-uri"` | JSON-RPC error with code `-32602` | Error |

### handleRequest — built-in methods

| # | Describe | It | Input | Expected | Type |
|---|----------|----|-------|----------|------|
| 22 | handleRequest | ping returns empty result | `ping` request | `successResponse id (object [])` | Happy |
| 23 | handleRequest | notifications/cancelled returns Nothing | `notifications/cancelled` notification | Returns `Nothing` | Happy |
| 24 | handleRequest | unknown method returns -32601 | `method: "foo/bar"` | Error with code `-32601` | Error |
| 25 | handleRequest | prompts/list returns empty list | `prompts/list` request | Response with `prompts: []` | Happy |
| 26 | handleRequest | prompts/get returns error | `prompts/get` with any name | JSON-RPC error (not found) | Error |

### newServerState

| # | Describe | It | Input | Expected | Type |
|---|----------|----|-------|----------|------|
| 27 | newServerState | initializes with initialized = False | Fresh state | `ConcurrentVar.get state.initialized == False` | Happy |
| 28 | newServerState | pre-computes cachedToolsList as valid JSON | State with command schemas | `Json.decodeBytes @Json.Value state.cachedToolsList` succeeds | Happy |
| 29 | newServerState | pre-computes cachedResourcesList as valid JSON | State with query schemas | `Json.decodeBytes @Json.Value state.cachedResourcesList` succeeds | Happy |
| 30 | newServerState | stores serverName and serverVersion | Config values | `state.serverName` and `state.serverVersion` match | Happy |

---

## Test Count Summary

| Module | Happy | Edge | Error | Security | Total |
|--------|-------|------|-------|----------|-------|
| McpSpec | 4 | 1 | 3 | 0 | 8 |
| JsonRpcSpec | 10 | 5 | 5 | 0 | 20 |*
| ResponseSpec | 7 | 4 | 0 | 1 | 12 |*
| ProtocolSpec | 16 | 4 | 7 | 0 | 27 |*

*Includes sub-totals from all describe blocks within the module.

**Totals: 37 happy, 14 edge/boundary, 15 error, 1 security = 67 test cases**
**Edge-to-happy ratio: ~0.8:1** (including error cases as edge: 30:37 = ~0.8:1)
