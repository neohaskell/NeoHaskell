# Integration.Agent Test Specification

This document is the implementation-ready test plan for Phase 6 (Test Spec Design) of "Integration.Agent - Provider-Agnostic AI Agent via Tool Calling".

Source of truth used:
- `docs/architecture/integration-agent.md`
- `docs/decisions/0045-integration-agent.md` (especially section 11)
- Existing patterns in `core/test/Schema/OpenApiSpec.hs`, `core/test-core/RedactedSpec.hs`, `core/test/SchemaSpec.hs`, `integrations/test/Integration/OpenRouter/ResponseSpec.hs`, `integrations/test/Integration/Ocr/Ai/InternalSpec.hs`

## Test Suite Placement and Registration

| Scope | Test File Path | Suite | Registration Method |
|---|---|---|---|
| Schema converter | `core/test/Schema/JsonSchemaSpec.hs` | `nhcore-test` (and mirror into `nhcore-test-core` for parity with `Schema.OpenApiSpec`) | Add `Schema.JsonSchemaSpec` to `other-modules` in `core/nhcore.cabal` |
| Agent public facade | `integrations/test/Integration/Agent/Spec.hs` | `nhintegrations-test` | Add module name to `other-modules` in `integrations/nhintegrations.cabal` |
| Agent types | `integrations/test/Integration/Agent/TypesSpec.hs` | `nhintegrations-test` | Add module name to `other-modules` in `integrations/nhintegrations.cabal` |
| Agent internal pipeline helpers | `integrations/test/Integration/Agent/InternalSpec.hs` | `nhintegrations-test` | Add module name to `other-modules` in `integrations/nhintegrations.cabal` |
| Compile-time contracts (no Show) | `integrations/test/Integration/Agent/CompileSpec.hs` | `nhintegrations-test` | Add module name to `other-modules`; module should use `{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}` and `Test.CompileTime.shouldNotTypecheck` |
| OpenRouter response changes | `integrations/test/Integration/OpenRouter/ResponseSpec.hs` (extend existing file) | `nhintegrations-test` | Already registered; add new `describe` blocks in same module |

Important placement decision:
- Agent tests do **not** go in `core/test` or `core/test-service`.
- Agent/OpenRouter integration tests belong in `integrations/test` under `nhintegrations-test`.

## Shared Fixtures and Mocking Strategy

Use these shared fixtures to keep tests deterministic and explicit:

- `AddItemCommand`: `{ cartId :: Text, stockId :: Text, quantity :: Int }` with `Json.FromJSON`, `Json.ToJSON`, `ToSchema`, `Documented` (`description = "Add an item to the shopping cart"`), `type instance NameOf AddItemCommand = "AddItem"`.
- `ClearCartCommand`: `{ cartId :: Text }` with empty `Documented.description = ""`, `type instance NameOf ClearCartCommand = "ClearCart"`.
- `AgentCommand`: sum type for pipeline tests:
  - `CommandOk AddItemCommand`
  - `CommandError Text`
  - `FromJSON AgentCommand` decodes object payload as `CommandOk`; `onError = CommandError`.
- Canonical valid args JSON text: `{"cartId":"cart-1","stockId":"stock-1","quantity":2}`.
- Canonical malformed args JSON text: `{"cartId":"cart-1","quantity":`.
- Long tool name fixture: `longName = Text.repeat 150 "X"`.

Mocking notes for `Integration.Agent.Internal`:
- `parseFirstToolCall`, `validateToolName`, `decodeArguments`, `buildMessages`, and `buildAgentRequestBody` are pure; no IO mocking required.
- `handleAgentHttpSuccess` and `handleAgentHttpError` are pure over `Http.Response`; use synthetic `Http.Response` values.
- `executeAgent` empty-tools preflight tests should use an in-memory `ActionContext` (same construction pattern as `core/test/Integration/ContextSpec.hs` and `integrations/test/Integration/Oura/InternalSpec.hs`).

## Module: `Schema.JsonSchema`

### `toJsonSchema`

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | Converts `SNull` | `SNull` | `{"type":"null"}` | happy |
| 2 | Converts `SBool` | `SBool` | `{"type":"boolean"}` | happy |
| 3 | Converts `SInt` | `SInt` | `{"type":"integer"}` | happy |
| 4 | Converts `SNumber` | `SNumber` | `{"type":"number"}` | edge |
| 5 | Converts `SText` | `SText` | `{"type":"string"}` | happy |
| 6 | Converts array items | `SArray SText` | `{"type":"array","items":{"type":"string"}}` | edge |
| 7 | Optional unwrap is transparent | `SOptional SInt` | `{"type":"integer"}` | edge |
| 8 | Empty object schema | `SObject []` | `{"type":"object","properties":{},"required":[],"additionalProperties":false}` | edge |
| 9 | Single required field object | `SObject [FieldSchema "x" SText True ""]` | `{"type":"object","properties":{"x":{"type":"string"}},"required":["x"],"additionalProperties":false}` | happy |
| 10 | Single optional field object | `SObject [FieldSchema "x" (SOptional SText) False ""]` | `{"type":"object","properties":{"x":{"type":"string"}},"required":[],"additionalProperties":false}` | edge |
| 11 | Mixed required and optional fields | `SObject [FieldSchema "a" SInt True "", FieldSchema "b" (SOptional SText) False ""]` | `required` contains only `"a"`; both `a` and `b` in `properties`; `additionalProperties=false` | edge |
| 12 | Optional via `fieldRequired=False` even without `SOptional` | `SObject [FieldSchema "a" SInt False ""]` | `required=[]`, property `a` still present as integer | edge |
| 13 | Nested array-of-object | `SArray (SObject [FieldSchema "id" SText True ""])` | `items` is object schema with required `id` and `additionalProperties=false` | edge |
| 14 | Nested object with optional field | `SObject [FieldSchema "child" (SObject [FieldSchema "nick" (SOptional SText) False ""]) True ""]` | Parent required contains `child`; child required is empty; both objects include `additionalProperties=false` | edge |
| 15 | Enum preserves original casing | `SEnum ["Active","INACTIVE","pending"]` | `{"type":"string","enum":["Active","INACTIVE","pending"]}` | edge |
| 16 | Empty enum is preserved | `SEnum []` | `{"type":"string","enum":[]}` | edge |
| 17 | Union emits `oneOf` with each variant schema | `SUnion [("Circle", SObject [FieldSchema "radius" SInt True ""]), ("Square", SObject [FieldSchema "side" SInt True ""])]` | `{"oneOf":[{"type":"object",...},{"type":"object",...}]}` (2 entries, in input order) | happy |
| 18 | Empty union | `SUnion []` | `{"oneOf":[]}` | edge |
| 19 | Reference format | `SRef "CartEntity"` | `{"$ref":"#/definitions/CartEntity"}` | happy |
| 20 | Empty reference name | `SRef ""` | `{"$ref":"#/definitions/"}` | edge |

Implementation note for assertions:
- Compare `Json.Value` structurally (not encoded text ordering).

## Module: `Integration.Agent.Types`

### `defaultConfig`

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | Defaults match contract | `defaultConfig` | `Config { systemPrompt = Nothing, temperature = Nothing, maxTokens = Nothing, timeoutSeconds = 120 }` | happy |
| 2 | Timeout default is 120 seconds | `defaultConfig.timeoutSeconds` | `120` | edge |
| 3 | Optional tuning fields default to `Nothing` | `defaultConfig.systemPrompt`, `defaultConfig.temperature`, `defaultConfig.maxTokens` | All are `Nothing` | edge |
| 4 | Pure value is stable across calls | `defaultConfig == defaultConfig` | `True` | edge |

### `CommandTool` (Eq and Show instances)

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | `Eq` true for identical values | `toolA == toolAClone` | `True` | happy |
| 2 | `Eq` false when `toolName` differs | `toolA == toolDifferentName` | `False` | edge |
| 3 | `Eq` false when `toolDefinition` differs | `toolA == toolDifferentDefinition` | `False` | edge |
| 4 | `Show` includes stable field labels | `show toolA` | Contains `"toolName"`, `"toolDescription"`, and `"toolDefinition"` substrings | edge |

### `Config` (compile-time no-`Show` contract)

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | `show` is rejected at compile-time | `show (defaultConfig :: Config)` via `shouldNotTypecheck` | Fails typecheck with `No instance for (Show Config)` | error |
| 2 | `Show Config` constraint cannot be satisfied | `requiresShow (defaultConfig :: Config)` where `requiresShow :: Show a => a -> Text` via `shouldNotTypecheck` | Fails typecheck with missing `Show Config` | error |
| 3 | `Eq` still works | `(defaultConfig == defaultConfig)` | `True` | edge |

## Module: `Integration.Agent`

### `commandTool`

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | Tool name from `NameOf` | `commandTool @AddItemCommand` | `toolName == "AddItem"` | happy |
| 2 | Description from `Documented` | `commandTool @AddItemCommand` | `toolDescription == "Add an item to the shopping cart"` | edge |
| 3 | Empty description falls back to type name | `commandTool @ClearCartCommand` (`Documented.description = ""`) | `toolDescription == "ClearCart"` | edge |
| 4 | Definition has OpenRouter function wrapper | `commandTool @AddItemCommand` | `toolDefinition` equals `{"type":"function","function":{...}}` | edge |
| 5 | Parameters schema equals `Schema.JsonSchema.toJsonSchema` output | `commandTool @AddItemCommand` | `toolDefinition.function.parameters == toJsonSchema (toSchema @AddItemCommand)` | edge |
| 6 | Deterministic purity | Call `commandTool @AddItemCommand` twice | Values are equal (`Eq`) | edge |

### `agent`

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | Smart constructor fills all fields | `agent "prompt" [tool] "anthropic/claude-3.5-sonnet" onErrorFn` | `prompt/tools/model/onError` preserved and `config == defaultConfig` | happy |
| 2 | `onError` callback is preserved | `req = agent ... onErrorFn`; evaluate `req.onError "boom"` | Returns exact command produced by `onErrorFn` | edge |
| 3 | Empty tools list is preserved by constructor | `agent "p" [] "model" onErrorFn` | `req.tools == []` | edge |
| 4 | Empty prompt/model are not mutated | `agent "" [tool] "" onErrorFn` | `req.prompt == ""` and `req.model == ""` | edge |

## Module: `Integration.Agent.Internal`

### `buildMessages`

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | No system prompt | `buildMessages defaultConfig "Hello"` | Exactly one message: `Message.user "Hello"` | happy |
| 2 | System prompt prepended | `buildMessages defaultConfig{systemPrompt=Just "You are strict."} "Hello"` | Exactly two messages in order: `Message.system ...`, then `Message.user ...` | edge |
| 3 | Empty system prompt still included | `buildMessages defaultConfig{systemPrompt=Just ""} "Hello"` | Two messages, first is `Message.system ""` | edge |

### `parseFirstToolCall`

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | No choices | `Response { choices = [] }` | `Err "No choices in OpenRouter response"` | error |
| 2 | First choice has no tool calls | `Response { choices = [Choice { toolCalls = [] }] }` | `Err "Model did not return a tool call"` | error |
| 3 | Returns first tool call from first choice | First choice has `[callA]` | `Ok callA` | happy |
| 4 | Ignores later choices | First choice has `[]`, second choice has `[callB]` | `Err "Model did not return a tool call"` | edge |
| 5 | Ignores additional calls in first choice | First choice has `[callA, callB]` | `Ok callA` | edge |

### `validateToolName`

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | Registered tool accepted | Registered names include `"AddItem"`; tool call name `"AddItem"` | `Ok` with unchanged tool call | happy |
| 2 | Unregistered tool rejected | Registered names `"AddItem"`, `"RemoveItem"`; returned name `"DeleteAll"` | `Err "Agent returned unregistered tool: DeleteAll"` | error |
| 3 | Long name is truncated in message | Returned name length > 100 | `Err [fmt|Agent returned unregistered tool: #{Text.take 100 longName}|]` | edge |
| 4 | Name matching is case-sensitive | Registered `"AddItem"`; returned `"additem"` | `Err "Agent returned unregistered tool: additem"` | edge |
| 5 | Empty registered set rejects any tool | Registered `[]`; returned `"AddItem"` | `Err "Agent returned unregistered tool: AddItem"` | error |

### `decodeArguments`

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | Valid command JSON decodes | Tool call name `"AddItem"`, arguments `{"cartId":"cart-1","stockId":"stock-1","quantity":2}` | `Ok AddItemCommand { cartId = "cart-1", stockId = "stock-1", quantity = 2 }` | happy |
| 2 | Malformed JSON returns sanitized error | Tool call name `"AddItem"`, malformed args text | `Err "Failed to decode tool arguments for AddItem"` | error |
| 3 | Wrong shape (missing field) returns same error | Tool call name `"AddItem"`, args `{"cartId":"cart-1","stockId":"stock-1"}` | `Err "Failed to decode tool arguments for AddItem"` | error |
| 4 | Wrong field type returns same error | Tool call name `"AddItem"`, args `{"cartId":"cart-1","stockId":"stock-1","quantity":"two"}` | `Err "Failed to decode tool arguments for AddItem"` | error |
| 5 | Long tool name is truncated in error text | Tool call name length > 100 with malformed args | `Err [fmt|Failed to decode tool arguments for #{Text.take 100 longName}|]` | edge |

### `buildAgentRequestBody`

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | Required Agent tool fields are always set | Request with one tool, default config, no system prompt | `RequestBody.tools == Just [toolDefinition]` and `RequestBody.tool_choice == Just "required"`; `stream == False` | happy |
| 2 | System prompt contributes first message | `config.systemPrompt = Just "You are strict."` | `messages == [Message.system "You are strict.", Message.user prompt]` | edge |
| 3 | Temperature/max tokens propagate | `config.temperature = Just 0.2`, `config.maxTokens = Just 512` | `RequestBody.temperature == Just 0.2` and `RequestBody.max_tokens == Just 512` | edge |
| 4 | Tool order is preserved | Tools `[toolA, toolB]` | `RequestBody.tools == Just [toolA.toolDefinition, toolB.toolDefinition]` | edge |
| 5 | Empty tool array remains explicit | Tools `[]` | `RequestBody.tools == Just []` and `tool_choice == Just "required"` | edge |

### `handleAgentHttpSuccess`

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | Parse failure is sanitized | HTTP 200 with `body = Json.Null` | `CommandError "Failed to parse OpenRouter response"` | error |
| 2 | Rate limit status | HTTP 429 | `CommandError "OpenRouter rate limit exceeded"` | error |
| 3 | Client error branch (4xx) | HTTP 400 | `CommandError "OpenRouter request error (HTTP 400)"` | error |
| 4 | Another 4xx branch | HTTP 404 | `CommandError "OpenRouter request error (HTTP 404)"` | edge |
| 5 | Server error branch (5xx) | HTTP 503 | `CommandError "OpenRouter server error (HTTP 503)"` | error |
| 6 | Parsed response has no choices | HTTP 200 with `Response { choices = [] }` JSON body | `CommandError "No choices in OpenRouter response"` | error |
| 7 | Parsed response choice has no tool calls | HTTP 200 with first choice `tool_calls = []` | `CommandError "Model did not return a tool call"` | error |
| 8 | Unregistered tool from model | HTTP 200 with first tool name `"DeleteAll"` and valid args JSON | `CommandError "Agent returned unregistered tool: DeleteAll"` | error |
| 9 | Arguments decode failure | HTTP 200 with registered tool `"AddItem"` and malformed args | `CommandError "Failed to decode tool arguments for AddItem"` | error |
| 10 | Full happy path dispatch | HTTP 200 with registered tool `"AddItem"` and valid args JSON | `CommandOk AddItemCommand { cartId = "cart-1", stockId = "stock-1", quantity = 2 }` | happy |

### `handleAgentHttpError`

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | Passes through provider/network error text | Error text `"connection reset by peer"` | `CommandError "connection reset by peer"` | error |
| 2 | Passes through structured provider message | Error text `"HTTP client timeout after 30s"` | `CommandError "HTTP client timeout after 30s"` | edge |
| 3 | Empty error text remains empty | Error text `""` | `CommandError ""` | edge |

### `executeAgent` (ADR section 11 preflight)

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | Empty tools list is rejected before dispatch | `Request.tools = []` with any prompt/model/config | Task result is `Ok (Just payload)` where payload decodes to `CommandError "Agent.agent: tools list is empty"` | error |
| 2 | Empty tools rejection is deterministic for whitespace prompt | `Request.tools = []`, `prompt = "   "` | Same error command payload text: `"Agent.agent: tools list is empty"` | edge |
| 3 | Empty tools rejection ignores tuning config | `Request.tools = []`, non-default temperature/maxTokens/systemPrompt | Same error command payload text: `"Agent.agent: tools list is empty"` | edge |

## Module: `Integration.OpenRouter.Response` (modifications)

### `ToolCallFunction` (FromJSON/ToJSON and redaction contract)

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | Decodes valid function call payload | JSON `{"name":"AddItem","arguments":"{\"cartId\":\"c1\",\"quantity\":2}"}` | `Result.Ok ToolCallFunction`; `name == "AddItem"`; `arguments |> Redacted.unwrap` equals raw JSON text | happy |
| 2 | Preserves escaped JSON string exactly | JSON with escaped quotes/newlines in `arguments` | Unwrapped arguments text is byte-for-byte equal to JSON string value | edge |
| 3 | Rejects non-string `arguments` | JSON `{"name":"AddItem","arguments":{}}` | `Result.Err ...` (parse failure) | error |
| 4 | JSON round-trip | Encode then decode `ToolCallFunction` value | Decoded value equals original (`Eq`) and arguments remain wrapped | edge |
| 5 | No `Show` instance (compile-time) | `show (value :: ToolCallFunction)` via `shouldNotTypecheck` | Fails typecheck with missing `Show ToolCallFunction` | error |

### `ToolCall` (FromJSON/ToJSON)

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | Decodes valid tool call object | JSON with `id` and nested `function` object | `Result.Ok ToolCall` with exact `id` and nested `ToolCallFunction` values | happy |
| 2 | JSON round-trip | Encode then decode `ToolCall` | `Result.Ok original` | edge |
| 3 | Missing `id` fails | JSON without `id` | `Result.Err ...` | error |
| 4 | Malformed nested `function` fails | JSON `function` missing `arguments` | `Result.Err ...` | error |

### `Choice` (FromJSON/ToJSON backward compatibility)

| # | Test Case | Input | Expected Output | Category |
|---|---|---|---|---|
| 1 | Backward compatibility without `tool_calls` | JSON choice with `message`, `finish_reason`, `index`, and no `tool_calls` field | `Result.Ok choice` and `choice.toolCalls == []` | edge |
| 2 | Decodes explicit `tool_calls` array | JSON choice with one `tool_calls` entry | `choice.toolCalls` length is 1 and nested values decode correctly | happy |
| 3 | Missing `index` and missing `tool_calls` use defaults | JSON choice with only `message` + `finish_reason` | `index == 0` and `toolCalls == []` | edge |
| 4 | Choice JSON round-trip with non-empty tool calls | Encode/decode `Choice` containing one `ToolCall` | `Result.Ok original` | edge |
| 5 | Response round-trip remains valid with tool calls in choices | Encode/decode `Response` containing one `Choice` with `toolCalls` populated | `Result.Ok original` | edge |

## ADR Section 11 Error Coverage Matrix

| ADR 0045 section 11 failure mode | Required error text | Covered by test cases |
|---|---|---|
| Empty `tools` array | `"Agent.agent: tools list is empty"` | `executeAgent` #1, #2, #3 |
| Network / HTTP error | Provider error message passthrough | `handleAgentHttpError` #1, #2, #3 |
| OpenRouter API error (4xx) | `"OpenRouter request error (HTTP NNN)"` | `handleAgentHttpSuccess` #3, #4 |
| No choices in response | `"No choices in OpenRouter response"` | `parseFirstToolCall` #1; `handleAgentHttpSuccess` #6 |
| No tool calls in first choice | `"Model did not return a tool call"` | `parseFirstToolCall` #2; `handleAgentHttpSuccess` #7 |
| Unregistered tool name | `"Agent returned unregistered tool: <name>"` (100-char truncation) | `validateToolName` #2, #3, #4, #5; `handleAgentHttpSuccess` #8 |
| Malformed arguments JSON | `"Failed to decode tool arguments for <name>"` (100-char truncation) | `decodeArguments` #2, #5; `handleAgentHttpSuccess` #9 |
| Command type mismatch | same as malformed args text above | `decodeArguments` #3, #4; `handleAgentHttpSuccess` #9 |

## Coverage Guardrails

- Minimum test count per public function: satisfied (all listed functions have >=3 test cases).
- Edge/error emphasis: this plan is intentionally edge-heavy (>= 3:1 edge+error vs happy).
- Serialization round-trips are explicitly included for all modified `FromJSON`/`ToJSON` types (`ToolCallFunction`, `ToolCall`, `Choice`, and `Response` with tool calls).
- Compile-time privacy contracts are explicitly tested/documented (no `Show` for `Config` and `ToolCallFunction`).
