# ADR-0045: Integration.Agent — Provider-Agnostic AI Agent via Tool Calling

## Status

Proposed

## Context

NeoHaskell has `Integration.OpenRouter` for free-form chat completions and `Integration.Ocr.Ai`
for AI-powered document extraction. Both integrations produce unstructured text responses that
Jess must parse manually to extract meaningful domain actions. With the OpenRouter tool-calling
API, the AI model can instead select and populate a specific domain command by name — returning
structured JSON that maps directly onto an existing command record — eliminating all parsing
code from Jess's domain.

### Current State

1. **`Integration.OpenRouter`** — Text-in / text-out chat completions via OpenRouter. No support
   for tool calling (`tools`, `tool_choice`). The response is a free-form assistant message that
   Jess must inspect and parse herself.

2. **`Integration.Ocr.Ai`** — AI-powered document extraction via OpenRouter. Piggybacks on
   the OpenRouter → HTTP pipeline. Establishes the two-persona model (Jess/Nick) and the
   `Request`/`Config`/`defaultConfig`/`onError` record pattern that this ADR mirrors.

3. **`Schema.ToSchema`** — Automatic JSON schema derivation for any `Generic` type. Already
   used by command TH macros (ADR-0017) to expose command shapes to the OpenAPI spec. The
   same schema ADT can be converted to JSON Schema for OpenRouter tool definitions.

4. **`Schema.OpenApi`** — Converts NeoHaskell `Schema` ADT to OpenAPI 3.0 objects. There is
   no equivalent converter to plain JSON Schema (Aeson `Value`), which is the format that
   OpenRouter's `tools` array requires.

5. **`Integration.emitCommand`** — Dispatches a command payload from within a `ToAction`
   instance. Already handles JSON decoding of the command body and routing to the correct
   command executor.

6. **`Service.Command.Core.NameOf`** — Type family mapping a command type to its wire name
   (`Symbol`). Combined with `ToSchema`, provides everything needed to build a tool definition
   at compile time.

### Use Cases

- **Conversational cart assistant**: A customer types "add two blue widgets to my cart". The
  agent picks `AddItem` or `RemoveItem` based on intent, populates the fields from the sentence,
  and emits the command — no parsing code required.

- **Automated document routing**: An incoming email is summarised by an AI. The agent picks
  `RouteToLegal`, `RouteToSupport`, or `ArchiveDocument` depending on content, and the correct
  command is dispatched.

- **Natural-language form filling**: A user fills in a support form in plain prose. The agent
  maps the prose onto a structured `CreateTicket` command, ensuring all required fields are
  populated before the command reaches the domain.

- **Workflow orchestration**: After a payment is processed, an AI agent reads the payment
  context and picks one of `FulfillOrder`, `PartialFulfill`, or `EscalateToReview` depending
  on inventory signals embedded in the entity state.

### Design Goals

1. **Zero-parsing DX**: Jess should never write JSON parsing code. The AI response must
   be decoded and dispatched automatically. If the only callback Jess needs for failures is
   `onError :: Text -> command`, the happy path requires no user code at all.

2. **Provider-agnostic surface**: The `Request` record Jess fills in must not reference
   OpenRouter, HTTP, or any specific provider. A future `AnthropicAgent` or `BedrockAgent`
   module should be drop-in replacements without changing Jess's integration function.

3. **Type-safe tool registration**: Tools must be extracted from real command types (not
   stringly-typed). `commandTool @AddItem` should be the full API — name, description, and
   schema are inferred at compile time from `NameOf`, `Documented`, and `ToSchema`.

4. **Minimal new surface area**: Reuse `Integration.OpenRouter`, `Schema`, and
   `Integration.emitCommand`. New code is limited to wiring them together, with five new
   modules and two targeted modifications to OpenRouter types.

5. **Jess at 10 PM**: Jess should be able to wire a new AI agent in a single `Integration.batch`
   block during an evening session, without reading provider documentation or writing any
   JSON handling code.

### GitHub Issue

- [#561: Integration.Agent — AI agent integration via tool calling](https://github.com/neohaskell/NeoHaskell/issues/561)

## Decision

### 1. Module Placement: Separate Agent Namespace

Five new modules are added to `nhintegrations` and one new module to `nhcore`'s `schema`
source directory. The two-persona split (Jess/Nick) from ADR-0008 is preserved throughout:

| Module | Source Dir | Persona | Purpose |
|--------|-----------|---------|---------|
| `Integration.Agent` | `integrations/` | Jess | Public re-export façade: `Request`, `commandTool`, `agent` |
| `Integration.Agent.Types` | `integrations/` | Jess | Core types: `Request`, `Config`, `CommandTool` |
| `Integration.Agent.Internal` | `integrations/` | Nick | `ToAction` instance, OpenRouter wiring, response parsing |
| `Schema.JsonSchema` | `core/schema/` | Nick | Convert `Schema` ADT to Aeson `Json.Value` (JSON Schema format) |

OpenRouter is extended with two targeted modifications:

| Module | Change |
|--------|--------|
| `Integration.OpenRouter.Response` | Add `ToolCall`, `ToolCallFunction` types; extend `Choice` with `toolCalls` field |
| `Integration.OpenRouter.Internal` | Serialise `tools` and `tool_choice` fields in `RequestBody` when present |

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Extend `Integration.OpenRouter` with agent support | Rejected | Mixes concerns: Jess's chat API would gain confusing `tools` fields. The OpenRouter module is a provider detail; agent is a higher-level abstraction. |
| `Integration.Ai.Agent` | Rejected | Inconsistent with `Integration.Ocr.Ai` and `Integration.Audio.Transcribe` naming. The `Integration.Agent` namespace directly signals intent without implying a single provider. |
| `Integration.Agent` (new namespace) | **Chosen** | Self-documenting, mirrors the OCR/Audio namespace pattern, keeps provider details hidden behind the façade. |

### 2. New Module: `Schema.JsonSchema`

`Schema.JsonSchema` follows the `Schema.OpenApi` converter pattern, translating the
library-agnostic `Schema` ADT to a plain Aeson `Json.Value` compliant with JSON Schema
draft-07 (the format OpenRouter's `function.parameters` field expects).

```haskell
module Schema.JsonSchema
  ( -- * Conversion
    toJsonSchema
  ) where

-- | Convert a NeoHaskell Schema to a JSON Schema Aeson Value.
--
-- Follows JSON Schema draft-07 semantics:
--
-- * SNull       → {"type": "null"}
-- * SBool       → {"type": "boolean"}
-- * SInt        → {"type": "integer"}
-- * SNumber     → {"type": "number"}
-- * SText       → {"type": "string"}
-- * SArray s    → {"type": "array", "items": <s>}
-- * SOptional s → inner schema only (caller removes field from "required")
-- * SObject fs  → {"type": "object", "properties": {...}, "required": [...],
--                  "additionalProperties": false}
-- * SEnum vs    → {"type": "string", "enum": [...]}
-- * SUnion vs   → {"oneOf": [...]}
-- * SRef name   → {"$ref": "#/definitions/<name>"}
toJsonSchema :: Schema -> Json.Value
```

The `SObject` conversion collects required field names by filtering fields whose
`fieldRequired` is `True`, then appends `"additionalProperties": false` so that OpenRouter
rejects malformed AI responses early.

### 3. Type Definitions

All types use NeoHaskell conventions: descriptive type parameters, strict fields (project-wide
`Strict` extension), `Result` not `Either`, `Task` not `IO`.

#### `CommandTool` — a single tool definition

```haskell
-- | A tool extracted from a command type for use with AI agents.
--
-- Build with 'commandTool' to ensure the name, description, and schema are
-- consistent with the actual command type:
--
-- @
-- tools =
--   [ Agent.commandTool \@AddItem
--   , Agent.commandTool \@RemoveItem
--   ]
-- @
--
-- The description is pulled from the 'Documented' typeclass instance.
-- If no 'Documented' instance exists or description is empty, the tool
-- name is used as the description.
data CommandTool = CommandTool
  { toolName :: Text
  -- ^ Wire name derived from 'NameOf' (e.g., "AddItem")
  , toolDescription :: Text
  -- ^ Human-readable description from 'Documented' instance
  , toolDefinition :: Json.Value
  -- ^ Pre-computed tool definition: {"type":"function","function":{name,description,parameters}}.
  --   Built once at 'commandTool' call site to avoid per-request allocation.
  }
  deriving (Show, Eq, Generic)
```

#### `Config` — optional tuning parameters

```haskell
-- | Optional configuration for an Agent request.
--
-- Use 'defaultConfig' as a starting point:
--
-- @
-- Agent.defaultConfig
--   { systemPrompt = Just "You are a helpful shopping assistant."
--   , temperature  = Just 0.2
--   }
-- @
data Config = Config
  { systemPrompt :: Maybe Text
  -- ^ Optional system prompt. Nothing = no system message.
  , temperature :: Maybe Float
  -- ^ Sampling temperature (0.0–2.0). Lower values produce more
  --   deterministic tool selection. Nothing = model default.
  , maxTokens :: Maybe Int
  -- ^ Maximum tokens in the response. Nothing = model default.
  , timeoutSeconds :: {-# UNPACK #-} Int
  -- ^ Request timeout in seconds. Default: 120.
  }
  deriving (Eq, Generic)
  -- NOTE: Config intentionally does NOT derive Show.
  -- systemPrompt may contain confidential business logic.
```

#### `Request` — Jess's API record

```haskell
-- | Provider-agnostic AI agent request.
--
-- The @command@ type parameter is the domain command type emitted on
-- success or failure. Both 'onError' and the internal dispatch
-- must produce the same @command@.
--
-- NOTE: 'Request' intentionally does NOT derive 'Show'.
-- Do not add 'Show' — 'prompt' contains user data transmitted
-- to third-party AI providers. See ADR-0045 §12.
--
-- When using multiple tools, @command@ must be a sum type or
-- envelope type whose 'FromJSON' instance handles all tool
-- argument shapes. See §8 for a complete example.
--
-- == Example
--
-- @
-- Agent.agent
--   question
--   [ Agent.commandTool \@AddItem
--   , Agent.commandTool \@RemoveItem
--   ]
--   "anthropic/claude-3.5-sonnet"
--   (\\err -> AgentFailed { error = err })
--   |> Integration.outbound
-- @
data Request command = Request
  { prompt :: Text
  -- ^ User prompt sent to the AI model
  , tools :: Array CommandTool
  -- ^ Available commands exposed as tools. Must be non-empty.
  , model :: Text
  -- ^ Model identifier (e.g., "anthropic/claude-3.5-sonnet")
  , config :: Config
  -- ^ Optional tuning parameters
  , onError :: Text -> command
  -- ^ Callback for AI failures, malformed responses, or network errors
  }
  deriving (Generic)
```

**Note**: There is no `onSuccess` callback. The integration decodes
`tool_calls[0].function.arguments` as the command JSON, then routes it through
`Integration.emitCommand` directly. Jess provides only `onError` because the
success path requires no user code — the AI already selected and populated the command.

### 4. Smart Constructor

```haskell
-- | Extract a tool definition from a command type.
--
-- Uses 'NameOf' for the wire name, 'Documented' for the description,
-- and 'ToSchema' for the JSON Schema.
--
-- The description comes from the command's 'Documented' instance.
-- Commands that leave 'Documented.description' empty use the type
-- name as fallback.
--
-- The full tool definition JSON is pre-computed once and stored in
-- 'CommandTool.toolDefinition', eliminating per-request allocation.
--
-- For best performance, bind the result at module level to ensure
-- the schema is computed exactly once (as a CAF):
--
-- @
-- addItemTool :: CommandTool
-- addItemTool = Agent.commandTool \@AddItem
-- @
--
-- Inline usage (e.g., directly in Integration.batch) is correct but
-- relies on GHC's float-out optimisation for memoisation.
--
-- @
-- Agent.commandTool \@AddItem
-- @
commandTool ::
  forall command name.
  ( ToSchema command
  , Documented command
  , NameOf command ~ name
  , KnownSymbol name
  ) =>
  CommandTool
commandTool = do
  let name = Text.fromLinkedList (GhcSymbol.symbolVal (Proxy @name))
  let desc = Documented.description @command
  let schema = Schema.toSchema @command |> Schema.JsonSchema.toJsonSchema
  let description = case desc of
        "" -> name
        d -> d
  let definition =
        Json.object
          [ ("type", Json.toJSON ("function" :: Text))
          , ( "function"
            , Json.object
                [ ("name", Json.toJSON name)
                , ("description", Json.toJSON description)
                , ("parameters", schema)
                ]
            )
          ]
  CommandTool
    { toolName = name
    , toolDescription = description
    , toolDefinition = definition
    }


-- | Smart constructor for an Agent.Request with default config.
--
-- Equivalent to building the 'Request' record with 'defaultConfig'.
--
-- Recommended: keep the tools list small (≤10 tools) to reduce
-- model confusion and token usage.
--
-- @
-- Agent.agent
--   customerMessage
--   [ Agent.commandTool \@AddItem
--   , Agent.commandTool \@RemoveItem
--   ]
--   "anthropic/claude-3.5-sonnet"
--   (\\err -> AgentFailed { error = err })
-- @
agent ::
  forall command.
  Text ->
  Array CommandTool ->
  Text ->
  (Text -> command) ->
  Request command
agent prompt tools model onError =
  Request
    { prompt
    , tools
    , model
    , config = defaultConfig
    , onError
    }


-- | Default agent configuration.
--
-- * System prompt: None
-- * Temperature: None (model default)
-- * Max tokens: None (model default)
-- * Timeout: 120 seconds
{-# INLINE defaultConfig #-}
defaultConfig :: Config
defaultConfig = Config
  { systemPrompt = Nothing
  , temperature = Nothing
  , maxTokens = Nothing
  , timeoutSeconds = 120
  }
```

### 5. Internal Module: `Integration.Agent.Internal`

`Integration.Agent.Internal` implements `ToAction` for `Request command`. It piggybacks on the
existing OpenRouter → HTTP pipeline, following the same pattern as `Integration.Ocr.Ai.Internal`:

```text
Agent.Request
  → ToAction.toAction
    → Integration.action \ctx ->
      → Build OpenRouter.RequestBody with tools + tool_choice "required"
      → Delegate to Integration.OpenRouter.Internal.toHttpRequest
        → Integration.Http (actual network call)
      → Parse response.choices[0].toolCalls[0]
      → Validate tool name against registered tools  (F-1: prompt injection defence)
      → Unwrap Redacted arguments, decode as command JSON
      → Integration.emitCommand commandPayload
      OR
      → agent.onError errorText    (on parse failure / unregistered tool / API error)
```

#### Execution Flow (pseudo-code in NeoHaskell style)

```haskell
instance
  ( Json.ToJSON command
  , Json.FromJSON command
  , KnownSymbol (NameOf command)
  ) =>
  Integration.ToAction (Request command)
  where
  toAction agentRequest =
    Integration.action \ctx -> do
      let openRouterRequest = buildOpenRouterRequest agentRequest
      let httpRequest = OpenRouter.Internal.toHttpRequest openRouterRequest
      let response <- Integration.runAction (Integration.toAction httpRequest) ctx
      case parseFirstToolCall response of
        Err errText -> Integration.emitCommand (Integration.makeCommandPayload (agentRequest.onError errText))
        Ok toolCall ->
          case validateToolName agentRequest.tools toolCall of
            Err errText -> Integration.emitCommand (Integration.makeCommandPayload (agentRequest.onError errText))
            Ok validToolCall ->
              case decodeArguments @command validToolCall of
                Err errText -> Integration.emitCommand (Integration.makeCommandPayload (agentRequest.onError errText))
                Ok command -> Integration.emitCommand (Integration.makeCommandPayload command)
```

#### OpenRouter Request Construction

The internal module builds an `OpenRouter.Request command` with two additions to the request
body: a `tools` array and `tool_choice = "required"`. The `tools` array is constructed by
mapping each `CommandTool` to the OpenRouter function tool format:

```haskell
-- | Build the tools array for the OpenRouter request body.
-- Since CommandTool stores the pre-computed toolDefinition, this is a trivial map.
buildToolDefinitions :: Array CommandTool -> Array Json.Value
buildToolDefinitions tools =
  tools |> Array.map .toolDefinition
```

`tool_choice = "required"` forces the model to always call one of the provided tools — it
cannot respond with plain text. This is what eliminates the need for an `onSuccess` callback
with raw text handling.

#### Tool-Call Response Parsing

```haskell
-- | Parse the first tool call from an OpenRouter response.
parseFirstToolCall :: OpenRouter.Response -> Result Text ToolCallResult
parseFirstToolCall response = do
  let firstChoice = response.choices |> Array.first
  case firstChoice of
    Nothing -> Err "No choices in OpenRouter response"
    Just choice -> case choice.toolCalls |> Array.first of
      Nothing -> Err "Model did not return a tool call"
      Just toolCall -> Ok toolCall


-- | Validate that the AI-returned tool name is in the registered tools list.
-- Defence against prompt injection: the model could be tricked into returning
-- a tool name that was not registered, bypassing intended dispatch.
validateToolName :: Array CommandTool -> ToolCall -> Result Text ToolCall
validateToolName registeredTools toolCall = do
  let registeredNames = registeredTools |> Array.map .toolName
  let returnedName = toolCall.function.name
  if registeredNames |> Array.any (== returnedName)
    then Ok toolCall
    else Err [fmt|Agent returned unregistered tool: #{returnedName |> Text.take 100}|]


-- | Decode the tool call arguments as the target command type.
-- Unwraps the Redacted arguments before decoding.
-- The tool name in error messages is truncated to 100 chars (F-6: sanitisation).
decodeArguments :: forall command. Json.FromJSON command => ToolCall -> Result Text command
decodeArguments toolCall =
  let rawArgs = toolCall.function.arguments |> Redacted.unwrap
  case rawArgs |> Json.decodeText of
    Err _ -> Err [fmt|Failed to decode tool arguments for #{toolCall.function.name |> Text.take 100}|]
    Ok command -> Ok command
```

### 6. OpenRouter Modifications

#### `Integration.OpenRouter.Response` — New `ToolCall` Types

Two new types are added. The existing `Choice` type gains an optional `toolCalls` field:

```haskell
-- | A function call made by the model.
--
-- NOTE: 'arguments' is wrapped in 'Redacted' because it contains
-- AI-decoded command JSON with user-correlated data (cart IDs, quantities,
-- etc.) that must not appear in logs. Unwrap only inside 'decodeArguments'.
data ToolCallFunction = ToolCallFunction
  { name :: Text
  -- ^ Name of the tool (matches 'CommandTool.toolName')
  , arguments :: Redacted Text
  -- ^ JSON-encoded arguments string, redacted to prevent log leakage.
  --   Unwrap with 'Redacted.unwrap' only inside 'decodeArguments'.
  }
  deriving (Eq, Generic)
  -- NOTE: Show intentionally omitted — arguments contain user data.


-- | A tool call returned by the model.
data ToolCall = ToolCall
  { id :: Text
  -- ^ Unique call identifier (e.g., "call_abc123")
  , function :: ToolCallFunction
  -- ^ The function name and encoded arguments
  }
  deriving (Eq, Generic)
```

The `Choice` type is extended with:

```haskell
data Choice = Choice
  { message :: Message
  , finishReason :: FinishReason
  , index :: {-# UNPACK #-} Int
  , toolCalls :: Array ToolCall
  -- ^ Tool calls made by the model (empty array for non-agent responses,
  --   populated for tool_choice = "required" requests)
  }
```

Backward compatibility: `FromJSON Choice` uses `.:?` with a default of `[]` for `tool_calls`,
so existing chat completion code continues to work without modification.

#### `Integration.OpenRouter.Internal` — Extend `RequestBody`

`RequestBody` gains two optional fields:

```haskell
data RequestBody = RequestBody
  { messages :: Array Message
  , model :: Text
  , stream :: Bool
  , temperature :: Maybe Float
  , max_tokens :: Maybe Int
  , top_p :: Maybe Float
  , frequency_penalty :: Maybe Float
  , presence_penalty :: Maybe Float
  , tools :: Maybe (Array Json.Value)
  -- ^ Tool definitions array (Nothing = omit field, preserving current behaviour)
  , tool_choice :: Maybe Text
  -- ^ "required" forces the model to call a tool (Nothing = omit field)
  }
```

The `ToJSON RequestBody` instance already uses the optional-field pattern from the existing
code. The two new fields are appended to `optionalFields` and omitted when `Nothing`.

### 7. OpenRouter Wire Format

#### Request (additions to existing body)

```json
{
  "messages": [
    {"role": "user", "content": "Add two blue widgets to my cart"}
  ],
  "model": "anthropic/claude-3.5-sonnet",
  "stream": false,
  "tools": [
    {
      "type": "function",
      "function": {
        "name": "AddItem",
        "description": "Add an item to the shopping cart",
        "parameters": {
          "type": "object",
          "properties": {
            "cartId":   {"type": "string"},
            "stockId":  {"type": "string"},
            "quantity": {"type": "integer"}
          },
          "required": ["cartId", "stockId", "quantity"],
          "additionalProperties": false
        }
      }
    }
  ],
  "tool_choice": "required"
}
```

#### Response (with tool call)

```json
{
  "id": "gen-abc123",
  "model": "anthropic/claude-3.5-sonnet",
  "choices": [
    {
      "finish_reason": "tool_calls",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": null,
        "tool_calls": [
          {
            "id": "call_xyz",
            "type": "function",
            "function": {
              "name": "AddItem",
              "arguments": "{\"cartId\":\"...\",\"stockId\":\"...\",\"quantity\":2}"
            }
          }
        ]
      }
    }
  ]
}
```

### 8. Jess's Usage (Complete Example)

```haskell
import Integration qualified
import Integration.Agent qualified as Agent

cartIntegrations :: CartEntity -> CartEvent -> Integration.Outbound
cartIntegrations cart event = case event of
  CustomerAsked question -> Integration.batch
    [ Agent.agent
        question
        [ Agent.commandTool @AddItem
        , Agent.commandTool @RemoveItem
        ]
        "anthropic/claude-3.5-sonnet"
        (\err -> AgentFailed { cartId = cart.id, error = err })
        |> Integration.outbound
    ]
  _ -> Integration.none
```

No JSON parsing. No provider-specific code. No `onSuccess` callback — the command is
dispatched automatically when the AI selects a tool.

**Important**: When using multiple tools, the `command` type parameter must be a sum type
or envelope type whose `FromJSON` instance handles all tool argument shapes. In the
example above, `CartEvent` is the sum type that covers both `AddItem` and `RemoveItem`
shapes. If `command` were just `AddItem`, a `RemoveItem` response from the AI would fail
`FromJSON` decoding and route to `onError`.

For advanced configuration:

```haskell
Integration.outbound Agent.Request
  { prompt = question
  , tools =
      [ Agent.commandTool @AddItem
      , Agent.commandTool @RemoveItem
      , Agent.commandTool @ClearCart
      ]
  , model = "anthropic/claude-3.5-sonnet"
  , config = Agent.defaultConfig
      { systemPrompt = Just "You are a helpful shopping assistant. Only perform actions the user explicitly requests."
      , temperature = Just 0.1
      }
  , onError = \err -> AgentFailed { cartId = cart.id, error = err }
  }
```

### 9. Instance Decisions

| Instance | Decision | Rationale |
|----------|----------|-----------|
| `Generic` | **Derived** on all new types | Standard for NeoHaskell |
| `Eq` | **Derived** on `Config`, `CommandTool`, `ToolCall`, `ToolCallFunction` | Testing and comparison |
| `Show` | **Derived** on `CommandTool` only. Intentionally **omitted** on `Config` (systemPrompt leak), `Request` (prompt leak), `ToolCallFunction` (arguments leak), `ToolCall` (inherits from ToolCallFunction) | ADR-0016 Redacted policy; see §12 |
| `ToJSON` / `FromJSON` | **Derived** on `CommandTool` | No custom wire format needed |
| `ToJSON` / `FromJSON` | **Custom** on `ToolCall`, `ToolCallFunction`, updated `Choice` | Must match OpenRouter API field names (`tool_calls`, `function.arguments`). `FromJSON ToolCallFunction` wraps `arguments` in `Redacted`. |
| `ToAction` | **Custom** in `Integration.Agent.Internal` | Orchestration logic |

### 10. Performance Considerations

- **Strict fields**: All new types use strict fields (project-wide `Strict` extension).
  The `toolDefinition :: Json.Value` in `CommandTool` is evaluated once at call site and shared
  across requests.

- **Pre-computed tool definitions**: `CommandTool` stores the complete
  `{"type":"function","function":{...}}` JSON value at construction time.
  `buildToolDefinitions` is a trivial `Array.map .toolDefinition` — zero per-request allocation.

- **INLINE pragmas**: `defaultConfig`, `commandTool`, and `agent` are marked `{-# INLINE #-}`
  as small, frequently-called functions.

- **CAF memoisation**: When `commandTool @AddItem` is bound at module level, GHC creates a
  Constant Applicative Form (CAF) — computed exactly once per process lifetime. Inline usage
  inside closures relies on GHC's float-out pass, which is best-effort. The recommended pattern:

  ```haskell
  addItemTool :: CommandTool
  addItemTool = Agent.commandTool @AddItem
  ```

- **UNPACK on Int fields**: `Config.timeoutSeconds` and `Choice.index` use `{-# UNPACK #-}`
  to store the `Int` unboxed, reducing GC pressure.

- **Single tool call only (v1)**: The integration takes `tool_calls[0]`. If the model
  returns multiple tool calls, only the first is dispatched. This is consistent with
  `tool_choice = "required"` which typically returns exactly one call.

- **Timeout**: The default 120-second timeout matches `Integration.OpenRouter.defaultConfig`.
  Agent requests are typically faster than audio transcription but slower than simple chat
  completions due to schema processing on the model side.

- **Worker pool impact**: Each in-flight agent request holds a dispatcher worker thread.
  Applications making frequent agent calls should set `eventProcessingTimeoutMs` in
  dispatcher config to at least `120000`.

### 11. Error Handling

Errors are routed to `onError :: Text -> command` in all failure cases:

| Failure Mode | Error Text | Notes |
|---|---|---|
| Empty `tools` array | `"Agent.agent: tools list is empty"` | Validated before HTTP call |
| Network / HTTP error | Provider error message | Passed through from `Http.Request.onError` |
| OpenRouter API error (4xx) | `"OpenRouter request error (HTTP NNN)"` | Consistent with existing OpenRouter handling |
| No choices in response | `"No choices in OpenRouter response"` | Should not occur with valid requests |
| No tool calls in choice | `"Model did not return a tool call"` | Occurs if model ignores `tool_choice = "required"` |
| Unregistered tool name | `"Agent returned unregistered tool: <name>"` | Tool name truncated to 100 chars. Defence against prompt injection (F-1). |
| Malformed arguments JSON | `"Failed to decode tool arguments for <name>"` | AI returned syntactically invalid JSON. Tool name truncated to 100 chars (F-6). |
| Command type mismatch | `"Failed to decode tool arguments for <name>"` | AI returned JSON with wrong shape |

No sensitive information (API keys, internal IDs, raw HTTP responses) is exposed in
error messages.

### 12. Data Privacy Notice

The module Haddock documentation MUST include a Data Privacy Notice:

```haskell
-- == Data Privacy Notice
--
-- The user prompt is sent to third-party AI providers via OpenRouter.
-- Do not include PII, credentials, or confidential business data in prompts
-- unless appropriate data processing agreements are in place.
-- See ADR-0045 for security and privacy considerations.
```

## Consequences

### Positive

1. **Zero-parsing DX**: Jess writes no JSON parsing code. The AI selects a typed command,
   and the integration dispatches it automatically. The only callback required is `onError`.

2. **Compile-time safety**: `commandTool @AddItem` fails at compile time if `AddItem` has no
   `NameOf`, `ToSchema`, or `Documented` instance, catching misconfigured integrations before
   deployment.

3. **Reuses existing infrastructure**: No changes to `ActionContext`, `Integration.emitCommand`,
   or the HTTP layer. The OpenRouter modifications are backward-compatible (optional fields with
   `Nothing` defaults).

4. **Provider-agnostic by design**: The `Request` record contains no OpenRouter-specific fields.
   A future `Bedrock` or `Anthropic` provider can implement `ToAction (Request command)` and
   Jess's code needs no changes.

5. **Schema reuse**: `commandTool @AddItem` derives the same schema that the OpenAPI endpoint
   uses (via `ToSchema`). There is a single source of truth for the command's shape.

6. **Mirrors familiar pattern**: Jess already knows `Integration.Ocr.Ai`. The agent module
   uses the same `Request`/`Config`/`defaultConfig` shape and the same `Integration.outbound`
   wiring. If Jess can use OCR transcription, she can wire an AI agent without learning
   new concepts.

### Negative

1. **No `onSuccess` callback**: The integration auto-dispatches the first tool call. Jess
   cannot inspect or modify the command before it is dispatched. Applications needing pre-dispatch
   validation must encode that logic in the command's `decideImpl`.

2. **Single tool call only (v1)**: Only `tool_calls[0]` is dispatched. If the AI returns
   multiple tool calls (e.g., the model interprets the prompt as requiring two actions), only
   the first is executed. This is a known v1 limitation.

3. **OpenRouter only (v1)**: Despite the provider-agnostic `Request` type, the only `ToAction`
   instance in v1 targets OpenRouter. Other providers require a separate implementation.

4. **API costs per request**: Each agent call incurs LLM API costs. Tool-calling requests
   include the full JSON Schema for every tool in the prompt, which increases token usage
   compared to plain chat completions.

5. **No conversation history**: Each agent call is a single-turn exchange. Multi-turn agents
   (with memory of previous turns) are out of scope for v1.

### Risks

1. **AI selects wrong tool**: The model may misinterpret a prompt and select an unintended
   command. Mitigation: write focused tool descriptions, provide a system prompt that scopes
   the agent's role, and keep the tools list small.

2. **AI populates wrong field values**: The model may correctly identify the command but
   populate a field incorrectly (e.g., wrong quantity). Mitigation: domain validation in
   `decideImpl` rejects invalid commands before events are appended.

3. **Malformed arguments from AI**: Despite `tool_choice = "required"` and `additionalProperties: false`,
   some models occasionally return syntactically valid JSON that fails `FromJSON` decoding
   (wrong types, missing required fields). Mitigation: `onError` handles this gracefully.

4. **Schema drift**: If a command type's fields change after the tool schema is generated,
   the AI may receive an outdated schema from a cached `CommandTool`. Mitigation: `commandTool`
   is a pure function — schema evaluation happens at call site on each deployment, not at
   compile time. No caching layer exists in v1.

5. **Provider outage**: If OpenRouter is unreachable, all agent integrations fail with a network
   error routed to `onError`. Mitigation: applications should implement compensating domain logic
   in `onError` (e.g., queue the request for retry, notify the user).

### Mitigations

1. **Focused system prompts**: The `systemPrompt` config field allows Jess to scope the agent's
   behaviour precisely. A well-written system prompt significantly reduces tool selection errors.

2. **Domain validation as safety net**: Every command still passes through `decideImpl` before
   events are appended. Invalid or nonsensical AI-generated field values are rejected by existing
   business rules, not by the integration layer.

3. **Small tools lists**: The fewer tools the agent sees, the less likely it is to pick the wrong
   one. The v1 constraint of a single tool call encourages focused, single-purpose agent
   integrations.

4. **Low temperature**: Setting `config.temperature = Just 0.1` reduces stochastic variation in
   tool selection, making agent behaviour more predictable in production.

5. **Future: multi-tool dispatch** ([#561](https://github.com/neohaskell/NeoHaskell/issues/561)):
   A v2 enhancement can add `onSuccess :: command -> Integration.Outbound` to allow Jess to
   inspect and chain multiple tool calls. The module design does not preclude this.

## References

- [#561: Integration.Agent — AI agent integration via tool calling](https://github.com/neohaskell/NeoHaskell/issues/561)
- [ADR-0008: Integration Pattern](0008-integration-pattern.md) — Two-persona model (Jess/Nick)
- [ADR-0015: HTTP Outbound Integration](0015-http-outbound-integration.md) — HTTP integration that OpenRouter piggybacks on
- [ADR-0017: ToSchema Auto-Derivation in Command TH](0017-toschema-auto-derivation-in-command-th.md) — `ToSchema` and `NameOf` instances
- [ADR-0023: AI-Powered OCR via Multimodal Models](0023-ai-pdf-transcription.md) — Structural template this ADR mirrors
- [ADR-0041: Audio Transcription Integration](0041-audio-transcription-integration.md) — Companion AI integration
- [Integration.Ocr.Ai](../../integrations/Integration/Ocr/Ai.hs) — OCR module (structural template)
- [Integration.OpenRouter.Internal](../../integrations/Integration/OpenRouter/Internal.hs) — OpenRouter implementation (template for agent internal)
- [Integration.OpenRouter.Response](../../integrations/Integration/OpenRouter/Response.hs) — Response types extended by this ADR
- [Schema.hs](../../core/schema/Schema.hs) — Schema ADT (`SObject`, `SText`, etc.)
- [Schema/OpenApi.hs](../../core/schema/Schema/OpenApi.hs) — Schema converter (template for `Schema.JsonSchema`)
- [Integration.hs](../../core/service/Integration.hs) — `emitCommand`, `makeCommandPayload`, `ToAction`
