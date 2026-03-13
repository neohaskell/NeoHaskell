# Integration.Agent — Architecture Specification

> **Based on**: ADR-0045  
> **For**: Implementer — follow this document exactly. Zero design decisions remain open.

---

## Module Map

| File Path | Module Name | Persona | Purpose |
|-----------|-------------|---------|---------|
| `core/schema/Schema/JsonSchema.hs` | `Schema.JsonSchema` | Nick | Convert `Schema` ADT → Aeson `Json.Value` (JSON Schema draft-07) |
| `integrations/Integration/Agent/Types.hs` | `Integration.Agent.Types` | Jess | `Request`, `Config`, `CommandTool` type definitions + `defaultConfig` |
| `integrations/Integration/Agent.hs` | `Integration.Agent` | Jess | Public facade: re-exports Types + defines `commandTool`, `agent` |
| `integrations/Integration/Agent/Internal.hs` | `Integration.Agent.Internal` | Nick | `ToAction (Request command)` instance, full execution pipeline |
| `integrations/Integration/OpenRouter/Response.hs` | *(modified)* | Nick | Add `ToolCall`, `ToolCallFunction`; extend `Choice` with `toolCalls` |
| `integrations/Integration/OpenRouter/Internal.hs` | *(modified)* | Nick | Add `tools`, `tool_choice` fields to `RequestBody` |

---

## Dependency Graph

```
Integration.Agent
  └── imports Integration.Agent.Types        (re-exports all types)
  └── imports Schema qualified               (ToSchema typeclass)
  └── imports Schema.JsonSchema qualified    (toJsonSchema)
  └── imports Documented qualified           (description)
  └── imports GHC.TypeLits qualified as GhcSymbol  (symbolVal)
  └── imports Service.Command.Core (NameOf)

Integration.Agent.Internal
  └── imports Integration.Agent.Types        (Request, Config, CommandTool)
  └── imports Integration qualified          (ToAction, action, emitCommand, runAction)
  └── imports Integration.Http qualified as Http
  └── imports Integration.Http.Internal ()  ← instance only; ToAction (Http.Request command)
  └── imports Integration.OpenRouter.Internal (RequestBody (..), buildHeaders)
  └── imports Integration.OpenRouter.Message qualified as Message
  └── imports Integration.OpenRouter.Response qualified as OpenRouter
  └── imports Redacted qualified

Schema.JsonSchema
  └── imports Schema (Schema (..), FieldSchema (..))
  └── imports Json qualified
  └── imports Array qualified
  └── imports Text qualified
```

---

## Type Definitions

Copy these verbatim. Do not alter field order, derivings, or pragmas.

### `CommandTool` — `Integration.Agent.Types`

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

### `Config` — `Integration.Agent.Types`

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

### `Request` — `Integration.Agent.Types`

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

### `ToolCallFunction` — `Integration.OpenRouter.Response`

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
```

### `ToolCall` — `Integration.OpenRouter.Response`

```haskell
-- | A tool call returned by the model.
data ToolCall = ToolCall
  { id :: Text
  -- ^ Unique call identifier (e.g., "call_abc123")
  , function :: ToolCallFunction
  -- ^ The function name and encoded arguments
  }
  deriving (Eq, Generic)
  -- NOTE: Show intentionally omitted — inherits sensitive data from ToolCallFunction.
```

### `Choice` — modified in `Integration.OpenRouter.Response`

Replace the existing `Choice` definition with:

```haskell
-- | A single completion choice from the model.
--
-- Most requests return a single choice.
-- 'toolCalls' is populated for tool_choice = "required" requests,
-- empty for regular chat completion requests (backward compatible).
data Choice = Choice
  { message :: Message
  -- ^ The assistant's response message
  , finishReason :: FinishReason
  -- ^ Why generation stopped
  , index :: {-# UNPACK #-} Int
  -- ^ Index of this choice (0 for single responses)
  , toolCalls :: Array ToolCall
  -- ^ Tool calls made by the model (empty array for non-agent responses,
  --   populated for tool_choice = "required" requests)
  }
  deriving (Eq, Generic)
  -- NOTE: Show removed — toolCalls contains ToolCall which has no Show.
```

**Important**: `Choice` and `Response` must have `Show` removed from their `deriving` clauses because `ToolCall` (contained inside `Choice`) intentionally has no `Show` instance. Both `Eq` and `Generic` remain.

### `RequestBody` — modified in `Integration.OpenRouter.Internal`

Add two fields to the existing `RequestBody`:

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
  deriving (Show, Eq, Generic)
```

---

## Function Signatures

### `Schema.JsonSchema`

```haskell
-- | Convert a NeoHaskell Schema to a JSON Schema Aeson Value.
--
-- Follows JSON Schema draft-07 semantics. See conversion rules below.
toJsonSchema :: Schema -> Json.Value
```

### `Integration.Agent.Types`

```haskell
-- | Default agent configuration.
--
-- * System prompt: None
-- * Temperature: None (model default)
-- * Max tokens: None (model default)
-- * Timeout: 120 seconds
{-# INLINE defaultConfig #-}
defaultConfig :: Config
```

### `Integration.Agent` (public facade)

```haskell
-- | Extract a tool definition from a command type.
--
-- Uses 'NameOf' for the wire name, 'Documented' for the description,
-- and 'ToSchema' for the JSON Schema. The full tool definition JSON is
-- pre-computed once and stored in 'CommandTool.toolDefinition'.
--
-- For best performance, bind at module level (creates a CAF):
--
-- @
-- addItemTool :: CommandTool
-- addItemTool = Agent.commandTool \@AddItem
-- @
{-# INLINE commandTool #-}
commandTool ::
  forall command name.
  ( ToSchema command
  , Documented command
  , NameOf command ~ name
  , GhcSymbol.KnownSymbol name
  ) =>
  CommandTool

-- | Smart constructor for an Agent.Request with default config.
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
{-# INLINE agent #-}
agent ::
  forall command.
  Text ->
  Array CommandTool ->
  Text ->
  (Text -> command) ->
  Request command
```

### `Integration.Agent.Internal`

```haskell
-- | ToAction instance — enables Integration.outbound on Agent.Request.
instance
  ( Json.ToJSON command
  , Json.FromJSON command
  , GhcSymbol.KnownSymbol (NameOf command)
  ) =>
  Integration.ToAction (Request command)
  where
  toAction :: Request command -> Integration.Action

-- | Execute the full agent pipeline: build HTTP request, run it,
-- parse tool call response, validate, decode, emit command.
executeAgent ::
  forall command.
  ( Json.ToJSON command
  , Json.FromJSON command
  , GhcSymbol.KnownSymbol (NameOf command)
  ) =>
  Integration.ActionContext ->
  Request command ->
  Task Integration.IntegrationError (Maybe Integration.CommandPayload)

-- | Build the OpenRouter RequestBody including tools and tool_choice.
-- Sets tool_choice = "required" unconditionally.
buildAgentRequestBody ::
  forall command.
  Request command ->
  OpenRouter.Internal.RequestBody

-- | Build the messages array from config and user prompt.
-- Prepends a system message if config.systemPrompt is Just.
buildMessages ::
  Config ->
  Text ->
  Array Message.Message

-- | Handle an HTTP response from the OpenRouter API for an agent request.
-- Parses the response body, extracts the first tool call, validates
-- the tool name, and decodes arguments as the command type.
-- Pure function — returns command directly (Http.Request.onSuccess contract).
handleAgentHttpSuccess ::
  forall command.
  Json.FromJSON command =>
  Request command ->
  Http.Response ->
  command

-- | Handle an HTTP transport error by delegating to onError.
handleAgentHttpError ::
  forall command.
  Request command ->
  Text ->
  command

-- | Parse the first tool call from an OpenRouter response.
-- Returns Err if choices is empty or the first choice has no tool calls.
parseFirstToolCall ::
  OpenRouter.Response ->
  Result Text OpenRouter.ToolCall

-- | Validate that the AI-returned tool name is in the registered tools list.
-- Defence against prompt injection (F-1 from ADR-0045).
-- Tool name in error messages is truncated to 100 chars.
validateToolName ::
  Array CommandTool ->
  OpenRouter.ToolCall ->
  Result Text OpenRouter.ToolCall

-- | Decode the tool call arguments as the target command type.
-- Unwraps the Redacted arguments text, then JSON-decodes as command.
-- Tool name in error messages is truncated to 100 chars (F-6 sanitisation).
decodeArguments ::
  forall command.
  Json.FromJSON command =>
  OpenRouter.ToolCall ->
  Result Text command

-- | Base URL for OpenRouter API. Defined once, shared with OpenRouter.Internal.
openRouterBaseUrl :: Text
```

---

## Exact Imports Per Module

### `core/schema/Schema/JsonSchema.hs`

```haskell
module Schema.JsonSchema
  ( -- * Conversion
    toJsonSchema
  ) where

import Array qualified
import Basics
import Json qualified
import Maybe (Maybe (..))
import Schema (FieldSchema (..), Schema (..))
import Text (Text)
import Text qualified
```

### `integrations/Integration/Agent/Types.hs`

```haskell
module Integration.Agent.Types
  ( -- * Types
    Request (..)
  , Config (..)
  , CommandTool (..)

    -- * Config Helpers
  , defaultConfig
  ) where

import Array (Array)
import Basics
import Json qualified
import Maybe (Maybe (..))
import Text (Text)
```

### `integrations/Integration/Agent.hs`

```haskell
-- | # Integration.Agent — Provider-Agnostic AI Agent via Tool Calling
--
-- == Two-Persona Model
--
-- * __Jess (Integration User)__: Configures agents with pure records.
--   No JSON, no HTTP, no provider details visible.
--
-- * __Nick (Integration Developer)__: Implements 'ToAction' in
--   "Integration.Agent.Internal".
--
-- == Quick Start
--
-- @
-- import Integration qualified
-- import Integration.Agent qualified as Agent
--
-- cartIntegrations :: CartEntity -> CartEvent -> Integration.Outbound
-- cartIntegrations cart event = case event of
--   CustomerAsked question -> Integration.batch
--     [ Agent.agent
--         question
--         [ Agent.commandTool \@AddItem
--         , Agent.commandTool \@RemoveItem
--         ]
--         "anthropic/claude-3.5-sonnet"
--         (\\err -> AgentFailed { cartId = cart.id, error = err })
--         |> Integration.outbound
--     ]
--   _ -> Integration.none
-- @
--
-- == Data Privacy Notice
--
-- The user prompt is sent to third-party AI providers via OpenRouter.
-- Do not include PII, credentials, or confidential business data in prompts
-- unless appropriate data processing agreements are in place.
-- See ADR-0045 for security and privacy considerations.
module Integration.Agent
  ( -- * Request Configuration (Jess's API)
    Request (..)
  , Config (..)
  , CommandTool (..)

    -- * Config Helpers
  , defaultConfig

    -- * Smart Constructors
  , commandTool
  , agent
  ) where

import Array (Array)
import Basics
import Data.Proxy (Proxy (..))
import Documented (Documented)
import Documented qualified
import GHC.TypeLits qualified as GhcSymbol
import Integration.Agent.Types (CommandTool (..), Config (..), Request (..))
import Integration.Agent.Types qualified as Types
import Json qualified
import Schema (ToSchema)
import Schema qualified
import Schema.JsonSchema qualified
import Service.Command.Core (NameOf)
import Text (Text)
import Text qualified
```

### `integrations/Integration/Agent/Internal.hs`

```haskell
{-# LANGUAGE UndecidableInstances #-}

-- | Internal implementation for Integration.Agent.
--
-- This module contains Nick's code — the 'ToAction' instance,
-- HTTP request construction, and tool-call response parsing.
--
-- __This module is not part of Jess's API.__
module Integration.Agent.Internal where

import Array (Array)
import Array qualified
import Basics
import GHC.TypeLits qualified as GhcSymbol
import Integration qualified
import Integration.Agent.Types (CommandTool (..), Config (..), Request (..))
import Integration.Http qualified as Http
import Integration.Http.Internal ()              -- ToAction (Http.Request command) instance
import Integration.OpenRouter.Internal (RequestBody (..), buildHeaders)
import Integration.OpenRouter.Message qualified as Message
import Integration.OpenRouter.Response qualified as OpenRouter
import Json qualified
import Maybe (Maybe (..))
import Redacted qualified
import Result qualified
import Service.Command.Core (NameOf)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
```

### `integrations/Integration/OpenRouter/Response.hs` (additions)

Add at the top of imports:

```haskell
import Redacted (Redacted)
```

Then add the two new types and modify `Choice`. See **OpenRouter Modifications** section for exact diff.

### `integrations/Integration/OpenRouter/Internal.hs` (additions)

No new imports needed. The `RequestBody` type gains two fields and `ToJSON RequestBody` gains two optional field entries.

---

## Integration Points

### How `commandTool` works (pure, compile-time)

```
commandTool @AddItem
  ├── GhcSymbol.symbolVal (Proxy @(NameOf AddItem))  → "AddItem"       (NameOf type family)
  ├── Documented.description @AddItem                 → "Add an item…"  (Documented typeclass)
  └── Schema.toSchema @AddItem
        |> Schema.JsonSchema.toJsonSchema              → Json.Value      (JSON Schema object)
  → CommandTool { toolName, toolDescription, toolDefinition }
```

### How `Integration.outbound (Agent.agent ...)` works (runtime)

```
Agent.Request command
  │
  ▼ Integration.Agent.Internal.ToAction
  │
  ├── executeAgent ctx agentRequest
  │     │
  │     ├── buildAgentRequestBody agentRequest
  │     │     ├── buildMessages config prompt        → Array Message
  │     │     ├── Array.map .toolDefinition tools    → Array Json.Value
  │     │     └── RequestBody { ..., tools = Just toolDefs, tool_choice = Just "required" }
  │     │
  │     ├── Http.Request
  │     │     { method = Http.POST
  │     │     , url    = "https://openrouter.ai/api/v1/chat/completions"
  │     │     , headers = buildHeaders openRouterConfig   ← from OpenRouter.Internal
  │     │     , body    = Http.json body
  │     │     , onSuccess = handleAgentHttpSuccess agentRequest
  │     │     , onError   = Just (handleAgentHttpError agentRequest)
  │     │     , auth    = Http.Bearer "${OPENROUTER_API_KEY}"
  │     │     , retry   = Http.noRetry
  │     │     , timeoutSeconds = agentRequest.config.timeoutSeconds
  │     │     }
  │     │
  │     └── Integration.runAction ctx (Integration.toAction httpRequest)
  │           └── Http.Internal.ToAction executes the request
  │                 └── on HTTP 2xx: handleAgentHttpSuccess agentRequest httpResponse
  │                       ├── Json.decode httpResponse.body  → OpenRouter.Response
  │                       ├── parseFirstToolCall response    → Result Text ToolCall
  │                       ├── validateToolName tools toolCall → Result Text ToolCall
  │                       ├── decodeArguments @command toolCall → Result Text command
  │                       └── Ok cmd → cmd  (returned to Http.Internal which calls emitCommand)
  │
  └── Task IntegrationError (Maybe CommandPayload)
```

### Why `buildHeaders` from `OpenRouter.Internal` is used

The Agent's `Config` has no `referer`/`title` fields, so `buildHeaders` will receive a minimal `OpenRouter.Config` (all `Nothing`). This produces an empty headers array, which is correct. The pattern mirrors OCR's approach.

Convert `Agent.Config` → `OpenRouter.Config` inside `buildAgentRequestBody`:

```haskell
-- Inside buildAgentRequestBody, build the OpenRouter config for buildHeaders:
let openRouterConfig = OpenRouter.Config
      { temperature      = agentRequest.config.temperature
      , maxTokens        = agentRequest.config.maxTokens
      , topP             = Nothing
      , frequencyPenalty = Nothing
      , presencePenalty  = Nothing
      , referer          = Nothing
      , title            = Nothing
      , timeoutSeconds   = agentRequest.config.timeoutSeconds
      }
```

Pass `openRouterConfig` to `buildHeaders openRouterConfig` to get `Array (Text, Text)`.

---

## OpenRouter Modifications

### `Integration.OpenRouter.Response` — exact changes

**Step 1**: Add import at top (after existing imports):

```haskell
import Redacted (Redacted)
import Redacted qualified
```

**Step 2**: Add two new types before `Choice`:

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


instance Json.FromJSON ToolCallFunction where
  parseJSON = Json.withObject "ToolCallFunction" \obj -> do
    name <- obj Json..: "name"
    rawArguments <- obj Json..: "arguments"
    let arguments = Redacted.wrap rawArguments
    Json.yield ToolCallFunction {name, arguments}

instance Json.ToJSON ToolCallFunction where
  toJSON tcf =
    Json.object
      [ ("name", Json.toJSON tcf.name)
      , ("arguments", Json.toJSON (Redacted.unwrap tcf.arguments))
      ]


-- | A tool call returned by the model.
data ToolCall = ToolCall
  { id :: Text
  -- ^ Unique call identifier (e.g., "call_abc123")
  , function :: ToolCallFunction
  -- ^ The function name and encoded arguments
  }
  deriving (Eq, Generic)
  -- NOTE: Show intentionally omitted — inherits sensitive data from ToolCallFunction.


instance Json.FromJSON ToolCall where
  parseJSON = Json.withObject "ToolCall" \obj -> do
    id <- obj Json..: "id"
    function <- obj Json..: "function"
    Json.yield ToolCall {id, function}

instance Json.ToJSON ToolCall where
  toJSON tc =
    Json.object
      [ ("id", Json.toJSON tc.id)
      , ("function", Json.toJSON tc.function)
      ]
```

**Step 3**: Replace `Choice` definition:

Remove `deriving (Show, Eq, Generic)` → replace with `deriving (Eq, Generic)`.  
Add `toolCalls` field.  
Update `FromJSON Choice` to use `.:?` with default `[]`.  
Update `ToJSON Choice` to include `tool_calls`.

```haskell
-- | A single completion choice from the model.
data Choice = Choice
  { message :: Message
  -- ^ The assistant's response message
  , finishReason :: FinishReason
  -- ^ Why generation stopped
  , index :: {-# UNPACK #-} Int
  -- ^ Index of this choice (0 for single responses)
  , toolCalls :: Array ToolCall
  -- ^ Tool calls made by the model (empty array for non-agent responses,
  --   populated for tool_choice = "required" requests)
  }
  deriving (Eq, Generic)
  -- NOTE: Show removed — toolCalls contains ToolCall which has no Show.


instance Json.FromJSON Choice where
  parseJSON = Json.withObject "Choice" \obj -> do
    message <- obj Json..: "message"
    finishReason <- obj Json..: "finish_reason"
    index <- obj Json..:? "index" Json..!= 0
    toolCalls <- obj Json..:? "tool_calls" Json..!= []  -- backward compatible: [] for non-agent
    Json.yield Choice {message, finishReason, index, toolCalls}


instance Json.ToJSON Choice where
  toJSON choice =
    Json.object
      [ ("message", Json.toJSON choice.message)
      , ("finish_reason", Json.toJSON choice.finishReason)
      , ("index", Json.toJSON choice.index)
      , ("tool_calls", Json.toJSON choice.toolCalls)
      ]
```

**Step 4**: `Response` — remove `Show` from `deriving`:

```haskell
-- existing deriving (Show, Eq, Generic)  →  deriving (Eq, Generic)
```

Update `module Integration.OpenRouter.Response` export list to add:

```haskell
module Integration.OpenRouter.Response
  ( -- * Response Types
    Response (..)
  , Choice (..)
  , Usage (..)
  , FinishReason (..)

    -- * Tool Call Types
  , ToolCall (..)
  , ToolCallFunction (..)
  ) where
```

### `Integration.OpenRouter.Internal` — exact changes

**Step 1**: Add two fields to `RequestBody`:

```haskell
data RequestBody = RequestBody
  { messages          :: Array Message
  , model             :: Text
  , stream            :: Bool
  , temperature       :: Maybe Float
  , max_tokens        :: Maybe Int
  , top_p             :: Maybe Float
  , frequency_penalty :: Maybe Float
  , presence_penalty  :: Maybe Float
  , tools             :: Maybe (Array Json.Value)
  -- ^ Tool definitions array (Nothing = omit field, preserving current behaviour)
  , tool_choice       :: Maybe Text
  -- ^ "required" forces the model to call a tool (Nothing = omit field)
  }
  deriving (Show, Eq, Generic)
```

**Step 2**: Update `ToJSON RequestBody` to include two new optional fields:

```haskell
instance Json.ToJSON RequestBody where
  toJSON body = do
    let required =
          [ ("messages", Json.toJSON body.messages)
          , ("model", Json.toJSON body.model)
          , ("stream", Json.toJSON body.stream)
          ]
    let optionalFields =
          [ body.temperature       |> fmap (\v -> ("temperature", Json.toJSON v))
          , body.max_tokens        |> fmap (\v -> ("max_tokens", Json.toJSON v))
          , body.top_p             |> fmap (\v -> ("top_p", Json.toJSON v))
          , body.frequency_penalty |> fmap (\v -> ("frequency_penalty", Json.toJSON v))
          , body.presence_penalty  |> fmap (\v -> ("presence_penalty", Json.toJSON v))
          , body.tools             |> fmap (\v -> ("tools", Json.toJSON v))
          , body.tool_choice       |> fmap (\v -> ("tool_choice", Json.toJSON v))
          ]
          |> Array.getJusts
    let allFields = required |> Array.append optionalFields |> Array.toLinkedList
    Json.object allFields
```

**Step 3**: Update `buildRequestBody` to initialise the two new fields as `Nothing`:

```haskell
buildRequestBody :: forall command. Request command -> RequestBody
buildRequestBody request =
  RequestBody
    { messages          = request.messages
    , model             = request.model
    , stream            = False
    , temperature       = request.config.temperature
    , max_tokens        = request.config.maxTokens
    , top_p             = request.config.topP
    , frequency_penalty = request.config.frequencyPenalty
    , presence_penalty  = request.config.presencePenalty
    , tools             = Nothing   -- ← new field, Nothing preserves existing behaviour
    , tool_choice       = Nothing   -- ← new field, Nothing preserves existing behaviour
    }
```

---

## Schema.JsonSchema — Conversion Rules

Implement `toJsonSchema` as a `case` on `Schema`. Follow the `Schema.OpenApi.toOpenApiSchema` pattern exactly (case match, return `Json.Value`).

| Schema constructor | JSON Schema output |
|---|---|
| `SNull` | `{"type": "null"}` |
| `SBool` | `{"type": "boolean"}` |
| `SInt` | `{"type": "integer"}` |
| `SNumber` | `{"type": "number"}` |
| `SText` | `{"type": "string"}` |
| `SArray inner` | `{"type": "array", "items": toJsonSchema inner}` |
| `SOptional inner` | `toJsonSchema inner` (no wrapping — required/optional is signalled by SObject) |
| `SObject fields` | `{"type": "object", "properties": {...}, "required": [...], "additionalProperties": false}` |
| `SEnum variants` | `{"type": "string", "enum": [...]}` — preserve original casing (do NOT lowercase; AI must match exact command names) |
| `SUnion variants` | `{"oneOf": [toJsonSchema schema for each variant]}` |
| `SRef name` | `{"$ref": "#/definitions/<name>"}` |

### `SObject` detail

```
properties  = Map from fieldName → toJsonSchema fieldSchema  (for ALL fields)
required    = [ fieldName | field ← fields, field.fieldRequired == True ]
              -- fieldRequired is False for SOptional fields (set by Schema generic deriv)
```

Append `("additionalProperties", Json.Bool False)` unconditionally. This makes OpenRouter reject malformed AI responses early.

### Implementation skeleton

```haskell
toJsonSchema :: Schema -> Json.Value
toJsonSchema schema = case schema of
  SNull ->
    Json.object [("type", Json.toJSON ("null" :: Text))]

  SBool ->
    Json.object [("type", Json.toJSON ("boolean" :: Text))]

  SInt ->
    Json.object [("type", Json.toJSON ("integer" :: Text))]

  SNumber ->
    Json.object [("type", Json.toJSON ("number" :: Text))]

  SText ->
    Json.object [("type", Json.toJSON ("string" :: Text))]

  SArray innerSchema -> do
    let items = toJsonSchema innerSchema
    Json.object
      [ ("type", Json.toJSON ("array" :: Text))
      , ("items", items)
      ]

  SOptional innerSchema ->
    toJsonSchema innerSchema

  SObject fields -> do
    let properties =
          fields
            |> Array.map (\field -> (field.fieldName, toJsonSchema field.fieldSchema))
            |> Array.toLinkedList
            -- convert to Json.Object / Json.object call
    let requiredNames =
          fields
            |> Array.takeIf (\field -> field.fieldRequired)
            |> Array.map (\field -> field.fieldName)
            |> Array.toLinkedList
    Json.object
      [ ("type", Json.toJSON ("object" :: Text))
      , ("properties", Json.object properties)
      , ("required", Json.toJSON requiredNames)
      , ("additionalProperties", Json.Bool False)
      ]

  SEnum variants -> do
    let enumValues = variants |> Array.toLinkedList
    Json.object
      [ ("type", Json.toJSON ("string" :: Text))
      , ("enum", Json.toJSON enumValues)
      ]

  SUnion variants -> do
    let oneOf =
          variants
            |> Array.map (\(_name, variantSchema) -> toJsonSchema variantSchema)
            |> Array.toLinkedList
    Json.object [("oneOf", Json.toJSON oneOf)]

  SRef refName ->
    Json.object [("$ref", Json.toJSON [fmt|#/definitions/#{refName}|])]
```

**Note on `Json.object`**: It takes `[(Text, Json.Value)]` (a linked list of pairs, matching the OpenRouter.Internal pattern). Use `Array.toLinkedList` to convert from `Array` before passing to `Json.object`.

---

## `commandTool` Implementation Contract

The body uses NeoHaskell's `do` with `let` bindings (pure — no monadic bind `<-`):

```haskell
{-# INLINE commandTool #-}
commandTool ::
  forall command name.
  ( ToSchema command
  , Documented command
  , NameOf command ~ name
  , GhcSymbol.KnownSymbol name
  ) =>
  CommandTool
commandTool = do
  let name = GhcSymbol.symbolVal (Proxy @name) |> Text.fromLinkedList
  let desc = Documented.description @command
  let schema = Schema.toSchema @command |> Schema.JsonSchema.toJsonSchema
  let description = case desc of
        "" -> name
        d  -> d
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
    { toolName        = name
    , toolDescription = description
    , toolDefinition  = definition
    }
```

---

## `agent` Implementation Contract

```haskell
{-# INLINE agent #-}
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
    , config  = Types.defaultConfig
    , onError
    }
```

---

## `executeAgent` Implementation Contract

```haskell
executeAgent ctx agentRequest = do
  let messages = buildMessages agentRequest.config agentRequest.prompt
  let toolDefs  = agentRequest.tools |> Array.map .toolDefinition
  let openRouterConfig = OpenRouter.Request.Config
        { temperature      = agentRequest.config.temperature
        , maxTokens        = agentRequest.config.maxTokens
        , topP             = Nothing
        , frequencyPenalty = Nothing
        , presencePenalty  = Nothing
        , referer          = Nothing
        , title            = Nothing
        , timeoutSeconds   = agentRequest.config.timeoutSeconds
        }
  let headers = buildHeaders openRouterConfig
  let body = RequestBody
        { messages          = messages
        , model             = agentRequest.model
        , stream            = False
        , temperature       = agentRequest.config.temperature
        , max_tokens        = agentRequest.config.maxTokens
        , top_p             = Nothing
        , frequency_penalty = Nothing
        , presence_penalty  = Nothing
        , tools             = Just toolDefs
        , tool_choice       = Just "required"
        }
  let httpRequest = Http.Request
        { method         = Http.POST
        , url            = openRouterChatCompletionsUrl
        , headers        = headers
        , body           = Http.json body
        , onSuccess      = handleAgentHttpSuccess agentRequest
        , onError        = Just (handleAgentHttpError agentRequest)
        , auth           = Http.Bearer "${OPENROUTER_API_KEY}"
        , retry          = Http.noRetry
        , timeoutSeconds = agentRequest.config.timeoutSeconds
        }
  let httpAction = Integration.toAction httpRequest
  Integration.runAction ctx httpAction
```

Import needed for `OpenRouter.Request.Config`: `import Integration.OpenRouter.Request qualified as OpenRouter`.

---

## `handleAgentHttpSuccess` Implementation Contract

Pattern mirrors `OpenRouter.Internal.handleSuccess`. Pure function, type `Http.Response -> command`.

```haskell
handleAgentHttpSuccess agentRequest httpResponse =
  case httpResponse.statusCode of
    code | code >= 200 && code < 300 ->
      case httpResponse.body |> Json.decode of
        Result.Err _ ->
          agentRequest.onError "Failed to parse OpenRouter response"
        Result.Ok openRouterResponse ->
          case parseFirstToolCall openRouterResponse of
            Err errText -> agentRequest.onError errText
            Ok toolCall ->
              case validateToolName agentRequest.tools toolCall of
                Err errText -> agentRequest.onError errText
                Ok validToolCall ->
                  case decodeArguments @command validToolCall of
                    Err errText -> agentRequest.onError errText
                    Ok cmd      -> cmd
    429 ->
      agentRequest.onError "OpenRouter rate limit exceeded"
    code | code >= 400 && code < 500 ->
      agentRequest.onError [fmt|OpenRouter request error (HTTP #{code})|]
    code ->
      agentRequest.onError [fmt|OpenRouter server error (HTTP #{code})|]
```

Note: `Json.decode` operates on `httpResponse.body` exactly as in `OpenRouter.Internal.handleSuccess`. Use the same call site — no change in decoding approach.

---

## `decodeArguments` Implementation Contract

```haskell
decodeArguments toolCall = do
  let rawArgs = toolCall.function.arguments |> Redacted.unwrap
  case rawArgs |> Json.decodeText of
    Result.Err _ ->
      Err [fmt|Failed to decode tool arguments for #{toolCall.function.name |> Text.take 100}|]
    Result.Ok cmd ->
      Ok cmd
```

**`Json.decodeText`**: If this function does not yet exist in the `Json` module, implement as:
```haskell
rawArgs |> Text.toBytes |> Json.decode
```
where `Text.toBytes :: Text -> Bytes` converts to UTF-8 bytes before `Json.decode`. Check the `Json` module for the correct Text→JSON decode function before building.

---

## nhcore Utilities Reference

| Operation | Function to use |
|-----------|----------------|
| Get type name as Text | `GhcSymbol.symbolVal (Proxy @name) \|> Text.fromLinkedList` |
| Get Documented description | `Documented.description @command` |
| Get ToSchema schema | `Schema.toSchema @command` |
| Convert Schema to JSON Schema | `Schema.JsonSchema.toJsonSchema schema` |
| Map array | `Array.map f xs` |
| Filter array | `Array.takeIf predicate xs` |
| Get first element | `Array.first xs` — returns `Maybe element` |
| Check any element matches | `Array.any predicate xs` |
| Unwrap Redacted | `Redacted.unwrap redacted` |
| Wrap in Redacted | `Redacted.wrap value` |
| JSON object from pairs | `Json.object [(Text, Json.Value)]` — takes linked list |
| JSON encode to Value | `Json.toJSON value` |
| JSON decode from bytes | `Json.decode bytes` — returns `Result Text a` |
| Truncate text | `Text.take 100 text` |
| Format string | `[fmt|message #{var}|]` |
| Array to linked list | `Array.toLinkedList xs` |
| Empty array | `Array.empty` |
| Pipe operator | `x \|> f` |

---

## Cabal Changes

### `core/nhcore.cabal`

In the `exposed-modules` list, add `Schema.JsonSchema` after `Schema`:

```
Schema
Schema.JsonSchema    ← add this line
Schema.OpenApi
```

In `test-suite nhcore-test`, add to `other-modules`:

```
Schema.JsonSchemaSpec    ← add this line (after SchemaSpec)
SchemaSpec
```

### `integrations/nhintegrations.cabal`

In the `exposed-modules` list, add three new modules:

```
Integration.Agent              ← add
Integration.Agent.Internal     ← add
Integration.Agent.Types        ← add
Integration.Ocr.Ai             ← existing
Integration.Ocr.Ai.Internal    ← existing
```

In `test-suite nhintegrations-test`, add to `other-modules`:

```
Integration.Agent.InternalSpec    ← add (after Integration.Ocr.Ai.InternalSpec)
Integration.Ocr.Ai.InternalSpec   ← existing
```

---

## Test Plan

### `core/test/Schema/JsonSchemaSpec.hs`

Module: `Schema.JsonSchemaSpec`  
Test suite: `nhcore-test` (add to `other-modules` in nhcore.cabal)

**Required test cases:**

| Test | Input | Expected output |
|------|-------|----------------|
| SNull | `SNull` | `{"type": "null"}` |
| SBool | `SBool` | `{"type": "boolean"}` |
| SInt | `SInt` | `{"type": "integer"}` |
| SNumber | `SNumber` | `{"type": "number"}` |
| SText | `SText` | `{"type": "string"}` |
| SArray | `SArray SText` | `{"type": "array", "items": {"type": "string"}}` |
| SOptional | `SOptional SInt` | `{"type": "integer"}` (same as SInt — wrapping is transparent) |
| SObject required only | `SObject [FieldSchema "x" SText True ""]` | `{"type":"object","properties":{"x":{"type":"string"}},"required":["x"],"additionalProperties":false}` |
| SObject mixed optional | `SObject [FieldSchema "a" SInt True "", FieldSchema "b" SText False ""]` | required = ["a"] only; "b" present in properties but absent from required |
| SEnum | `SEnum ["Active", "Inactive"]` | `{"type":"string","enum":["Active","Inactive"]}` — original casing preserved |
| SUnion | `SUnion [("Circle", SObject [...]), ("Square", SObject [...])]` | `{"oneOf":[...]}` |
| SRef | `SRef "CartEntity"` | `{"$ref":"#/definitions/CartEntity"}` |
| SObject additionalProperties | any SObject | output always contains `"additionalProperties": false` |

**Template:**

```haskell
module Schema.JsonSchemaSpec (spec) where

import Basics
import Json qualified
import Schema (FieldSchema (..), Schema (..))
import Schema.JsonSchema qualified
import Test (Spec)
import Test qualified

spec :: Spec
spec = Test.describe "Schema.JsonSchema" do
  Test.describe "toJsonSchema" do
    Test.it "converts SNull" do
      let result = Schema.JsonSchema.toJsonSchema SNull
      result `Test.shouldBe` Json.object [("type", Json.toJSON ("null" :: Text))]
    -- ... one Test.it per table row above
```

### `integrations/test/Integration/Agent/InternalSpec.hs`

Module: `Integration.Agent.InternalSpec`  
Test suite: `nhintegrations-test` (add to `other-modules` in nhintegrations.cabal)

**Required test cases:**

| Function | Test case | What to assert |
|----------|-----------|---------------|
| `parseFirstToolCall` | empty choices | `Err "No choices in OpenRouter response"` |
| `parseFirstToolCall` | choice with empty toolCalls | `Err "Model did not return a tool call"` |
| `parseFirstToolCall` | choice with one tool call | `Ok toolCall` with correct id and function |
| `validateToolName` | tool name in registered list | `Ok toolCall` unchanged |
| `validateToolName` | tool name NOT in registered list | `Err` containing "unregistered tool" |
| `validateToolName` | long unregistered name (>100 chars) | error message truncated to 100 chars |
| `decodeArguments` | valid JSON for command type | `Ok command` decoded correctly |
| `decodeArguments` | invalid JSON string | `Err` containing "Failed to decode tool arguments" |
| `decodeArguments` | wrong JSON shape | `Err` containing "Failed to decode tool arguments" |
| `buildMessages` | no system prompt | single user message |
| `buildMessages` | with system prompt | system message first, user message second |

**Note on exports**: `Integration.Agent.Internal` must export the tested functions. The module declaration should use:

```haskell
module Integration.Agent.Internal
  ( -- * For Testing Only
    parseFirstToolCall
  , validateToolName
  , decodeArguments
  , buildMessages
  , buildAgentRequestBody
  ) where
```

---

## Error Message Catalogue

The implementer must use these exact strings — they are tested and documented in ADR-0045 §11.

| Failure mode | Error text |
|---|---|
| Empty choices | `"No choices in OpenRouter response"` |
| No tool calls in first choice | `"Model did not return a tool call"` |
| Unregistered tool name | `[fmt|Agent returned unregistered tool: #{name \|> Text.take 100}|]` |
| Malformed arguments JSON | `[fmt|Failed to decode tool arguments for #{name \|> Text.take 100}|]` |
| Failed to parse HTTP body | `"Failed to parse OpenRouter response"` |
| Rate limited | `"OpenRouter rate limit exceeded"` |
| 4xx HTTP error | `[fmt|OpenRouter request error (HTTP #{code})|]` |
| 5xx HTTP error | `[fmt|OpenRouter server error (HTTP #{code})|]` |

---

## Instance Summary

| Type | Show | Eq | Generic | ToJSON | FromJSON | Notes |
|------|------|----|---------|--------|----------|-------|
| `CommandTool` | ✓ | ✓ | ✓ | derived | derived | |
| `Config` | ✗ | ✓ | ✓ | — | — | No Show: may contain confidential system prompt |
| `Request command` | ✗ | — | ✓ | — | — | No Show: prompt contains user data |
| `ToolCallFunction` | ✗ | ✓ | ✓ | custom | custom | No Show: arguments are user data |
| `ToolCall` | ✗ | ✓ | ✓ | custom | custom | No Show: inherits from ToolCallFunction |
| `Choice` | ✗ | ✓ | ✓ | custom | custom | Show removed: contains Array ToolCall |
| `Response` | ✗ | ✓ | ✓ | custom | custom | Show removed: contains Array Choice |

---

## Known Constraints (v1)

- `tool_choice = "required"` always — no opt-out
- Only `tool_calls[0]` is dispatched — multiple tool calls are silently dropped
- No `onSuccess` callback — command is auto-dispatched
- No conversation history — single-turn only
- OpenRouter is the only provider — `ToAction` is OpenRouter-specific despite provider-agnostic `Request`
