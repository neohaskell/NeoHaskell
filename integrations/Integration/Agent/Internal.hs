{-# LANGUAGE UndecidableInstances #-}

-- | Internal implementation for Integration.Agent.
--
-- This module contains Nick's code — the 'ToAction' instance,
-- HTTP request construction, and tool-call response parsing.
--
-- __This module is not part of Jess's API.__
module Integration.Agent.Internal
  ( -- * For Testing Only
    parseFirstToolCall
  , validateToolName
  , decodeArguments
  , buildMessages
  , buildAgentRequestBody
  , handleAgentHttpSuccess
  , handleAgentHttpError
  , executeAgent
  , openRouterBaseUrl
  ) where

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
import Integration.OpenRouter.Request qualified as OpenRouterRequest
import Integration.OpenRouter.Response qualified as OpenRouter
import Json qualified
import Maybe (Maybe (..))
import Redacted qualified
import Result (Result (..))
import Service.Command.Core (NameOf)
import Task (Task)
import Text (Text)
import Text qualified


-- | ToAction instance — enables Integration.outbound on Agent.Request.
instance
  ( Json.ToJSON command
  , Json.FromJSON command
  , GhcSymbol.KnownSymbol (NameOf command)
  ) =>
  Integration.ToAction (Request command)
  where
  toAction request = Integration.action (\ctx -> executeAgent ctx request)


-- | Base URL for OpenRouter API. Defined once, shared with OpenRouter.Internal.
openRouterBaseUrl :: Text
openRouterBaseUrl = "https://openrouter.ai/api/v1"


-- | Full URL for chat completions endpoint. Top-level CAF — computed once per process.
openRouterChatCompletionsUrl :: Text
openRouterChatCompletionsUrl = "https://openrouter.ai/api/v1/chat/completions"


-- | Build the messages array from config and user prompt.
-- Prepends a system message if config.systemPrompt is Just.
buildMessages ::
  Config ->
  Text ->
  Array Message.Message
buildMessages config prompt = do
  let userMsg = Message.user prompt
  case config.systemPrompt of
    Nothing ->
      Array.fromLinkedList [userMsg]
    Just sysText -> do
      let sysMsg = Message.system sysText
      Array.fromLinkedList [sysMsg, userMsg]


-- | Build the OpenRouter RequestBody including tools and tool_choice.
-- Sets tool_choice = "required" unconditionally.
{-# INLINE buildAgentRequestBody #-}
buildAgentRequestBody ::
  forall command.
  Request command ->
  RequestBody
buildAgentRequestBody request = do
  let messages = buildMessages request.config request.prompt
  let toolDefs = request.tools |> Array.map (\tool -> tool.toolDefinition)
  RequestBody
    { messages = messages
    , model = request.model
    , stream = False
    , temperature = request.config.temperature
    , max_tokens = request.config.maxTokens
    , top_p = Nothing
    , frequency_penalty = Nothing
    , presence_penalty = Nothing
    , tools = Just toolDefs
    , tool_choice = Just "required"
    }


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
                    Ok cmd -> cmd
    429 ->
      agentRequest.onError "OpenRouter rate limit exceeded"
    code | code >= 400 && code < 500 ->
      agentRequest.onError [fmt|OpenRouter request error (HTTP #{code})|]
    code ->
      agentRequest.onError [fmt|OpenRouter server error (HTTP #{code})|]


-- | Handle an HTTP transport error by delegating to onError.
handleAgentHttpError ::
  forall command.
  Request command ->
  Text ->
  command
handleAgentHttpError agentRequest errorText =
  agentRequest.onError errorText


-- | Parse the first tool call from an OpenRouter response.
-- Returns Err if choices is empty or the first choice has no tool calls.
parseFirstToolCall ::
  OpenRouter.Response ->
  Result Text OpenRouter.ToolCall
parseFirstToolCall response =
  case Array.first response.choices of
    Nothing ->
      Err "No choices in OpenRouter response"
    Just choice ->
      case Array.first choice.toolCalls of
        Nothing ->
          Err "Model did not return a tool call"
        Just toolCall ->
          Ok toolCall


-- | Validate that the AI-returned tool name is in the registered tools list.
-- Defence against prompt injection (F-1 from ADR-0045).
-- Tool name in error messages is truncated to 100 chars.
validateToolName ::
  Array CommandTool ->
  OpenRouter.ToolCall ->
  Result Text OpenRouter.ToolCall
validateToolName tools toolCall = do
  let name = toolCall.function.name
  let truncatedName = name |> Text.left 100
  let isRegistered = tools |> Array.any (\tool -> tool.toolName == name)
  if isRegistered
    then Ok toolCall
    else Err [fmt|Agent returned unregistered tool: #{truncatedName}|]


-- | Decode the tool call arguments as the target command type.
-- Unwraps the Redacted arguments text, then JSON-decodes as command.
-- Tool name in error messages is truncated to 100 chars (F-6 sanitisation).
decodeArguments ::
  forall command.
  Json.FromJSON command =>
  OpenRouter.ToolCall ->
  Result Text command
decodeArguments toolCall = do
  let rawArgs = toolCall.function.arguments |> Redacted.unwrap
  let toolName = toolCall.function.name |> Text.left 100
  case rawArgs |> Json.decodeText of
    Result.Err _ ->
      Err [fmt|Failed to decode tool arguments for #{toolName}|]
    Result.Ok cmd ->
      Ok cmd


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
executeAgent ctx agentRequest = do
  let body = buildAgentRequestBody agentRequest
  let openRouterConfig = OpenRouterRequest.Config
        { temperature = agentRequest.config.temperature
        , maxTokens = agentRequest.config.maxTokens
        , topP = Nothing
        , frequencyPenalty = Nothing
        , presencePenalty = Nothing
        , referer = Nothing
        , title = Nothing
        , timeoutSeconds = agentRequest.config.timeoutSeconds
        }
  let headers = buildHeaders openRouterConfig
  let httpRequest = Http.Request
        { method = Http.POST
        , url = openRouterChatCompletionsUrl
        , headers = headers
        , body = Http.json body
        , onSuccess = handleAgentHttpSuccess agentRequest
        , onError = Just (handleAgentHttpError agentRequest)
        , auth = Http.Bearer "${OPENROUTER_API_KEY}"
        , retry = Http.noRetry
        , timeoutSeconds = agentRequest.config.timeoutSeconds
        }
  let httpAction = Integration.toAction httpRequest
  Integration.runAction ctx httpAction
