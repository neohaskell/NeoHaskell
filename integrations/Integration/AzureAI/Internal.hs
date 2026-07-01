{-# LANGUAGE UndecidableInstances #-}

-- | Internal implementation for Azure AI chat-completions integration.
--
-- Contains the transformation from 'Request' to 'Http.Request', the single
-- 'Redacted.unwrap' site for the API key, and the 'ToAction (Request command)'
-- instance that enables 'Integration.outbound'.
--
-- __This module is not re-exported from 'Integration.AzureAI'__ (only
-- 'toHttpRequest' is re-exported from the facade as an advanced escape hatch).
module Integration.AzureAI.Internal
  ( -- * Transformation (exported for testing)
    toHttpRequest

    -- * Internal types and helpers (exported for tests)
  , RequestBody (..)
  , buildRequestBody
  , sanitizeApiVersion
  , handleSuccess
  , handleError
  ) where

import Array (Array)
import Array qualified
import Basics
import Char (Char)
import Char qualified
import Integration (ToAction (..))
import Integration.AzureAI.Request (Config (..), Request (..), endpointUrl)
import Integration.Http qualified as Http
import Integration.OpenRouter.Message (Message)
import Integration.OpenRouter.Response ()
import Json qualified
import Maybe (Maybe (..))
import Redacted qualified
import Result (Result (..))
import Service.Command.Core (NameOf)
import Text (Text)
import Text qualified


-- | Nick's internal JSON body. Jess never sees this type.
-- Mirrors 'Integration.OpenRouter.Internal.RequestBody' minus tools/tool_choice.
data RequestBody = RequestBody
  { messages          :: Array Message
  , model             :: Text
  , stream            :: Bool
  , temperature       :: Maybe Float
  , max_tokens        :: Maybe Int
  , top_p             :: Maybe Float
  , frequency_penalty :: Maybe Float
  , presence_penalty  :: Maybe Float
  }
  deriving (Show, Eq, Generic)


-- | Hand-written ToJSON: messages/model/stream always present; sampling fields
-- omitted (never serialised as null) when Nothing. Mirrors OpenRouter.Internal.
instance Json.ToJSON RequestBody where
  toJSON body = do
    let required =
          [ ("messages", Json.toJSON body.messages)
          , ("model", Json.toJSON body.model)
          , ("stream", Json.toJSON body.stream)
          ]
    let optionalFields =
          [ body.temperature |> fmap (\v -> ("temperature", Json.toJSON v))
          , body.max_tokens |> fmap (\v -> ("max_tokens", Json.toJSON v))
          , body.top_p |> fmap (\v -> ("top_p", Json.toJSON v))
          , body.frequency_penalty |> fmap (\v -> ("frequency_penalty", Json.toJSON v))
          , body.presence_penalty |> fmap (\v -> ("presence_penalty", Json.toJSON v))
          ]
          |> Array.getJusts
    let allFields = required |> Array.append optionalFields |> Array.toLinkedList
    Json.object allFields


-- | ToAction instance: pure delegation through toHttpRequest.
-- No Result branch — AzureEndpoint type proves https+allowlist at construction time.
-- The simpler OpenRouter form (not ACS's planAction) because the type, not a
-- runtime check, carries the security invariant.
instance
  (Json.ToJSON command, KnownSymbol (NameOf command)) =>
  ToAction (Request command)
  where
  toAction req =
    req
      |> toHttpRequest
      |> Integration.toAction


-- | Bridge a Request into an Http.Request. The ONLY Redacted.unwrap site.
-- The endpoint is already https+allowlisted by construction (AzureEndpoint),
-- so there is no runtime https branch here — that guard moved to azureEndpoint.
--
-- @
-- case AzureAI.azureEndpoint "https://my-res.openai.azure.com" of
--   Result.Ok endpoint ->
--     AzureAI.chatCompletion endpoint msgs "gpt-4o" onOk onErr
--       |> AzureAI.toHttpRequest
--       |> Integration.outbound
--   Result.Err reason -> Integration.none
-- @
toHttpRequest :: forall command. Request command -> Http.Request command
toHttpRequest req =
  do
    let endpointBase = endpointUrl req.config.endpoint
    let apiVersion = sanitizeApiVersion req.config.apiVersion
    let url = [fmt|#{endpointBase}/models/chat/completions?api-version=#{apiVersion}|]
    let keyValue = Redacted.unwrap req.apiKey   -- the ONLY Redacted.unwrap in this integration
    let body = buildRequestBody req
    Http.Request
      { method = Http.POST
      , url
      , headers = Array.empty
      , body = Http.json body
      , onSuccess = handleSuccess req
      , onError = Just (handleError req)
      , auth = Http.ApiKey "api-key" keyValue
      , retry = Http.noRetry
      , timeoutSeconds = req.config.timeoutSeconds
      }


-- | Build the JSON request body from a Request.
-- Call sites: (1) toHttpRequest (production), (2) InternalSpec body-shape tests.
buildRequestBody :: forall command. Request command -> RequestBody
buildRequestBody req =
  RequestBody
    { messages = req.messages
    , model = req.model
    , stream = False
    , temperature = req.config.temperature
    , max_tokens = req.config.maxTokens
    , top_p = req.config.topP
    , frequency_penalty = req.config.frequencyPenalty
    , presence_penalty = req.config.presencePenalty
    }


-- | Allowed characters in an api-version string: alphanumeric and dash.
-- Module-private helper for 'sanitizeApiVersion'.
isApiVersionChar :: Char -> Bool
isApiVersionChar c = Char.isAlphaNum c || c == '-'


-- | Neutralise query-parameter injection through apiVersion (SEC-003).
-- Keeps only [A-Za-z0-9-]; drops '&', '?', '#', whitespace, and all else.
-- Legitimate values ("2024-10-21", "2024-05-01-preview") pass through unchanged.
-- Call sites: (1) toHttpRequest (production), (2) InternalSpec SEC-003 tests.
sanitizeApiVersion :: Text -> Text
sanitizeApiVersion raw =
  raw |> Text.filter isApiVersionChar


-- | Dispatch the Model Inference HTTP response on status code.
-- 2xx: decode as Response and call onSuccess; errors: sanitised onError
-- (no key, no raw body). Mirrors OpenRouter.Internal.handleSuccess with
-- Azure-branded messages.
-- Call sites: (1) toHttpRequest (as Http.Request.onSuccess), (2) InternalSpec.
handleSuccess :: forall command. Request command -> Http.Response -> command
handleSuccess req httpResponse =
  case httpResponse.statusCode of
    code | code >= 200 && code < 300 ->
      case httpResponse.body |> Json.decode of
        Result.Err _parseError ->
          req.onError "Failed to parse Azure AI response"
        Result.Ok response ->
          req.onSuccess response
    429 ->
      req.onError "Azure AI rate limit exceeded"
    code | code >= 400 && code < 500 ->
      req.onError [fmt|Azure AI request error (HTTP #{code})|]
    code ->
      req.onError [fmt|Azure AI server error (HTTP #{code})|]


-- | Pass a transport-level error through to onError unchanged.
-- Integration.Http already sanitises transport errors.
-- Call sites: (1) toHttpRequest (as Http.Request.onError), (2) InternalSpec.
handleError :: forall command. Request command -> Text -> command
handleError req errorText =
  req.onError errorText
