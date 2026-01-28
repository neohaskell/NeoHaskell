{-# LANGUAGE UndecidableInstances #-}

-- | Internal implementation for OpenRouter integration.
--
-- This module contains Nick's code - the transformation from
-- 'OpenRouter.Request' to 'Http.Request'.
--
-- __This module exports 'toHttpRequest' for the public API.__
module Integration.OpenRouter.Internal
  ( -- * Transformation
    toHttpRequest

    -- * Internal Types (for testing)
  , RequestBody (..)
  , buildHeaders
  ) where

import Array (Array)
import Array qualified
import Basics
import Integration qualified
import Integration.Http qualified as Http
import Integration.OpenRouter.Message (Message)
import Integration.OpenRouter.Request (Config (..), Request (..))
import Json qualified
import Maybe (Maybe (..))
import Result qualified
import Service.Command.Core (NameOf)
import Text (Text)


-- | ToAction instance that enables @Integration.outbound@ to work directly
-- with 'OpenRouter.Request'.
--
-- This allows Jess to write:
--
-- @
-- OpenRouter.chatCompletion
--   [Message.user question]
--   "anthropic/claude-3.5-sonnet"
--   handleSuccess
--   handleError
--   |> Integration.outbound
-- @
--
-- Instead of manually converting to Http.Request first.
instance
  (Json.ToJSON command, KnownSymbol (NameOf command)) =>
  Integration.ToAction (Request command)
  where
  toAction config =
    config
      |> toHttpRequest
      |> Integration.toAction


-- | Base URL for OpenRouter API.
baseUrl :: Text
baseUrl = "https://openrouter.ai/api/v1"


-- | Internal JSON body sent to OpenRouter API.
--
-- This is Nick's concern - Jess never sees this type.
data RequestBody = RequestBody
  { messages :: Array Message
  , model :: Text
  , stream :: Bool
  , temperature :: Maybe Float
  , max_tokens :: Maybe Int
  , top_p :: Maybe Float
  , frequency_penalty :: Maybe Float
  , presence_penalty :: Maybe Float
  }
  deriving (Show, Eq, Generic)


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


-- | Transform an OpenRouter.Request into an Http.Request.
--
-- This is the core of the piggyback pattern. We build an 'Http.Request'
-- that the existing HTTP integration will execute.
--
-- == Usage
--
-- @
-- let openRouterRequest = OpenRouter.chatCompletion { ... }
-- let httpRequest = OpenRouter.toHttpRequest openRouterRequest
-- Integration.outbound httpRequest
-- @
toHttpRequest :: forall command. Request command -> Http.Request command
toHttpRequest openRouterRequest = do
  let body = buildRequestBody openRouterRequest
  let headers = buildHeaders openRouterRequest.config
  Http.Request
    { method = Http.POST
    , url = [fmt|#{baseUrl}/chat/completions|]
    , headers = headers
    , body = Http.json body
    , onSuccess = handleSuccess openRouterRequest
    , onError = Just (handleError openRouterRequest)
    , auth = Http.Bearer "${OPENROUTER_API_KEY}"
    , retry = Http.noRetry -- LLM calls are expensive, don't auto-retry
    , timeoutSeconds = openRouterRequest.config.timeoutSeconds
    }


-- | Build the JSON request body from OpenRouter.Request.
buildRequestBody :: forall command. Request command -> RequestBody
buildRequestBody request =
  RequestBody
    { messages = request.messages
    , model = request.model
    , stream = False -- v1: no streaming support
    , temperature = request.config.temperature
    , max_tokens = request.config.maxTokens
    , top_p = request.config.topP
    , frequency_penalty = request.config.frequencyPenalty
    , presence_penalty = request.config.presencePenalty
    }


-- | Build optional headers (Referer, X-Title).
buildHeaders :: Config -> Array (Text, Text)
buildHeaders config = do
  let refererHeader = config.referer |> fmap \url -> ("HTTP-Referer", url)
  let titleHeader = config.title |> fmap \name -> ("X-Title", name)
  [refererHeader, titleHeader]
    |> Array.getJusts


-- | Parse the HTTP response and invoke the user's onSuccess callback.
handleSuccess :: forall command. Request command -> Http.Response -> command
handleSuccess request httpResponse = do
  case httpResponse.body |> Json.decode of
    Result.Err parseError ->
      -- JSON parsing failed - invoke onError
      request.onError [fmt|Failed to parse response: #{parseError}|]
    Result.Ok openRouterResponse ->
      request.onSuccess openRouterResponse


-- | Handle HTTP errors by invoking the user's onError callback.
handleError :: forall command. Request command -> Text -> command
handleError request errorText =
  request.onError errorText
