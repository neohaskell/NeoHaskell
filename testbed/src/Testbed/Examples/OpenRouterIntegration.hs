-- | Example of using the OpenRouter integration.
--
-- This demonstrates how Jess (the integration user) would use the
-- OpenRouter API in a real application.
--
-- == Usage Pattern
--
-- @
-- -- In your service's integrations:
-- myIntegration entity event =
--   case event of
--     NeedAiHelp question -> askAi question
--     _ -> Integration.none
-- @
module Testbed.Examples.OpenRouterIntegration
  ( -- * Example Events
    AiEvent (..)

    -- * Integration Helpers
  , askAi
  , askAiWithConfig
  ) where

import Array qualified
import Basics
import Integration.Http qualified as Http
import Integration.OpenRouter qualified as OpenRouter
import Integration.OpenRouter.Message qualified as Message
import Json qualified
import Maybe (Maybe (..))
import Text (Text)


-- | Events that can be emitted by AI interactions.
data AiEvent
  = AiAnswered
      { answer :: Text
      , model :: Text
      , tokensUsed :: Int
      }
  | AiFailed
      { error :: Text
      }
  deriving (Eq, Show, Generic)


instance Json.ToJSON AiEvent
instance Json.FromJSON AiEvent


-- | Simple helper to ask a question using Claude 3.5 Sonnet.
--
-- This is the simplest way Jess can use the OpenRouter integration.
--
-- @
-- askAi "What is the capital of France?" |> Integration.outbound
-- @
askAi :: Text -> Http.Request AiEvent
askAi question =
  OpenRouter.chatCompletion
    [Message.user question]
    "anthropic/claude-3.5-sonnet"
    handleSuccess
    handleError
    |> OpenRouter.toHttpRequest


-- | Ask a question with a custom system prompt and model.
--
-- @
-- askAiWithConfig
--   "You are a pirate."
--   "openai/gpt-4o"
--   "How are you?"
--   |> Integration.outbound
-- @
askAiWithConfig :: Text -> Text -> Text -> Http.Request AiEvent
askAiWithConfig systemPrompt model question =
  OpenRouter.toHttpRequest
    OpenRouter.Request
      { messages =
          [ Message.system systemPrompt
          , Message.user question
          ]
      , model = model
      , config = OpenRouter.defaultConfig
      , onSuccess = handleSuccess
      , onError = handleError
      }


-- | Handle successful API response.
handleSuccess :: OpenRouter.Response -> AiEvent
handleSuccess response =
  case response.choices |> Array.first of
    Just choice ->
      AiAnswered
        { answer = choice.message.content
        , model = response.model
        , tokensUsed = case response.usage of
            Just usage -> usage.totalTokens
            Nothing -> 0
        }
    Nothing ->
      AiFailed {error = "No choices in response"}


-- | Handle API errors.
handleError :: Text -> AiEvent
handleError err = AiFailed {error = err}
