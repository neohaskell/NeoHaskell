-- | Example of using the OpenRouter integration.
--
-- This demonstrates how Jess (the integration user) would use the
-- OpenRouter API in a real application.
--
-- == Simple Usage (Recommended)
--
-- @
-- import Integration qualified
-- import Integration.OpenRouter qualified as OpenRouter
-- import Integration.OpenRouter.Message qualified as Message
--
-- -- In your integrations, use the smart constructor directly:
-- myIntegration entity event =
--   case event of
--     NeedAiHelp question ->
--       OpenRouter.chatCompletion
--         [Message.user question]
--         "anthropic/claude-3.5-sonnet"
--         (\\response -> GotAiResponse {response})
--         (\\err -> AiError {err})
--         |> Integration.outbound
--         |> Integration.batch . Array.singleton
--     _ -> Integration.none
-- @
--
-- == Advanced: Helper Functions
--
-- For more complex scenarios, you can create helper functions that return
-- the intermediate 'Http.Request' type. This is useful for testing or
-- when you need to compose multiple transformations.
module Testbed.Examples.OpenRouterIntegration
  ( -- * Example Events
    AiEvent (..)

    -- * Helper Functions (Advanced)
    -- | These helpers return 'Http.Request' for composition.
    -- For simple use cases, use 'OpenRouter.chatCompletion' directly.
  , askAi
  , askAiWithConfig
  ) where

import Array qualified
import Basics
import Integration.Http qualified as Http
import Integration.OpenRouter qualified as OpenRouter
import Integration.OpenRouter.Message (Content (..), ContentPart (..))
import Integration.OpenRouter.Message qualified as Message
import Json qualified
import Maybe (Maybe (..))
import Text qualified
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


-- | Helper that returns an 'Http.Request' for composition.
--
-- For simple use cases, use the smart constructor directly:
--
-- @
-- OpenRouter.chatCompletion
--   [Message.user "What is the capital of France?"]
--   "anthropic/claude-3.5-sonnet"
--   handleSuccess
--   handleError
--   |> Integration.outbound
-- @
--
-- This helper is useful when you need the intermediate 'Http.Request',
-- for example in tests or when composing with other HTTP transformations.
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
-- For simple use cases, use the Request record directly:
--
-- @
-- Integration.outbound OpenRouter.Request
--   { messages = [Message.system "You are a pirate.", Message.user "How are you?"]
--   , model = "openai/gpt-4o"
--   , config = OpenRouter.defaultConfig
--   , onSuccess = handleSuccess
--   , onError = handleError
--   }
-- @
--
-- This helper returns 'Http.Request' for advanced composition scenarios.
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
    Just choice -> do
      let answerText = case choice.message.content of
            TextContent text -> text
            MultiContent parts ->
              parts
                |> Array.map (\part -> case part of
                    TextPart text -> text
                    ImageUrlPart _ -> "")
                |> Text.concat
      AiAnswered
        { answer = answerText
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
