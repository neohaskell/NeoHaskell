-- | # OpenRouter API Integration
--
-- This integration provides access to 300+ AI models through the
-- OpenRouter API, piggybacking on the existing HTTP integration.
--
-- == Two-Persona Model
--
-- Following the NeoHaskell integration pattern:
--
-- * __Jess (Integration User)__: Configures requests with 'Request' records.
--   No HTTP details, no JSON encoding, no error handling boilerplate.
--
-- * __Nick (Integration Developer)__: Implements transformation to 'Http.Request'
--   in "Integration.OpenRouter.Internal".
--
-- == Quick Start
--
-- @
-- import Integration qualified
-- import Integration.OpenRouter qualified as OpenRouter
-- import Integration.OpenRouter.Message qualified as Message
--
-- -- In your integrations:
-- askAi :: Text -> Integration.Action
-- askAi question = do
--   let request = OpenRouter.chatCompletion
--         { messages =
--             [ Message.system "You are a helpful assistant."
--             , Message.user question
--             ]
--         , model = "anthropic/claude-3.5-sonnet"
--         , onSuccess = \\response -> GotAiResponse {response}
--         , onError = \\err -> AiError {err}
--         }
--   Integration.outbound (OpenRouter.toHttpRequest request)
-- @
--
-- == Configuration
--
-- Set your API key in the environment:
--
-- > export OPENROUTER_API_KEY=sk-or-v1-xxxxx
--
-- == Available Models
--
-- See <https://openrouter.ai/models> for the full list. Common choices:
--
-- * @"anthropic/claude-3.5-sonnet"@ - Fast, capable
-- * @"openai/gpt-4o"@ - OpenAI's latest
-- * @"meta-llama/llama-3.1-70b-instruct"@ - Open source
-- * @"google/gemini-pro-1.5"@ - Google's multimodal model
--
-- == Advanced Configuration
--
-- @
-- OpenRouter.Request
--   { messages = conversationHistory
--   , model = "openai/gpt-4o"
--   , config = OpenRouter.defaultConfig
--       { temperature = Just 0.9      -- More creative
--       , maxTokens = Just 2000       -- Longer responses
--       , referer = Just "https://myapp.com"  -- For rankings
--       , title = Just "MyApp"        -- For rankings
--       }
--   , onSuccess = handleResponse
--   , onError = handleError
--   }
-- @
--
-- == Known Limitations (v1)
--
-- * No streaming support
-- * No tool/function calling
-- * No multimodal (images) support
--
-- These will be added in future versions.
module Integration.OpenRouter
  ( -- * Request Building
    Request (..)
  , Config (..)
  , defaultConfig
  , chatCompletion

    -- * Response Types
  , Response (..)
  , Choice (..)
  , Usage (..)
  , FinishReason (..)

    -- * Message Types
  , Message (..)
  , Role (..)
  , user
  , assistant
  , system

    -- * Transformation
  , toHttpRequest
  ) where

import Integration.OpenRouter.Internal (toHttpRequest)
import Integration.OpenRouter.Message (Message (..), Role (..), assistant, system, user)
import Integration.OpenRouter.Request (Config (..), Request (..), chatCompletion, defaultConfig)
import Integration.OpenRouter.Response (Choice (..), FinishReason (..), Response (..), Usage (..))
