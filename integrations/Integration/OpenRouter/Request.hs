-- | Request types for OpenRouter chat completions.
--
-- This module provides the 'Request' type that Jess configures
-- to make chat completion requests via OpenRouter.
--
-- == Quick Start
--
-- @
-- import Integration qualified
-- import Integration.OpenRouter qualified as OpenRouter
-- import Integration.OpenRouter.Message qualified as Message
--
-- -- Simple usage with smart constructor:
-- OpenRouter.chatCompletion
--   [Message.user "Hello!"]
--   "anthropic/claude-3.5-sonnet"
--   (\\response -> GotResponse {response})
--   (\\err -> GotError {err})
--   |> Integration.outbound
-- @
--
-- == Configuration
--
-- For advanced options like temperature and max tokens, use the
-- 'Request' record directly:
--
-- @
-- Integration.outbound OpenRouter.Request
--   { messages = myMessages
--   , model = "openai/gpt-4o"
--   , config = OpenRouter.defaultConfig
--       { temperature = Just 0.9
--       , maxTokens = Just 500
--       }
--   , onSuccess = handleSuccess
--   , onError = handleError
--   }
-- @
module Integration.OpenRouter.Request
  ( -- * Request Configuration
    Request (..)
  , Config (..)
  , defaultConfig

    -- * Smart Constructor
  , chatCompletion
  ) where

import Array (Array)
import Basics
import Integration.OpenRouter.Message (Message)
import Integration.OpenRouter.Response (Response)
import Maybe (Maybe (..))
import Text (Text)


-- | Optional configuration for chat completion requests.
--
-- Use 'defaultConfig' as a starting point and override fields as needed:
--
-- @
-- config = OpenRouter.defaultConfig
--   { temperature = Just 0.7
--   , maxTokens = Just 1000
--   }
-- @
data Config = Config
  { temperature :: Maybe Float
    -- ^ Sampling temperature (0.0-2.0). Higher = more creative.
    -- Default: model-dependent (usually 1.0)
  , maxTokens :: Maybe Int
    -- ^ Maximum tokens to generate. Default: model-dependent
  , topP :: Maybe Float
    -- ^ Nucleus sampling parameter (0.0-1.0). Default: 1.0
  , frequencyPenalty :: Maybe Float
    -- ^ Penalize repeated tokens based on frequency (-2.0 to 2.0)
  , presencePenalty :: Maybe Float
    -- ^ Penalize tokens based on presence in text so far (-2.0 to 2.0)
  , referer :: Maybe Text
    -- ^ HTTP-Referer header for OpenRouter rankings (your site URL)
  , title :: Maybe Text
    -- ^ X-Title header for OpenRouter rankings (your app name)
  , timeoutSeconds :: Int
    -- ^ Request timeout in seconds. Default: 60
  }
  deriving (Show, Eq, Generic)


-- | Sensible default configuration.
--
-- All optional parameters are 'Nothing' (use model defaults).
-- Timeout is set to 60 seconds.
--
-- @
-- config = OpenRouter.defaultConfig { temperature = Just 0.5 }
-- @
defaultConfig :: Config
defaultConfig =
  Config
    { temperature = Nothing
    , maxTokens = Nothing
    , topP = Nothing
    , frequencyPenalty = Nothing
    , presencePenalty = Nothing
    , referer = Nothing
    , title = Nothing
    , timeoutSeconds = 60
    }


-- | Chat completion request configuration.
--
-- This is what Jess (the user) builds. The integration transforms this
-- into an 'Http.Request' via 'toHttpRequest'.
--
-- == Fields
--
-- * 'messages' - The conversation history (required)
-- * 'model' - Model identifier like @"anthropic/claude-3.5-sonnet"@ (required)
-- * 'config' - Optional parameters (temperature, max_tokens, etc.)
-- * 'onSuccess' - Command to emit when API returns successfully (required)
-- * 'onError' - Command to emit on failure (required)
--
-- == Example
--
-- @
-- Integration.outbound OpenRouter.Request
--   { messages = [Message.system "Be concise.", Message.user question]
--   , model = "anthropic/claude-3.5-sonnet"
--   , config = OpenRouter.defaultConfig
--   , onSuccess = \\response -> GotAnswer { response }
--   , onError = \\err -> AiError { err }
--   }
-- @
data Request command = Request
  { messages :: Array Message
    -- ^ The conversation history
  , model :: Text
    -- ^ Model identifier (e.g., "anthropic/claude-3.5-sonnet", "openai/gpt-4o")
  , config :: Config
    -- ^ Optional parameters (temperature, max_tokens, etc.)
  , onSuccess :: Response -> command
    -- ^ Command to emit when the API returns successfully
  , onError :: Text -> command
    -- ^ Command to emit on failure (network error, parse error, API error)
  }


-- | Smart constructor for creating a basic chat completion request.
--
-- Uses 'defaultConfig' for optional parameters. For custom configuration,
-- use the 'Request' record constructor directly.
--
-- @
-- request = OpenRouter.chatCompletion
--   [Message.user "Hello!"]
--   "anthropic/claude-3.5-sonnet"
--   (\\response -> GotResponse {response})
--   (\\err -> GotError {err})
-- @
chatCompletion ::
  forall command.
  Array Message ->
  Text ->
  (Response -> command) ->
  (Text -> command) ->
  Request command
chatCompletion messages model onSuccess onError =
  Request
    { messages
    , model
    , config = defaultConfig
    , onSuccess
    , onError
    }
