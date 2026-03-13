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
  -- ^ Sampling temperature (0.0-2.0). Lower values produce more
  --   deterministic tool selection. Nothing = model default.
  , maxTokens :: Maybe Int
  -- ^ Maximum tokens in the response. Nothing = model default.
  , timeoutSeconds :: {-# UNPACK #-} Int
  -- ^ Request timeout in seconds. Default: 120.
  }
  deriving (Eq, Generic)
  -- NOTE: Config intentionally does NOT derive Show.
  -- systemPrompt may contain confidential business logic.


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


-- | Default agent configuration.
--
-- * System prompt: None
-- * Temperature: None (model default)
-- * Max tokens: None (model default)
-- * Timeout: 120 seconds
{-# INLINE defaultConfig #-}
defaultConfig :: Config
defaultConfig =
  Config
    { systemPrompt = Nothing
    , temperature = Nothing
    , maxTokens = Nothing
    , timeoutSeconds = 120
    }
