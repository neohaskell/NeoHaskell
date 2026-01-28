-- | Message types for OpenRouter chat completions.
--
-- This module provides the 'Message' and 'Role' types for building
-- conversations with AI models via OpenRouter.
--
-- == Quick Start
--
-- @
-- import Integration.OpenRouter.Message (Message)
-- import Integration.OpenRouter.Message qualified as Message
--
-- messages =
--   [ Message.system "You are a helpful assistant."
--   , Message.user "What is Haskell?"
--   , Message.assistant "Haskell is a functional programming language."
--   , Message.user "Tell me more."
--   ]
-- @
module Integration.OpenRouter.Message
  ( -- * Types
    Message (..)
  , Role (..)

    -- * Smart Constructors
  , user
  , assistant
  , system
  ) where

import Basics
import Json qualified
import Text (Text)


-- | Role of a message participant in the conversation.
--
-- OpenRouter (like OpenAI) uses roles to distinguish between:
--
-- * 'User' - The human asking questions
-- * 'Assistant' - The AI responding
-- * 'System' - Instructions that guide the AI's behavior
data Role
  = User
  | Assistant
  | System
  deriving (Show, Eq, Generic)


instance Json.ToJSON Role where
  toJSON role = do
    let text = case role of
          User -> "user" :: Text
          Assistant -> "assistant"
          System -> "system"
    Json.toJSON text


instance Json.FromJSON Role where
  parseJSON = Json.withText "Role" \text -> do
    case text of
      "user" -> Json.yield User
      "assistant" -> Json.yield Assistant
      "system" -> Json.yield System
      other -> Json.fail [fmt|Unknown role: #{other}|]


-- | A single message in a conversation.
--
-- Messages form the conversation history sent to the model.
-- Use the smart constructors 'user', 'assistant', and 'system'
-- for convenient message creation.
data Message = Message
  { role :: Role
  , content :: Text
  }
  deriving (Show, Eq, Generic)


instance Json.ToJSON Message where
  toJSON message =
    Json.object
      [ ("role", Json.toJSON message.role)
      , ("content", Json.toJSON message.content)
      ]


instance Json.FromJSON Message where
  parseJSON = Json.withObject "Message" \obj -> do
    role <- obj Json..: "role"
    content <- obj Json..: "content"
    Json.yield Message {role, content}


-- | Create a user message.
--
-- @
-- Message.user "What is the meaning of life?"
-- @
user :: Text -> Message
user content = Message {role = User, content}


-- | Create an assistant message.
--
-- Useful for providing example responses or continuing a conversation:
--
-- @
-- Message.assistant "I'd be happy to help with that."
-- @
assistant :: Text -> Message
assistant content = Message {role = Assistant, content}


-- | Create a system message.
--
-- System messages set the AI's behavior and personality:
--
-- @
-- Message.system "You are a helpful coding assistant. Be concise."
-- @
system :: Text -> Message
system content = Message {role = System, content}
