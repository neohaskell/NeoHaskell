-- | Message types for OpenRouter chat completions.
--
-- This module provides the 'Message' and 'Role' types for building
-- conversations with AI models via OpenRouter.
--
-- == Quick Start (Text-only)
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
--
-- == Multimodal (Images\/PDFs)
--
-- @
-- import Integration.OpenRouter.Message qualified as Message
--
-- -- Send a PDF attachment with a text prompt:
-- multimodalMsg = Message.userWithAttachment
--   "Extract all text from this document."
--   base64Content
--   "application\/pdf"
-- @
--
-- == Backward Compatibility
--
-- Text-only messages serialize identically to the previous format:
--
-- * @Message.user \"hello\"@ → @{\"role\":\"user\",\"content\":\"hello\"}@
--
-- Multimodal messages use the OpenAI content array format:
--
-- * @Message.userWithAttachment ...@ → @{\"role\":\"user\",\"content\":[...]}@
module Integration.OpenRouter.Message
  ( -- * Types
    Message (..)
  , Role (..)

    -- * Content Types (Multimodal)
  , Content (..)
  , ContentPart (..)
  , ImageUrl (..)

    -- * Smart Constructors
  , user
  , assistant
  , system

    -- * Multimodal Constructors
  , userWithAttachment
  ) where

import Array (Array)
import Array qualified
import Basics
import Result (Result (..))
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


-- | Image URL reference for multimodal content.
--
-- Used to embed images or documents as data URLs:
--
-- @
-- ImageUrl { url = "data:application\/pdf;base64,..." }
-- @
data ImageUrl = ImageUrl
  { url :: Text
  -- ^ Data URL (e.g., @\"data:application\/pdf;base64,...\"@)
  }
  deriving (Show, Eq, Generic)


instance Json.ToJSON ImageUrl where
  toJSON imageUrl =
    Json.object
      [ ("url", Json.toJSON imageUrl.url)
      ]


instance Json.FromJSON ImageUrl where
  parseJSON = Json.withObject "ImageUrl" \obj -> do
    url <- obj Json..: "url"
    Json.yield ImageUrl {url}


-- | A single part within multimodal content.
--
-- * 'TextPart' — A text segment
-- * 'ImageUrlPart' — An image or document attachment
data ContentPart
  = TextPart Text
  -- ^ Text content part
  | ImageUrlPart ImageUrl
  -- ^ Image\/attachment content part
  deriving (Show, Eq, Generic)


instance Json.ToJSON ContentPart where
  toJSON part = case part of
    TextPart text ->
      Json.object
        [ ("type", Json.toJSON ("text" :: Text))
        , ("text", Json.toJSON text)
        ]
    ImageUrlPart imageUrl ->
      Json.object
        [ ("type", Json.toJSON ("image_url" :: Text))
        , ("image_url", Json.toJSON imageUrl)
        ]


instance Json.FromJSON ContentPart where
  parseJSON = Json.withObject "ContentPart" \obj -> do
    partType <- obj Json..: "type"
    case (partType :: Text) of
      "text" -> do
        text <- obj Json..: "text"
        Json.yield (TextPart text)
      "image_url" -> do
        imageUrl <- obj Json..: "image_url"
        Json.yield (ImageUrlPart imageUrl)
      other ->
        Json.fail [fmt|Unknown content part type: #{other}|]


-- | Message content — either plain text or multimodal content array.
--
-- * 'TextContent' — Plain text (backward compatible with previous API)
-- * 'MultiContent' — Array of content parts for multimodal messages
--
-- == JSON Serialization
--
-- * @TextContent \"hello\"@ → @\"hello\"@ (plain JSON string)
-- * @MultiContent [...]@ → @[{\"type\":\"text\",...}, ...]@ (JSON array)
--
-- 'FromJSON' accepts both formats for backward compatibility.
data Content
  = TextContent Text
  -- ^ Plain text content (backward compatible)
  | MultiContent (Array ContentPart)
  -- ^ Array of content parts (text + images\/attachments)
  deriving (Show, Eq, Generic)


instance Json.ToJSON Content where
  toJSON content = case content of
    TextContent text ->
      Json.toJSON text
    MultiContent parts ->
      Json.toJSON parts


instance Json.FromJSON Content where
  parseJSON value = do
    -- Try parsing as a plain text string first (backward compatible).
    -- If that fails, parse as an array of content parts (multimodal).
    case Json.decode value of
      Ok (text :: Text) ->
        Json.yield (TextContent text)
      Err _ -> do
        parts <- Json.parseJSON value
        Json.yield (MultiContent parts)


-- | A single message in a conversation.
--
-- Messages form the conversation history sent to the model.
-- Use the smart constructors 'user', 'assistant', and 'system'
-- for convenient message creation.
--
-- The 'content' field supports both plain text and multimodal content.
-- Text-only messages serialize identically to the previous format.
data Message = Message
  { role :: Role
  , content :: Content
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
user text = Message {role = User, content = TextContent text}


-- | Create an assistant message.
--
-- Useful for providing example responses or continuing a conversation:
--
-- @
-- Message.assistant "I'd be happy to help with that."
-- @
assistant :: Text -> Message
assistant text = Message {role = Assistant, content = TextContent text}


-- | Create a system message.
--
-- System messages set the AI's behavior and personality:
--
-- @
-- Message.system "You are a helpful coding assistant. Be concise."
-- @
system :: Text -> Message
system text = Message {role = System, content = TextContent text}


-- | Create a multimodal user message with an attachment.
--
-- Builds a message with both a text prompt and an image\/document
-- attachment encoded as a data URL.
--
-- @
-- Message.userWithAttachment
--   "Extract all text from this document."
--   base64Content
--   "application\/pdf"
-- @
{-# INLINE userWithAttachment #-}
userWithAttachment :: Text -> Text -> Text -> Message
userWithAttachment prompt base64Content mimeType = do
  let dataUrl = [fmt|data:#{mimeType};base64,#{base64Content}|]
  let parts = Array.fromLinkedList
        [ TextPart prompt
        , ImageUrlPart ImageUrl {url = dataUrl}
        ]
  Message {role = User, content = MultiContent parts}
