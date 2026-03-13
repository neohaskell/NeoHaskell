{- HLINT ignore "Redundant id" -}
-- | Response types for OpenRouter chat completions.
--
-- This module provides types for parsing responses from the OpenRouter API.
-- These types are what Jess receives in the @onSuccess@ callback.
--
-- == Response Structure
--
-- A typical response contains:
--
-- * 'id' - Unique generation identifier
-- * 'model' - The actual model that processed the request
-- * 'choices' - Array of completion choices (usually one)
-- * 'usage' - Token usage statistics (optional)
--
-- == Example
--
-- @
-- onSuccess = \\response -> do
--   let firstChoice = response.choices |> Array.first
--   case firstChoice of
--     Just choice -> HandleResponse { content = choice.message.content }
--     Nothing -> HandleError { error = "No choices in response" }
-- @
module Integration.OpenRouter.Response
  ( -- * Response Types
    Response (..)
  , Choice (..)
  , Usage (..)
  , FinishReason (..)

    -- * Tool Call Types
  , ToolCall (..)
  , ToolCallFunction (..)
  ) where

import Array (Array)
import Array qualified
import Basics
import Integration.OpenRouter.Message (Message)
import Json qualified
import Maybe (Maybe)
import Redacted (Redacted)
import Redacted qualified
import Text (Text)
import Prelude qualified


-- | Why the model stopped generating tokens.
--
-- * 'Stop' - Natural completion (hit stop sequence or end of response)
-- * 'Length' - Hit max_tokens limit
-- * 'ContentFilter' - Content was filtered for safety
-- * 'Unknown' - Provider returned an unrecognized reason
data FinishReason
  = Stop
  | Length
  | ContentFilter
  | Unknown Text
  deriving (Show, Eq, Generic)


instance Json.FromJSON FinishReason where
  parseJSON = Json.withText "FinishReason" \text -> do
    let reason = case text of
          "stop" -> Stop
          "length" -> Length
          "content_filter" -> ContentFilter
          other -> Unknown other
    Json.yield reason


instance Json.ToJSON FinishReason where
  toJSON reason = do
    let text = case reason of
          Stop -> "stop" :: Text
          Length -> "length"
          ContentFilter -> "content_filter"
          Unknown other -> other
    Json.toJSON text


-- | Token usage statistics for the request.
--
-- Useful for monitoring costs and optimizing prompts.
--
-- @
-- case response.usage of
--   Just usage -> logUsage usage.totalTokens
--   Nothing -> pure ()
-- @
data Usage = Usage
  { promptTokens :: Int
    -- ^ Tokens used by the input messages
  , completionTokens :: Int
    -- ^ Tokens generated in the response
  , totalTokens :: Int
    -- ^ Total tokens (prompt + completion)
  }
  deriving (Show, Eq, Generic)


instance Json.FromJSON Usage where
  parseJSON = Json.withObject "Usage" \obj -> do
    promptTokens <- obj Json..: "prompt_tokens"
    completionTokens <- obj Json..: "completion_tokens"
    totalTokens <- obj Json..: "total_tokens"
    Json.yield Usage {promptTokens, completionTokens, totalTokens}


instance Json.ToJSON Usage where
  toJSON usage =
    Json.object
      [ ("prompt_tokens", Json.toJSON usage.promptTokens)
      , ("completion_tokens", Json.toJSON usage.completionTokens)
      , ("total_tokens", Json.toJSON usage.totalTokens)
      ]


-- | A function call made by the model.
--
-- NOTE: 'arguments' is wrapped in 'Redacted' because it contains
-- AI-decoded command JSON with user-correlated data (cart IDs, quantities,
-- etc.) that must not appear in logs. Unwrap only inside 'decodeArguments'.
data ToolCallFunction = ToolCallFunction
  { name :: Text
  -- ^ Name of the tool (matches 'CommandTool.toolName')
  , arguments :: Redacted Text
  -- ^ JSON-encoded arguments string, redacted to prevent log leakage.
  --   Unwrap with 'Redacted.unwrap' only inside 'decodeArguments'.
  }
  deriving (Generic)
  -- NOTE: Show intentionally omitted — arguments contain user data.
  -- NOTE: Eq implemented manually below (Redacted has no Eq).


-- | Manual Eq instance for ToolCallFunction.
-- Compares unwrapped arguments explicitly, as per Redacted.hs guidance.
instance Eq ToolCallFunction where
  a == b =
    (a.name Prelude.== b.name)
      Prelude.&& (Redacted.unwrap a.arguments Prelude.== Redacted.unwrap b.arguments)


instance Json.FromJSON ToolCallFunction where
  parseJSON = Json.withObject "ToolCallFunction" \obj -> do
    name <- obj Json..: "name"
    rawArguments <- obj Json..: "arguments"
    let arguments = Redacted.wrap rawArguments
    Json.yield ToolCallFunction {name, arguments}


instance Json.ToJSON ToolCallFunction where
  toJSON tcf =
    Json.object
      [ ("name", Json.toJSON tcf.name)
      , ("arguments", Json.toJSON ("<redacted>" :: Text))
      ]


-- | A tool call returned by the model.
data ToolCall = ToolCall
  { id :: Text
  -- ^ Unique call identifier (e.g., "call_abc123")
  , function :: ToolCallFunction
  -- ^ The function name and encoded arguments
  }
  deriving (Eq, Generic)
  -- NOTE: Show intentionally omitted — inherits sensitive data from ToolCallFunction.


instance Json.FromJSON ToolCall where
  parseJSON = Json.withObject "ToolCall" \obj -> do
    id <- obj Json..: "id"
    function <- obj Json..: "function"
    Json.yield ToolCall {id, function}


instance Json.ToJSON ToolCall where
  toJSON tc =
    Json.object
      [ ("id", Json.toJSON tc.id)
      , ("function", Json.toJSON tc.function)
      ]


-- | A single completion choice from the model.
--
-- Most requests return a single choice.
-- 'toolCalls' is populated for tool_choice = "required" requests,
-- empty for regular chat completion requests (backward compatible).
data Choice = Choice
  { message :: Message
  -- ^ The assistant's response message
  , finishReason :: FinishReason
  -- ^ Why generation stopped
  , index :: {-# UNPACK #-} Int
  -- ^ Index of this choice (0 for single responses)
  , toolCalls :: Array ToolCall
  -- ^ Tool calls made by the model (empty array for non-agent responses,
  --   populated for tool_choice = "required" requests)
  }
  deriving (Eq, Generic)
  -- NOTE: Show removed — toolCalls contains ToolCall which has no Show.


instance Json.FromJSON Choice where
  parseJSON = Json.withObject "Choice" \obj -> do
    message <- obj Json..: "message"
    finishReason <- obj Json..: "finish_reason"
    index <- obj Json..:? "index" Json..!= 0
    toolCalls <- obj Json..:? "tool_calls" Json..!= []  -- backward compatible: [] for non-agent
    Json.yield Choice {message, finishReason, index, toolCalls}


instance Json.ToJSON Choice where
  toJSON choice =
    case Array.length choice.toolCalls == 0 of
      True ->
        Json.object
          [ ("message", Json.toJSON choice.message)
          , ("finish_reason", Json.toJSON choice.finishReason)
          , ("index", Json.toJSON choice.index)
          ]
      False ->
        Json.object
          [ ("message", Json.toJSON choice.message)
          , ("finish_reason", Json.toJSON choice.finishReason)
          , ("index", Json.toJSON choice.index)
          , ("tool_calls", Json.toJSON choice.toolCalls)
          ]


-- | Full response from OpenRouter chat completion API.
--
-- This is what you receive in your @onSuccess@ callback:
--
-- @
-- onSuccess = \\response ->
--   case response.choices |> Array.first of
--     Just choice -> GotAnswer { answer = choice.message.content }
--     Nothing -> NoAnswer
-- @
data Response = Response
  { id :: Text
    -- ^ Unique generation identifier (e.g., "gen-abc123")
  , model :: Text
    -- ^ The model that processed the request
  , choices :: Array Choice
    -- ^ Completion choices (usually one element)
  , usage :: Maybe Usage
    -- ^ Token usage statistics (may be absent)
  }
  deriving (Eq, Generic)
  -- NOTE: Show removed — contains Array Choice which has no Show.


instance Json.FromJSON Response where
  parseJSON = Json.withObject "Response" \obj -> do
    id <- obj Json..: "id"
    model <- obj Json..: "model"
    choices <- obj Json..: "choices"
    usage <- obj Json..:? "usage"
    Json.yield Response {id, model, choices, usage}


instance Json.ToJSON Response where
  toJSON response =
    Json.object
      [ ("id", Json.toJSON response.id)
      , ("model", Json.toJSON response.model)
      , ("choices", Json.toJSON response.choices)
      , ("usage", Json.toJSON response.usage)
      ]
