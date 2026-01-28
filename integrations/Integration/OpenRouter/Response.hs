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
  ) where

import Array (Array)
import Basics
import Integration.OpenRouter.Message (Message)
import Json qualified
import Maybe (Maybe)
import Text (Text)


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


-- | A single completion choice from the model.
--
-- Most requests return a single choice, but the API supports
-- multiple choices via the @n@ parameter (not exposed in v1).
data Choice = Choice
  { message :: Message
    -- ^ The assistant's response message
  , finishReason :: FinishReason
    -- ^ Why generation stopped
  , index :: Int
    -- ^ Index of this choice (0 for single responses)
  }
  deriving (Show, Eq, Generic)


instance Json.FromJSON Choice where
  parseJSON = Json.withObject "Choice" \obj -> do
    message <- obj Json..: "message"
    finishReason <- obj Json..: "finish_reason"
    index <- obj Json..:? "index" Json..!= 0
    Json.yield Choice {message, finishReason, index}


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
  deriving (Show, Eq, Generic)


instance Json.FromJSON Response where
  parseJSON = Json.withObject "Response" \obj -> do
    id <- obj Json..: "id"
    model <- obj Json..: "model"
    choices <- obj Json..: "choices"
    usage <- obj Json..:? "usage"
    Json.yield Response {id, model, choices, usage}
