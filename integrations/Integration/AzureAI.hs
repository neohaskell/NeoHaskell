-- | Azure AI Foundry chat completions integration for NeoHaskell.
--
-- Provides access to Azure AI Foundry's Model Inference API for chat completions,
-- including GPT-4o and other Azure-hosted models.
--
-- == Quick Start
--
-- @
-- import Integration qualified
-- import Integration.AzureAI qualified as AzureAI
-- import Integration.AzureAI.Message qualified as Message
--
-- case AzureAI.azureEndpoint "https://my-res.openai.azure.com" of
--   Result.Ok endpoint ->
--     AzureAI.chatCompletion
--       endpoint
--       [Message.system "Be concise.", Message.user question]
--       "gpt-4o"
--       (\\response -> GotAnswer { response })
--       (\\err -> AiError { err })
--       |> Integration.outbound
--   Result.Err reason ->
--     Integration.none
-- @
--
-- == Configuration
--
-- Add to your @Config.hs@:
--
-- @
-- Config.field @(Redacted Text) "azureAiApiKey"
--   |> Config.doc "Azure AI API key (from the Azure Portal)"
--   |> Config.required
--   |> Config.envVar "AZURE_AI_API_KEY"
--   |> Config.secret
-- @
module Integration.AzureAI
  ( -- * Endpoint validation
    AzureEndpoint
  , azureEndpoint
  , azureEndpointAllowing
  , defaultAzureHostSuffixes

    -- * Request building
  , chatCompletion
  , Request (..)
  , Config (..)
  , defaultConfig

    -- * Advanced escape hatch
  , toHttpRequest

    -- * Response types (reused from OpenRouter, no new definitions)
  , Response (..)
  , Choice (..)
  , Usage (..)
  , FinishReason (..)

    -- * Message types (reused from OpenRouter, no new definitions)
  , Message (..)
  , Role (..)
  , Content (..)
  , ContentPart (..)
  , ImageUrl (..)
  , user
  , assistant
  , system
  , userWithAttachment
  ) where

import Integration.AzureAI.Internal (toHttpRequest)
import Integration.AzureAI.Request (AzureEndpoint, Config (..), Request (..), azureEndpoint, azureEndpointAllowing, chatCompletion, defaultAzureHostSuffixes, defaultConfig)
import Integration.OpenRouter.Message (Content (..), ContentPart (..), ImageUrl (..), Message (..), Role (..), assistant, system, user, userWithAttachment)
import Integration.OpenRouter.Response (Choice (..), FinishReason (..), Response (..), Usage (..))
