{-# LANGUAGE ImplicitParams #-}

-- | Request types and endpoint validation for Azure AI chat completions.
--
-- Provides 'AzureEndpoint' (validated by construction), 'Config', 'Request',
-- and the 'chatCompletion' smart constructor for the integration's callers.
module Integration.AzureAI.Request
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

    -- * Internal helpers (exported for tests and Internal.hs)
  , endpointUrl
  , hostOf
  ) where

import Array (Array)
import Array qualified
import Basics
import Integration.OpenRouter.Message (Message)
import Integration.OpenRouter.Response (Response)
import Maybe (Maybe (..))
import Maybe qualified
import Redacted (Redacted)
import Result (Result (..))
import Text (Text)
import Text qualified


-- | A validated Azure resource endpoint (https:// + allowed host suffix).
-- The constructor is NOT exported: use 'azureEndpoint' or 'azureEndpointAllowing'.
-- SEC-001: closed by construction — no external caller can forge a host.
newtype AzureEndpoint = AzureEndpoint Text
  deriving (Show, Eq)


-- | Transport and tuning configuration for an Azure AI chat-completion request.
data Config = Config
  { temperature      :: Maybe Float
    -- ^ Sampling temperature (omitted from the body when Nothing)
  , maxTokens        :: Maybe Int
    -- ^ Max tokens to generate (omitted from the body when Nothing)
  , topP             :: Maybe Float
    -- ^ Nucleus sampling (omitted from the body when Nothing)
  , frequencyPenalty :: Maybe Float
    -- ^ Frequency penalty (omitted from the body when Nothing)
  , presencePenalty  :: Maybe Float
    -- ^ Presence penalty (omitted from the body when Nothing)
  , endpoint         :: AzureEndpoint
    -- ^ Validated Azure resource endpoint (seeded by chatCompletion)
  , apiVersion       :: Text
    -- ^ API version, e.g. "2024-10-21"
  , timeoutSeconds   :: Int
    -- ^ Request timeout in seconds; default 60
  }
  deriving (Show, Eq, Generic)


-- | The request the caller builds; consumed by Integration.outbound.
-- 'apiKey' is 'Redacted Text', sourced from ?config by 'chatCompletion'.
-- No Show/ToJSON — the unwrapped key can never leak via a derived instance.
data Request command = Request
  { messages  :: Array Message
  , model     :: Text
  , config    :: Config
  , apiKey    :: Redacted Text
  , onSuccess :: Response -> command
  , onError   :: Text -> command
  }


-- | Module-private placeholder; always overwritten by 'chatCompletion'.
-- Built with the in-scope raw constructor — legal only inside Request.hs.
-- SEC-001: raw constructor stays module-private. The placeholder "" matches
-- no allowlist suffix and produces a non-resolvable URL (fails closed).
unsetEndpoint :: AzureEndpoint
unsetEndpoint = AzureEndpoint ""


-- | Default allowed host suffixes: public Azure AI hosts plus sovereign clouds.
--
-- @
-- -- openai.azure.com, cognitiveservices.azure.com, services.ai.azure.com,
-- -- inference.ai.azure.com, azure.us (Government), azure.cn (21Vianet)
-- AzureAI.defaultAzureHostSuffixes
-- @
defaultAzureHostSuffixes :: Array Text
defaultAzureHostSuffixes =
  Array.fromLinkedList
    [ "openai.azure.com"
    , "cognitiveservices.azure.com"
    , "services.ai.azure.com"
    , "inference.ai.azure.com"
    , "azure.us"
    , "azure.cn"
    ]


-- | Extract the host from an https URL, stripping scheme, path, query, and fragment.
-- SEC-002: strips '?' and '#' before the suffix match so query/fragment-embedded
-- allowed suffixes cannot smuggle a host past 'endsWith'.
hostOf :: Text -> Result Text Text
hostOf url =
  do
    let afterScheme = url |> Text.replace "https://" ""
    let host =
          afterScheme
            |> Text.split "/" |> Array.first |> Maybe.withDefault afterScheme
            |> Text.split "?" |> Array.first |> Maybe.withDefault afterScheme
            |> Text.split "#" |> Array.first |> Maybe.withDefault afterScheme
    if host == ""
      then Result.Err "Azure endpoint has no host"
      else Result.Ok host


-- | Build an endpoint validated against the DEFAULT Azure host suffixes.
-- Delegates to 'azureEndpointAllowing' with no extra suffixes.
--
-- @
-- case AzureAI.azureEndpoint "https://my-res.openai.azure.com" of
--   Result.Ok endpoint -> useEndpoint endpoint
--   Result.Err reason  -> logRejected reason
-- @
azureEndpoint :: Text -> Result Text AzureEndpoint
azureEndpoint raw =
  azureEndpointAllowing Array.empty raw


-- | Same validation, plus caller-supplied ADDITIONAL host suffixes for APIM
-- custom domains / Private Endpoints. The single Result-returning construction path.
-- SEC-001: https scheme enforced; host must end with an allowlisted suffix.
-- SEC-002: 'hostOf' strips query/fragment before the suffix match.
--
-- @
-- AzureAI.azureEndpointAllowing
--   ["ai.contoso-internal.net"]
--   "https://gateway.ai.contoso-internal.net"
-- @
azureEndpointAllowing :: Array Text -> Text -> Result Text AzureEndpoint
azureEndpointAllowing extraSuffixes raw =
  do
    let trimmed = Text.trim raw
    let allowed = defaultAzureHostSuffixes |> Array.append extraSuffixes
    if Text.toLower trimmed |> Text.startsWith "https://"
      then
        case hostOf (Text.toLower trimmed) of
          Result.Err reason ->
            Result.Err reason
          Result.Ok host ->
            -- Label-boundary match: the host must equal an allowed suffix or end
            -- with ".<suffix>". A plain 'Text.endsWith suffix host' would accept
            -- lookalikes such as "notazure.us" for the suffix "azure.us" (an
            -- attacker-registrable domain), leaking the api-key. SEC-001.
            if allowed |> Array.any (\suffix -> host == suffix || Text.endsWith [fmt|.#{suffix}|] host)
              then Result.Ok (AzureEndpoint trimmed)
              else Result.Err "Azure endpoint host is not in the allowed Azure host suffix set"
      else
        Result.Err "Azure endpoint must use https:// (refusing to send the api-key over cleartext)"


-- | Primary smart constructor. Sources the API key from ?config.azureAiApiKey;
-- seeds Config.endpoint from the validated AzureEndpoint argument; apiVersion +
-- tuning come from defaultConfig (override via the Request/Config record).
--
-- @
-- case AzureAI.azureEndpoint "https://my-res.openai.azure.com" of
--   Result.Ok endpoint ->
--     AzureAI.chatCompletion
--       endpoint
--       [AzureAI.system "Be concise.", AzureAI.user question]
--       "gpt-4o"
--       (\\response -> GotAnswer { response })
--       (\\err -> AiError { err })
--       |> Integration.outbound
--   Result.Err reason ->
--     Integration.none
-- @
chatCompletion ::
  forall command config.
  ( ?config :: config
  , HasField "azureAiApiKey" config (Redacted Text)
  ) =>
  AzureEndpoint ->           -- ^ validated Azure resource endpoint (distinct type — not Text)
  Array Message ->           -- ^ conversation history
  Text ->                    -- ^ deployment / model name, e.g. "gpt-4o"
  (Response -> command) ->   -- ^ onSuccess callback
  (Text -> command) ->       -- ^ onError callback
  Request command
chatCompletion endpointVal messagesVal modelVal onSuccess onError =
  Request
    { messages = messagesVal
    , model = modelVal
    , config = defaultConfig { endpoint = endpointVal }
    , apiKey = ?config.azureAiApiKey
    , onSuccess
    , onError
    }


-- | Tuning + transport defaults: all sampling knobs Nothing, pinned apiVersion, 60s timeout.
-- The 'endpoint' field is seeded with a module-private placeholder that
-- 'chatCompletion' ALWAYS overwrites (see note in the architecture doc).
--
-- @
-- AzureAI.defaultConfig { temperature = Just 0.5 }
-- @
defaultConfig :: Config
defaultConfig =
  Config
    { temperature = Nothing
    , maxTokens = Nothing
    , topP = Nothing
    , frequencyPenalty = Nothing
    , presencePenalty = Nothing
    , endpoint = unsetEndpoint          -- module-private placeholder; always overwritten
    , apiVersion = "2024-10-21"
    , timeoutSeconds = 60
    }


-- | Read the (non-secret) URL out of a validated endpoint. This is a plain
-- newtype projection, NOT a secret unwrap — the https+allowlist guarantee holds
-- by construction, so reading the string is safe.
-- Exported for Internal.hs and tests; NOT re-exported from the facade.
endpointUrl :: AzureEndpoint -> Text
endpointUrl endpointVal =
  case endpointVal of
    AzureEndpoint url ->
      url
