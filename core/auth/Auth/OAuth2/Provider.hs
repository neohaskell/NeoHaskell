-- | # Auth.OAuth2.Provider
--
-- OAuth2 Provider configuration for integrating external OAuth2 providers
-- (like Oura, GitHub, Google) into a NeoHaskell application.
--
-- = Overview
--
-- This module provides the configuration types needed to set up OAuth2
-- provider integration. The actual route handlers are in 'Auth.OAuth2.Routes'.
--
-- = Usage
--
-- @
-- -- STEP 1: Define callback command types
-- -- IMPORTANT: Commands store a REFERENCE to tokens, not the tokens themselves.
-- -- TokenSet has no ToJSON instance by design - tokens go to SecretStore.
--
-- data OuraConnected = OuraConnected
--   { odataUserId :: Text
--   , ouraTokenKey :: Text  -- Key for SecretStore lookup
--   }
--   deriving (Generic)
--
-- instance Json.ToJSON OuraConnected
-- type instance NameOf OuraConnected = "OuraConnected"
--
-- data OuraFailed = OuraFailed { userId :: Text, errorMessage :: Text }
--   deriving (Generic)
--
-- instance Json.ToJSON OuraFailed
-- type instance NameOf OuraFailed = "OuraFailed"
--
-- -- STEP 2: Configure the OAuth2 provider
-- ouraConfig :: OAuth2ProviderConfig
-- ouraConfig = OAuth2ProviderConfig
--   { provider = Provider
--       { name = "oura"
--       , authorizeEndpoint = "https://cloud.ouraring.com/oauth/authorize"
--       , tokenEndpoint = "https://api.ouraring.com/oauth/token"
--       }
--   , clientId = ClientId "your-client-id"
--   , clientSecret = mkClientSecret "your-client-secret"
--   , redirectUri = myRedirectUri  -- use Result.unwrap or handle Result from mkRedirectUri
--   , scopes = Array.fromLinkedList [Scope "personal"]
--   , onSuccess = \\userId tokens -> do
--       -- Store tokens in SecretStore (not shown), get back a key
--       let tokenKey = [fmt|oauth:oura:#{userId}|]
--       Integration.encodeCommand (OuraConnected userId tokenKey)
--   , onFailure = \\userId err -> do
--       let errMsg = show err |> Text.fromLinkedList
--       Integration.encodeCommand (OuraFailed userId errMsg)
--   , onDisconnect = \\userId -> Integration.encodeCommand (OuraDisconnected userId)
--   , successRedirectUrl = "https://yourapp.com/settings?connected=oura"
--   , failureRedirectUrl = "https://yourapp.com/settings?error=oauth"
--   }
-- @
--
-- = Callback Format
--
-- The callback functions ('onSuccess', 'onFailure', 'onDisconnect') must return
-- JSON text in 'Integration.CommandPayload' format. Use 'Integration.encodeCommand'
-- to create this automatically from your command types.
--
-- The JSON format is: @{ "commandType": "YourCommand", "commandData": {...} }@
--
-- = Token Security
--
-- __IMPORTANT__: 'TokenSet', 'AccessToken', and 'RefreshToken' intentionally have
-- NO 'ToJSON' instances. This is compile-time protection against token leakage.
--
-- * Store tokens in 'Auth.SecretStore', NOT in event stores or commands
-- * Commands should contain a reference key, not actual tokens
-- * If you try to serialize 'TokenSet' directly, you'll get a compile error
--
-- = Security
--
-- * All routes require JWT authentication (user must be logged in)
-- * State tokens are HMAC-signed to prevent tampering
-- * PKCE is used for authorization code flow
-- * One-time state consumption prevents replay attacks
-- * Token types have no ToJSON (compile-time serialization protection)
module Auth.OAuth2.Provider (
  -- * Configuration
  OAuth2ProviderConfig (..),
  ValidatedOAuth2ProviderConfig (..),

  -- * Callback Actions
  OAuth2Action (..),
) where

import Array (Array)
import Auth.OAuth2.Types (
  ClientId,
  ClientSecret,
  OAuth2Error,
  Provider,
  RedirectUri,
  Scope,
  TokenSet,
  ValidatedProvider,
 )
import Basics
import Text (Text)


-- | Action returned by OAuth2 callbacks to be dispatched by the application.
--
-- These actions are type-erased commands that the application can dispatch
-- to its command handlers. This allows OAuth2 integration without coupling
-- to specific domain types.
data OAuth2Action
  = -- | Success action with JSON-encoded command
    SuccessAction Text
  | -- | Failure action with JSON-encoded command
    FailureAction Text
  | -- | Disconnect action with JSON-encoded command
    DisconnectAction Text
  deriving (Generic, Show, Eq)


-- | Runtime-validated OAuth2 provider configuration.
--
-- This is created by validating an 'OAuth2ProviderConfig' at application startup.
-- The 'ValidatedProvider' field guarantees that endpoint URLs have been
-- validated for HTTPS and SSRF protection.
--
-- This type is used internally by Routes to enable high-throughput token
-- operations (50k+ req/s) without repeated endpoint validation.
data ValidatedOAuth2ProviderConfig = ValidatedOAuth2ProviderConfig
  { -- | The original unvalidated provider (for authorize URL generation)
    provider :: Provider
  , -- | Pre-validated provider for token operations
    validatedProvider :: ValidatedProvider
  , -- | OAuth2 client ID
    clientId :: ClientId
  , -- | OAuth2 client secret
    clientSecret :: ClientSecret
  , -- | Callback URL
    redirectUri :: RedirectUri
  , -- | Scopes to request
    scopes :: Array Scope
  , -- | Success callback
    onSuccess :: Text -> TokenSet -> Text
  , -- | Failure callback
    onFailure :: Text -> OAuth2Error -> Text
  , -- | Disconnect callback
    onDisconnect :: Text -> Text
  , -- | Success redirect URL
    successRedirectUrl :: Text
  , -- | Failure redirect URL
    failureRedirectUrl :: Text
  }


-- | Configuration for an OAuth2 provider integration.
--
-- This configures both the OAuth2 flow parameters and the application-level
-- callbacks for success/failure handling.
--
-- The callbacks must return JSON in 'Integration.CommandPayload' format.
-- Use 'Integration.encodeCommand' to encode your command types correctly.
--
-- __Note__: The 'onSuccess' callback receives 'TokenSet' but your command
-- should NOT contain the tokens directly (they have no ToJSON instance).
-- Instead, store tokens in 'SecretStore' and put a reference key in your command.
data OAuth2ProviderConfig = OAuth2ProviderConfig
  { -- | OAuth2 provider endpoints
    provider :: Provider
  , -- | OAuth2 client ID (from provider dashboard)
    clientId :: ClientId
  , -- | OAuth2 client secret (from provider dashboard, keep secure!)
    clientSecret :: ClientSecret
  , -- | Callback URL (must match provider dashboard configuration)
    redirectUri :: RedirectUri
  , -- | Scopes to request from the provider
    scopes :: Array Scope
  , -- | Called on successful token exchange.
    -- First arg is userId (from JWT), second is the token set.
    -- Returns JSON via 'Integration.encodeCommand'.
    onSuccess :: Text -> TokenSet -> Text
  , -- | Called when OAuth2 flow fails.
    -- First arg is userId (from JWT), second is the error.
    -- Returns JSON via 'Integration.encodeCommand'.
    onFailure :: Text -> OAuth2Error -> Text
  , -- | Called when user disconnects the provider.
    -- Arg is userId (from JWT).
    -- Returns JSON via 'Integration.encodeCommand'.
    onDisconnect :: Text -> Text
  , -- | URL to redirect user after successful connection
    successRedirectUrl :: Text
  , -- | URL to redirect user after failed connection
    failureRedirectUrl :: Text
  }
