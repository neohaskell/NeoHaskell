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
-- let ouraProvider = OAuth2Provider
--       { provider = Provider
--           { name = "oura"
--           , authorizeEndpoint = "https://cloud.ouraring.com/oauth/authorize"
--           , tokenEndpoint = "https://api.ouraring.com/oauth/token"
--           }
--       , clientId = ClientId "your-client-id"
--       , clientSecret = mkClientSecret "your-client-secret"
--       , redirectUri = mkRedirectUri "https://yourapp.com/callback/oura"
--       , scopes = Array.fromLinkedList [Scope "personal"]
--       , onSuccess = \\userId tokens -> emitCommand (StoreOuraTokens userId tokens)
--       , onFailure = \\userId err -> emitCommand (LogOAuthError userId err)
--       , onDisconnect = \\userId -> emitCommand (RevokeOuraAccess userId)
--       , successRedirectUrl = "https://yourapp.com/settings?connected=oura"
--       , failureRedirectUrl = "https://yourapp.com/settings?error=oauth"
--       }
-- @
--
-- = Security
--
-- * All routes require JWT authentication (user must be logged in)
-- * State tokens are HMAC-signed to prevent tampering
-- * PKCE is used for authorization code flow
-- * One-time state consumption prevents replay attacks
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
-- @
-- ouraConfig :: OAuth2ProviderConfig
-- ouraConfig = OAuth2ProviderConfig
--   { provider = Provider
--       { name = "oura"
--       , authorizeEndpoint = "https://cloud.ouraring.com/oauth/authorize"
--       , tokenEndpoint = "https://api.ouraring.com/oauth/token"
--       }
--   , clientId = ClientId "your-client-id"
--   , clientSecret = mkClientSecret "your-client-secret"
--   , redirectUri = myRedirectUri  -- Created with mkRedirectUri
--   , scopes = Array.fromLinkedList [Scope "personal", Scope "daily"]
--   , onSuccess = \\userId tokens -> Json.encodeText (StoreTokens userId tokens)
--   , onFailure = \\userId err -> Json.encodeText (LogError userId err)
--   , onDisconnect = \\userId -> Json.encodeText (RevokeAccess userId)
--   , successRedirectUrl = "https://app.example.com/settings?connected=oura"
--   , failureRedirectUrl = "https://app.example.com/settings?error=oauth"
--   }
-- @
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
    -- Returns JSON-encoded command to dispatch.
    onSuccess :: Text -> TokenSet -> Text
  , -- | Called when OAuth2 flow fails.
    -- First arg is userId (from JWT), second is the error.
    -- Returns JSON-encoded command to dispatch.
    onFailure :: Text -> OAuth2Error -> Text
  , -- | Called when user disconnects the provider.
    -- Arg is userId (from JWT).
    -- Returns JSON-encoded command to dispatch.
    onDisconnect :: Text -> Text
  , -- | URL to redirect user after successful connection
    successRedirectUrl :: Text
  , -- | URL to redirect user after failed connection
    failureRedirectUrl :: Text
  }
