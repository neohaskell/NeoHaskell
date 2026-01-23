-- | OAuth2 client support for NeoHaskell.
--
-- This module provides client-side OAuth2 functionality for connecting
-- to external APIs (Oura, GitHub, Google, etc.) using the Authorization Code flow.
--
-- = Quick Start
--
-- @
-- import Auth.OAuth2 (Provider (..), ClientId (..), OAuth2)
-- import Auth.OAuth2 qualified as OAuth2
--
-- -- Define your provider
-- ouraProvider :: Provider
-- ouraProvider = Provider
--   { name = "oura"
--   , authorizeEndpoint = "https://cloud.ouraring.com/oauth/authorize"
--   , tokenEndpoint = "https://api.ouraring.com/oauth/token"
--   }
--
-- -- Build authorization URL
-- let authUrl = OAuth2.authorizeUrl ouraProvider clientId redirectUri scopes state
--
-- -- Exchange code for tokens (after callback)
-- tokens <- OAuth2.exchangeCode ouraProvider clientId clientSecret redirectUri code
--
-- -- Refresh when needed
-- newTokens <- OAuth2.refreshToken ouraProvider clientId clientSecret refreshToken
-- @
--
-- = Security Notes
--
-- * Store tokens in 'Auth.SecretStore', NOT in the event store
-- * Always validate the State parameter on callback using 'validateState' (CSRF protection)
-- * Always validate redirect URIs using 'validateRedirectUri' before token exchange
-- * Never log or serialize 'ClientSecret' or tokens to events
--
-- = Secure Callback Handling
--
-- @
-- -- In your callback handler:
-- case OAuth2.validateState expectedState returnedState of
--   Err err -> -- Handle CSRF attack / expired state
--   Ok () -> do
--     -- State is valid, proceed with token exchange
--     tokens <- OAuth2.exchangeCode provider clientId clientSecret redirectUri code
-- @
--
-- = See Also
--
-- * 'Auth.SecretStore' - Secure token storage
-- * 'Auth' - Server-side JWT validation (for validating tokens FROM providers)
module Auth.OAuth2 (
  -- * Provider Configuration
  Provider (..),
  ValidatedProvider,
  getValidatedProvider,

  -- * Credentials
  ClientId (..),
  ClientSecret,
  mkClientSecret,
  unwrapClientSecret,
  RedirectUri,
  mkRedirectUri,
  unwrapRedirectUri,

  -- * Authorization Flow
  AuthorizationCode,
  mkAuthorizationCode,
  unwrapAuthorizationCode,
  State,
  mkState,
  unwrapState,
  Scope (..),

  -- * Tokens
  AccessToken,
  mkAccessToken,
  unwrapAccessToken,
  RefreshToken,
  mkRefreshToken,
  unwrapRefreshToken,
  TokenSet (..),

  -- * PKCE (Proof Key for Code Exchange - RFC 7636)
  CodeVerifier,
  mkCodeVerifier,
  mkCodeVerifierUnsafe,
  unwrapCodeVerifier,
  CodeChallenge (..),
  CodeChallengeMethod (..),
  generateCodeVerifier,
  deriveCodeChallenge,
  authorizeUrlWithPkce,

  -- * Errors
  OAuth2Error (..),

  -- * Validation Helpers
  validateState,
  validateRedirectUri,
  validateProvider,

  -- * Operations
  authorizeUrl,
  exchangeCode,
  exchangeCodeWithPkce,
  refreshToken,

  -- * Operations (Pre-validated - High Performance)
  exchangeCodeValidated,
  exchangeCodeWithPkceValidated,
  refreshTokenValidated,
) where

import Auth.OAuth2.Client (
  authorizeUrl,
  authorizeUrlWithPkce,
  deriveCodeChallenge,
  exchangeCode,
  exchangeCodeValidated,
  exchangeCodeWithPkce,
  exchangeCodeWithPkceValidated,
  generateCodeVerifier,
  refreshToken,
  refreshTokenValidated,
  validateProvider,
 )
import Auth.OAuth2.Types (
  AccessToken,
  AuthorizationCode,
  ClientId (..),
  ClientSecret,
  CodeChallenge (..),
  CodeChallengeMethod (..),
  CodeVerifier,
  OAuth2Error (..),
  Provider (..),
  RedirectUri,
  RefreshToken,
  Scope (..),
  State,
  TokenSet (..),
  ValidatedProvider,
  getValidatedProvider,
  mkAccessToken,
  mkAuthorizationCode,
  mkClientSecret,
  mkCodeVerifier,
  mkCodeVerifierUnsafe,
  mkRedirectUri,
  mkRefreshToken,
  mkState,
  unwrapAccessToken,
  unwrapAuthorizationCode,
  unwrapClientSecret,
  unwrapCodeVerifier,
  unwrapRedirectUri,
  unwrapRefreshToken,
  unwrapState,
  validateRedirectUri,
  validateState,
 )
