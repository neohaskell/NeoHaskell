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
-- * Always validate the State parameter on callback (CSRF protection)
-- * Never log or serialize 'ClientSecret' or tokens to events
--
-- = See Also
--
-- * 'Auth.SecretStore' - Secure token storage
-- * 'Auth' - Server-side JWT validation (for validating tokens FROM providers)
module Auth.OAuth2 (
  -- * Provider Configuration
  Provider (..),

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

  -- * Errors
  OAuth2Error (..),

  -- * Operations
  authorizeUrl,
  exchangeCode,
  refreshToken,
) where

import Auth.OAuth2.Client (authorizeUrl, exchangeCode, refreshToken)
import Auth.OAuth2.Types (
  AccessToken,
  AuthorizationCode,
  ClientId (..),
  ClientSecret,
  OAuth2Error (..),
  Provider (..),
  RedirectUri,
  RefreshToken,
  Scope (..),
  State,
  TokenSet (..),
  mkAccessToken,
  mkAuthorizationCode,
  mkClientSecret,
  mkRedirectUri,
  mkRefreshToken,
  mkState,
  unwrapAccessToken,
  unwrapAuthorizationCode,
  unwrapClientSecret,
  unwrapRedirectUri,
  unwrapRefreshToken,
  unwrapState,
 )
