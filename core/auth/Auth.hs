-- | JWT Authentication for NeoHaskell.
--
-- This module provides JWT token validation for event-sourced applications.
-- It's designed to work with external OAuth providers (Keycloak, Auth0, Okta, etc.)
-- using OpenID Connect Discovery.
--
-- = Quick Start
--
-- @
-- app :: Application
-- app =
--   Application.new
--     |> Application.withAuth "https://auth.example.com"
--     |> Application.withService myService
-- @
--
-- = What Jess Sees
--
-- * 'UserClaims' - The authenticated user's identity
-- * 'AuthOptions' - Declarative auth requirements (Everyone, Authenticated, etc.)
-- * 'AuthOverrides' - Optional advanced configuration
--
-- Everything else (JWKS caching, token validation, etc.) is internal.
module Auth (
  -- * User Identity
  UserClaims (..),

  -- * Endpoint Requirements
  AuthOptions (..),

  -- * Configuration
  AuthOverrides (..),
  defaultOverrides,

  -- * Errors (for pattern matching)
  AuthError (..),
) where

import Auth.Claims (UserClaims (..))
import Auth.Config (AuthOverrides (..), defaultOverrides)
import Auth.Error (AuthError (..))
import Auth.Options (AuthOptions (..))
