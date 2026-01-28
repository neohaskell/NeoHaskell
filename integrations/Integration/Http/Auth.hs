-- | Authentication configuration for HTTP outbound integrations.
--
-- This module provides authentication patterns for external APIs.
-- All values support environment variable expansion using @${VAR_NAME}@ syntax.
--
-- == Security
--
-- Never hardcode secrets in source code. Always use environment variables:
--
-- @
-- -- Good: Secret from environment
-- auth = Http.Bearer "${API_TOKEN}"
--
-- -- Bad: Hardcoded secret
-- auth = Http.Bearer "sk_live_abc123"
-- @
module Integration.Http.Auth
  ( Auth (..)
  ) where

import Basics
import Text (Text)


-- | Authentication configuration with environment variable expansion.
--
-- Values like @"${API_KEY}"@ are expanded from environment at runtime.
-- Missing variables result in 'Integration.AuthenticationError'.
--
-- == Supported Patterns
--
-- * 'NoAuth': No authentication (e.g., public APIs, webhooks with auth in URL)
-- * 'Bearer': OAuth2/JWT Bearer token in Authorization header
-- * 'Basic': HTTP Basic auth with username and password
-- * 'ApiKey': Custom header with API key value
--
-- == Examples
--
-- @
-- -- Bearer token (OAuth2, JWT)
-- auth = Http.Bearer "${GITHUB_TOKEN}"
-- -- Sends: Authorization: Bearer ghp_xxx...
--
-- -- Basic auth
-- auth = Http.Basic "${STRIPE_KEY}" ""
-- -- Sends: Authorization: Basic base64(key:)
--
-- -- API key in custom header
-- auth = Http.ApiKey "X-Api-Key" "${SHIPPO_API_KEY}"
-- -- Sends: X-Api-Key: shippo_xxx...
--
-- -- No auth (webhook with auth in URL)
-- auth = Http.NoAuth
-- @
data Auth
  = -- | No authentication required
    NoAuth
  | -- | Bearer token authentication
    --
    -- Produces: @Authorization: Bearer \<token\>@
    Bearer Text
  | -- | HTTP Basic authentication
    --
    -- Produces: @Authorization: Basic base64(user:pass)@
    Basic
      { username :: Text
      , password :: Text
      }
  | -- | Custom API key header
    --
    -- Produces: @\<headerName\>: \<headerValue\>@
    ApiKey
      { headerName :: Text
      , headerValue :: Text
      }
  deriving (Eq, Generic)


-- | Show instance that redacts sensitive credentials.
--
-- SECURITY: Never expose actual credential values in logs or error messages.
instance Show Auth where
  show NoAuth = "NoAuth"
  show (Bearer _) = "Bearer \"***\""
  show (Basic {}) = "Basic {username = ***, password = ***}"
  show (ApiKey {}) = "ApiKey {headerName = ***, headerValue = ***}"


-- SECURITY: Auth values should never be serialized to JSON as they contain secrets.
-- If you need to serialize auth config, use a separate type without the actual values.
