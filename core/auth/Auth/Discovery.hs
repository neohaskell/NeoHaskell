-- | OpenID Connect Discovery and JWKS fetching.
-- This module handles fetching configuration from OAuth providers.
module Auth.Discovery (
  -- * Discovery
  discoverConfig,
  fetchJwks,
  -- * Types
  DiscoveryDocument (..),
) where

import Array (Array)
import Array qualified
import Auth.Config (AuthConfig (..), AuthOverrides (..), defaultAllowedAlgorithms)
import Auth.Error (DiscoveryError (..))
import Basics
import Crypto.JOSE qualified as Jose
import Http.Client qualified as Http
import Json qualified
import Maybe qualified
import Result (Result (..))
import Task (Task)
import Task qualified
import Text (Text)


-- | OpenID Connect Discovery document structure.
-- We only parse the fields we need.
data DiscoveryDocument = DiscoveryDocument
  { issuer :: Text,
    jwks_uri :: Text
  }
  deriving (Generic, Show)


instance Json.FromJSON DiscoveryDocument


instance Json.ToJSON DiscoveryDocument


-- | Discover auth configuration from an OAuth provider's base URL.
-- Fetches the OpenID Connect discovery document and JWKS keys.
--
-- Example:
-- @
-- config <- discoverConfig "https://auth.example.com" defaultOverrides
-- @
discoverConfig ::
  Text ->
  AuthOverrides ->
  Task DiscoveryError AuthConfig
discoverConfig authServerUrl overrides = do
  -- 1. Fetch discovery document
  let discoveryUrl = [fmt|#{authServerUrl}/.well-known/openid-configuration|]

  discoveryResult <- fetchDiscoveryDocument discoveryUrl
  case discoveryResult of
    Err err -> Task.throw err
    Ok discovery -> do
      -- 2. Build config with discovered values + overrides
      -- Note: JWKS keys are managed by JwksManager, not stored in AuthConfig
      let config =
            AuthConfig
              { issuer = discovery.issuer,
                jwksUri = discovery.jwks_uri,
                audience = overrides.audience,
                permissionsClaim = overrides.permissionsClaim |> Maybe.withDefault "permissions",
                tenantIdClaim = overrides.tenantIdClaim,
                clockSkewSeconds = overrides.clockSkewSeconds |> Maybe.withDefault 60,
                refreshIntervalSeconds = 900,
                missingKidCooldownSeconds = 60,
                maxStaleSeconds = 86400,
                allowedAlgorithms =
                  overrides.allowedAlgorithms
                    |> Maybe.withDefault defaultAllowedAlgorithms,
                supportedCritHeaders = Array.empty
              }

      Task.yield config


-- | Fetch the OpenID Connect discovery document
fetchDiscoveryDocument ::
  Text ->
  Task err (Result DiscoveryError DiscoveryDocument)
fetchDiscoveryDocument url = do
  result <-
    Http.request
      |> Http.withUrl url
      |> Http.get @DiscoveryDocument
      |> Task.map Ok
      |> Task.recover (\(Http.Error msg) -> Task.yield (Err (DiscoveryFetchFailed msg)))

  Task.yield result


-- | Fetch JWKS from the given URI and extract keys.
-- Returns an array of JWKs that can be used for token validation.
fetchJwks ::
  Text ->
  Task err (Result DiscoveryError (Array Jose.JWK))
fetchJwks jwksUri = do
  result <-
    Http.request
      |> Http.withUrl jwksUri
      |> Http.get @Jose.JWKSet
      |> Task.map (\jwkSet -> extractJwkSetKeys jwkSet |> Array.fromLinkedList |> Ok)
      |> Task.recover (\(Http.Error msg) -> Task.yield (Err (JwksFetchFailed msg)))

  Task.yield result


-- | Extract keys from JWKSet.
-- JWKSet is a newtype, we need to pattern match to get the list.
extractJwkSetKeys :: Jose.JWKSet -> [Jose.JWK]
extractJwkSetKeys (Jose.JWKSet keys) = keys
