-- | OpenID Connect Discovery and JWKS fetching.
-- This module handles fetching configuration from OAuth providers.
--
-- SECURITY: All URLs are validated to require HTTPS and block SSRF vectors.
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
import Auth.UrlValidation (ValidationError (..))
import Auth.UrlValidation qualified as UrlValidation
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
-- SECURITY: Validates that authServerUrl uses HTTPS and is not a private IP.
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
  -- SECURITY: Validate the auth server URL
  let discoveryUrl = [fmt|#{authServerUrl}/.well-known/openid-configuration|]
  case UrlValidation.validateSecureUrl discoveryUrl of
    Err validationErr -> Task.throw (mapValidationError validationErr)
    Ok validatedUrl -> do
      -- 1. Fetch discovery document
      discoveryResult <- fetchDiscoveryDocument validatedUrl
      case discoveryResult of
        Err err -> Task.throw err
        Ok discovery -> do
          -- SECURITY: Validate the JWKS URI from discovery document
          case UrlValidation.validateSecureUrl discovery.jwks_uri of
            Err validationErr -> Task.throw (mapValidationError validationErr)
            Ok validatedJwksUri -> do
              -- 2. Build config with discovered values + overrides
              -- Note: JWKS keys are managed by JwksManager, not stored in AuthConfig
              let config =
                    AuthConfig
                      { issuer = discovery.issuer,
                        jwksUri = validatedJwksUri,
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


-- | Map URL validation errors to discovery errors.
mapValidationError :: ValidationError -> DiscoveryError
mapValidationError err =
  case err of
    NotHttps url -> UrlValidationFailed [fmt|URL must use HTTPS: #{url}|]
    PrivateIpBlocked url -> UrlValidationFailed [fmt|URL points to private/loopback IP (SSRF blocked): #{url}|]
    MalformedUrl url -> UrlValidationFailed [fmt|Malformed URL: #{url}|]
    MissingHostname url -> UrlValidationFailed [fmt|URL missing hostname: #{url}|]


-- | Fetch the OpenID Connect discovery document
-- SECURITY: 30 second timeout to prevent indefinite hangs
fetchDiscoveryDocument ::
  Text ->
  Task err (Result DiscoveryError DiscoveryDocument)
fetchDiscoveryDocument url = do
  result <-
    Http.request
      |> Http.withUrl url
      |> Http.withTimeout 30 -- 30 second timeout
      |> Http.get @DiscoveryDocument
      |> Task.map Ok
      |> Task.recover (\(Http.Error msg) -> Task.yield (Err (DiscoveryFetchFailed msg)))

  Task.yield result


-- | Fetch JWKS from the given URI and extract keys.
-- Returns an array of JWKs that can be used for token validation.
--
-- SECURITY: Validates that jwksUri uses HTTPS and is not a private IP.
-- | Fetch JWKS from the given URI and extract keys.
-- SECURITY: 30 second timeout to prevent indefinite hangs
fetchJwks ::
  Text ->
  Task err (Result DiscoveryError (Array Jose.JWK))
fetchJwks jwksUri = do
  -- SECURITY: Defense-in-depth validation (also validated in discoverConfig)
  case UrlValidation.validateSecureUrl jwksUri of
    Err validationErr -> Task.yield (Err (mapValidationError validationErr))
    Ok validatedUri -> do
      result <-
        Http.request
          |> Http.withUrl validatedUri
          |> Http.withTimeout 30 -- 30 second timeout
          |> Http.get @Jose.JWKSet
          |> Task.map (\jwkSet -> extractJwkSetKeys jwkSet |> Array.fromLinkedList |> Ok)
          |> Task.recover (\(Http.Error msg) -> Task.yield (Err (JwksFetchFailed msg)))

      Task.yield result


-- | Extract keys from JWKSet.
-- JWKSet is a newtype, we need to pattern match to get the list.
extractJwkSetKeys :: Jose.JWKSet -> [Jose.JWK]
extractJwkSetKeys (Jose.JWKSet keys) = keys
