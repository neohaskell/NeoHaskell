-- | OpenID Connect Discovery and JWKS fetching.
-- This module handles fetching configuration from OAuth providers.
--
-- SECURITY: All URLs are validated to require HTTPS and block SSRF vectors.
-- Optional domain allowlist restricts IdPs to specific trusted domains.
module Auth.Discovery (
  -- * Discovery
  discoverConfig,
  fetchJwks,
  -- * Types
  DiscoveryDocument (..),
) where

import Array (Array)
import Array qualified
import Auth.Config (AuthConfig (..), AuthOverrides (..), defaultAllowedAlgorithms, mkAuthConfig)
import Auth.Error (DiscoveryError (..))
import Auth.UrlValidation (ValidationError (..))
import Auth.UrlValidation qualified as UrlValidation
import Basics
import Crypto.JOSE qualified as Jose
import Http.Client qualified as Http
import Json qualified
import Maybe (Maybe (..))
import Maybe qualified
import Network.URI qualified as URI
import Result (Result (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


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
  validatedDiscoveryUrl <- validateUrlOrThrow discoveryUrl
  validateAllowlistOrThrow validatedDiscoveryUrl overrides.allowedIdpDomains

  -- Fetch discovery document
  discovery <- fetchDiscoveryDocumentOrThrow validatedDiscoveryUrl

  -- SECURITY: Validate the JWKS URI from discovery document
  validatedJwksUri <- validateUrlOrThrow discovery.jwks_uri
  validateAllowlistOrThrow validatedJwksUri overrides.allowedIdpDomains

  -- Build config with discovered values + overrides
  Task.yield (buildAuthConfig discovery validatedJwksUri overrides)


-- | Validate URL or throw a DiscoveryError.
validateUrlOrThrow :: Text -> Task DiscoveryError Text
validateUrlOrThrow url =
  case UrlValidation.validateSecureUrl url of
    Err validationErr -> Task.throw (mapValidationError validationErr)
    Ok validated -> Task.yield validated


-- | Validate against domain allowlist or throw a DiscoveryError.
validateAllowlistOrThrow :: Text -> Maybe (Array Text) -> Task DiscoveryError Unit
validateAllowlistOrThrow url allowlist =
  case validateDomainAllowlist url allowlist of
    Err err -> Task.throw err
    Ok () -> Task.yield ()


-- | Fetch discovery document or throw a DiscoveryError.
fetchDiscoveryDocumentOrThrow :: Text -> Task DiscoveryError DiscoveryDocument
fetchDiscoveryDocumentOrThrow url = do
  result <- fetchDiscoveryDocument url
  case result of
    Err err -> Task.throw err
    Ok doc -> Task.yield doc


-- | Build AuthConfig from discovery document and overrides.
-- Uses mkAuthConfig to precompute allowedAlgorithmsSet for O(1) lookups.
buildAuthConfig :: DiscoveryDocument -> Text -> AuthOverrides -> AuthConfig
buildAuthConfig discovery validatedJwksUri overrides = do
  let algs = overrides.allowedAlgorithms |> Maybe.withDefault defaultAllowedAlgorithms
  mkAuthConfig
    discovery.issuer
    validatedJwksUri
    overrides.audience
    (overrides.permissionsClaim |> Maybe.withDefault "permissions")
    overrides.tenantIdClaim
    (overrides.clockSkewSeconds |> Maybe.withDefault 60)
    900
    60
    86400
    algs
    Array.empty


-- | Map URL validation errors to discovery errors.
mapValidationError :: ValidationError -> DiscoveryError
mapValidationError err =
  case err of
    NotHttps url -> UrlValidationFailed [fmt|URL must use HTTPS: #{url}|]
    PrivateIpBlocked url -> UrlValidationFailed [fmt|URL points to private/loopback IP (SSRF blocked): #{url}|]
    MalformedUrl url -> UrlValidationFailed [fmt|Malformed URL: #{url}|]
    MissingHostname url -> UrlValidationFailed [fmt|URL missing hostname: #{url}|]
    DnsResolutionBlocked url privateIp -> UrlValidationFailed [fmt|DNS resolution blocked - #{url} resolves to private IP: #{privateIp}|]
    DnsResolutionFailed url errMsg -> UrlValidationFailed [fmt|DNS resolution failed for #{url}: #{errMsg}|]


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


-- | Validate URL against domain allowlist.
-- If allowlist is Nothing, all domains are allowed (default behavior).
-- If allowlist is Just [], no domains are allowed (fail-closed).
-- Otherwise, the URL's domain must match one of the allowlisted domains.
-- Comparison is case-insensitive (domains are normalized to lowercase).
validateDomainAllowlist :: Text -> Maybe (Array Text) -> Result DiscoveryError ()
validateDomainAllowlist url maybeAllowlist =
  case maybeAllowlist of
    Nothing -> Ok () -- No allowlist = all domains allowed
    Just allowlist ->
      case extractDomain url of
        Nothing -> Err (UrlValidationFailed [fmt|Cannot extract domain from URL: #{url}|])
        Just domain -> do
          -- Case-insensitive comparison: normalize both to lowercase
          let domainLower = Text.toLower domain
          let isAllowed = Array.any (\allowed -> Text.toLower allowed == domainLower) allowlist
          case isAllowed of
            True -> Ok ()
            False -> Err (UrlValidationFailed [fmt|Domain not in allowlist: #{domain}|])


-- | Extract domain (hostname) from URL.
-- Returns the hostname normalized to lowercase for consistent comparison.
extractDomain :: Text -> Maybe Text
extractDomain url = do
  let urlString = Text.toLinkedList url
  case URI.parseURI urlString of
    Nothing -> Nothing
    Just uri ->
      case URI.uriAuthority uri of
        Nothing -> Nothing
        Just auth -> Just (URI.uriRegName auth |> Text.fromLinkedList |> Text.toLower)
