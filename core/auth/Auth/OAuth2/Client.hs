-- | OAuth2 client operations for Authorization Code flow.
--
-- This module provides the core functions needed to:
--
-- 1. Build authorization URLs (redirect user to provider)
-- 2. Exchange authorization codes for tokens
-- 3. Refresh access tokens
--
-- = Usage
--
-- @
-- -- 1. Generate PKCE verifier and redirect user to authorize
-- verifier <- OAuth2.generateCodeVerifier
-- let challenge = OAuth2.deriveCodeChallenge verifier
-- let authUrl = OAuth2.authorizeUrlWithPkce provider clientId redirectUri scopes state challenge S256
-- -- Store verifier in session, redirect user to authUrl...
--
-- -- 2. On callback, validate state and exchange code for tokens
-- case OAuth2.validateState expectedState returnedState of
--   Err err -> -- handle CSRF attack
--   Ok () -> do
--     tokens <- OAuth2.exchangeCodeWithPkce provider clientId clientSecret redirectUri code verifier
--     -- Store tokens in SecretStore...
--
-- -- 3. Later, refresh the token
-- newTokens <- OAuth2.refreshToken provider clientId clientSecret tokens.refreshToken
-- @
--
-- = Security
--
-- * Always use PKCE ('authorizeUrlWithPkce', 'exchangeCodeWithPkce') when possible
-- * Always validate the 'State' parameter on callback using 'validateState' (CSRF protection)
-- * Always validate redirect URIs using 'validateRedirectUri' before token exchange
-- * Store tokens in 'Auth.SecretStore', NOT in event store
-- * Never log full authorization URLs (they contain state parameter)
-- * Encrypt tokens at rest
--
-- = Deployment Requirements
--
-- __CRITICAL__: Disable HTTP proxy environment variables in production:
--
-- @
-- unset HTTP_PROXY HTTPS_PROXY http_proxy https_proxy
-- @
--
-- Proxies can bypass SSRF protections by routing validated URLs through
-- attacker-controlled infrastructure. Even with URL validation, a malicious
-- proxy can redirect requests to internal IPs.
--
-- Alternatively, configure your deployment to block outbound proxy usage
-- at the network level for OAuth2 token endpoints.
module Auth.OAuth2.Client (
  -- * Provider Validation
  validateProvider,

  -- * Authorization
  authorizeUrl,
  authorizeUrlWithPkce,

  -- * Token Exchange (with per-request validation)
  exchangeCode,
  exchangeCodeWithPkce,

  -- * Token Exchange (pre-validated provider - faster)
  exchangeCodeValidated,
  exchangeCodeWithPkceValidated,

  -- * Token Refresh (with per-request validation)
  refreshToken,

  -- * Token Refresh (pre-validated provider - faster)
  refreshTokenValidated,

  -- * PKCE Helpers
  generateCodeVerifier,
  deriveCodeChallenge,
) where

import Array (Array)
import Array qualified
import Auth.OAuth2.Types (
  AuthorizationCode,
  ClientId (..),
  ClientSecret,
  CodeChallenge (..),
  CodeChallengeMethod (..),
  CodeVerifier,
  OAuth2Error (..),
  Provider (..),
  RedirectUri,
  unwrapRedirectUri,
  RefreshToken,
  Scope (..),
  State,
  TokenSet (..),
  ValidatedProvider,
  getValidatedProvider,
  mkAccessToken,
  mkCodeVerifierUnsafe,
  mkRefreshToken,
  unsafeValidatedProvider,
  unwrapAuthorizationCode,
  unwrapClientSecret,
  unwrapCodeVerifier,
  unwrapRefreshToken,
  unwrapState,
 )
import Auth.UrlValidation (ValidationError (..))
import Auth.UrlValidation qualified as UrlValidation
import Basics
import Bytes qualified
import Log qualified
import Char (Char)
import Crypto.Hash qualified as Hash
import LinkedList qualified
import Crypto.Random qualified as Random
import Data.ByteArray.Encoding qualified as Encoding
import Data.ByteString qualified as BS
import Http.Client qualified as Http
import Json qualified
import Maybe (Maybe (..))
import Maybe qualified
import Result (Result (..))
import Network.URI qualified as URI
import System.IO qualified as GhcIO
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText ()


-- | Build an OAuth2 authorization URL.
--
-- The user should be redirected to this URL to begin the OAuth2 flow.
--
-- @
-- let state = State "random-csrf-token"
-- let scopes = [Scope "read", Scope "write"]
-- let url = OAuth2.authorizeUrl ouraProvider clientId redirectUri scopes state
-- -- Redirect user to url...
-- @
--
-- SECURITY: The 'State' parameter MUST be:
--
-- * Cryptographically random
-- * Stored in session before redirect
-- * Validated on callback before calling 'exchangeCode'
authorizeUrl ::
  Provider ->
  ClientId ->
  RedirectUri ->
  Array Scope ->
  State ->
  Text
authorizeUrl provider clientIdVal redirectUriVal scopes stateVal = do
  let extraParams = Array.fromLinkedList []
  buildAuthorizeUrl provider clientIdVal redirectUriVal scopes stateVal extraParams


-- | Exchange an authorization code for tokens.
--
-- Call this after the user is redirected back to your callback URL
-- with an authorization code.
--
-- @
-- tokens <- OAuth2.exchangeCode provider clientId clientSecret redirectUri code
-- -- Store tokens in SecretStore...
-- @
--
-- SECURITY: Validate the 'State' parameter BEFORE calling this function.
exchangeCode ::
  Provider ->
  ClientId ->
  ClientSecret ->
  RedirectUri ->
  AuthorizationCode ->
  Task OAuth2Error TokenSet
exchangeCode provider clientIdVal clientSecretVal redirectUriVal codeVal = do
  let tokenEndpoint = provider.tokenEndpoint
  let (ClientId clientId) = clientIdVal
  let clientSecret = unwrapClientSecret clientSecretVal
  let redirectUri = unwrapRedirectUri redirectUriVal
  let code = unwrapAuthorizationCode codeVal
  let formParams =
        [ ("grant_type", "authorization_code")
        , ("code", code)
        , ("redirect_uri", redirectUri)
        , ("client_id", clientId)
        , ("client_secret", clientSecret)
        ]
  requestTokens tokenEndpoint formParams


-- | Refresh an access token using a refresh token.
--
-- Call this when the access token is expired or about to expire.
--
-- @
-- case tokens.refreshToken of
--   Just rt -> do
--     newTokens <- OAuth2.refreshToken provider clientId clientSecret rt
--     -- Update stored tokens...
--   Nothing -> -- Cannot refresh, need re-authorization
-- @
--
-- NOTE: Some providers rotate refresh tokens. Always store the new
-- 'refreshToken' from the response, even if it looks the same.
refreshToken ::
  Provider ->
  ClientId ->
  ClientSecret ->
  RefreshToken ->
  Task OAuth2Error TokenSet
refreshToken provider clientIdVal clientSecretVal refreshTokenVal = do
  let tokenEndpoint = provider.tokenEndpoint
  let (ClientId clientId) = clientIdVal
  let clientSecret = unwrapClientSecret clientSecretVal
  let refresh = unwrapRefreshToken refreshTokenVal
  let formParams =
        [ ("grant_type", "refresh_token")
        , ("refresh_token", refresh)
        , ("client_id", clientId)
        , ("client_secret", clientSecret)
        ]
  requestTokens tokenEndpoint formParams


-- ============================================================================
-- Provider Validation (for high-throughput scenarios)
-- ============================================================================

-- | Validate a provider's endpoints once, returning a 'ValidatedProvider'.
--
-- Use this at application startup to pre-validate OAuth2 providers.
-- Then use the 'exchangeCodeValidated', 'exchangeCodeWithPkceValidated',
-- and 'refreshTokenValidated' functions for high-throughput token operations
-- without repeated endpoint validation.
--
-- SECURITY: Validates both authorize and token endpoints for:
--
-- * HTTPS requirement (tokens contain secrets)
-- * No private/loopback IPs (SSRF protection)
-- * DNS resolution check (rebinding attack prevention)
--
-- @
-- -- At application startup
-- validatedProvider <- OAuth2.validateProvider ouraProvider
--
-- -- Later, in request handlers (50k req/s without validation overhead)
-- tokens <- OAuth2.exchangeCodeValidated validatedProvider clientId clientSecret redirectUri code
-- @
--
-- NOTE: Provider endpoint validation happens ONCE here, not on every token request.
-- If provider endpoints change (rare), restart the application to re-validate.
validateProvider :: Provider -> Task OAuth2Error ValidatedProvider
validateProvider provider = do
  -- Validate authorize endpoint
  authResult <- UrlValidation.validateSecureUrlWithDns provider.authorizeEndpoint
  case authResult of
    Err validationError -> do
      let errMsg = sanitizeValidationError validationError
      Task.throw (EndpointValidationFailed errMsg)
    Ok _ -> do
      -- Validate token endpoint
      tokenResult <- UrlValidation.validateSecureUrlWithDns provider.tokenEndpoint
      case tokenResult of
        Err validationError -> do
          let errMsg = sanitizeValidationError validationError
          Task.throw (EndpointValidationFailed errMsg)
        Ok _ -> Task.yield (unsafeValidatedProvider provider)


-- | Exchange an authorization code for tokens using a pre-validated provider.
--
-- This is the high-performance variant of 'exchangeCode' - it skips
-- endpoint URL validation since the provider was already validated
-- via 'validateProvider'.
--
-- Use this for high-throughput scenarios (50k+ req/s) where repeated
-- DNS validation would be a bottleneck.
--
-- @
-- -- Provider validated at startup
-- tokens <- OAuth2.exchangeCodeValidated validatedProvider clientId clientSecret redirectUri code
-- @
--
-- SECURITY: The 'ValidatedProvider' type ensures endpoints were validated.
exchangeCodeValidated ::
  ValidatedProvider ->
  ClientId ->
  ClientSecret ->
  RedirectUri ->
  AuthorizationCode ->
  Task OAuth2Error TokenSet
exchangeCodeValidated validatedProvider clientIdVal clientSecretVal redirectUriVal codeVal = do
  let provider = getValidatedProvider validatedProvider
  let tokenEndpoint = provider.tokenEndpoint
  let (ClientId clientId) = clientIdVal
  let clientSecret = unwrapClientSecret clientSecretVal
  let redirectUri = unwrapRedirectUri redirectUriVal
  let code = unwrapAuthorizationCode codeVal
  let formParams =
        [ ("grant_type", "authorization_code")
        , ("code", code)
        , ("redirect_uri", redirectUri)
        , ("client_id", clientId)
        , ("client_secret", clientSecret)
        ]
  requestTokensValidated tokenEndpoint formParams


-- | Exchange an authorization code with PKCE using a pre-validated provider.
--
-- High-performance variant of 'exchangeCodeWithPkce' - skips endpoint
-- URL validation since the provider was already validated.
--
-- @
-- tokens <- OAuth2.exchangeCodeWithPkceValidated validatedProvider clientId clientSecret redirectUri code verifier
-- @
exchangeCodeWithPkceValidated ::
  ValidatedProvider ->
  ClientId ->
  ClientSecret ->
  RedirectUri ->
  AuthorizationCode ->
  CodeVerifier ->
  Task OAuth2Error TokenSet
exchangeCodeWithPkceValidated validatedProvider clientIdVal clientSecretVal redirectUriVal codeVal verifierVal = do
  let provider = getValidatedProvider validatedProvider
  let tokenEndpoint = provider.tokenEndpoint
  let (ClientId clientId) = clientIdVal
  let clientSecret = unwrapClientSecret clientSecretVal
  let redirectUri = unwrapRedirectUri redirectUriVal
  let code = unwrapAuthorizationCode codeVal
  let verifier = unwrapCodeVerifier verifierVal
  let formParams =
        [ ("grant_type", "authorization_code")
        , ("code", code)
        , ("redirect_uri", redirectUri)
        , ("client_id", clientId)
        , ("client_secret", clientSecret)
        , ("code_verifier", verifier)
        ]
  requestTokensValidated tokenEndpoint formParams


-- | Refresh an access token using a pre-validated provider.
--
-- High-performance variant of 'refreshToken' - skips endpoint
-- URL validation since the provider was already validated.
--
-- @
-- newTokens <- OAuth2.refreshTokenValidated validatedProvider clientId clientSecret refreshToken
-- @
refreshTokenValidated ::
  ValidatedProvider ->
  ClientId ->
  ClientSecret ->
  RefreshToken ->
  Task OAuth2Error TokenSet
refreshTokenValidated validatedProvider clientIdVal clientSecretVal refreshTokenVal = do
  let provider = getValidatedProvider validatedProvider
  let tokenEndpoint = provider.tokenEndpoint
  let (ClientId clientId) = clientIdVal
  let clientSecret = unwrapClientSecret clientSecretVal
  let refresh = unwrapRefreshToken refreshTokenVal
  let formParams =
        [ ("grant_type", "refresh_token")
        , ("refresh_token", refresh)
        , ("client_id", clientId)
        , ("client_secret", clientSecret)
        ]
  requestTokensValidated tokenEndpoint formParams


-- ============================================================================
-- Internal helpers
-- ============================================================================

-- | Build an authorization URL with the given parameters.
-- Used by both authorizeUrl and authorizeUrlWithPkce.
buildAuthorizeUrl ::
  Provider ->
  ClientId ->
  RedirectUri ->
  Array Scope ->
  State ->
  Array (Text, Text) ->
  Text
buildAuthorizeUrl provider clientIdVal redirectUriVal scopes stateVal extraParams = do
  let authEndpoint = provider.authorizeEndpoint
  let (ClientId clientId) = clientIdVal
  let redirectUri = unwrapRedirectUri redirectUriVal
  let state = unwrapState stateVal
  let scopeText =
        scopes
          |> Array.map (\(Scope s) -> s)
          |> Text.joinWith " "
  let baseParams :: Array (Text, Text) =
        Array.fromLinkedList
          [ ("response_type", "code")
          , ("client_id", clientId)
          , ("redirect_uri", redirectUri)
          , ("state", state)
          , ("scope", scopeText)
          ]
  let params = baseParams |> Array.append extraParams
  let queryString = encodeQueryParams params
  [fmt|#{authEndpoint}?#{queryString}|]


-- | Encode query parameters as a URL-encoded query string.
encodeQueryParams :: Array (Text, Text) -> Text
encodeQueryParams params = do
  let encodeParam :: (Text, Text) -> Text
      encodeParam (key, val) = do
        let encodedVal = urlEncode val
        [fmt|#{key}=#{encodedVal}|]
  params
    |> Array.map encodeParam
    |> Text.joinWith "&"


-- ============================================================================
-- PKCE (Proof Key for Code Exchange) - RFC 7636
-- ============================================================================

-- | Generate a cryptographically secure code verifier for PKCE.
--
-- Returns a 43-character base64url-encoded random string, suitable for
-- the code_verifier parameter in PKCE flows.
--
-- @
-- verifier <- OAuth2.generateCodeVerifier
-- let challenge = OAuth2.deriveCodeChallenge verifier
-- let authUrl = OAuth2.authorizeUrlWithPkce provider clientId redirectUri scopes state challenge S256
-- @
generateCodeVerifier ::
  forall error.
  Task error CodeVerifier
generateCodeVerifier = do
  -- Generate 32 random bytes -> 43 characters when base64url encoded
  randomBytes <- Task.fromIO (Random.getRandomBytes 32 :: GhcIO.IO BS.ByteString)
  let encoded = Encoding.convertToBase Encoding.Base64URLUnpadded randomBytes
  let verifierText = Bytes.fromLegacy encoded |> Text.fromBytes
  Task.yield (mkCodeVerifierUnsafe verifierText)


-- | Derive a code challenge from a code verifier using S256 method.
--
-- Computes: base64url(sha256(code_verifier))
--
-- This is the recommended method per RFC 7636.
deriveCodeChallenge :: CodeVerifier -> CodeChallenge
deriveCodeChallenge verifierVal = do
  let verifier = unwrapCodeVerifier verifierVal
  let verifierBytes = Text.toBytes verifier |> Bytes.unwrap
  let hashDigest = Hash.hashWith Hash.SHA256 verifierBytes
  let challengeBytes = Encoding.convertToBase Encoding.Base64URLUnpadded hashDigest
  let challengeText = Bytes.fromLegacy challengeBytes |> Text.fromBytes
  CodeChallenge challengeText


-- | Build an OAuth2 authorization URL with PKCE support.
--
-- Use this instead of 'authorizeUrl' when the provider supports PKCE
-- (recommended for all public clients, optional for confidential clients).
--
-- @
-- verifier <- OAuth2.generateCodeVerifier
-- let challenge = OAuth2.deriveCodeChallenge verifier
-- let authUrl = OAuth2.authorizeUrlWithPkce provider clientId redirectUri scopes state challenge S256
-- -- Store verifier securely for use in exchangeCodeWithPkce
-- @
authorizeUrlWithPkce ::
  Provider ->
  ClientId ->
  RedirectUri ->
  Array Scope ->
  State ->
  CodeChallenge ->
  CodeChallengeMethod ->
  Text
authorizeUrlWithPkce provider clientIdVal redirectUriVal scopes stateVal challengeVal method = do
  let (CodeChallenge challenge) = challengeVal
  let methodText = case method of
        S256 -> "S256"
        Plain -> "plain"
  let pkceParams =
        Array.fromLinkedList
          [ ("code_challenge", challenge)
          , ("code_challenge_method", methodText)
          ]
  buildAuthorizeUrl provider clientIdVal redirectUriVal scopes stateVal pkceParams


-- | Exchange an authorization code for tokens with PKCE verification.
--
-- Use this when the authorization was initiated with 'authorizeUrlWithPkce'.
-- The code verifier must be the same one used to generate the code challenge.
--
-- @
-- -- Retrieve the verifier stored during authorization
-- tokens <- OAuth2.exchangeCodeWithPkce provider clientId clientSecret redirectUri code verifier
-- @
exchangeCodeWithPkce ::
  Provider ->
  ClientId ->
  ClientSecret ->
  RedirectUri ->
  AuthorizationCode ->
  CodeVerifier ->
  Task OAuth2Error TokenSet
exchangeCodeWithPkce provider clientIdVal clientSecretVal redirectUriVal codeVal verifierVal = do
  let tokenEndpoint = provider.tokenEndpoint
  let (ClientId clientId) = clientIdVal
  let clientSecret = unwrapClientSecret clientSecretVal
  let redirectUri = unwrapRedirectUri redirectUriVal
  let code = unwrapAuthorizationCode codeVal
  let verifier = unwrapCodeVerifier verifierVal
  let formParams =
        [ ("grant_type", "authorization_code")
        , ("code", code)
        , ("redirect_uri", redirectUri)
        , ("client_id", clientId)
        , ("client_secret", clientSecret)
        , ("code_verifier", verifier)
        ]
  requestTokens tokenEndpoint formParams


-- ============================================================================
-- Internal
-- ============================================================================

-- | Internal token response from OAuth2 provider.
-- Uses snake_case to match OAuth2 spec field names.
-- SECURITY: No Show instance - contains access_token/refresh_token secrets.
data TokenResponse = TokenResponse
  { access_token :: Text
  , refresh_token :: Maybe Text
  , expires_in :: Maybe Int
  , token_type :: Maybe Text
  }
  deriving (Generic)


instance Json.FromJSON TokenResponse


-- | OAuth2 error response from provider (RFC 6749 Section 5.2).
-- Used to parse 4xx error responses from token endpoints.
-- Uses snake_case to match OAuth2 spec field names.
data TokenErrorResponse = TokenErrorResponse
  { error :: Text
  , error_description :: Maybe Text
  , error_uri :: Maybe Text
  }
  deriving (Generic)


instance Json.FromJSON TokenErrorResponse


-- | Map OAuth2 error code to our error type.
-- RFC 6749 defines standard error codes for token endpoint errors.
mapOAuthErrorCode :: Text -> Maybe Text -> OAuth2Error
mapOAuthErrorCode errorCode maybeDescription = do
  let description = maybeDescription |> Maybe.withDefault errorCode
  case errorCode of
    "invalid_grant" -> InvalidGrant description
    "invalid_client" -> InvalidClient description
    "invalid_scope" -> ScopeDenied description
    "unauthorized_client" -> InvalidClient description
    "invalid_request" -> TokenRequestFailed description
    "unsupported_grant_type" -> TokenRequestFailed description
    _ -> TokenRequestFailed description


-- | Make a token request to the provider.
-- SECURITY: Validates endpoint URL for HTTPS and SSRF protection with DNS resolution.
-- Parses OAuth2 error responses (RFC 6749) into specific error types.
requestTokens ::
  Text ->
  [(Text, Text)] ->
  Task OAuth2Error TokenSet
requestTokens tokenEndpoint formParams = do
  -- CRITICAL: Validate endpoint URL before making request
  -- 1. Must use HTTPS (tokens contain secrets)
  -- 2. Must not be private/loopback IP (SSRF protection)
  -- 3. DNS resolution checked for rebinding attacks
  validationResult <- UrlValidation.validateSecureUrlWithDns tokenEndpoint
  case validationResult of
    Err validationError -> do
      -- SECURITY: Sanitize error message - don't expose full URL (may have secrets in query)
      let errMsg = sanitizeValidationError validationError
      Task.throw (EndpointValidationFailed errMsg)
    Ok _ -> do
      let request =
            Http.request
              |> Http.withUrl tokenEndpoint
              |> Http.withTimeout 10 -- 10s timeout for OAuth2 token requests
      let formArray = formParams |> Array.fromLinkedList
      result <-
        Http.postForm @TokenResponse request formArray
          |> Task.mapError parseHttpError
          |> Task.asResult
      case result of
        Err err -> Task.throw err
        Ok response -> do
          Task.yield
            TokenSet
              { accessToken = mkAccessToken response.body.access_token
              , refreshToken = response.body.refresh_token |> fmap mkRefreshToken
              , expiresInSeconds = response.body.expires_in
              }


-- | Make a token request to a pre-validated provider endpoint.
-- SECURITY: Endpoint URL validation is SKIPPED because ValidatedProvider
-- guarantees the URL was validated at construction time.
-- This is the high-performance path for 50k+ req/s scenarios.
--
-- AUDIT: Logs security events for EU/GDPR compliance:
-- * Token request attempts (with sanitized endpoint)
-- * Token request failures (with error category, no secrets)
-- * Token request successes (no token values logged)
requestTokensValidated ::
  Text ->
  [(Text, Text)] ->
  Task OAuth2Error TokenSet
requestTokensValidated tokenEndpoint formParams = do
  -- AUDIT: Log token request attempt (sanitized - no secrets)
  let sanitizedEndpoint = sanitizeUrlForAudit tokenEndpoint
  Log.withScope [("component", "OAuth2"), ("endpoint", sanitizedEndpoint)] do
    Log.info "Token request (pre-validated)"
      |> Task.ignoreError
  let request =
        Http.request
          |> Http.withUrl tokenEndpoint
          |> Http.withTimeout 10 -- 10s timeout for OAuth2 token requests
  let formArray = formParams |> Array.fromLinkedList
  result <-
    Http.postForm @TokenResponse request formArray
      |> Task.mapError parseHttpError
      |> Task.asResult
  case result of
    Err err -> do
      -- AUDIT: Log failure (error category only, no secrets)
      Log.withScope [("component", "OAuth2"), ("endpoint", sanitizedEndpoint), ("errorCategory", errorCategory err)] do
        Log.warn "Token request failed"
          |> Task.ignoreError
      Task.throw err
    Ok response -> do
      -- AUDIT: Log success (no token values)
      Log.withScope [("component", "OAuth2"), ("endpoint", sanitizedEndpoint)] do
        Log.info "Token request succeeded"
          |> Task.ignoreError
      Task.yield
        TokenSet
          { accessToken = mkAccessToken response.body.access_token
          , refreshToken = response.body.refresh_token |> fmap mkRefreshToken
          , expiresInSeconds = response.body.expires_in
          }


-- | Sanitize URL for audit logs - extract only scheme://host:port
-- SECURITY: Never log full URLs which may contain query parameters with secrets
sanitizeUrlForAudit :: Text -> Text
sanitizeUrlForAudit urlText = do
  let urlString = Text.toLinkedList urlText
  case URI.parseURI urlString of
    Nothing -> "<invalid-url>"
    Just uri -> do
      let scheme = Text.fromLinkedList (URI.uriScheme uri)
      case URI.uriAuthority uri of
        Nothing -> scheme
        Just auth -> do
          let host = Text.fromLinkedList (URI.uriRegName auth)
          let port = Text.fromLinkedList (URI.uriPort auth)
          Text.concat [scheme, "//", host, port]


-- | Extract error category for audit logging (no secrets)
errorCategory :: OAuth2Error -> Text
errorCategory err = case err of
  TokenRequestFailed _ -> "token_request_failed"
  InvalidGrant _ -> "invalid_grant"
  InvalidClient _ -> "invalid_client"
  ScopeDenied _ -> "scope_denied"
  MalformedResponse _ -> "malformed_response"
  NetworkError _ -> "network_error"
  EndpointValidationFailed _ -> "endpoint_validation_failed"
  InvalidState _ -> "invalid_state"
  InvalidPkceVerifier _ -> "invalid_pkce_verifier"
  InvalidRedirectUri _ -> "invalid_redirect_uri"


-- | Parse HTTP error to OAuth2Error, attempting to extract OAuth2 error response.
-- If the error message contains OAuth2 error JSON, parse it to specific error type.
-- Otherwise, return a generic NetworkError.
parseHttpError :: Http.Error -> OAuth2Error
parseHttpError httpError =
  case httpError of
    Http.Error msg ->
      -- Try to find and parse OAuth2 error JSON in the error message
      -- HTTP client errors for status codes include the response body
      -- Note: This is a best-effort parse - if it fails, we fall back to NetworkError
      case parseOAuthErrorFromMessage msg of
        Just oauthError -> oauthError
        Nothing -> NetworkError msg


-- | Attempt to extract OAuth2 error from error message.
-- The HTTP library includes response body in some error messages.
parseOAuthErrorFromMessage :: Text -> Maybe OAuth2Error
parseOAuthErrorFromMessage msg = do
  -- Look for JSON object pattern in the message
  -- OAuth2 errors have format: {"error":"...", "error_description":"..."}
  let msgStr = Text.toLinkedList msg
  case findJsonInMessage msgStr of
    Nothing -> Nothing
    Just jsonStr -> do
      case Json.decodeText (Text.fromLinkedList jsonStr) of
        Err _ -> Nothing
        Ok (errorResponse :: TokenErrorResponse) ->
          Just (mapOAuthErrorCode errorResponse.error errorResponse.error_description)


-- | Find a JSON object in a message string (simple heuristic).
findJsonInMessage :: [Char] -> Maybe [Char]
findJsonInMessage [] = Nothing
findJsonInMessage ('{' : rest) = do
  -- Found opening brace, try to find matching close
  case findClosingBrace rest 1 [] of
    Just jsonChars -> Just ('{' : jsonChars)
    Nothing -> findJsonInMessage rest
findJsonInMessage (_ : rest) = findJsonInMessage rest


-- | Find closing brace, tracking nesting depth.
-- Accumulates characters in reverse order, then reverses at the end.
findClosingBrace :: [Char] -> Int -> [Char] -> Maybe [Char]
findClosingBrace [] _ _ = Nothing
findClosingBrace (c : rest) depth acc = do
  let newAcc = c : acc -- Build in reverse for efficiency
  case c of
    '{' -> findClosingBrace rest (depth + 1) newAcc
    '}' -> do
      case depth == 1 of
        True -> Just (LinkedList.reverse newAcc)
        False -> findClosingBrace rest (depth - 1) newAcc
    _ -> findClosingBrace rest depth newAcc


-- | URL encoding (percent-encoding) using Network.URI.
-- Uses the standard library for correctness with all characters including
-- UTF-8, percent signs, and other edge cases.
urlEncode :: Text -> Text
urlEncode text = do
  let charString = Text.toLinkedList text
  let encoded = URI.escapeURIString URI.isUnreserved charString
  Text.fromLinkedList encoded


-- | Sanitize ValidationError to avoid exposing full URLs in error messages.
-- SECURITY: URLs may contain sensitive query params (state, tokens, etc.)
-- We only expose the error category and sanitized host, not full URL.
sanitizeValidationError :: ValidationError -> Text
sanitizeValidationError err = case err of
  NotHttps url -> Text.append "URL must use HTTPS: " (sanitizeUrlForError url)
  PrivateIpBlocked url -> Text.append "URL points to private IP: " (sanitizeUrlForError url)
  MalformedUrl _ -> "Malformed URL"
  MissingHostname _ -> "URL missing hostname"
  DnsResolutionBlocked url _ -> Text.append "DNS resolved to private IP: " (sanitizeUrlForError url)
  -- SECURITY: Don't expose DNS resolution failure reasons - may leak infrastructure info
  DnsResolutionFailed url _ -> Text.append "DNS resolution failed for " (sanitizeUrlForError url)
  DnsResolutionTimeout url -> Text.append "DNS resolution timed out for " (sanitizeUrlForError url)
  SingleLabelHostname _ -> "Single-label hostname not allowed (use FQDN)"


-- | Extract just scheme://host:port from a URL for safe error messages.
sanitizeUrlForError :: Text -> Text
sanitizeUrlForError urlText = do
  let urlString = Text.toLinkedList urlText
  case URI.parseURI urlString of
    Nothing -> "<invalid>"
    Just uri -> do
      let scheme = Text.fromLinkedList (URI.uriScheme uri)
      case URI.uriAuthority uri of
        Nothing -> scheme
        Just auth -> do
          let host = Text.fromLinkedList (URI.uriRegName auth)
          let port = Text.fromLinkedList (URI.uriPort auth)
          Text.append scheme (Text.append "//" (Text.append host port))
