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
-- -- 1. Redirect user to authorize
-- let authUrl = OAuth2.authorizeUrl provider clientId redirectUri scopes state
-- -- redirect user to authUrl...
--
-- -- 2. On callback, exchange code for tokens
-- tokens <- OAuth2.exchangeCode provider clientId clientSecret redirectUri code
--
-- -- 3. Later, refresh the token
-- newTokens <- OAuth2.refreshToken provider clientId clientSecret tokens.refreshToken
-- @
--
-- = Security
--
-- * Always validate the 'State' parameter on callback (CSRF protection)
-- * Store tokens in 'Auth.SecretStore', NOT in event store
-- * Set appropriate timeouts on token requests
module Auth.OAuth2.Client (
  -- * Authorization
  authorizeUrl,
  authorizeUrlWithPkce,

  -- * Token Exchange
  exchangeCode,
  exchangeCodeWithPkce,

  -- * Token Refresh
  refreshToken,

  -- * PKCE Helpers
  generateCodeVerifier,
  deriveCodeChallenge,
) where

import Array (Array)
import Array qualified
import Auth.OAuth2.Types (
  AccessToken (..),
  AuthorizationCode (..),
  ClientId (..),
  ClientSecret (..),
  CodeChallenge (..),
  CodeChallengeMethod (..),
  CodeVerifier (..),
  OAuth2Error (..),
  Provider (..),
  RedirectUri (..),
  RefreshToken (..),
  Scope (..),
  State (..),
  TokenSet (..),
 )
import Auth.UrlValidation qualified as UrlValidation
import Basics
import Bytes qualified
import Crypto.Hash qualified as Hash
import Crypto.Random qualified as Random
import Data.ByteArray.Encoding qualified as Encoding
import Data.ByteString qualified as BS
import Http.Client qualified as Http
import Json qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Network.URI qualified as URI
import System.IO qualified as GhcIO
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)


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
authorizeUrl provider (ClientId clientId) (RedirectUri redirectUri) scopes (State state) = do
  let authEndpoint = provider.authorizeEndpoint
  let scopeText =
        scopes
          |> Array.map (\(Scope s) -> s)
          |> Text.joinWith " "
  let params :: Array (Text, Text) =
        Array.fromLinkedList
          [ ("response_type", "code")
          , ("client_id", clientId)
          , ("redirect_uri", redirectUri)
          , ("state", state)
          , ("scope", scopeText)
          ]
  let encodeParam :: (Text, Text) -> Text
      encodeParam (key, val) = do
        let encodedVal = urlEncode val
        [fmt|#{key}=#{encodedVal}|]
  let queryString =
        params
          |> Array.map encodeParam
          |> Text.joinWith "&"
  [fmt|#{authEndpoint}?#{queryString}|]


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
exchangeCode provider (ClientId clientId) (ClientSecret clientSecret) (RedirectUri redirectUri) (AuthorizationCode code) = do
  let tokenEndpoint = provider.tokenEndpoint
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
refreshToken provider (ClientId clientId) (ClientSecret clientSecret) (RefreshToken refresh) = do
  let tokenEndpoint = provider.tokenEndpoint
  let formParams =
        [ ("grant_type", "refresh_token")
        , ("refresh_token", refresh)
        , ("client_id", clientId)
        , ("client_secret", clientSecret)
        ]
  requestTokens tokenEndpoint formParams


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
  Task.yield (CodeVerifier verifierText)


-- | Derive a code challenge from a code verifier using S256 method.
--
-- Computes: base64url(sha256(code_verifier))
--
-- This is the recommended method per RFC 7636.
deriveCodeChallenge :: CodeVerifier -> CodeChallenge
deriveCodeChallenge (CodeVerifier verifier) = do
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
authorizeUrlWithPkce provider (ClientId clientId) (RedirectUri redirectUri) scopes (State state) (CodeChallenge challenge) method = do
  let authEndpoint = provider.authorizeEndpoint
  let scopeText =
        scopes
          |> Array.map (\(Scope s) -> s)
          |> Text.joinWith " "
  let methodText = case method of
        S256 -> "S256"
        Plain -> "plain"
  let params :: Array (Text, Text) =
        Array.fromLinkedList
          [ ("response_type", "code")
          , ("client_id", clientId)
          , ("redirect_uri", redirectUri)
          , ("state", state)
          , ("scope", scopeText)
          , ("code_challenge", challenge)
          , ("code_challenge_method", methodText)
          ]
  let encodeParam :: (Text, Text) -> Text
      encodeParam (key, val) = do
        let encodedVal = urlEncode val
        [fmt|#{key}=#{encodedVal}|]
  let queryString =
        params
          |> Array.map encodeParam
          |> Text.joinWith "&"
  [fmt|#{authEndpoint}?#{queryString}|]


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
exchangeCodeWithPkce provider (ClientId clientId) (ClientSecret clientSecret) (RedirectUri redirectUri) (AuthorizationCode code) (CodeVerifier verifier) = do
  let tokenEndpoint = provider.tokenEndpoint
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


-- | Make a token request to the provider.
-- SECURITY: Validates endpoint URL for HTTPS and SSRF protection with DNS resolution.
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
      let errMsg = toText (show validationError)
      Task.throw (EndpointValidationFailed errMsg)
    Ok _ -> do
      let request =
            Http.request
              |> Http.withUrl tokenEndpoint
              |> Http.withTimeout 10 -- 10s timeout for OAuth2 token requests
      let formArray = formParams |> Array.fromLinkedList
      result <-
        Http.postForm @TokenResponse request formArray
          |> Task.mapError (\(Http.Error msg) -> NetworkError msg)
          |> Task.asResult
      case result of
        Err err -> Task.throw err
        Ok response -> do
          Task.yield
            TokenSet
              { accessToken = AccessToken response.access_token
              , refreshToken = response.refresh_token |> fmap RefreshToken
              , expiresInSeconds = response.expires_in
              }


-- | URL encoding (percent-encoding) using Network.URI.
-- Uses the standard library for correctness with all characters including
-- UTF-8, percent signs, and other edge cases.
urlEncode :: Text -> Text
urlEncode text = do
  let charString = Text.toLinkedList text
  let encoded = URI.escapeURIString URI.isUnreserved charString
  Text.fromLinkedList encoded
