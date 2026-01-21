-- | Core OAuth2 types for client-side OAuth2 flows.
--
-- These types support the Authorization Code flow for connecting
-- to external APIs (Oura, GitHub, Google, etc.).
--
-- = Security Notes
--
-- * Tokens should be stored in 'SecretStore', NOT in the event store
-- * 'State' parameter is mandatory for CSRF protection
-- * 'ClientSecret' should never be logged or serialized to events
module Auth.OAuth2.Types (
  -- * Provider Configuration
  Provider (..),

  -- * Credentials
  ClientId (..),
  ClientSecret (..),
  RedirectUri (..),

  -- * Authorization Flow
  AuthorizationCode (..),
  State (..),
  Scope (..),

  -- * PKCE (Proof Key for Code Exchange)
  CodeVerifier (..),
  CodeChallenge (..),
  CodeChallengeMethod (..),

  -- * Tokens
  AccessToken (..),
  RefreshToken (..),
  TokenSet (..),

  -- * Errors
  OAuth2Error (..),

  -- * Validation
  validateState,
  validateRedirectUri,
) where

import Basics
import Char (Char)
import Data.List qualified as GhcList
import Json qualified
import LinkedList qualified
import Maybe (Maybe (..))
import Network.URI qualified as URI
import Result (Result (..))
import Text (Text)
import Text qualified


-- | OAuth2 provider configuration.
--
-- Contains the endpoints needed for the Authorization Code flow.
--
-- @
-- ouraProvider :: Provider
-- ouraProvider = Provider
--   { name = "oura"
--   , authorizeEndpoint = "https://cloud.ouraring.com/oauth/authorize"
--   , tokenEndpoint = "https://api.ouraring.com/oauth/token"
--   }
-- @
data Provider = Provider
  { name :: Text
  , authorizeEndpoint :: Text
  , tokenEndpoint :: Text
  }
  deriving (Generic, Show, Eq)


instance Json.FromJSON Provider


instance Json.ToJSON Provider


-- | OAuth2 client identifier.
newtype ClientId = ClientId Text
  deriving (Generic, Show, Eq)


instance Json.FromJSON ClientId


instance Json.ToJSON ClientId


-- | OAuth2 client secret.
--
-- WARNING: Never log or serialize this to events.
-- Show instance is redacted for security.
-- No JSON instances - use explicit conversion if needed.
newtype ClientSecret = ClientSecret Text
  deriving (Generic, Eq)


-- | Redacted Show instance - NEVER reveals the actual secret
instance Show ClientSecret where
  show _ = "ClientSecret <REDACTED>"


-- | OAuth2 redirect URI (callback URL).
newtype RedirectUri = RedirectUri Text
  deriving (Generic, Show, Eq)


instance Json.FromJSON RedirectUri


instance Json.ToJSON RedirectUri


-- | Authorization code received from the OAuth2 provider.
--
-- This is exchanged for tokens via 'Auth.OAuth2.Client.exchangeCode'.
-- Show instance is redacted for security (codes are secrets).
-- No ToJSON instance - prevent accidental serialization.
newtype AuthorizationCode = AuthorizationCode Text
  deriving (Generic, Eq)


-- | Redacted Show instance - NEVER reveals the actual code
instance Show AuthorizationCode where
  show _ = "AuthorizationCode <REDACTED>"


instance Json.FromJSON AuthorizationCode


-- | State parameter for CSRF protection.
--
-- SECURITY: Must be:
-- * Cryptographically random
-- * Stored in session before redirect
-- * Validated on callback before token exchange
-- * Single-use (rejected if reused)
--
-- Show instance is redacted for security (state tokens are secrets).
-- No ToJSON instance - prevent accidental serialization.
newtype State = State Text
  deriving (Generic, Eq)


-- | Redacted Show instance - NEVER reveals the actual state
instance Show State where
  show _ = "State <REDACTED>"


instance Json.FromJSON State


-- | Validate that the returned state matches the expected state.
-- SECURITY: This is critical for CSRF protection. Always validate state
-- before exchanging the authorization code for tokens.
--
-- @
-- case validateState expectedState returnedState of
--   Ok () -> OAuth2.exchangeCode provider clientId secret redirectUri code
--   Err err -> Task.throw err
-- @
validateState :: State -> State -> Result OAuth2Error ()
validateState (State expected) (State returned) =
  case expected == returned of
    True -> Ok ()
    False -> Err (InvalidState "State parameter mismatch - possible CSRF attack")


-- | Validate that a redirect URI matches the expected registered URI.
-- SECURITY: Redirect URI validation is critical to prevent authorization code theft.
--
-- Validates:
-- 1. Exact string match (no normalization that could allow bypasses)
-- 2. No fragment component (fragments are forbidden in redirect URIs per RFC 6749)
-- 3. Uses HTTPS (except localhost for development)
--
-- @
-- case validateRedirectUri registeredUri receivedUri of
--   Ok () -> -- proceed with token exchange
--   Err err -> -- reject the request
-- @
validateRedirectUri :: RedirectUri -> RedirectUri -> Result OAuth2Error ()
validateRedirectUri (RedirectUri registered) (RedirectUri received) = do
  -- Step 1: Exact match required (no normalization)
  case registered == received of
    False -> Err (InvalidRedirectUri "Redirect URI does not match registered URI")
    True -> do
      -- Step 2: No fragment allowed
      let uriString = Text.toLinkedList received
      case URI.parseURI uriString of
        Nothing -> Err (InvalidRedirectUri "Malformed redirect URI")
        Just uri -> do
          case URI.uriFragment uri of
            "" -> do
              -- Step 3: Must use HTTPS (except localhost)
              case URI.uriScheme uri of
                "https:" -> Ok ()
                "http:" -> do
                  -- Allow http only for localhost (development)
                  case URI.uriAuthority uri of
                    Nothing -> Err (InvalidRedirectUri "Redirect URI must use HTTPS")
                    Just auth -> do
                      let host = URI.uriRegName auth
                      case isLocalhost host of
                        True -> Ok ()
                        False -> Err (InvalidRedirectUri "Redirect URI must use HTTPS (http allowed only for localhost)")
                _ -> Err (InvalidRedirectUri "Redirect URI must use HTTPS scheme")
            _ -> Err (InvalidRedirectUri "Redirect URI must not contain a fragment")


-- | Check if a hostname is localhost (for development redirect URIs).
isLocalhost :: [Char] -> Bool
isLocalhost host =
  host == "localhost"
    || host == "127.0.0.1"
    || host == "[::1]"
    || LinkedList.any (\prefix -> GhcList.isPrefixOf prefix host) ["localhost:", "127.0.0.1:", "[::1]:"]


-- | OAuth2 scope(s) being requested.
newtype Scope = Scope Text
  deriving (Generic, Show, Eq)


instance Json.FromJSON Scope


instance Json.ToJSON Scope


-- | OAuth2 access token.
--
-- Used in Authorization header: @Authorization: Bearer \<token\>@
-- Show instance is redacted for security.
-- No JSON instances - use explicit conversion if needed.
newtype AccessToken = AccessToken Text
  deriving (Generic, Eq)


-- | Redacted Show instance - NEVER reveals the actual token
instance Show AccessToken where
  show _ = "AccessToken <REDACTED>"


-- | OAuth2 refresh token.
--
-- Used to obtain new access tokens without user interaction.
-- Some providers rotate refresh tokens on each use.
-- Show instance is redacted for security.
-- No JSON instances - use explicit conversion if needed.
newtype RefreshToken = RefreshToken Text
  deriving (Generic, Eq)


-- | Redacted Show instance - NEVER reveals the actual token
instance Show RefreshToken where
  show _ = "RefreshToken <REDACTED>"


-- | Complete set of tokens from an OAuth2 token response.
--
-- @
-- case maybeTokenSet of
--   Just tokens -> do
--     let (AccessToken bearer) = tokens.accessToken
--     Http.request
--       |> Http.addHeader "Authorization" [fmt|Bearer #{bearer}|]
--       |> Http.get
--   Nothing -> Task.throw "Not connected"
-- @
--
-- Show instance is redacted (contains secrets).
-- No JSON instances - tokens should be stored via SecretStore, not serialized.
data TokenSet = TokenSet
  { accessToken :: AccessToken
  , refreshToken :: Maybe RefreshToken
  , expiresInSeconds :: Maybe Int
    -- ^ Duration in seconds until access token expires (from OAuth2 expires_in).
    -- This is NOT a unix timestamp - it's a duration from the time of issuance.
    -- To get expiry time: currentTime + expiresInSeconds
  }
  deriving (Generic, Eq)


-- | Redacted Show instance - uses redacted inner types
instance Show TokenSet where
  show ts = do
    let access = show ts.accessToken
    let refresh = show ts.refreshToken
    let expires = show ts.expiresInSeconds
    [fmt|TokenSet {accessToken = #{access}, refreshToken = #{refresh}, expiresInSeconds = #{expires}}|]


-- | PKCE code verifier (RFC 7636).
--
-- A cryptographically random string used as the secret in PKCE.
-- Must be 43-128 characters from [A-Z], [a-z], [0-9], "-", ".", "_", "~".
-- Show instance is redacted for security.
-- No ToJSON instance - prevent accidental serialization.
newtype CodeVerifier = CodeVerifier Text
  deriving (Generic, Eq)


-- | Redacted Show instance - NEVER reveals the actual verifier
instance Show CodeVerifier where
  show _ = "CodeVerifier <REDACTED>"


instance Json.FromJSON CodeVerifier


-- | PKCE code challenge (RFC 7636).
--
-- Derived from CodeVerifier using SHA256 + base64url encoding.
-- Sent to authorization endpoint; verifier is sent to token endpoint.
newtype CodeChallenge = CodeChallenge Text
  deriving (Generic, Show, Eq)


instance Json.FromJSON CodeChallenge


instance Json.ToJSON CodeChallenge


-- | PKCE code challenge method.
-- S256 is recommended; Plain should only be used when SHA256 is unavailable.
data CodeChallengeMethod
  = -- | SHA256 hash of verifier (recommended)
    S256
  | -- | Plain text verifier (not recommended)
    Plain
  deriving (Generic, Show, Eq)


instance Json.FromJSON CodeChallengeMethod


instance Json.ToJSON CodeChallengeMethod


-- | Errors that can occur during OAuth2 operations.
data OAuth2Error
  = -- | Token request failed (network or HTTP error)
    TokenRequestFailed Text
  | -- | Invalid grant (code expired, already used, etc.)
    InvalidGrant Text
  | -- | Invalid client credentials
    InvalidClient Text
  | -- | Requested scope was denied
    ScopeDenied Text
  | -- | Token response was malformed
    MalformedResponse Text
  | -- | Network error during token exchange
    NetworkError Text
  | -- | Endpoint URL failed security validation (HTTPS/SSRF)
    EndpointValidationFailed Text
  | -- | State parameter validation failed (CSRF protection)
    InvalidState Text
  | -- | PKCE verifier validation failed
    InvalidPkceVerifier Text
  | -- | Redirect URI validation failed
    InvalidRedirectUri Text
  deriving (Generic, Show, Eq)


instance Json.FromJSON OAuth2Error


instance Json.ToJSON OAuth2Error
