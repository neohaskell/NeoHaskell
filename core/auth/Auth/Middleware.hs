-- | HTTP middleware for JWT authentication.
-- Integrates with WebTransport to authenticate requests.
module Auth.Middleware (
  -- * Token extraction
  extractToken,
  extractTokenFromQuery,
  -- * Authentication check
  checkAuth,
  checkAuthWithToken,
  -- * HTTP response helpers
  respondWithAuthError,
  -- * AuthContext for handlers
  AuthContext (..),
) where

import Auth.Claims (UserClaims (..))
import Auth.Config (AuthConfig (..))
import Auth.Error (AuthError (..))
import Auth.Jwt qualified as Jwt
import Auth.Jwks (JwksManager)
import Auth.Jwks qualified as Jwks
import Auth.Options (AuthOptions (..))
import Basics
import Bytes qualified
import Log qualified
import Data.ByteString qualified as GhcBS
import Json qualified
import Map qualified
import Maybe (Maybe (..))
import Network.HTTP.Types.Header qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified as Wai
import Result (Result (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)


-- | Authentication context passed to handlers.
data AuthContext = AuthContext
  { claims :: Maybe UserClaims,
    -- ^ User claims (Nothing if endpoint is public)
    isAuthenticated :: Bool
    -- ^ Whether the request was authenticated
  }
  deriving (Show, Eq)


-- | Empty auth context for unauthenticated requests.
emptyAuthContext :: AuthContext
emptyAuthContext =
  AuthContext
    { claims = Nothing,
      isAuthenticated = False
    }


-- | Extract Bearer token from Authorization header.
-- Returns Nothing if no Authorization header or not Bearer scheme.
-- Handles whitespace variations: "  Bearer  token  " is accepted.
extractToken :: Wai.Request -> Maybe Text
extractToken request = do
  let headers = Wai.requestHeaders request

  -- Find Authorization header
  case findHeader HTTP.hAuthorization headers of
    Nothing -> Nothing
    Just headerValue -> do
      -- Trim leading/trailing whitespace from header value
      let headerText = headerValue |> Bytes.fromLegacy |> Text.fromBytes |> Text.trim
      -- RFC 7235: Authorization scheme is case-insensitive
      -- Check for "Bearer" prefix (case-insensitive), then handle optional space
      let headerLower = Text.toLower headerText
      case Text.startsWith "bearer" headerLower of
        False -> Nothing
        True -> do
          -- Drop "Bearer" prefix, trim any whitespace between scheme and token
          let afterBearer = Text.dropLeft 6 headerText |> Text.trim
          Just afterBearer


-- | Extract token from ?token= query parameter.
-- SECURITY: Only use for endpoints where Authorization header is infeasible
-- (e.g., browser redirect flows like /connect/{provider}).
-- Prefer 'extractToken' (header-based) for all other endpoints.
-- See ADR-0031.
extractTokenFromQuery :: Wai.Request -> Maybe Text
extractTokenFromQuery request = do
  let queryParams = Wai.queryString request
  case findQueryParam "token" queryParams of
    Nothing -> Nothing
    Just tokenBytes -> Just (tokenBytes |> Bytes.fromLegacy |> Text.fromBytes |> Text.trim)


-- | Find a header by name (case-insensitive).
findHeader :: HTTP.HeaderName -> [HTTP.Header] -> Maybe GhcBS.ByteString
findHeader name headers =
  case headers of
    [] -> Nothing
    ((headerName, headerValue) : rest) ->
      case headerName == name of
        True -> Just headerValue
        False -> findHeader name rest


-- | Find a query parameter by name.
findQueryParam :: GhcBS.ByteString -> [(GhcBS.ByteString, Maybe GhcBS.ByteString)] -> Maybe GhcBS.ByteString
findQueryParam name params =
  case params of
    [] -> Nothing
    ((paramName, paramValue) : rest) ->
      case paramName == name of
        True ->
          case paramValue of
            Nothing -> Nothing
            Just value -> Just value
        False -> findQueryParam name rest


-- | Check authentication and authorization for a request.
-- Returns Ok AuthContext on success, Err AuthError on failure.
checkAuth ::
  Maybe JwksManager ->
  -- ^ JWKS manager (Nothing if auth disabled)
  AuthConfig ->
  -- ^ Auth configuration
  AuthOptions ->
  -- ^ Auth requirements for this endpoint
  Wai.Request ->
  -- ^ Incoming request
  Task err (Result AuthError AuthContext)
checkAuth maybeManager config authOptions request = do
  case authOptions of
    Everyone -> do
      -- No authentication required (public endpoint)
      Log.debug "Public endpoint, skipping auth" |> Task.ignoreError
      Task.yield (Ok emptyAuthContext)
    Authenticated ->
      -- Require valid JWT (permission checks done in command's decide method)
      validateAndBuildContext maybeManager config request


-- | Validate a pre-extracted token and build auth context.
-- SECURITY: Use when the token is obtained from a non-standard source
-- (e.g., query parameter for browser redirect flows). The token receives
-- the same validation as header-extracted tokens. See ADR-0031.
checkAuthWithToken ::
  Maybe JwksManager ->
  -- ^ JWKS manager (Nothing if auth disabled)
  AuthConfig ->
  -- ^ Auth configuration
  Text ->
  -- ^ Pre-extracted token
  Task err (Result AuthError AuthContext)
checkAuthWithToken maybeManager config token = do
  Log.withScope [("component", "Auth.Middleware")] do
    case maybeManager of
      Nothing -> do
        Log.warn "Auth not configured but authentication required" |> Task.ignoreError
        Task.yield (Err (AuthInfraUnavailable "Authentication not configured"))
      Just manager -> do
        isStale <- Jwks.checkStaleness manager
        case isStale of
          True -> do
            Log.warn "JWKS keys are stale, returning 503" |> Task.ignoreError
            Task.yield (Err (AuthInfraUnavailable "Authentication keys are stale"))
          False ->
            validateTokenAndBuildContext manager config token


-- | Validate token and build auth context.
validateAndBuildContext ::
  Maybe JwksManager ->
  AuthConfig ->
  Wai.Request ->
  Task err (Result AuthError AuthContext)
validateAndBuildContext maybeManager config request = do
  Log.withScope [("component", "Auth.Middleware")] do
    -- Check if auth is enabled
    case maybeManager of
      Nothing -> do
        -- Auth not configured - treat as infrastructure error
        Log.warn "Auth not configured but authentication required" |> Task.ignoreError
        Task.yield (Err (AuthInfraUnavailable "Authentication not configured"))
      Just manager -> do
        -- Check key staleness (503 if keys too old)
        isStale <- Jwks.checkStaleness manager
        case isStale of
          True -> do
            Log.warn "JWKS keys are stale, returning 503" |> Task.ignoreError
            Task.yield (Err (AuthInfraUnavailable "Authentication keys are stale"))
          False -> do
            -- Extract token from request
            case extractToken request of
              Nothing -> do
                Log.debug "No Bearer token in request" |> Task.ignoreError
                Task.yield (Err TokenMissing)
              Just token ->
                validateTokenAndBuildContext manager config token


-- | Core token validation logic shared by checkAuth and checkAuthWithToken.
validateTokenAndBuildContext ::
  JwksManager ->
  AuthConfig ->
  Text ->
  Task err (Result AuthError AuthContext)
validateTokenAndBuildContext manager config token = do
  -- DoS protection: reject oversized tokens before parsing
  -- 8KB is generous (typical JWT is 1-2KB, max reasonable is ~4KB)
  let maxTokenSize = 8192
  case Text.length token > maxTokenSize of
    True -> do
      Log.warn "Token exceeds maximum size (DoS protection)" |> Task.ignoreError
      Task.yield (Err (TokenMalformed "Token exceeds maximum size"))
    False -> do
      -- OPTIMIZED: Parse header ONCE for kid extraction AND validation
      case Jwt.parseHeader token of
        Err err -> do
          Log.debug [fmt|Token header parse failed: #{toText err}|] |> Task.ignoreError
          Task.yield (Err err)
        Ok header -> do
          -- Get JWKSet (optimized: single key if kid found, all keys otherwise)
          -- If kid provided but not found, triggers background refresh
          jwkSet <- case header.kid of
            Just kid -> do
              (set, _kidFound) <- Jwks.getJwkSetForKidWithRefresh kid manager
              Task.yield set
            Nothing -> Jwks.getJwkSet manager
          -- Validate token with pre-parsed header (avoids re-parsing)
          validationResult <- Jwt.validateTokenWithParsedHeader config jwkSet header token
          case validationResult of
            Err err -> do
              Log.warn [fmt|Token validation failed: #{toText err}|] |> Task.ignoreError
              Task.yield (Err err)
            Ok claims -> do
              Log.debug "Token validated successfully" |> Task.ignoreError
              Task.yield
                ( Ok
                    AuthContext
                      { claims = Just claims,
                        isAuthenticated = True
                      }
                )


-- | Convert AuthError to appropriate HTTP response.
-- SECURITY: Uses generic messages to prevent information leakage.
respondWithAuthError ::
  AuthError ->
  (Wai.Response -> Task Text Wai.ResponseReceived) ->
  Task Text Wai.ResponseReceived
respondWithAuthError authError respond = do
  Log.withScope [("component", "Auth.Middleware")] do
    Log.debug [fmt|Returning auth error response|]
      |> Task.ignoreError
  let (status, message) = authErrorToResponse authError
  let responseBody =
        Map.fromArray [("error" :: Text, message)]
          |> Json.encodeText
          |> Text.toBytes
          |> Bytes.toLazyLegacy
  let headers =
        [ (HTTP.hContentType, "application/json"),
          ("WWW-Authenticate", "Bearer") -- RFC 6750
        ]
  respond (Wai.responseLBS status headers responseBody)


-- | Map AuthError to HTTP status and generic message.
-- Internal details are never exposed to prevent information leakage.
authErrorToResponse :: AuthError -> (HTTP.Status, Text)
authErrorToResponse err =
  case err of
    TokenMissing ->
      (HTTP.status401, "Authentication required")
    TokenMalformed _ ->
      (HTTP.status401, "Authentication failed")
    TokenExpired ->
      (HTTP.status401, "Authentication failed")
    TokenNotYetValid ->
      (HTTP.status401, "Authentication failed")
    SignatureInvalid ->
      (HTTP.status401, "Authentication failed")
    AlgorithmNotAllowed _ ->
      (HTTP.status401, "Authentication failed")
    UnsupportedCritHeader _ ->
      (HTTP.status401, "Authentication failed")
    KeyAlgorithmMismatch ->
      (HTTP.status401, "Authentication failed")
    IssuerMismatch _ _ ->
      (HTTP.status401, "Authentication failed")
    AudienceMismatch _ _ ->
      (HTTP.status401, "Authentication failed")
    KeyNotFound _ ->
      (HTTP.status401, "Authentication failed")
    AuthInfraUnavailable _ ->
      (HTTP.status503, "Service temporarily unavailable")
