-- | HTTP middleware for JWT authentication.
-- Integrates with WebTransport to authenticate requests.
module Auth.Middleware (
  -- * Token extraction
  extractToken,
  -- * Authentication check
  checkAuth,
  -- * HTTP response helpers
  respondWithAuthError,
  -- * AuthContext for handlers
  AuthContext (..),
) where

import Array qualified
import Auth.Claims (UserClaims (..))
import Auth.Config (AuthConfig (..))
import Auth.Error (AuthError (..))
import Auth.Jwt qualified as Jwt
import Auth.Jwks (JwksManager)
import Auth.Jwks qualified as Jwks
import Auth.Options (AuthOptions (..))
import Auth.Options qualified as AuthOptions
import Basics
import Bytes qualified
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
extractToken :: Wai.Request -> Maybe Text
extractToken request = do
  let headers = Wai.requestHeaders request

  -- Find Authorization header
  case findHeader HTTP.hAuthorization headers of
    Nothing -> Nothing
    Just headerValue -> do
      let headerText = headerValue |> Bytes.fromLegacy |> Text.fromBytes
      -- Check for "Bearer " prefix and extract token
      let bearerPrefix = "Bearer "
      case Text.startsWith bearerPrefix headerText of
        False -> Nothing
        True -> Just (Text.dropLeft (Text.length bearerPrefix) headerText)


-- | Find a header by name (case-insensitive).
findHeader :: HTTP.HeaderName -> [HTTP.Header] -> Maybe GhcBS.ByteString
findHeader name headers =
  case headers of
    [] -> Nothing
    ((headerName, headerValue) : rest) ->
      case headerName == name of
        True -> Just headerValue
        False -> findHeader name rest


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
    Everyone ->
      -- No authentication required
      Task.yield (Ok emptyAuthContext)
    Authenticated -> do
      -- Require valid JWT, any permissions
      validateAndBuildContext maybeManager config request
    RequireAllPermissions required -> do
      -- Require valid JWT + all listed permissions
      result <- validateAndBuildContext maybeManager config request
      case result of
        Err err -> Task.yield (Err err)
        Ok ctx -> do
          -- all = not (any not)
          let missingPermission = required |> Array.any (\p -> not (checkHasPermission p ctx))
          case missingPermission of
            True -> Task.yield (Err (InsufficientPermissions required))
            False -> Task.yield (Ok ctx)
    RequireAnyPermission required -> do
      -- Require valid JWT + at least one listed permission
      result <- validateAndBuildContext maybeManager config request
      case result of
        Err err -> Task.yield (Err err)
        Ok ctx -> do
          let hasAnyPermission = required |> Array.any (\p -> checkHasPermission p ctx)
          case hasAnyPermission of
            False -> Task.yield (Err (InsufficientPermissions required))
            True -> Task.yield (Ok ctx)
    Custom validator -> do
      -- Custom validation logic
      result <- validateAndBuildContext maybeManager config request
      case result of
        Err err -> Task.yield (Err err)
        Ok ctx ->
          case ctx.claims of
            Nothing -> Task.yield (Err TokenMissing)
            Just claims ->
              case validator claims of
                Err (AuthOptions.AuthOptionsError msg) ->
                  Task.yield (Err (TokenMalformed msg)) -- Map custom error to auth error
                Ok () -> Task.yield (Ok ctx)


-- | Validate token and build auth context.
validateAndBuildContext ::
  Maybe JwksManager ->
  AuthConfig ->
  Wai.Request ->
  Task err (Result AuthError AuthContext)
validateAndBuildContext maybeManager config request = do
  -- Check if auth is enabled
  case maybeManager of
    Nothing ->
      -- Auth not configured - treat as infrastructure error
      Task.yield (Err (AuthInfraUnavailable "Authentication not configured"))
    Just manager -> do
      -- Extract token from request
      case extractToken request of
        Nothing ->
          Task.yield (Err TokenMissing)
        Just token -> do
          -- Get keys from manager
          keys <- Jwks.getAllKeys manager
          -- Validate token
          validationResult <- Jwt.validateToken config keys token
          case validationResult of
            Err err -> Task.yield (Err err)
            Ok claims ->
              Task.yield
                ( Ok
                    AuthContext
                      { claims = Just claims,
                        isAuthenticated = True
                      }
                )


-- | Check if context has a specific permission.
checkHasPermission :: Text -> AuthContext -> Bool
checkHasPermission permission ctx =
  case ctx.claims of
    Nothing -> False
    Just claims -> claims.permissions |> Array.any (\p -> p == permission)


-- | Convert AuthError to appropriate HTTP response.
-- SECURITY: Uses generic messages to prevent information leakage.
respondWithAuthError ::
  AuthError ->
  (Wai.Response -> Task Text Wai.ResponseReceived) ->
  Task Text Wai.ResponseReceived
respondWithAuthError authError respond = do
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
    InsufficientPermissions _ ->
      (HTTP.status403, "Forbidden")
    AuthInfraUnavailable _ ->
      (HTTP.status503, "Service temporarily unavailable")
