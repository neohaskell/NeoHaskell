-- | # Auth.OAuth2.Routes
--
-- HTTP route handlers for OAuth2 provider integration.
--
-- = Routes
--
-- * @GET /connect/{provider}@ - Initiate OAuth2 flow (requires JWT)
-- * @GET /callback/{provider}@ - OAuth2 callback (no JWT, validates state)
-- * @POST /disconnect/{provider}@ - Disconnect provider (requires JWT)
--
-- = Security
--
-- The connect and disconnect routes require JWT authentication because they
-- need the userId to:
--
-- 1. Encode userId in state token (connect) - so callback knows who to store tokens for
-- 2. Delete tokens for the correct user (disconnect)
--
-- The callback route does NOT require JWT because:
--
-- 1. User is redirected back from external OAuth2 provider
-- 2. The state token (HMAC-signed) carries the userId securely
-- 3. State is validated and consumed atomically (one-time use)
--
-- = Flow
--
-- @
-- 1. User clicks "Connect Oura" button
-- 2. Frontend calls GET /connect/oura (with JWT)
-- 3. Server generates state token (contains userId), stores verifier
-- 4. Server redirects to Oura authorization URL
-- 5. User authorizes on Oura
-- 6. Oura redirects to GET /callback/oura?code=...&state=...
-- 7. Server validates state, exchanges code for tokens
-- 8. Server calls onSuccess callback, stores tokens
-- 9. Server redirects user to successRedirectUrl
-- @
module Auth.OAuth2.Routes (
  -- * Route Handlers
  OAuth2Routes (..),
  createRoutes,

  -- * Dependencies
  OAuth2RouteDeps (..),

  -- * Errors
  OAuth2RouteError (..),
) where

import Auth.OAuth2.Client qualified as OAuth2
import Auth.OAuth2.Provider (OAuth2Action (..), OAuth2ProviderConfig (..))
import Auth.OAuth2.StateToken (HmacKey, StatePayload (..), StateTokenError)
import Auth.OAuth2.StateToken qualified as StateToken
import Auth.OAuth2.TransactionStore (Transaction (..), TransactionStore)
import Auth.OAuth2.TransactionStore qualified as TransactionStore
import Auth.OAuth2.Types (
  CodeChallengeMethod (..),
  OAuth2Error (..),
  mkAuthorizationCode,
  mkState,
 )
import Basics
import Data.Time.Clock.POSIX qualified as GhcPosix
import Prelude qualified as GhcPrelude
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | Dependencies required by OAuth2 routes.
--
-- These are injected at route creation time.
data OAuth2RouteDeps = OAuth2RouteDeps
  { -- | HMAC key for signing/verifying state tokens
    hmacKey :: HmacKey
  , -- | Store for PKCE verifiers (one-time use)
    transactionStore :: TransactionStore
  , -- | Configured OAuth2 providers by name
    providers :: Map Text OAuth2ProviderConfig
  }


-- | OAuth2 route handlers.
--
-- These are WAI-independent handlers that can be integrated into WebTransport.
data OAuth2Routes = OAuth2Routes
  { -- | Handle GET /connect/{provider}
    -- Returns: Redirect URL to OAuth2 provider
    handleConnect :: Text -> Text -> Task OAuth2RouteError Text
  , -- | Handle GET /callback/{provider}?code=...&state=...
    -- Returns: (redirect URL, maybe action to dispatch)
    handleCallback :: Text -> Text -> Text -> Task OAuth2RouteError (Text, Maybe OAuth2Action)
  , -- | Handle POST /disconnect/{provider}
    -- Returns: Action to dispatch
    handleDisconnect :: Text -> Text -> Task OAuth2RouteError OAuth2Action
  }


-- | Errors that can occur in OAuth2 routes.
data OAuth2RouteError
  = -- | Provider not configured
    ProviderNotFound Text
  | -- | State token validation failed
    StateValidationFailed StateTokenError
  | -- | State not found in transaction store (expired or already used)
    StateNotFound
  | -- | OAuth2 token exchange failed
    TokenExchangeFailed OAuth2Error
  | -- | Provider name in state doesn't match route
    ProviderMismatch Text Text
  | -- | Missing required query parameter
    MissingParameter Text
  deriving (Generic, Show, Eq)


-- | Create OAuth2 route handlers with the given dependencies.
createRoutes :: OAuth2RouteDeps -> OAuth2Routes
createRoutes deps =
  OAuth2Routes
    { handleConnect = handleConnectImpl deps
    , handleCallback = handleCallbackImpl deps
    , handleDisconnect = handleDisconnectImpl deps
    }


-- | Implementation of GET /connect/{provider}
--
-- 1. Validate provider exists
-- 2. Generate PKCE verifier and challenge
-- 3. Generate state token (HMAC-signed, contains userId)
-- 4. Store verifier in transaction store
-- 5. Build and return authorization URL
handleConnectImpl ::
  OAuth2RouteDeps ->
  -- | Provider name from URL
  Text ->
  -- | User ID from JWT
  Text ->
  Task OAuth2RouteError Text
handleConnectImpl deps providerName userId = do
  -- 1. Look up provider config
  config <- case deps.providers |> Map.get providerName of
    Nothing -> Task.throw (ProviderNotFound providerName)
    Just c -> Task.yield c

  -- 2. Generate PKCE verifier and challenge
  verifier <- OAuth2.generateCodeVerifier |> Task.mapError (\_ -> ProviderNotFound "PKCE generation failed")
  let challenge = OAuth2.deriveCodeChallenge verifier

  -- 3. Generate state token
  nowSeconds <- getCurrentTimeSeconds
  let expiresAt = nowSeconds + 300 -- 5 minutes TTL
  let nonce = generateNonce nowSeconds userId -- Simple nonce from timestamp + userId hash
  let payload =
        StatePayload
          { provider = providerName
          , userId = userId
          , nonce = nonce
          , issuedAt = nowSeconds
          , expiresAt = expiresAt
          }
  stateToken <- StateToken.encodeStateToken deps.hmacKey payload
    |> Task.mapError (\_ -> ProviderNotFound "State token encoding failed")

  -- 4. Store verifier in transaction store (keyed by state token)
  let transaction = Transaction {verifier = verifier, expiresAt = expiresAt}
  let txKey = TransactionStore.fromText stateToken
  deps.transactionStore.put txKey transaction
    |> Task.mapError (\_ -> ProviderNotFound "Transaction store error")

  -- 5. Build authorization URL
  let authUrl =
        OAuth2.authorizeUrlWithPkce
          config.provider
          config.clientId
          config.redirectUri
          config.scopes
          (mkState stateToken)
          challenge
          S256

  Task.yield authUrl


-- | Implementation of GET /callback/{provider}?code=...&state=...
--
-- 1. Validate state token (HMAC signature, TTL, provider match)
-- 2. Consume verifier from transaction store (one-time use)
-- 3. Exchange authorization code for tokens
-- 4. Call onSuccess callback
-- 5. Return redirect URL
handleCallbackImpl ::
  OAuth2RouteDeps ->
  -- | Provider name from URL
  Text ->
  -- | Authorization code from query
  Text ->
  -- | State token from query
  Text ->
  Task OAuth2RouteError (Text, Maybe OAuth2Action)
handleCallbackImpl deps providerName code stateToken = do
  -- 1. Look up provider config
  config <- case deps.providers |> Map.get providerName of
    Nothing -> Task.throw (ProviderNotFound providerName)
    Just c -> Task.yield c

  -- 2. Validate state token
  nowSeconds <- getCurrentTimeSeconds
  payload <- StateToken.decodeStateToken deps.hmacKey nowSeconds stateToken
    |> Task.mapError StateValidationFailed

  -- 3. Verify provider matches (mix-up attack prevention)
  case payload.provider == providerName of
    False -> Task.throw (ProviderMismatch payload.provider providerName)
    True -> pass

  -- 4. Consume verifier from transaction store (one-time use, atomic)
  let txKey = TransactionStore.fromText stateToken
  maybeTransaction <- deps.transactionStore.consume txKey
    |> Task.mapError (\_ -> StateNotFound)
  transaction :: Transaction <- case maybeTransaction of
    Nothing -> Task.throw StateNotFound
    Just tx -> Task.yield tx

  -- 5. Exchange code for tokens
  tokensResult <-
    OAuth2.exchangeCodeWithPkce
      config.provider
      config.clientId
      config.clientSecret
      config.redirectUri
      (mkAuthorizationCode code)
      transaction.verifier
      |> Task.asResult

  case tokensResult of
    Err oauthError -> do
      -- Call failure callback
      let actionJson = config.onFailure payload.userId oauthError
      let action = FailureAction actionJson
      Task.yield (config.failureRedirectUrl, Just action)
    Ok tokens -> do
      -- Call success callback
      let actionJson = config.onSuccess payload.userId tokens
      let action = SuccessAction actionJson
      Task.yield (config.successRedirectUrl, Just action)


-- | Implementation of POST /disconnect/{provider}
--
-- 1. Validate provider exists
-- 2. Call onDisconnect callback
-- 3. Return action to dispatch
handleDisconnectImpl ::
  OAuth2RouteDeps ->
  -- | Provider name from URL
  Text ->
  -- | User ID from JWT
  Text ->
  Task OAuth2RouteError OAuth2Action
handleDisconnectImpl deps providerName userId = do
  -- 1. Look up provider config
  config <- case deps.providers |> Map.get providerName of
    Nothing -> Task.throw (ProviderNotFound providerName)
    Just c -> Task.yield c

  -- 2. Call disconnect callback
  let actionJson = config.onDisconnect userId
  Task.yield (DisconnectAction actionJson)


-- | Generate a simple nonce from timestamp and userId.
--
-- This doesn't need to be cryptographically random because:
-- 1. The state token is already HMAC-signed (tamper-proof)
-- 2. The nonce just needs to be unique per request
-- 3. Combined with userId + timestamp, collisions are extremely unlikely
generateNonce :: Int -> Text -> Text
generateNonce timestamp userId = do
  -- Simple hash-like combination
  let combined = [fmt|#{timestamp}-#{userId}|]
  -- Take first 32 chars for uniqueness
  combined |> Text.left 32


-- | Get current time as Unix seconds.
getCurrentTimeSeconds :: forall error. Task error Int
getCurrentTimeSeconds = do
  posixTime <- GhcPosix.getPOSIXTime |> Task.fromIO
  let seconds :: Int = GhcPrelude.floor (GhcPrelude.realToFrac posixTime :: GhcPrelude.Double)
  Task.yield seconds
