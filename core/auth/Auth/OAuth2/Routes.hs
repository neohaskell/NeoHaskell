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
-- 1. Store userId server-side in TransactionStore (connect) - GDPR compliant
-- 2. Delete tokens for the correct user (disconnect)
--
-- The callback route does NOT require JWT because:
--
-- 1. User is redirected back from external OAuth2 provider
-- 2. The state token (HMAC-signed) identifies the transaction
-- 3. userId is retrieved from server-side TransactionStore (NOT from token)
-- 4. State is validated and consumed atomically (one-time use)
--
-- = GDPR Compliance
--
-- NO PII (userId, email, etc.) is stored in the state token. The state token
-- only contains: provider name, cryptographic nonce, and timestamps.
-- userId is stored server-side in TransactionStore and retrieved at callback.
--
-- = Flow
--
-- @
-- 1. User clicks "Connect Oura" button
-- 2. Frontend calls GET /connect/oura (with JWT)
-- 3. Server generates state token (provider + nonce), stores verifier + userId
-- 4. Server redirects to Oura authorization URL
-- 5. User authorizes on Oura
-- 6. Oura redirects to GET /callback/oura?code=...&state=...
-- 7. Server validates state, retrieves userId from TransactionStore
-- 8. Server exchanges code for tokens, calls onSuccess callback
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
import Auth.OAuth2.Provider (OAuth2Action (..), ValidatedOAuth2ProviderConfig (..))
import Auth.OAuth2.RateLimiter (RateLimiter (..), RateLimitResult (..))
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
import Auth.SecretStore (SecretStore (..), TokenKey (..))
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
  , -- | Secure storage for OAuth2 tokens
    secretStore :: SecretStore
  , -- | Configured OAuth2 providers by name (pre-validated at startup)
    providers :: Map Text ValidatedOAuth2ProviderConfig
  , -- | Rate limiter for /connect endpoint (per userId)
    connectRateLimiter :: RateLimiter
  , -- | Rate limiter for /callback endpoint (per IP, but we use state token as proxy)
    callbackRateLimiter :: RateLimiter
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
  | -- | Rate limit exceeded (retry after N seconds)
    RateLimited Int
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
-- 1. Check rate limit (per userId)
-- 2. Validate provider exists
-- 3. Generate PKCE verifier and challenge
-- 4. Generate state token (HMAC-signed, NO PII - just provider + nonce)
-- 5. Store verifier + userId in transaction store (server-side)
-- 6. Build and return authorization URL
handleConnectImpl ::
  OAuth2RouteDeps ->
  -- | Provider name from URL
  Text ->
  -- | User ID from JWT
  Text ->
  Task OAuth2RouteError Text
handleConnectImpl deps providerName userId = do
  -- 1. Check rate limit (per userId)
  rateLimitResult <- deps.connectRateLimiter.checkLimit userId
    |> Task.mapError (\_ -> ProviderNotFound "Rate limit check failed")
  case rateLimitResult of
    Limited retryAfter -> Task.throw (RateLimited retryAfter)
    Allowed -> pass

  -- 2. Look up provider config
  config <- case deps.providers |> Map.get providerName of
    Nothing -> Task.throw (ProviderNotFound providerName)
    Just c -> Task.yield c

  -- 3. Generate PKCE verifier and challenge
  verifier <- OAuth2.generateCodeVerifier |> Task.mapError (\_ -> ProviderNotFound "PKCE generation failed")
  let challenge = OAuth2.deriveCodeChallenge verifier

  -- 4. Generate state token (NO userId - GDPR compliance)
  nowSeconds <- getCurrentTimeSeconds
  let expiresAt = nowSeconds + 300 -- 5 minutes TTL
  -- Use cryptographically secure random nonce
  nonce <- StateToken.generateNonce
    |> Task.mapError (\_ -> ProviderNotFound "Nonce generation failed")
  let payload =
        StatePayload
          { provider = providerName
          , nonce = nonce
          , issuedAt = nowSeconds
          , expiresAt = expiresAt
          }
  stateToken <- StateToken.encodeStateToken deps.hmacKey payload
    |> Task.mapError (\_ -> ProviderNotFound "State token encoding failed")

  -- 5. Store verifier + userId in transaction store (userId stays server-side)
  let transaction = Transaction {verifier = verifier, userId = userId, expiresAt = expiresAt}
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
-- 1. Check rate limit (per state token - proxy for IP)
-- 2. Validate state token (HMAC signature, TTL, provider match)
-- 3. Consume transaction from store (one-time use, retrieves userId)
-- 4. Exchange authorization code for tokens
-- 5. Call onSuccess/onFailure callback with userId from transaction
-- 6. Return redirect URL
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
  -- 1. Check rate limit (per state token prefix - proxy for request origin)
  -- We use the first 16 chars of state token as a rate limit key
  -- This prevents brute-force attacks while allowing legitimate retries
  let rateLimitKey = stateToken |> Text.left 16
  rateLimitResult <- deps.callbackRateLimiter.checkLimit rateLimitKey
    |> Task.mapError (\_ -> ProviderNotFound "Rate limit check failed")
  case rateLimitResult of
    Limited retryAfter -> Task.throw (RateLimited retryAfter)
    Allowed -> pass

  -- 2. Look up provider config
  config <- case deps.providers |> Map.get providerName of
    Nothing -> Task.throw (ProviderNotFound providerName)
    Just c -> Task.yield c

  -- 3. Validate state token
  nowSeconds <- getCurrentTimeSeconds
  payload <- StateToken.decodeStateToken deps.hmacKey nowSeconds stateToken
    |> Task.mapError StateValidationFailed

  -- 4. Verify provider matches (mix-up attack prevention)
  case payload.provider == providerName of
    False -> Task.throw (ProviderMismatch payload.provider providerName)
    True -> pass

  -- 5. Consume transaction from store (one-time use, atomic)
  -- This retrieves userId which was stored server-side (GDPR compliant)
  let txKey = TransactionStore.fromText stateToken
  maybeTransaction <- deps.transactionStore.consume txKey
    |> Task.mapError (\_ -> StateNotFound)
  transaction :: Transaction <- case maybeTransaction of
    Nothing -> Task.throw StateNotFound
    Just tx -> Task.yield tx

  -- userId comes from server-side storage, NOT from state token
  let retrievedUserId = transaction.userId

  -- 5. Exchange code for tokens (using pre-validated provider for performance)
  tokensResult <-
    OAuth2.exchangeCodeWithPkceValidated
      config.validatedProvider
      config.clientId
      config.clientSecret
      config.redirectUri
      (mkAuthorizationCode code)
      transaction.verifier
      |> Task.asResult

  case tokensResult of
    Err oauthError -> do
      -- Call failure callback with server-side userId
      let actionJson = config.onFailure retrievedUserId oauthError
      let action = FailureAction actionJson
      Task.yield (config.failureRedirectUrl, Just action)
    Ok tokens -> do
      -- Auto-store tokens in SecretStore with deterministic key
      let tokenKey = TokenKey [fmt|oauth:#{providerName}:#{retrievedUserId}|]
      let store = deps.secretStore
      _ <-
        store.put tokenKey tokens
          |> Task.mapError (\storeErr -> TokenExchangeFailed (NetworkError [fmt|Failed to store tokens: #{storeErr}|]))
      -- Call success callback with TokenKey (not raw tokens)
      let actionJson = config.onSuccess retrievedUserId tokenKey
      let action = SuccessAction actionJson
      Task.yield (config.successRedirectUrl, Just action)


-- | Implementation of POST /disconnect/{provider}
--
-- 1. Validate provider exists
-- 2. Delete tokens from SecretStore
-- 3. Call onDisconnect callback
-- 4. Return action to dispatch
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

  -- 2. Delete tokens from SecretStore
  let tokenKey = TokenKey [fmt|oauth:#{providerName}:#{userId}|]
  let store = deps.secretStore
  _ <-
    store.delete tokenKey
      |> Task.ignoreError  -- Deletion failure is not critical

  -- 3. Call disconnect callback
  let actionJson = config.onDisconnect userId
  Task.yield (DisconnectAction actionJson)


-- | Get current time as Unix seconds.
getCurrentTimeSeconds :: forall error. Task error Int
getCurrentTimeSeconds = do
  posixTime <- GhcPosix.getPOSIXTime |> Task.fromIO
  let seconds :: Int = GhcPrelude.floor (GhcPrelude.realToFrac posixTime :: GhcPrelude.Double)
  Task.yield seconds
