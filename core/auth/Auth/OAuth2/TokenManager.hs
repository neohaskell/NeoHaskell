module Auth.OAuth2.TokenManager (
  TokenManager,
  TokenManagerConfig (..),
  mkTokenManagerConfig,
  new,
  getToken,
  startRefreshLoop,
) where

import AsyncTask qualified
import Auth.OAuth2.TokenRefresh (TokenRefreshError (..), withSingleFlightRefresh)
import Auth.OAuth2.Types (OAuth2Error, RefreshToken, TokenSet (..))
import Auth.SecretStore (SecretStore (..), TokenKey (..))
import Basics

import Core
import DateTime qualified
import Lock qualified
import Task qualified


-- | Configuration for the TokenManager.
data TokenManagerConfig = TokenManagerConfig
  { -- | How many seconds before expiry to proactively refresh.
    proactiveRefreshLeadSeconds :: !Int
  , -- | Maximum number of refresh retries before giving up.
    maxRefreshRetries :: !Int
  }
  deriving (Generic, Show, Eq)


-- | Smart constructor that validates TokenManagerConfig.
-- Returns Err if proactiveRefreshLeadSeconds <= 0 or maxRefreshRetries < 1.
mkTokenManagerConfig :: Int -> Int -> Result Text TokenManagerConfig
mkTokenManagerConfig leadSeconds retries = do
  case leadSeconds > 0 && retries >= 1 of
    True ->
      Ok
        TokenManagerConfig
          { proactiveRefreshLeadSeconds = leadSeconds
          , maxRefreshRetries = retries
          }
    False ->
      Err "Invalid config: proactiveRefreshLeadSeconds > 0 and maxRefreshRetries >= 1 required"


-- | Opaque token manager type.
data TokenManager = TokenManager
  { secretStore :: !SecretStore
  , config :: !TokenManagerConfig
  , refreshLock :: !Lock
  }


-- | Create a new TokenManager.
new :: SecretStore -> TokenManagerConfig -> Task Text TokenManager
new store cfg = do
  lock <- Lock.new
  Task.yield
    TokenManager
      { secretStore = store
      , config = cfg
      , refreshLock = lock
      }


-- | Get a token, refreshing proactively if it is near expiry.
-- Returns the stored TokenSet, or Err if not found.
getToken ::
  TokenManager ->
  -- | Provider name (e.g., "oura")
  Text ->
  -- | User ID
  Text ->
  -- | Injectable refresh action
  (RefreshToken -> Task OAuth2Error TokenSet) ->
  Task (TokenRefreshError Text) TokenSet
getToken manager providerName userId refreshAction = do
  let tokenKey = TokenKey [fmt|oauth:#{providerName}:#{userId}|]
  maybeTokens <-
    manager.secretStore.get tokenKey
      |> Task.mapError StorageError
  case maybeTokens of
    Nothing -> Task.throw (TokenNotFound userId)
    Just tokenSet -> do
      -- Check if proactive refresh is needed
      shouldRefresh <- needsProactiveRefresh manager tokenSet
      case shouldRefresh of
        False -> Task.yield tokenSet
        True -> do
          case tokenSet.refreshToken of
            Nothing -> Task.yield tokenSet
            Just rt -> do
              newTokens <-
                withSingleFlightRefresh manager.refreshLock refreshAction rt
                  |> Task.mapError RefreshFailed
              manager.secretStore.atomicModify tokenKey (\_ -> Just newTokens)
                |> Task.mapError StorageError
              Task.yield newTokens


-- | Check if a token needs proactive refresh based on expiresAt and lead time.
needsProactiveRefresh :: TokenManager -> TokenSet -> Task (TokenRefreshError Text) Bool
needsProactiveRefresh manager tokenSet = do
  case tokenSet.expiresAt of
    Nothing -> Task.yield False
    Just expiresAt -> do
      now <- DateTime.now |> Task.mapError (\_ -> StorageError "clock error")
      let nowSeconds = DateTime.toEpochSeconds now
      let expirySeconds = DateTime.toEpochSeconds expiresAt
      let leadSeconds = fromIntegral manager.config.proactiveRefreshLeadSeconds
      Task.yield (nowSeconds + leadSeconds >= expirySeconds)


-- | Start a background loop that proactively refreshes tokens before expiry.
-- This loop runs indefinitely until the task is cancelled.
startRefreshLoop ::
  TokenManager ->
  -- | Provider name
  Text ->
  -- | User ID
  Text ->
  -- | Injectable refresh action
  (RefreshToken -> Task OAuth2Error TokenSet) ->
  Task Text Unit
startRefreshLoop manager providerName userId refreshAction = do
  let loop = do
        -- Check every 30 seconds
        AsyncTask.sleep 30000
        result <-
          getToken manager providerName userId refreshAction
            |> Task.mapError toText
            |> Task.asResult
        case result of
          Ok _ -> loop
          Err _ -> loop
  loop
