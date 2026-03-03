module Auth.OAuth2.TokenRefresh (
  TokenRefreshError (..),
  withSingleFlightRefresh,
  withValidToken,
) where

import Auth.OAuth2.Types (OAuth2Error (..), RefreshToken, TokenSet (..), unwrapAccessToken, unwrapRefreshToken)
import Auth.SecretStore (SecretStore (..), TokenKey (..))
import ConcurrentMap (ConcurrentMap)
import ConcurrentMap qualified
import ConcurrentVar qualified
import Core
import IO qualified
import Lock qualified
import Result qualified
import Task qualified


-- | Errors that can occur during token refresh operations.
data TokenRefreshError err
  = -- | No tokens stored for userId. The Text parameter is the userId (NOT the full key).
    -- Example: TokenNotFound "user-123" (not "oauth:provider:user-123")
    TokenNotFound Text
  | -- | Refresh token is missing from stored token set
    RefreshTokenMissing
  | -- | Token refresh request failed
    RefreshFailed OAuth2Error
  | -- | Storage operation failed
    StorageError Text
  | -- | Custom action failed (e.g., HTTP request)
    ActionFailed err
  deriving (Generic, Show, Eq)


type RefreshWaiter = ConcurrentVar (Result OAuth2Error TokenSet)


globalRefreshGuardLock :: Lock
globalRefreshGuardLock =
  (Lock.new :: Task Never Lock)
    |> Task.runNoErrors
    |> IO.dangerouslyRun
{-# NOINLINE globalRefreshGuardLock #-}


globalRefreshFlights :: ConcurrentMap Text RefreshWaiter
globalRefreshFlights =
  (ConcurrentMap.new :: Task Never (ConcurrentMap Text RefreshWaiter))
    |> Task.runNoErrors
    |> IO.dangerouslyRun
{-# NOINLINE globalRefreshFlights #-}


-- | Execute token refresh with single-flight guard semantics.
--
-- Concurrent callers are serialized via a module-internal global lock so only
-- one refresh request runs at a time per refresh token. Other callers with the
-- same refresh token wait for the leader's result.
--
-- NOTE: The @_callerLock@ parameter is accepted for API compatibility but
-- ignored — the function always uses the module-internal 'globalRefreshGuardLock'
-- to ensure a single serialization domain for 'globalRefreshFlights'.
withSingleFlightRefresh ::
  Lock ->
  (RefreshToken -> Task OAuth2Error TokenSet) ->
  RefreshToken ->
  Task OAuth2Error TokenSet
withSingleFlightRefresh _callerLock refreshAction refreshToken = do
  let refreshKey = unwrapRefreshToken refreshToken
  (isLeader, refreshWaiter) <-
    Lock.with globalRefreshGuardLock do
      maybeWaiter <- ConcurrentMap.get refreshKey globalRefreshFlights
      case maybeWaiter of
        Just existingWaiter -> Task.yield (False, existingWaiter)
        Nothing -> do
          newWaiter <- ConcurrentVar.new
          ConcurrentMap.set refreshKey newWaiter globalRefreshFlights
          Task.yield (True, newWaiter)

  case isLeader of
    True -> do
      safeRefreshResult <-
        (refreshAction refreshToken |> Task.asResult :: Task Never (Result OAuth2Error TokenSet))
          |> Task.asResultSafe
      let refreshResult =
            case safeRefreshResult of
              Ok typedResult -> typedResult
              Err _unexpectedErr ->
                Result.Err (TokenRequestFailed "Token refresh crashed unexpectedly")
      ConcurrentVar.set refreshResult refreshWaiter
      Lock.with globalRefreshGuardLock do
        ConcurrentMap.remove refreshKey globalRefreshFlights
      case refreshResult of
        Result.Ok tokenSet -> Task.yield tokenSet
        Result.Err refreshError -> Task.throw refreshError
    False -> do
      -- Non-destructive read: peek uses readMVar (does not remove the value).
      -- This is safe for multiple waiters — all can read the result.
      refreshResult <- ConcurrentVar.peek refreshWaiter
      case refreshResult of
        Result.Ok tokenSet -> Task.yield tokenSet
        Result.Err refreshError -> Task.throw refreshError


-- | Execute an action with automatic token refresh on unauthorized errors.
--
-- This function:
-- 1. Retrieves tokens from SecretStore using key "oauth:{providerName}:{userId}"
-- 2. Executes the action with the access token
-- 3. If the predicate identifies an "unauthorized" error, refreshes the token
-- 4. Stores the new TokenSet and retries the action ONCE
--
-- The refresh action is injectable for testability. In production, pass:
-- @Auth.OAuth2.Client.refreshTokenValidated validatedProvider clientId clientSecret@
--
-- Example production usage:
--
-- > withValidToken store "oura" userId
-- >   (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
-- >   isUnauthorized
-- >   myAction
--
-- Example test usage with mock:
--
-- > withValidToken mockStore "provider" "user1"
-- >   mockRefreshSuccess
-- >   isUnauthorized
-- >   (\token -> Task.yield [fmt|got #{token}|])
withValidToken ::
  forall err value.
  SecretStore ->
  -- | Provider name (e.g., "oura")
  Text ->
  -- | User ID
  Text ->
  -- | Injectable refresh action
  (RefreshToken -> Task OAuth2Error TokenSet) ->
  -- | Predicate: is this error unauthorized?
  (err -> Bool) ->
  -- | Action using access token
  (Text -> Task err value) ->
  Task (TokenRefreshError err) value
withValidToken store providerName userId refreshAction isUnauthorized action = do
  -- 1. Build TokenKey using production format
  let tokenKey = TokenKey [fmt|oauth:#{providerName}:#{userId}|]

  -- 2. Get tokens from store
  (maybeTokens :: Maybe TokenSet) <-
    store.get tokenKey
      |> Task.mapError StorageError

  case maybeTokens of
    Nothing -> Task.throw (TokenNotFound userId)
    Just tokenSet -> do
      -- 3. Extract access token and run action
      let accessToken = unwrapAccessToken tokenSet.accessToken

      firstResult <-
        action accessToken
          |> Task.mapError ActionFailed
          |> Task.asResult

      case firstResult of
        Result.Ok result -> Task.yield result
        Result.Err (ActionFailed err) | isUnauthorized err -> do
          -- 4. Need to refresh - check for refresh token
          case tokenSet.refreshToken of
            Nothing -> Task.throw RefreshTokenMissing
            Just rt -> do
              -- 5. Call injectable refresh action
              newTokens <-
                withSingleFlightRefresh globalRefreshGuardLock refreshAction rt
                  |> Task.mapError RefreshFailed

              -- 6. Store new tokens
              store.atomicModify tokenKey (\_ -> Just newTokens)
                |> Task.mapError StorageError

              -- 7. Retry ONCE with new access token
              let newAccessToken = unwrapAccessToken newTokens.accessToken
              action newAccessToken
                |> Task.mapError ActionFailed
        Result.Err err -> Task.throw err
