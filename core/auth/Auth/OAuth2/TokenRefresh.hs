module Auth.OAuth2.TokenRefresh (
  TokenRefreshError (..),
  withValidToken,
) where

import Auth.OAuth2.Types (OAuth2Error, RefreshToken, TokenSet (..), unwrapAccessToken)
import Auth.SecretStore (SecretStore (..), TokenKey (..))
import ConcurrentMap (ConcurrentMap)
import ConcurrentMap qualified
import Core
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
  | -- | Per-key lock acquisition timed out (prevents deadlock).
    -- The Text parameter is the lock key (format: "providerName:userId").
    -- SECURITY: Lock key does NOT contain userId alone — it is safe to log.
    LockAcquisitionTimeout Text
  deriving (Generic, Show, Eq)


-- | Execute an action with automatic token refresh on unauthorized errors.
--
-- This function:
-- 1. Retrieves tokens from SecretStore using key "oauth:{providerName}:{userId}"
-- 2. Executes the action with the access token
-- 3. If the predicate identifies an "unauthorized" error, refreshes the token
-- 4. Uses per-key locking to prevent duplicate concurrent refreshes (#333)
-- 5. Stores the new TokenSet and retries the action ONCE
--
-- The refresh action is injectable for testability. In production, pass:
-- @Auth.OAuth2.Client.refreshTokenValidated validatedProvider clientId clientSecret@
--
-- Example production usage:
--
-- > withValidToken store refreshLocks "oura" userId
-- >   (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
-- >   isUnauthorized
-- >   myAction
--
-- Example test usage with mock:
--
-- > withValidToken mockStore mockLocks "provider" "user1"
-- >   mockRefreshSuccess
-- >   isUnauthorized
-- >   (\token -> Task.yield [fmt|got #{token}|])
{-# INLINE withValidToken #-}
withValidToken ::
  forall err value.
  SecretStore ->
  -- | Per-key refresh lock map (prevents duplicate concurrent refreshes)
  ConcurrentMap Text Lock ->
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
withValidToken store refreshLocks providerName userId refreshAction isUnauthorized action = do
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
              -- 5. Acquire per-key refresh lock (prevents duplicate concurrent refreshes).
              -- Lock key format: "providerName:userId" (safe to log — not userId alone).
              let lockKey = [fmt|#{providerName}:#{userId}|]
              newLock <- Lock.new |> Task.mapError StorageError
              (refreshLock, _) <- refreshLocks |> ConcurrentMap.getOrInsert lockKey newLock
                |> Task.mapError StorageError

              -- 6. Acquire lock with 30s timeout (prevents deadlock).
              lockResult <- Lock.withTimeout 30 refreshLock do
                -- 7. Double-check: re-read token after acquiring lock.
                -- Another concurrent request may have already refreshed it.
                reCheckedTokens <-
                  store.get tokenKey
                    |> Task.mapError StorageError

                case reCheckedTokens of
                  Nothing ->
                    -- Token deleted between reads — signal not found.
                    Task.throw (TokenNotFound userId)
                  Just currentTokenSet -> do
                    let currentAccessToken = unwrapAccessToken currentTokenSet.accessToken
                    if currentAccessToken != accessToken
                      then
                        -- Another thread already refreshed — use their fresh token.
                        Task.yield currentTokenSet
                      else do
                        -- Token not yet refreshed — perform the refresh.
                        -- Call injectable refresh action
                        newTokens <-
                          refreshAction rt
                            |> Task.mapError RefreshFailed

                        -- Atomically store refreshed tokens (TOCTOU fix from #334).
                        -- Note: OverloadedRecordDot doesn't support rank-2 fields, so we use case..of.
                        case store of
                          SecretStore { atomicModifyReturning = amrFn } ->
                            amrFn tokenKey (\_ -> Task.yield (Just newTokens, newTokens))
                              |> Task.mapError StorageError

              case lockResult of
                Err _ ->
                  Task.throw (LockAcquisitionTimeout lockKey)
                Ok freshTokens -> do
                  -- 8. Retry ONCE with new access token
                  let newAccessToken = unwrapAccessToken freshTokens.accessToken
                  action newAccessToken
                    |> Task.mapError ActionFailed
        Result.Err err -> Task.throw err
