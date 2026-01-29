module Auth.OAuth2.TokenRefresh (
  TokenRefreshError (..),
  withValidToken,
) where

import Auth.OAuth2.Types (OAuth2Error, RefreshToken, TokenSet (..), unwrapAccessToken)
import Auth.SecretStore (SecretStore (..), TokenKey (..))
import Core
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
                refreshAction rt
                  |> Task.mapError RefreshFailed

              -- 6. Store new tokens
              store.atomicModify tokenKey (\_ -> Just newTokens)
                |> Task.mapError StorageError

              -- 7. Retry ONCE with new access token
              let newAccessToken = unwrapAccessToken newTokens.accessToken
              action newAccessToken
                |> Task.mapError ActionFailed
        Result.Err err -> Task.throw err
