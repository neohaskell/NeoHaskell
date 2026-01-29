{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Integration.Oura.Internal (
  -- Types
  OuraHttpError (..),
  HttpFetch,
  -- Internal functions (for testing and SyncAll)
  realHttpFetch,
  executeDailySleepInternal,
  executeDailyActivityInternal,
  executeDailyReadinessInternal,
  executeHeartRateInternal,
  -- Helpers (needed by SyncAll)
  mapTokenError,
  isUnauthorized,
  getOuraProvider,
  urlEncodeDatetime,
) where

import Auth.OAuth2.TokenRefresh (TokenRefreshError (..), withValidToken)
import Auth.OAuth2.Client qualified as OAuth2
import Auth.OAuth2.Provider (ValidatedOAuth2ProviderConfig (..))  -- Needed for getOuraProvider return type
import Http.Client qualified as Http
import Integration (ActionContext (..), IntegrationError (..), CommandPayload)
import Integration qualified
import Integration.Oura.Types (PaginatedResponse (..), SleepData, ActivityData, ReadinessData, HeartRateData)
import Integration.Oura.Sleep (DailySleep (..))
import Integration.Oura.Activity (DailyActivity (..))
import Integration.Oura.Readiness (DailyReadiness (..))
import Integration.Oura.HeartRate (HeartRate (..))
import Service.Command.Core (NameOf)
import Map qualified
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Array (Array)
import Array qualified
import Maybe (Maybe (..))
import Result (Result (..))  -- For Json.decodeBytes result handling
import Basics
import ToText (toText)
import "base" GHC.TypeLits qualified as GhcTypeLits (KnownSymbol)
import Json qualified

-- | Custom error for HTTP calls + 401/429 detection
data OuraHttpError
  = Unauthorized              -- 401: trigger token refresh
  | OuraRateLimited Int       -- 429: rate limit hit, Int = suggested retry-after seconds (default 60)
  | OtherHttpError Text       -- Other errors
  deriving (Show, Eq)

-- | Helper to construct RateLimited error (disambiguates from Integration.RateLimited)
makeRateLimitedError :: Int -> OuraHttpError
makeRateLimitedError seconds = OuraRateLimited seconds

-- | Canonical injectable HTTP fetch type for testability (USED EVERYWHERE in plan)
-- Takes: access token -> URL -> returns PaginatedResponse on success, throws OuraHttpError on failure
-- NOTE: This is the ONLY definition of HttpFetch - all snippets/tests should use this exact type
type HttpFetch a = Text -> Text -> Task OuraHttpError (PaginatedResponse a)

-- | Real HTTP fetch implementation using Http.getRaw (NOT Http.get!)
-- 
-- CRITICAL: We use Http.getRaw because Http.get throws StatusCodeException on non-2xx.
-- Http.getRaw uses setRequestIgnoreStatus so we can inspect statusCode before decoding JSON.
-- 
-- Flow:
-- 1. Make request with Http.getRaw (returns Bytes body, no throw on 401/429)
-- 2. Check statusCode for 401/429
-- 3. If 2xx, decode JSON with Json.decodeBytes (returns Result Text value)
realHttpFetch :: forall a. (Json.FromJSON a) => HttpFetch a
realHttpFetch accessToken url = do
  response <- Http.request
    |> Http.withUrl url
    |> Http.addHeader "Authorization" [fmt|Bearer #{accessToken}|]
    |> Http.getRaw  -- Returns Response Bytes, no throw on non-2xx
    |> Task.mapError (\(Http.Error msg) -> OtherHttpError msg)
  -- Check status code FIRST (before any JSON decoding)
  case response.statusCode of
    401 -> Task.throw Unauthorized
    429 -> do
      -- Extract Retry-After header if present, default to 60 seconds
      let retryAfter = case Array.find (\(k, _) -> k == "Retry-After") response.headers of
            Nothing -> 60
            Just (_, v) -> case Text.toInt v of
              Nothing -> 60
              Just n -> n
      Task.throw (makeRateLimitedError retryAfter)
    statusCode | statusCode >= 200 && statusCode < 300 -> do
      -- Success - decode JSON with Json.decodeBytes (returns Result Text value)
      case Json.decodeBytes response.body of
        Ok paginated -> Task.yield paginated
        Err decodeErr -> Task.throw (OtherHttpError [fmt|JSON decode failed: #{decodeErr}|])
    other ->
      Task.throw (OtherHttpError [fmt|Unexpected status code: #{other}|])

-- | ToAction for DailySleep - uses actionWithContext
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (DailySleep command) where
  toAction config = Integration.actionWithContext \ctx ->
    executeDailySleep realHttpFetch ctx config

-- | ToAction for DailyActivity
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (DailyActivity command) where
  toAction config = Integration.actionWithContext \ctx ->
    executeDailyActivity realHttpFetch ctx config

-- | ToAction for DailyReadiness
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (DailyReadiness command) where
  toAction config = Integration.actionWithContext \ctx ->
    executeDailyReadiness realHttpFetch ctx config

-- | ToAction for HeartRate
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (HeartRate command) where
  toAction config = Integration.actionWithContext \ctx ->
    executeHeartRate realHttpFetch ctx config

-- | Execute DailySleep - wraps withValidToken around internal
-- Handles onError callback: if present, emit error command; if absent, throw
executeDailySleep ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch SleepData ->
  ActionContext ->
  DailySleep command ->
  Task IntegrationError (Maybe CommandPayload)
executeDailySleep httpFetch ctx config = do
  let DailySleep userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/daily_sleep?start_date=#{startDate}&end_date=#{endDate}|]
  -- Execute with token refresh and error handling
  fetchResult <- withValidToken
    secretStore
    "oura"
    userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> executeDailySleepInternal httpFetch accessToken baseUrl)
    |> Task.asResult
  -- Handle result with onError callback
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

-- | Internal function for testing - takes accessToken directly, fetches all pages
executeDailySleepInternal ::
  HttpFetch SleepData ->
  Text ->  -- access token
  Text ->  -- base URL (without next_token)
  Task OuraHttpError (Array SleepData)
executeDailySleepInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- Similar pattern for Activity, Readiness, HeartRate - all follow same onError handling rule
executeDailyActivity ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch ActivityData ->
  ActionContext ->
  DailyActivity command ->
  Task IntegrationError (Maybe CommandPayload)
executeDailyActivity httpFetch ctx config = do
  let DailyActivity userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/daily_activity?start_date=#{startDate}&end_date=#{endDate}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  -- Handle result with onError callback (same pattern as executeDailySleep)
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeDailyActivityInternal :: HttpFetch ActivityData -> Text -> Text -> Task OuraHttpError (Array ActivityData)
executeDailyActivityInternal = fetchAllPages

executeDailyReadiness ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch ReadinessData ->
  ActionContext ->
  DailyReadiness command ->
  Task IntegrationError (Maybe CommandPayload)
executeDailyReadiness httpFetch ctx config = do
  let DailyReadiness userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/daily_readiness?start_date=#{startDate}&end_date=#{endDate}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  -- Handle result with onError callback (same pattern as executeDailySleep)
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeDailyReadinessInternal :: HttpFetch ReadinessData -> Text -> Text -> Task OuraHttpError (Array ReadinessData)
executeDailyReadinessInternal = fetchAllPages

executeHeartRate ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch HeartRateData ->
  ActionContext ->
  HeartRate command ->
  Task IntegrationError (Maybe CommandPayload)
executeHeartRate httpFetch ctx config = do
  let HeartRate userId startDatetime endDatetime onSuccess onError = config
  let ActionContext secretStore _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  -- NOTE: HeartRate uses start_datetime/end_datetime, and + must be URL-encoded
  -- We URL-encode the datetime values to handle + in timezone offsets
  let startEncoded = urlEncodeDatetime startDatetime
  let endEncoded = urlEncodeDatetime endDatetime
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/heartrate?start_datetime=#{startEncoded}&end_datetime=#{endEncoded}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  -- Handle result with onError callback (same pattern as other endpoints)
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeHeartRateInternal :: HttpFetch HeartRateData -> Text -> Text -> Task OuraHttpError (Array HeartRateData)
executeHeartRateInternal = fetchAllPages

-- | URL-encode datetime for Oura API (handles + in timezone)
urlEncodeDatetime :: Text -> Text
urlEncodeDatetime dt = dt |> Text.replace "+" "%2B"

-- | Get Oura provider from context or throw error
-- NOTE: ValidatedOAuth2ProviderConfig is from core/auth/Auth/OAuth2/Provider.hs:149-172
getOuraProvider :: ActionContext -> Task Integration.IntegrationError ValidatedOAuth2ProviderConfig
getOuraProvider ctx = do
  let ActionContext _ providerRegistry = ctx
  case Map.get "oura" providerRegistry of
    Nothing -> Task.throw (UnexpectedError "Oura provider not configured. Add Oura.makeOuraConfig to Application.withOAuth2.")
    Just provider -> Task.yield provider

-- | Check if error is Unauthorized (401) - used by withValidToken to trigger refresh
isUnauthorized :: OuraHttpError -> Bool
isUnauthorized err = case err of
  Unauthorized -> True
  OuraRateLimited _ -> False  -- Don't retry on rate limit
  OtherHttpError _ -> False

-- | Map TokenRefreshError to IntegrationError
-- TokenRefreshError err has constructors (core/auth/Auth/OAuth2/TokenRefresh.hs:14-26):
--   TokenNotFound Text (userId)
--   RefreshTokenMissing
--   RefreshFailed OAuth2Error
--   StorageError Text
--   ActionFailed err
mapTokenError :: TokenRefreshError OuraHttpError -> IntegrationError
mapTokenError err = case err of
  TokenNotFound userId -> AuthenticationError [fmt|Token not found for user: #{userId}|]
  RefreshTokenMissing -> AuthenticationError "Refresh token missing"
  RefreshFailed oauth2Err -> AuthenticationError [fmt|Refresh failed: #{toText oauth2Err}|]
  StorageError msg -> UnexpectedError [fmt|Storage error: #{msg}|]
  ActionFailed (OtherHttpError msg) -> NetworkError msg
  ActionFailed Unauthorized -> AuthenticationError "Unauthorized after refresh"
  ActionFailed (OuraRateLimited retryAfter) -> RateLimited retryAfter  -- Re-use existing IntegrationError constructor

-- | Fetch all pages using pagination
-- Uses baseUrl for first request, then appends &next_token=X for subsequent requests
fetchAllPages ::
  forall a.
  HttpFetch a ->
  Text ->  -- access token
  Text ->  -- base URL (without next_token)
  Task OuraHttpError (Array a)
fetchAllPages httpFetch accessToken baseUrl =
  fetchPagesLoop httpFetch accessToken baseUrl Nothing Array.empty

-- | Pagination loop - accumulates results until next_token is Nothing
-- Keeps track of baseUrl separately from pagination to avoid accumulating tokens
fetchPagesLoop ::
  forall a.
  HttpFetch a ->
  Text ->          -- access token
  Text ->          -- base URL (never changes)
  Maybe Text ->    -- current next_token (Nothing for first request)
  Array a ->       -- accumulated results
  Task OuraHttpError (Array a)
fetchPagesLoop httpFetch accessToken baseUrl maybeNextToken accumulated = do
  -- Build URL: baseUrl for first request, baseUrl&next_token=X for subsequent
  let url = case maybeNextToken of
        Nothing -> baseUrl
        Just token -> [fmt|#{baseUrl}&next_token=#{token}|]
  paginated <- httpFetch accessToken url  -- Returns PaginatedResponse a directly
  let PaginatedResponse responseData nextToken = paginated
  let newAccumulated = accumulated |> Array.append (Array.fromLinkedList responseData)
  case nextToken of
    Nothing -> Task.yield newAccumulated
    Just nextTok -> fetchPagesLoop httpFetch accessToken baseUrl (Just nextTok) newAccumulated
