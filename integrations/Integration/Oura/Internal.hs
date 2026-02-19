{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Integration.Oura.Internal (
  -- Types
  OuraHttpError (..),
  HttpFetch,
  HttpFetchSingle,
  -- Internal functions (for testing and SyncAll)
  realHttpFetch,
  realHttpFetchSingle,
  executeDailySleepInternal,
  executeDailyActivityInternal,
  executeDailyReadinessInternal,
  executeHeartRateInternal,
  executeWorkoutInternal,
  executeSessionInternal,
  executePersonalInfoInternal,
  executeDailyStressInternal,
  executeDailySpO2Internal,
  executeDailyResilienceInternal,
  executeDailyCardiovascularAgeInternal,
  executeVO2MaxInternal,
  executeEnhancedTagInternal,
  executeSleepTimeInternal,
  executeRestModePeriodInternal,
  executeRingConfigurationInternal,
  executeSleepPeriodInternal,
  -- Helpers (needed by SyncAll)
  mapTokenError,
  isUnauthorized,
  getOuraProvider,
  urlEncodeDatetime,
  urlEncodeParam,
) where

import Auth.OAuth2.TokenRefresh (TokenRefreshError (..), withValidToken)
import Auth.OAuth2.Client qualified as OAuth2
import Auth.OAuth2.Provider (ValidatedOAuth2ProviderConfig (..))  -- Needed for getOuraProvider return type
import Http.Client qualified as Http
import Integration (ActionContext (..), IntegrationError (..), CommandPayload)
import Integration qualified
import Integration.Oura.Types (PaginatedResponse (..), SleepData, ActivityData, ReadinessData, HeartRateData, WorkoutData, SessionData, PersonalInfoData, DailyStressData, DailySpO2Data, DailyResilienceData, DailyCardiovascularAgeData, VO2MaxData, EnhancedTagData, SleepTimeData, RestModePeriodData, RingConfigurationData, SleepPeriodData)
import Integration.Oura.Sleep (DailySleep (..))
import Integration.Oura.Activity (DailyActivity (..))
import Integration.Oura.Readiness (DailyReadiness (..))
import Integration.Oura.HeartRate (HeartRate (..))
import Integration.Oura.Workout (Workout (..))
import Integration.Oura.Session (Session (..))
import Integration.Oura.PersonalInfo (PersonalInfo (..))
import Integration.Oura.DailyStress (DailyStress (..))
import Integration.Oura.DailySpO2 (DailySpO2 (..))
import Integration.Oura.DailyResilience (DailyResilience (..))
import Integration.Oura.DailyCardiovascularAge (DailyCardiovascularAge (..))
import Integration.Oura.VO2Max (VO2Max (..))
import Integration.Oura.EnhancedTag (EnhancedTag (..))
import Integration.Oura.SleepTime (SleepTime (..))
import Integration.Oura.RestModePeriod (RestModePeriod (..))
import Integration.Oura.RingConfiguration (RingConfiguration (..))
import Integration.Oura.SleepPeriod (SleepPeriod (..))
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

-- | Injectable HTTP fetch type for non-paginated endpoints (like PersonalInfo)
-- Takes: access token -> URL -> returns decoded JSON object directly
type HttpFetchSingle a = Text -> Text -> Task OuraHttpError a

-- | Shared HTTP request with Bearer auth and status code handling
--
-- Handles the common HTTP dispatch logic used by both realHttpFetch and realHttpFetchSingle:
-- 1. Make request with Http.getRaw (returns Bytes body, no throw on 401/429)
-- 2. Check statusCode: 401 -> Unauthorized, 429 -> parse Retry-After + RateLimited
-- 3. If 2xx, decode JSON with Json.decodeBytes (type-directed: PaginatedResponse or flat)
-- 4. Other status codes become OtherHttpError
httpRequestWithAuth ::
  forall decoded.
  (Json.FromJSON decoded) =>
  Text ->  -- access token
  Text ->  -- URL
  Task OuraHttpError decoded
httpRequestWithAuth accessToken url = do
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
      -- NOTE: HTTP headers are case-insensitive per RFC 7230, so we normalize to lowercase
      let retryAfter = case Array.find (\(k, _) -> Text.toLower k == "retry-after") response.headers of
            Nothing -> 60
            Just (_, v) -> case Text.toInt v of
              Nothing -> 60
              Just n -> n
      Task.throw (makeRateLimitedError retryAfter)
    statusCode | statusCode >= 200 && statusCode < 300 -> do
      -- Success - decode JSON with Json.decodeBytes (returns Result Text value)
      case Json.decodeBytes response.body of
        Ok value -> Task.yield value
        Err decodeErr -> Task.throw (OtherHttpError [fmt|JSON decode failed: #{decodeErr}|])
    other ->
      Task.throw (OtherHttpError [fmt|Unexpected status code: #{other}|])

-- | Real HTTP fetch implementation - decodes as PaginatedResponse via httpRequestWithAuth
realHttpFetch :: forall value. (Json.FromJSON value) => HttpFetch value
realHttpFetch accessToken url = httpRequestWithAuth accessToken url

-- | Real HTTP fetch for non-paginated endpoints - decodes flat JSON via httpRequestWithAuth
realHttpFetchSingle :: forall value. (Json.FromJSON value) => HttpFetchSingle value
realHttpFetchSingle accessToken url = httpRequestWithAuth accessToken url

-- | ToAction for DailySleep - uses action
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (DailySleep command) where
  toAction config = Integration.action \ctx ->
    executeDailySleep realHttpFetch ctx config

-- | ToAction for DailyActivity
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (DailyActivity command) where
  toAction config = Integration.action \ctx ->
    executeDailyActivity realHttpFetch ctx config

-- | ToAction for DailyReadiness
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (DailyReadiness command) where
  toAction config = Integration.action \ctx ->
    executeDailyReadiness realHttpFetch ctx config

-- | ToAction for HeartRate
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (HeartRate command) where
  toAction config = Integration.action \ctx ->
    executeHeartRate realHttpFetch ctx config

-- | ToAction for Workout
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (Workout command) where
  toAction config = Integration.action \ctx ->
    executeWorkout realHttpFetch ctx config

-- | ToAction for Session
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (Session command) where
  toAction config = Integration.action \ctx ->
    executeSession realHttpFetch ctx config

-- | ToAction for PersonalInfo
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (PersonalInfo command) where
  toAction config = Integration.action \ctx ->
    executePersonalInfo realHttpFetchSingle ctx config

-- | ToAction for DailyStress
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (DailyStress command) where
  toAction config = Integration.action \ctx ->
    executeDailyStress realHttpFetch ctx config

-- | ToAction for DailySpO2
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (DailySpO2 command) where
  toAction config = Integration.action \ctx ->
    executeDailySpO2 realHttpFetch ctx config

-- | ToAction for DailyResilience
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (DailyResilience command) where
  toAction config = Integration.action \ctx ->
    executeDailyResilience realHttpFetch ctx config

-- | ToAction for DailyCardiovascularAge
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (DailyCardiovascularAge command) where
  toAction config = Integration.action \ctx ->
    executeDailyCardiovascularAge realHttpFetch ctx config

-- | ToAction for VO2Max
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (VO2Max command) where
  toAction config = Integration.action \ctx ->
    executeVO2Max realHttpFetch ctx config

-- | ToAction for EnhancedTag
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (EnhancedTag command) where
  toAction config = Integration.action \ctx ->
    executeEnhancedTag realHttpFetch ctx config

-- | ToAction for SleepTime
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (SleepTime command) where
  toAction config = Integration.action \ctx ->
    executeSleepTime realHttpFetch ctx config

-- | ToAction for RestModePeriod
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (RestModePeriod command) where
  toAction config = Integration.action \ctx ->
    executeRestModePeriod realHttpFetch ctx config

-- | ToAction for RingConfiguration
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (RingConfiguration command) where
  toAction config = Integration.action \ctx ->
    executeRingConfiguration realHttpFetch ctx config

-- | ToAction for SleepPeriod
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (SleepPeriod command) where
  toAction config = Integration.action \ctx ->
    executeSleepPeriod realHttpFetch ctx config

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
  let ActionContext secretStore _ _ = ctx
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
  let ActionContext secretStore _ _ = ctx
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
executeDailyActivityInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

executeDailyReadiness ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch ReadinessData ->
  ActionContext ->
  DailyReadiness command ->
  Task IntegrationError (Maybe CommandPayload)
executeDailyReadiness httpFetch ctx config = do
  let DailyReadiness userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
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
executeDailyReadinessInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

executeHeartRate ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch HeartRateData ->
  ActionContext ->
  HeartRate command ->
  Task IntegrationError (Maybe CommandPayload)
executeHeartRate httpFetch ctx config = do
  let HeartRate userId startDatetime endDatetime onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
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
executeHeartRateInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- | Execute Workout
executeWorkout ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch WorkoutData ->
  ActionContext ->
  Workout command ->
  Task IntegrationError (Maybe CommandPayload)
executeWorkout httpFetch ctx config = do
  let Workout userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/workout?start_date=#{startDate}&end_date=#{endDate}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeWorkoutInternal :: HttpFetch WorkoutData -> Text -> Text -> Task OuraHttpError (Array WorkoutData)
executeWorkoutInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- | Execute Session (uses datetime params like HeartRate)
executeSession ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch SessionData ->
  ActionContext ->
  Session command ->
  Task IntegrationError (Maybe CommandPayload)
executeSession httpFetch ctx config = do
  let Session userId startDatetime endDatetime onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let startEncoded = urlEncodeDatetime startDatetime
  let endEncoded = urlEncodeDatetime endDatetime
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/session?start_datetime=#{startEncoded}&end_datetime=#{endEncoded}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeSessionInternal :: HttpFetch SessionData -> Text -> Text -> Task OuraHttpError (Array SessionData)
executeSessionInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- | Execute PersonalInfo (non-paginated, no date params)
executePersonalInfo ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetchSingle PersonalInfoData ->
  ActionContext ->
  PersonalInfo command ->
  Task IntegrationError (Maybe CommandPayload)
executePersonalInfo httpFetchSingle ctx config = do
  let PersonalInfo userId onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let url = "https://api.ouraring.com/v2/usercollection/personal_info"
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> httpFetchSingle accessToken url)
    |> Task.asResult
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executePersonalInfoInternal :: HttpFetchSingle PersonalInfoData -> Text -> Text -> Task OuraHttpError PersonalInfoData
executePersonalInfoInternal httpFetchSingle accessToken url = httpFetchSingle accessToken url

-- | Execute DailyStress
executeDailyStress ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch DailyStressData ->
  ActionContext ->
  DailyStress command ->
  Task IntegrationError (Maybe CommandPayload)
executeDailyStress httpFetch ctx config = do
  let DailyStress userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/daily_stress?start_date=#{startDate}&end_date=#{endDate}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeDailyStressInternal :: HttpFetch DailyStressData -> Text -> Text -> Task OuraHttpError (Array DailyStressData)
executeDailyStressInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- | Execute DailySpO2
executeDailySpO2 ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch DailySpO2Data ->
  ActionContext ->
  DailySpO2 command ->
  Task IntegrationError (Maybe CommandPayload)
executeDailySpO2 httpFetch ctx config = do
  let DailySpO2 userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/daily_spo2?start_date=#{startDate}&end_date=#{endDate}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeDailySpO2Internal :: HttpFetch DailySpO2Data -> Text -> Text -> Task OuraHttpError (Array DailySpO2Data)
executeDailySpO2Internal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- | Execute DailyResilience
executeDailyResilience ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch DailyResilienceData ->
  ActionContext ->
  DailyResilience command ->
  Task IntegrationError (Maybe CommandPayload)
executeDailyResilience httpFetch ctx config = do
  let DailyResilience userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/daily_resilience?start_date=#{startDate}&end_date=#{endDate}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeDailyResilienceInternal :: HttpFetch DailyResilienceData -> Text -> Text -> Task OuraHttpError (Array DailyResilienceData)
executeDailyResilienceInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- | Execute DailyCardiovascularAge
executeDailyCardiovascularAge ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch DailyCardiovascularAgeData ->
  ActionContext ->
  DailyCardiovascularAge command ->
  Task IntegrationError (Maybe CommandPayload)
executeDailyCardiovascularAge httpFetch ctx config = do
  let DailyCardiovascularAge userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/daily_cardiovascular_age?start_date=#{startDate}&end_date=#{endDate}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeDailyCardiovascularAgeInternal :: HttpFetch DailyCardiovascularAgeData -> Text -> Text -> Task OuraHttpError (Array DailyCardiovascularAgeData)
executeDailyCardiovascularAgeInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- | Execute VO2Max
executeVO2Max ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch VO2MaxData ->
  ActionContext ->
  VO2Max command ->
  Task IntegrationError (Maybe CommandPayload)
executeVO2Max httpFetch ctx config = do
  let VO2Max userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/vo2_max?start_date=#{startDate}&end_date=#{endDate}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeVO2MaxInternal :: HttpFetch VO2MaxData -> Text -> Text -> Task OuraHttpError (Array VO2MaxData)
executeVO2MaxInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- | Execute EnhancedTag
executeEnhancedTag ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch EnhancedTagData ->
  ActionContext ->
  EnhancedTag command ->
  Task IntegrationError (Maybe CommandPayload)
executeEnhancedTag httpFetch ctx config = do
  let EnhancedTag userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/enhanced_tag?start_date=#{startDate}&end_date=#{endDate}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeEnhancedTagInternal :: HttpFetch EnhancedTagData -> Text -> Text -> Task OuraHttpError (Array EnhancedTagData)
executeEnhancedTagInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- | Execute SleepTime
executeSleepTime ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch SleepTimeData ->
  ActionContext ->
  SleepTime command ->
  Task IntegrationError (Maybe CommandPayload)
executeSleepTime httpFetch ctx config = do
  let SleepTime userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/sleep_time?start_date=#{startDate}&end_date=#{endDate}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeSleepTimeInternal :: HttpFetch SleepTimeData -> Text -> Text -> Task OuraHttpError (Array SleepTimeData)
executeSleepTimeInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- | Execute RestModePeriod
executeRestModePeriod ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch RestModePeriodData ->
  ActionContext ->
  RestModePeriod command ->
  Task IntegrationError (Maybe CommandPayload)
executeRestModePeriod httpFetch ctx config = do
  let RestModePeriod userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/rest_mode_period?start_date=#{startDate}&end_date=#{endDate}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeRestModePeriodInternal :: HttpFetch RestModePeriodData -> Text -> Text -> Task OuraHttpError (Array RestModePeriodData)
executeRestModePeriodInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- | Execute RingConfiguration
executeRingConfiguration ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch RingConfigurationData ->
  ActionContext ->
  RingConfiguration command ->
  Task IntegrationError (Maybe CommandPayload)
executeRingConfiguration httpFetch ctx config = do
  let RingConfiguration userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/ring_configuration?start_date=#{startDate}&end_date=#{endDate}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeRingConfigurationInternal :: HttpFetch RingConfigurationData -> Text -> Text -> Task OuraHttpError (Array RingConfigurationData)
executeRingConfigurationInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- | Execute SleepPeriod
executeSleepPeriod ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  HttpFetch SleepPeriodData ->
  ActionContext ->
  SleepPeriod command ->
  Task IntegrationError (Maybe CommandPayload)
executeSleepPeriod httpFetch ctx config = do
  let SleepPeriod userId startDate endDate onSuccess onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let baseUrl = [fmt|https://api.ouraring.com/v2/usercollection/sleep?start_date=#{startDate}&end_date=#{endDate}|]
  fetchResult <- withValidToken secretStore "oura" userId
    (OAuth2.refreshTokenValidated validatedProvider clientId clientSecret)
    isUnauthorized
    (\accessToken -> fetchAllPages httpFetch accessToken baseUrl)
    |> Task.asResult
  case fetchResult of
    Ok result -> Integration.emitCommand (onSuccess result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)

executeSleepPeriodInternal :: HttpFetch SleepPeriodData -> Text -> Text -> Task OuraHttpError (Array SleepPeriodData)
executeSleepPeriodInternal httpFetch accessToken baseUrl =
  fetchAllPages httpFetch accessToken baseUrl

-- | URL-encode datetime for Oura API (handles + in timezone)
urlEncodeDatetime :: Text -> Text
urlEncodeDatetime dt = dt |> Text.replace "+" "%2B"

-- | URL-encode a query parameter value for safe URL interpolation
-- Handles common reserved characters that could corrupt requests:
-- + (often used in encoded spaces or timezone offsets)
-- & (query parameter separator)
-- = (key-value separator)
-- % (percent encoding itself)
-- # (fragment identifier)
-- ? (query string start)
-- / (path separator)
-- This is critical for pagination tokens which may contain any of these characters.
urlEncodeParam :: Text -> Text
urlEncodeParam param =
  param
    |> Text.replace "%" "%25"  -- MUST be first to avoid double-encoding
    |> Text.replace "+" "%2B"
    |> Text.replace "&" "%26"
    |> Text.replace "=" "%3D"
    |> Text.replace "#" "%23"
    |> Text.replace "?" "%3F"
    |> Text.replace "/" "%2F"
    |> Text.replace " " "%20"

-- | Get Oura provider from context or throw error
-- NOTE: ValidatedOAuth2ProviderConfig is from core/auth/Auth/OAuth2/Provider.hs:149-172
getOuraProvider :: ActionContext -> Task Integration.IntegrationError ValidatedOAuth2ProviderConfig
getOuraProvider ctx = do
  let ActionContext _ providerRegistry _ = ctx
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
  forall value.
  HttpFetch value ->
  Text ->  -- access token
  Text ->  -- base URL (without next_token)
  Task OuraHttpError (Array value)
fetchAllPages httpFetch accessToken baseUrl =
  fetchPagesLoop httpFetch accessToken baseUrl Nothing Array.empty

-- | Pagination loop - accumulates results until next_token is Nothing
-- Keeps track of baseUrl separately from pagination to avoid accumulating tokens
fetchPagesLoop ::
  forall value.
  HttpFetch value ->
  Text ->          -- access token
  Text ->          -- base URL (never changes)
  Maybe Text ->    -- current next_token (Nothing for first request)
  Array value ->       -- accumulated results
  Task OuraHttpError (Array value)
fetchPagesLoop httpFetch accessToken baseUrl maybeNextToken accumulated = do
  -- Build URL: baseUrl for first request, baseUrl&next_token=X for subsequent
  -- NOTE: Token must be URL-encoded to handle reserved characters like +, &, =
  let url = case maybeNextToken of
        Nothing -> baseUrl
        Just token -> do
          let encodedToken = urlEncodeParam token
          [fmt|#{baseUrl}&next_token=#{encodedToken}|]
  paginated <- httpFetch accessToken url  -- Returns PaginatedResponse a directly
  let PaginatedResponse responseData nextToken = paginated
  let newAccumulated = accumulated |> Array.append (Array.fromLinkedList responseData)
  case nextToken of
    Nothing -> Task.yield newAccumulated
    Just nextTok -> fetchPagesLoop httpFetch accessToken baseUrl (Just nextTok) newAccumulated
