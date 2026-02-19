{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Integration.Oura.SyncAll (
  SyncAll (..),
  SyncResult (..),
  -- | Internal: for testing only
  executeSyncAll,
) where

import AsyncTask qualified
import Auth.OAuth2.TokenRefresh (withValidToken)
import Auth.OAuth2.Client qualified as OAuth2
import Integration (ActionContext (..))
import Integration qualified
import Integration.Oura.Types (SleepData, ActivityData, ReadinessData, HeartRateData, WorkoutData, SessionData, PersonalInfoData, DailyStressData, DailySpO2Data, DailyResilienceData, DailyCardiovascularAgeData, VO2MaxData, EnhancedTagData, SleepTimeData, RestModePeriodData, RingConfigurationData, SleepPeriodData)
import Integration.Oura.Internal (HttpFetch, HttpFetchSingle, realHttpFetch, realHttpFetchSingle, executeDailySleepInternal, executeDailyActivityInternal, executeDailyReadinessInternal, executeHeartRateInternal, executeWorkoutInternal, executeSessionInternal, executePersonalInfoInternal, executeDailyStressInternal, executeDailySpO2Internal, executeDailyResilienceInternal, executeDailyCardiovascularAgeInternal, executeVO2MaxInternal, executeEnhancedTagInternal, executeSleepTimeInternal, executeRestModePeriodInternal, executeRingConfigurationInternal, executeSleepPeriodInternal, mapTokenError, isUnauthorized, getOuraProvider)
import Auth.OAuth2.Provider (ValidatedOAuth2ProviderConfig (..))
import Service.Command.Core (NameOf)
import Array (Array)
import Text (Text)
import Maybe (Maybe (..))
import Result (Result (..))
import Basics
import GHC.Generics ()
import Json qualified
import "base" GHC.TypeLits qualified as GhcTypeLits (KnownSymbol)
import Task (Task)
import Task qualified
import ToText (toText)

data SyncAll command = SyncAll
  { userId :: Text
  , startDate :: Text  -- YYYY-MM-DD (converted to datetime for heartrate/session)
  , endDate :: Text    -- YYYY-MM-DD (converted to datetime for heartrate/session)
  , onComplete :: SyncResult -> command
  , onError :: Maybe (Text -> command)
  }

data SyncResult = SyncResult
  { sleep :: Array SleepData
  , activity :: Array ActivityData
  , readiness :: Array ReadinessData
  , heartRate :: Array HeartRateData
  , workout :: Array WorkoutData
  , session :: Array SessionData
  , personalInfo :: PersonalInfoData
  , dailyStress :: Array DailyStressData
  , dailySpO2 :: Array DailySpO2Data
  , dailyResilience :: Array DailyResilienceData
  , dailyCardiovascularAge :: Array DailyCardiovascularAgeData
  , vo2Max :: Array VO2MaxData
  , enhancedTag :: Array EnhancedTagData
  , sleepTime :: Array SleepTimeData
  , restModePeriod :: Array RestModePeriodData
  , ringConfiguration :: Array RingConfigurationData
  , sleepPeriod :: Array SleepPeriodData
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON SyncResult

-- | Base URL for Oura Ring API v2 usercollection endpoints
ouraApiBase :: Text
ouraApiBase = "https://api.ouraring.com/v2/usercollection"


-- ToAction runs all 17 in parallel via AsyncTask.runConcurrently pairs
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (SyncAll command) where
  toAction config = Integration.action \ctx ->
    executeSyncAll realHttpFetch realHttpFetchSingle ctx config

-- | Execute SyncAll with injectable fetch (for testing)
executeSyncAll ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  (forall value. Json.FromJSON value => HttpFetch value) ->  -- Polymorphic fetch (paginated)
  HttpFetchSingle PersonalInfoData ->             -- Single fetch (non-paginated)
  ActionContext ->
  SyncAll command ->
  Task Integration.IntegrationError (Maybe Integration.CommandPayload)
executeSyncAll httpFetch httpFetchSingle ctx config = do
  let SyncAll userId startDate endDate onComplete onError = config
  let ActionContext secretStore _ _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let refreshAction = OAuth2.refreshTokenValidated validatedProvider clientId clientSecret
  
  -- Build date range URLs
  let sleepUrl = [fmt|#{ouraApiBase}/daily_sleep?start_date=#{startDate}&end_date=#{endDate}|]
  let activityUrl = [fmt|#{ouraApiBase}/daily_activity?start_date=#{startDate}&end_date=#{endDate}|]
  let readinessUrl = [fmt|#{ouraApiBase}/daily_readiness?start_date=#{startDate}&end_date=#{endDate}|]
  let workoutUrl = [fmt|#{ouraApiBase}/workout?start_date=#{startDate}&end_date=#{endDate}|]
  let dailyStressUrl = [fmt|#{ouraApiBase}/daily_stress?start_date=#{startDate}&end_date=#{endDate}|]
  let dailySpO2Url = [fmt|#{ouraApiBase}/daily_spo2?start_date=#{startDate}&end_date=#{endDate}|]
  let dailyResilienceUrl = [fmt|#{ouraApiBase}/daily_resilience?start_date=#{startDate}&end_date=#{endDate}|]
  let dailyCardiovascularAgeUrl = [fmt|#{ouraApiBase}/daily_cardiovascular_age?start_date=#{startDate}&end_date=#{endDate}|]
  let vo2MaxUrl = [fmt|#{ouraApiBase}/vo2_max?start_date=#{startDate}&end_date=#{endDate}|]
  let enhancedTagUrl = [fmt|#{ouraApiBase}/enhanced_tag?start_date=#{startDate}&end_date=#{endDate}|]
  let sleepTimeUrl = [fmt|#{ouraApiBase}/sleep_time?start_date=#{startDate}&end_date=#{endDate}|]
  let restModePeriodUrl = [fmt|#{ouraApiBase}/rest_mode_period?start_date=#{startDate}&end_date=#{endDate}|]
  let ringConfigurationUrl = [fmt|#{ouraApiBase}/ring_configuration?start_date=#{startDate}&end_date=#{endDate}|]
  let sleepPeriodUrl = [fmt|#{ouraApiBase}/sleep?start_date=#{startDate}&end_date=#{endDate}|]
  -- Datetime endpoints (HeartRate, Session use UTC datetime with Z suffix)
  let startDatetime :: Text = [fmt|#{startDate}T00:00:00Z|]
  let endDatetime :: Text = [fmt|#{endDate}T23:59:59Z|]
  let heartRateUrl = [fmt|#{ouraApiBase}/heartrate?start_datetime=#{startDatetime}&end_datetime=#{endDatetime}|]
  let sessionUrl = [fmt|#{ouraApiBase}/session?start_datetime=#{startDatetime}&end_datetime=#{endDatetime}|]
  -- PersonalInfo: no date params
  let personalInfoUrl = [fmt|#{ouraApiBase}/personal_info|]
  
  -- Execute with token refresh - all 17 requests share the same token
  fetchResult <- withValidToken secretStore "oura" userId refreshAction isUnauthorized
    (\accessToken -> do
      -- Run all 17 in parallel pairs (8 pairs + 1 standalone)
      (sleepResult, activityResult) <- AsyncTask.runConcurrently
        ( executeDailySleepInternal httpFetch accessToken sleepUrl
        , executeDailyActivityInternal httpFetch accessToken activityUrl
        )
      (readinessResult, heartRateResult) <- AsyncTask.runConcurrently
        ( executeDailyReadinessInternal httpFetch accessToken readinessUrl
        , executeHeartRateInternal httpFetch accessToken heartRateUrl
        )
      (workoutResult, sessionResult) <- AsyncTask.runConcurrently
        ( executeWorkoutInternal httpFetch accessToken workoutUrl
        , executeSessionInternal httpFetch accessToken sessionUrl
        )
      (personalInfoResult, dailyStressResult) <- AsyncTask.runConcurrently
        ( executePersonalInfoInternal httpFetchSingle accessToken personalInfoUrl
        , executeDailyStressInternal httpFetch accessToken dailyStressUrl
        )
      (dailySpO2Result, dailyResilienceResult) <- AsyncTask.runConcurrently
        ( executeDailySpO2Internal httpFetch accessToken dailySpO2Url
        , executeDailyResilienceInternal httpFetch accessToken dailyResilienceUrl
        )
      (dailyCardiovascularAgeResult, vo2MaxResult) <- AsyncTask.runConcurrently
        ( executeDailyCardiovascularAgeInternal httpFetch accessToken dailyCardiovascularAgeUrl
        , executeVO2MaxInternal httpFetch accessToken vo2MaxUrl
        )
      (enhancedTagResult, sleepTimeResult) <- AsyncTask.runConcurrently
        ( executeEnhancedTagInternal httpFetch accessToken enhancedTagUrl
        , executeSleepTimeInternal httpFetch accessToken sleepTimeUrl
        )
      (restModePeriodResult, ringConfigurationResult) <- AsyncTask.runConcurrently
        ( executeRestModePeriodInternal httpFetch accessToken restModePeriodUrl
        , executeRingConfigurationInternal httpFetch accessToken ringConfigurationUrl
        )
      sleepPeriodResult <- executeSleepPeriodInternal httpFetch accessToken sleepPeriodUrl
      Task.yield SyncResult
        { sleep = sleepResult
        , activity = activityResult
        , readiness = readinessResult
        , heartRate = heartRateResult
        , workout = workoutResult
        , session = sessionResult
        , personalInfo = personalInfoResult
        , dailyStress = dailyStressResult
        , dailySpO2 = dailySpO2Result
        , dailyResilience = dailyResilienceResult
        , dailyCardiovascularAge = dailyCardiovascularAgeResult
        , vo2Max = vo2MaxResult
        , enhancedTag = enhancedTagResult
        , sleepTime = sleepTimeResult
        , restModePeriod = restModePeriodResult
        , ringConfiguration = ringConfigurationResult
        , sleepPeriod = sleepPeriodResult
        }
    )
    |> Task.asResult
  -- Handle result with onError callback (same pattern as individual endpoints)
  case fetchResult of
    Ok result -> Integration.emitCommand (onComplete result)
    Err tokenErr -> do
      let errorMsg = toText (mapTokenError tokenErr)
      case onError of
        Just handler -> Integration.emitCommand (handler errorMsg)
        Nothing -> Task.throw (mapTokenError tokenErr)
