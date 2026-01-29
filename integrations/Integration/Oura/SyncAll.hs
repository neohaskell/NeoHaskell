{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Integration.Oura.SyncAll (
  SyncAll (..),
  SyncResult (..),
) where

import AsyncTask qualified
import Auth.OAuth2.TokenRefresh (withValidToken)
import Auth.OAuth2.Client qualified as OAuth2
import Integration (ActionContext (..))
import Integration qualified
import Integration.Oura.Types (SleepData, ActivityData, ReadinessData, HeartRateData)
import Integration.Oura.Internal (HttpFetch, realHttpFetch, executeDailySleepInternal, executeDailyActivityInternal, executeDailyReadinessInternal, executeHeartRateInternal, mapTokenError, isUnauthorized, getOuraProvider)
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
  , startDate :: Text  -- YYYY-MM-DD (converted to datetime for heartrate)
  , endDate :: Text    -- YYYY-MM-DD (converted to datetime for heartrate)
  , onComplete :: SyncResult -> command
  , onError :: Maybe (Text -> command)
  }

data SyncResult = SyncResult
  { sleep :: Array SleepData
  , activity :: Array ActivityData
  , readiness :: Array ReadinessData
  , heartRate :: Array HeartRateData
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON SyncResult

-- ToAction runs all 4 in parallel via AsyncTask.runConcurrently
instance (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  Integration.ToAction (SyncAll command) where
  toAction config = Integration.actionWithContext \ctx ->
    executeSyncAll realHttpFetch ctx config

-- | Execute SyncAll with injectable fetch (for testing)
executeSyncAll ::
  forall command.
  (Json.ToJSON command, GhcTypeLits.KnownSymbol (NameOf command)) =>
  (forall a. Json.FromJSON a => HttpFetch a) ->  -- Polymorphic fetch
  ActionContext ->
  SyncAll command ->
  Task Integration.IntegrationError (Maybe Integration.CommandPayload)
executeSyncAll httpFetch ctx config = do
  let SyncAll userId startDate endDate onComplete onError = config
  let ActionContext secretStore _ = ctx
  providerConfig <- getOuraProvider ctx
  let ValidatedOAuth2ProviderConfig {validatedProvider, clientId, clientSecret} = providerConfig
  let refreshAction = OAuth2.refreshTokenValidated validatedProvider clientId clientSecret
  
  -- Build base URLs
  let sleepUrl = [fmt|https://api.ouraring.com/v2/usercollection/daily_sleep?start_date=#{startDate}&end_date=#{endDate}|]
  let activityUrl = [fmt|https://api.ouraring.com/v2/usercollection/daily_activity?start_date=#{startDate}&end_date=#{endDate}|]
  let readinessUrl = [fmt|https://api.ouraring.com/v2/usercollection/daily_readiness?start_date=#{startDate}&end_date=#{endDate}|]
  -- HeartRate uses datetime with negative timezone offset (avoids encoding issues)
  let startDatetime :: Text = [fmt|#{startDate}T00:00:00-00:00|]
  let endDatetime :: Text = [fmt|#{endDate}T23:59:59-00:00|]
  let heartRateUrl = [fmt|https://api.ouraring.com/v2/usercollection/heartrate?start_datetime=#{startDatetime}&end_datetime=#{endDatetime}|]
  
  -- Execute with token refresh - all 4 requests share the same token
  fetchResult <- withValidToken secretStore "oura" userId refreshAction isUnauthorized
    (\accessToken -> do
      -- Run all 4 in parallel pairs
      (sleepResult, activityResult) <- AsyncTask.runConcurrently
        ( executeDailySleepInternal httpFetch accessToken sleepUrl
        , executeDailyActivityInternal httpFetch accessToken activityUrl
        )
      (readinessResult, heartRateResult) <- AsyncTask.runConcurrently
        ( executeDailyReadinessInternal httpFetch accessToken readinessUrl
        , executeHeartRateInternal httpFetch accessToken heartRateUrl
        )
      Task.yield SyncResult
        { sleep = sleepResult
        , activity = activityResult
        , readiness = readinessResult
        , heartRate = heartRateResult
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
