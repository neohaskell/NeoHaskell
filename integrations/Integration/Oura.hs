-- | # Integration.Oura
--
-- Oura Ring API integration for health and sleep data.
--
-- == Quick Start
--
-- @
-- import Integration.Oura qualified as Oura
--
-- ouraIntegration entity event = case event of
--   UserRequestedSync userId -> Integration.batch
--     [ Integration.outbound Oura.DailySleep
--         { userId = userId
--         , startDate = "2024-01-01"
--         , endDate = "2024-01-07"
--         , onSuccess = OuraSleepReceived
--         , onError = Just OuraError
--         }
--     ]
--   _ -> Integration.none
-- @
--
-- == OAuth2 Setup
--
-- @
-- import Integration.Oura qualified as Oura
-- import Auth.OAuth2.Types (mkClientSecret, mkRedirectUri, ClientId (..))
-- import Auth.SecretStore (TokenKey (..))
-- 
-- -- NOTE: OAuth2 callback URL is /callback/{provider}
-- app = do
--   redirectUri <- case mkRedirectUri "http://localhost:3000/callback/oura" of
--     Ok uri -> Task.yield uri
--     Err err -> Task.throw [fmt|Invalid redirect URI: #{toText err}|]
--   Application.new
--     |> Application.withAuth authServerUrl []
--     |> Application.withOAuth2 "OAUTH2_HMAC_KEY"
--          [ Oura.makeOuraConfig
--              (ClientId "your-client-id")
--              (mkClientSecret "your-client-secret")
--              redirectUri
--              -- TokenKey has no ToJSON - unwrap to Text
--              (\\userId tokenKey -> do
--                let (TokenKey keyText) = tokenKey
--                Integration.encodeCommand (OuraConnected userId keyText))
--              (\\userId err -> Integration.encodeCommand (OuraConnectionFailed userId (toText err)))
--              (\\userId -> Integration.encodeCommand (OuraDisconnected userId))
--              "/settings"
--              "/settings?error=oura"
--          ]
-- @
--
-- == Periodic Sync Example
--
-- Timer.every is an INBOUND integration - it emits commands on a schedule.
-- The command handler then triggers the outbound Oura fetch.
--
-- @
-- import Integration.Timer qualified as Timer
-- 
-- ouraSyncTimer :: Integration.Inbound
-- ouraSyncTimer = Timer.every Timer.Every
--   { interval = Timer.hours 1
--   , toCommand = \\tickCount -> SyncOuraData
--       { userId = "user-123"
--       , requestedAt = tickCount
--       }
--   }
-- 
-- -- In command handler:
-- handleCommand (SyncOuraData {userId}) = Integration.batch
--   [ Integration.outbound Oura.DailySleep
--       { userId = userId, startDate = "2024-01-01", endDate = "2024-01-07"
--       , onSuccess = OuraSleepReceived, onError = Just OuraError
--       }
--   ]
-- @
module Integration.Oura (
  -- * Provider Config
  makeOuraConfig,
  ouraProvider,
  
  -- * Request Types (Original 4)
  DailySleep (..),
  DailyActivity (..),
  DailyReadiness (..),
  HeartRate (..),
  SyncAll (..),
  
  -- * Request Types (New 13)
  Workout (..),
  Session (..),
  PersonalInfo (..),
  DailyStress (..),
  DailySpO2 (..),
  DailyResilience (..),
  DailyCardiovascularAge (..),
  VO2Max (..),
  EnhancedTag (..),
  SleepTime (..),
  RestModePeriod (..),
  RingConfiguration (..),
  SleepPeriod (..),
  
  -- * Response Types (Original 4)
  SleepData (..),
  ActivityData (..),
  ReadinessData (..),
  HeartRateData (..),
  SyncResult (..),
  
  -- * Response Types (New 13)
  WorkoutData (..),
  SessionData (..),
  PersonalInfoData (..),
  DailyStressData (..),
  DailySpO2Data (..),
  DailyResilienceData (..),
  DailyCardiovascularAgeData (..),
  VO2MaxData (..),
  EnhancedTagData (..),
  SleepTimeData (..),
  RestModePeriodData (..),
  RingConfigurationData (..),
  SleepPeriodData (..),
  
  -- * Shared Types
  SleepContributors (..),
  SampleModel (..),
  ActivityContributors (..),
  ReadinessContributors (..),
   ResilienceContributors (..),
   SpO2AggregatedValues (..),
   RestModeEpisode (..),
  SleepPeriodReadiness (..),
  PaginatedResponse (..),
) where

import Integration.Oura.Provider (makeOuraConfig, ouraProvider)
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
import Integration.Oura.SyncAll (SyncAll (..), SyncResult (..))
import Integration.Oura.Types (SleepData (..), ActivityData (..), ReadinessData (..), HeartRateData (..), WorkoutData (..), SessionData (..), PersonalInfoData (..), DailyStressData (..), DailySpO2Data (..), DailyResilienceData (..), DailyCardiovascularAgeData (..), VO2MaxData (..), EnhancedTagData (..), SleepTimeData (..), RestModePeriodData (..), RingConfigurationData (..), SleepPeriodData (..), SleepContributors (..), SampleModel (..), ActivityContributors (..), ReadinessContributors (..), ResilienceContributors (..), SpO2AggregatedValues (..), RestModeEpisode (..), SleepPeriodReadiness (..), PaginatedResponse (..))
-- Note: ToAction instances are exported from Internal.hs via orphan instances
import Integration.Oura.Internal ()  -- Import for instances
