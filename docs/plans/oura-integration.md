# Oura Integration Implementation Plan

**Status**: Proposed
**Author**: DevEx Lead
**Date**: 2026-01-28
**Related**: ADR-0010 (OAuth2 Provider), ADR-0015 (HTTP Outbound)

---

## Overview

This plan describes the implementation of Oura Ring API integration for NeoHaskell. The integration enables applications to:

1. Connect user accounts via OAuth2
2. Fetch health data (sleep, activity, readiness, heart rate)
3. Store data as domain events via the command pipeline

The implementation builds on two foundations:
- **ADR-0010**: OAuth2 Provider architecture (token management)
- **ADR-0015**: HTTP Outbound integration (API calls)

---

## Design Decisions

| Concern | Decision | Rationale |
|---------|----------|-----------|
| Token refresh | Reactive (on 401) | Simpler implementation, avoids premature refreshes |
| Rate limiting | Queue + exponential backoff | Respects Oura's 5000/5min limit gracefully |
| Data storage | Command → Event (event sourced) | Consistent with NeoHaskell architecture |
| Webhook vs polling | Polling (for now) | Simpler initial implementation |

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           USER APPLICATION                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│   ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐    │
│   │  User Domain    │    │  Health Domain  │    │  Sync Domain    │    │
│   │                 │    │                 │    │                 │    │
│   │ OuraConnected   │───▶│ ImportSleepData │───▶│ ScheduleSync    │    │
│   │ OuraDisconnected│    │ ImportActivity  │    │ SyncCompleted   │    │
│   └─────────────────┘    └─────────────────┘    └─────────────────┘    │
│            │                      ▲                      │              │
│            │                      │                      │              │
│            ▼                      │                      ▼              │
│   ┌─────────────────────────────────────────────────────────────────┐  │
│   │                     INTEGRATION LAYER                            │  │
│   │  ┌─────────────┐    ┌─────────────┐    ┌─────────────────────┐  │  │
│   │  │ OAuth2      │    │ Oura        │    │ Timer               │  │  │
│   │  │ Provider    │    │ Integration │    │ Integration         │  │  │
│   │  │             │    │             │    │                     │  │  │
│   │  │ /connect    │    │ DailySleep  │    │ Every 6 hours       │  │  │
│   │  │ /callback   │    │ DailyActivity│   │ trigger sync        │  │  │
│   │  │ /disconnect │    │ Readiness   │    │                     │  │  │
│   │  └──────┬──────┘    └──────┬──────┘    └─────────────────────┘  │  │
│   │         │                  │                                     │  │
│   │         ▼                  ▼                                     │  │
│   │  ┌─────────────────────────────────────────────────────────────┐│  │
│   │  │              HTTP OUTBOUND (ADR-0015)                       ││  │
│   │  │  • Request queue with rate limiting                         ││  │
│   │  │  • Exponential backoff on 429                               ││  │
│   │  │  • Reactive token refresh on 401                            ││  │
│   │  └─────────────────────────────────────────────────────────────┘│  │
│   └─────────────────────────────────────────────────────────────────┘  │
│                                                                          │
│   ┌─────────────────────────────────────────────────────────────────┐  │
│   │                      STORAGE LAYER                               │  │
│   │  ┌─────────────┐    ┌─────────────┐    ┌─────────────────────┐  │  │
│   │  │ SecretStore │    │ EventStore  │    │ TransactionStore    │  │  │
│   │  │             │    │             │    │                     │  │  │
│   │  │ OAuth tokens│    │ Domain      │    │ OAuth2 state        │  │  │
│   │  │ (encrypted) │    │ events      │    │ (short-lived)       │  │  │
│   │  └─────────────┘    └─────────────┘    └─────────────────────┘  │  │
│   └─────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Oura API Reference

| Endpoint | URL | Scopes |
|----------|-----|--------|
| Authorize | `https://cloud.ouraring.com/oauth/authorize` | - |
| Token | `https://api.ouraring.com/oauth/token` | - |
| Revoke | `https://api.ouraring.com/oauth/revoke` | - |
| Sleep | `GET /v2/usercollection/sleep` | `daily` |
| Daily Activity | `GET /v2/usercollection/daily_activity` | `daily` |
| Daily Readiness | `GET /v2/usercollection/daily_readiness` | `daily` |
| Heart Rate | `GET /v2/usercollection/heartrate` | `heartrate` |
| Personal Info | `GET /v2/usercollection/personal_info` | `personal` |

**Rate Limit**: 5000 requests / 5 minutes (429 on exceed)

---

## Implementation Phases

### Phase 1: HTTP Integration Foundation

**Goal**: Implement ADR-0015 HTTP outbound integration with rate limiting.

**Deliverables**:

1. **Package structure**:
   ```
   integrations/
     http/
       nhintegration-http.cabal
       Integration/
         Http.hs                 -- Re-exports (Jess's API)
         Http/
           Request.hs            -- Request, Method, Body types
           Response.hs           -- Response type
           Auth.hs               -- Auth patterns
           Retry.hs              -- Retry configuration
           RateLimiter.hs        -- Request queue + backoff
           Internal.hs           -- ToAction implementation
   ```

2. **Rate limiter implementation**:
   ```haskell
   -- Integration/Http/RateLimiter.hs
   data RateLimiter = RateLimiter
     { requestsPerWindow :: Int      -- e.g., 5000
     , windowMs :: Int               -- e.g., 300000 (5 min)
     , currentCount :: ConcurrentVar Int
     , windowStart :: ConcurrentVar Int
     , queue :: Channel PendingRequest
     }

   -- Enqueue request, wait if rate limited
   enqueue :: RateLimiter -> Task err Response -> Task err Response

   -- Exponential backoff on 429
   handleRateLimit :: Response -> Task err Response
   ```

3. **Reactive token refresh**:
   ```haskell
   -- In Internal.hs
   executeWithAuth :: Auth -> Task IntegrationError Response -> Task IntegrationError Response
   executeWithAuth auth action = do
     response <- action
     case response.statusCode of
       401 -> do
         -- Refresh token and retry once
         newToken <- refreshOAuth2Token auth
         retryWithToken newToken action
       _ -> Task.yield response
   ```

**Tasks**:
- [ ] Create `integrations/http/` package structure
- [ ] Implement `Integration.Http` module with types
- [ ] Implement `Integration.Http.RateLimiter`
- [ ] Implement `Integration.Http.Internal` (ToAction)
- [ ] Add reactive 401 handling for OAuth2 tokens
- [ ] Unit tests for rate limiter
- [ ] Integration tests with mock HTTP server

**Estimated effort**: Medium (3-5 days)

---

### Phase 2: OAuth2 Token Refresh Support

**Goal**: Extend ADR-0010's SecretStore with refresh token flow.

**Deliverables**:

1. **Token refresh helper**:
   ```haskell
   -- Auth/OAuth2/TokenRefresh.hs

   -- Attempt to use access token, refresh if 401
   withValidToken ::
     Text ->              -- Provider name
     Text ->              -- User ID
     (Text -> Task err a) ->  -- Action using token
     Task err a
   withValidToken provider userId action = do
     tokenSet <- SecretStore.get [fmt|oauth2:tokens:{provider}:{userId}|]
     result <- action tokenSet.accessToken
       |> Task.attempt
     case result of
       Ok value -> Task.yield value
       Err err | isUnauthorized err -> do
         newTokenSet <- refreshToken provider tokenSet
         SecretStore.set [fmt|oauth2:tokens:{provider}:{userId}|] newTokenSet
         action newTokenSet.accessToken
       Err err -> Task.throw err
   ```

2. **Refresh token endpoint call**:
   ```haskell
   refreshToken :: Text -> TokenSet -> Task AuthError TokenSet
   refreshToken provider tokenSet = do
     providerConfig <- getProviderConfig provider
     response <- Http.post providerConfig.tokenUrl
       |> Http.form
           [ ("grant_type", "refresh_token")
           , ("refresh_token", tokenSet.refreshToken)
           , ("client_id", providerConfig.clientId)
           , ("client_secret", providerConfig.clientSecret)
           ]
       |> Http.send
     -- Parse and return new TokenSet
   ```

**Tasks**:
- [ ] Implement `Auth.OAuth2.TokenRefresh` module
- [ ] Add `withValidToken` helper
- [ ] Update `SecretStore` interface if needed
- [ ] Unit tests for refresh flow
- [ ] Integration test with Oura sandbox (if available)

**Estimated effort**: Small (1-2 days)

---

### Phase 3: Oura Integration Package

**Goal**: Implement Oura-specific integration using HTTP foundation.

**Deliverables**:

1. **Package structure**:
   ```
   integrations/
     oura/
       nhintegration-oura.cabal
       Integration/
         Oura.hs                 -- Re-exports (Jess's API)
         Oura/
           Sleep.hs              -- DailySleep config
           Activity.hs           -- DailyActivity config
           Readiness.hs          -- DailyReadiness config
           HeartRate.hs          -- HeartRate config
           Personal.hs           -- PersonalInfo config
           Types.hs              -- Response types
           Internal.hs           -- ToAction implementations
   ```

2. **Jess's API** (config records):
   ```haskell
   -- Integration/Oura/Sleep.hs

   -- | Fetch daily sleep data from Oura.
   -- Token is automatically retrieved from SecretStore.
   data DailySleep command = DailySleep
     { userId :: Text
     , startDate :: Date
     , endDate :: Date
     , onSuccess :: Array SleepData -> command
     , onError :: Maybe (Text -> command)
     }
   ```

3. **Nick's implementation** (ToAction):
   ```haskell
   -- Integration/Oura/Internal.hs

   instance (Json.ToJSON command, ...) => Integration.ToAction (DailySleep command) where
     toAction config = Integration.action do
       withValidToken "oura" config.userId \token -> do
         let url = [fmt|https://api.ouraring.com/v2/usercollection/sleep?start_date={config.startDate}&end_date={config.endDate}|]
         response <- Http.Request
           { method = Http.GET
           , url = url
           , headers = []
           , body = Http.noBody
           , auth = Http.Bearer token
           , retry = Http.defaultRetry
           , timeoutSeconds = 30
           }
           |> executeHttpRequest

         case Json.decode @OuraSleepResponse response.body of
           Err err -> handleError config.onError [fmt|Failed to parse Oura response: {err}|]
           Ok parsed -> Integration.emitCommand (config.onSuccess parsed.data)
   ```

4. **Response types**:
   ```haskell
   -- Integration/Oura/Types.hs

   data SleepData = SleepData
     { id :: Text
     , day :: Date
     , score :: Maybe Int
     , contributors :: SleepContributors
     , bedtimeStart :: Maybe DateTime
     , bedtimeEnd :: Maybe DateTime
     , duration :: Int              -- seconds
     , totalSleep :: Int            -- seconds
     , deepSleep :: Int             -- seconds
     , remSleep :: Int              -- seconds
     , lightSleep :: Int            -- seconds
     , awake :: Int                 -- seconds
     , efficiency :: Int            -- percentage
     }
     deriving (Generic, Eq, Show)

   instance Json.FromJSON SleepData where
     parseJSON = Json.genericParseJSON ouraJsonOptions

   -- Similar for ActivityData, ReadinessData, HeartRateData
   ```

**Tasks**:
- [ ] Create `integrations/oura/` package structure
- [ ] Define all response types with JSON parsing
- [ ] Implement `DailySleep` config and ToAction
- [ ] Implement `DailyActivity` config and ToAction
- [ ] Implement `DailyReadiness` config and ToAction
- [ ] Implement `HeartRate` config and ToAction
- [ ] Implement `PersonalInfo` config and ToAction
- [ ] Add `SyncAll` composite action
- [ ] Unit tests with mock responses
- [ ] Integration tests with Oura API (sandbox or real)

**Estimated effort**: Medium (3-5 days)

---

### Phase 4: Domain Integration

**Goal**: Wire Oura integration into application domain.

**Deliverables**:

1. **OAuth2 provider config**:
   ```haskell
   -- Config/OAuth2.hs

   ouraProvider :: OAuth2.Config
   ouraProvider = OAuth2.Config
     { name = "oura"
     , clientId = "${OURA_CLIENT_ID}"
     , clientSecret = "${OURA_CLIENT_SECRET}"
     , authorizeUrl = "https://cloud.ouraring.com/oauth/authorize"
     , tokenUrl = "https://api.ouraring.com/oauth/token"
     , scopes = ["daily", "heartrate", "personal"]
     , redirectUri = "${APP_BASE_URL}/callback/oura"
     , onSuccess = \userId info -> ConnectOuraAccount
         { userId = userId
         , ouraUserId = info.providerUserId
         , scopes = info.scopes
         , connectedAt = info.timestamp
         }
     , onFailure = \userId error -> LogOuraConnectionFailed
         { userId = userId
         , error = error
         }
     }
   ```

2. **Domain commands**:
   ```haskell
   -- Health/Commands.hs

   data ImportSleepData = ImportSleepData
     { userId :: Text
     , data :: Array SleepData
     }
     deriving (Generic)

   instance Command ImportSleepData where
     type NameOf ImportSleepData = "ImportSleepData"

   -- Similar for ImportActivityData, ImportReadinessData, etc.
   ```

3. **Domain events**:
   ```haskell
   -- Health/Events.hs

   data HealthEvent
     = SleepDataImported { records :: Array SleepRecord }
     | ActivityDataImported { records :: Array ActivityRecord }
     | ReadinessDataImported { records :: Array ReadinessRecord }
     | SyncCompleted { syncedAt :: DateTime, recordCount :: Int }
     deriving (Generic)
   ```

4. **Integration function**:
   ```haskell
   -- Health/Integrations.hs

   healthIntegrations :: HealthEntity -> HealthEvent -> Integration.Outbound
   healthIntegrations entity event = case event of
     -- After successful connection, do initial sync
     OuraAccountConnected info -> Integration.batch
       [ Integration.outbound Oura.SyncAll
           { userId = entity.userId
           , daysBack = 30  -- Initial 30-day backfill
           , onSuccess = \summary -> RecordInitialSync
               { userId = entity.userId
               , recordCount = summary.totalRecords
               }
           , onError = Just (\err -> LogSyncError { error = err })
           }
       ]

     -- Scheduled periodic sync
     SyncTriggered -> Integration.batch
       [ Integration.outbound Oura.DailySleep
           { userId = entity.userId
           , startDate = Date.addDays (-1) today
           , endDate = today
           , onSuccess = \data -> ImportSleepData { userId = entity.userId, data = data }
           , onError = Nothing
           }
       -- Similar for activity, readiness
       ]

     _ -> Integration.none
   ```

5. **Scheduled sync**:
   ```haskell
   -- App.hs

   app :: Application
   app =
     Application.new
       |> Application.withService healthService
       |> Application.withOAuth2Provider ouraProvider
       |> Application.withOutbound @HealthEntity healthIntegrations
       |> Application.withInbound (Integration.Timer.every (Hours 6) TriggerOuraSync)
   ```

**Tasks**:
- [ ] Define domain commands for health data import
- [ ] Define domain events for health data
- [ ] Implement HealthEntity and decider
- [ ] Wire OAuth2 provider in Application
- [ ] Implement healthIntegrations function
- [ ] Add scheduled sync timer
- [ ] End-to-end integration test

**Estimated effort**: Medium (3-5 days)

---

## Testing Strategy

### Unit Tests

| Component | Test Focus |
|-----------|------------|
| `Http.RateLimiter` | Queue behavior, backoff calculation, window reset |
| `OAuth2.TokenRefresh` | Refresh flow, error handling, SecretStore interaction |
| `Oura.*` ToAction | Request construction, response parsing |
| `Oura.Types` | JSON parsing for all response types |

### Integration Tests

| Test | Description |
|------|-------------|
| OAuth2 flow | Full connect/callback/disconnect with mock provider |
| HTTP rate limiting | Verify 429 handling with mock server |
| Token refresh | Verify 401 triggers refresh and retry |
| Oura API | Real API calls (with test account) |

### End-to-End Tests

| Test | Description |
|------|-------------|
| User connects Oura | OAuth2 flow → token stored → initial sync |
| Scheduled sync | Timer triggers → data fetched → events stored |
| Token expiry | Expired token → refresh → successful sync |
| Rate limit hit | Many requests → 429 → backoff → success |

---

## Milestones

| Milestone | Deliverable | Target |
|-----------|-------------|--------|
| M1 | HTTP Integration package (ADR-0015) | Week 1 |
| M2 | Token refresh support | Week 1 |
| M3 | Oura Integration package | Week 2 |
| M4 | Domain wiring + E2E tests | Week 2 |
| M5 | Documentation + polish | Week 3 |

---

## Open Questions (Resolved)

| Question | Resolution |
|----------|------------|
| Token refresh strategy | Reactive (on 401) |
| Rate limit handling | Queue + exponential backoff |
| Data storage approach | Command → Event (event sourced) |
| Webhook vs polling | Polling for now |

---

## Future Enhancements

1. **Webhook support**: Switch from polling to Oura webhooks for real-time updates
2. **Batch sync**: Optimize multiple-day fetches into fewer API calls
3. **Offline queue**: Queue commands when Oura API is unreachable
4. **Metrics**: Track sync latency, error rates, data freshness
5. **Multi-provider**: Abstract patterns for Fitbit, Garmin, Apple Health, etc.

---

## References

- [Oura API Authentication](https://cloud.ouraring.com/docs/authentication)
- [Oura API v2 Documentation](https://cloud.ouraring.com/v2/docs)
- [ADR-0010: OAuth2 Provider Architecture](../decisions/0010-oauth2-provider-architecture.md)
- [ADR-0015: HTTP Outbound Integration](../decisions/0015-http-outbound-integration.md)
- [ADR-0008: Integration Pattern](../decisions/0008-integration-pattern.md)
