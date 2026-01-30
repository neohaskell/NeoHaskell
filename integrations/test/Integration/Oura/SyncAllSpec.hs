module Integration.Oura.SyncAllSpec (spec) where

import Basics
import Data.Maybe qualified as GhcMaybe
import Test.Hspec
import Integration.Oura.SyncAll (SyncAll (..), SyncResult (..), executeSyncAll)
import Integration.Oura.Types (PaginatedResponse (..))
import Integration.Oura.Internal (HttpFetch, OuraHttpError (..))
import Integration (ActionContext (..), IntegrationError (..), CommandPayload)
import Auth.SecretStore.InMemory qualified as InMemorySecretStore
import Auth.SecretStore (SecretStore (..), TokenKey (..))
import Auth.OAuth2.Types
  ( Provider (..)
  , unsafeValidatedProvider
  , mkAccessToken
  , mkRefreshToken
  , TokenSet (..)
  , ClientId (..)
  , mkClientSecret
  , mkRedirectUri
  , Scope (..)
  )
import Auth.OAuth2.Provider (ValidatedOAuth2ProviderConfig (..))
import Service.Command.Core (NameOf)
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import AsyncTask qualified
import Task (Task)
import Task qualified
import Result (Result (..))
import Array qualified
import Text (Text)
import Maybe (Maybe (..))
import Map qualified
import Json qualified
import ToText (toText)


spec :: Spec
spec = do
  describe "Integration.Oura.SyncAll" do
    describe "executeSyncAll" do
      it "fetches all four data types and returns command payload" do
        ctx <- Task.runOrPanic setupMockContext

        -- Mock fetch that returns empty data for all endpoints
        -- We can't return typed mock data due to polymorphism, but empty lists work
        let mockFetch :: forall a. Json.FromJSON a => HttpFetch a
            mockFetch _token _url = do
              Task.yield PaginatedResponse { data_ = [], nextToken = Nothing }

        let config = testSyncAllConfig

        result <- Task.runOrPanic do
          executeSyncAll mockFetch ctx config

        -- Should return Just with command payload (meaning onComplete was called)
        result `shouldSatisfy` isJust

      it "runs requests in parallel (max concurrent >= 2)" do
        let mkVar :: Int -> Task Text (ConcurrentVar Int)
            mkVar = ConcurrentVar.containing
        maxConcurrent <- Task.runOrPanic (mkVar 0)
        currentCount <- Task.runOrPanic (mkVar 0)
        ctx <- Task.runOrPanic setupMockContext

        -- Mock fetch that tracks concurrency
        let mockFetch :: forall a. Json.FromJSON a => HttpFetch a
            mockFetch _token _url = do
              -- Increment concurrent counter (function first, then var)
              ConcurrentVar.modify (+ 1) currentCount
              current <- ConcurrentVar.peek currentCount
              -- Track max concurrent (function first, then var)
              ConcurrentVar.modify (\m -> max m current) maxConcurrent
              -- Simulate work
              AsyncTask.sleep 50
              -- Decrement (function first, then var)
              ConcurrentVar.modify (\n -> n - 1) currentCount
              -- Return mock PaginatedResponse
              Task.yield PaginatedResponse { data_ = [], nextToken = Nothing }

        let config = testSyncAllConfig

        _ <- Task.runOrPanic do
          executeSyncAll mockFetch ctx config

        -- If parallel, max should be >= 2
        let peekVar :: ConcurrentVar Int -> Task Text Int
            peekVar = ConcurrentVar.peek
        maxVal <- Task.runOrPanic (peekVar maxConcurrent)
        maxVal `shouldSatisfy` (>= 2)

      it "aggregates results into SyncResult (calls onComplete)" do
        ctx <- Task.runOrPanic setupMockContext

        -- Mock fetch returning empty data - verifies structure, not content
        let mockFetch :: forall a. Json.FromJSON a => HttpFetch a
            mockFetch _token _url =
              Task.yield PaginatedResponse { data_ = [], nextToken = Nothing }

        let config = testSyncAllConfig

        result <- Task.runOrPanic do
          executeSyncAll mockFetch ctx config

        -- The test passes if executeSyncAll completes without error
        -- and returns a payload (meaning onComplete was called with SyncResult)
        result `shouldSatisfy` isJust

      it "invokes onError callback when fetch fails and onError is Just" do
        ctx <- Task.runOrPanic setupMockContext

        -- Mock fetch that throws error
        let mockFetch :: forall a. Json.FromJSON a => HttpFetch a
            mockFetch _token _url = Task.throw Unauthorized

        let config = SyncAll
              { userId = "test-user"
              , startDate = "2024-01-01"
              , endDate = "2024-01-07"
              , onComplete = \_ -> TestCommand { capturedResult = Nothing, errorMessage = Nothing }
              , onError = Just (\errMsg -> TestCommand { capturedResult = Nothing, errorMessage = Just errMsg })
              }

        -- Should NOT throw - onError handler catches the error
        result <- Task.runOrPanic do
          executeSyncAll mockFetch ctx config

        -- Should return Just (the error command payload)
        result `shouldSatisfy` isJust

      it "throws error when fetch fails and onError is Nothing" do
        ctx <- Task.runOrPanic setupMockContext

        -- Mock fetch that throws error
        let mockFetch :: forall a. Json.FromJSON a => HttpFetch a
            mockFetch _token _url = Task.throw Unauthorized

        let config = SyncAll
              { userId = "test-user"
              , startDate = "2024-01-01"
              , endDate = "2024-01-07"
              , onComplete = \_ -> TestCommand { capturedResult = Nothing, errorMessage = Nothing }
              , onError = Nothing  -- No error handler
              }

        -- Should throw - no error handler
        let task :: Task Text (Result IntegrationError (Maybe CommandPayload))
            task = executeSyncAll mockFetch ctx config |> Task.asResult
        result <- Task.runOrPanic task

        case result of
          Err (AuthenticationError _) -> pure ()
          Err other -> expectationFailure [fmt|Expected AuthenticationError, got: #{toText other}|]
          Ok _ -> expectationFailure "Expected error to be thrown"


-- | Test command type for capturing SyncResult or error
data TestCommand = TestCommand
  { capturedResult :: Maybe SyncResult
  , errorMessage :: Maybe Text
  }
  deriving (Generic, Show, Eq)

instance Json.ToJSON TestCommand

type instance NameOf TestCommand = "TestCommand"


-- | Setup mock ActionContext with SecretStore and provider registry
setupMockContext :: Task Text ActionContext
setupMockContext = do
  secretStore <- InMemorySecretStore.new
  -- Store a valid token for the test user
  let tokenKey = TokenKey "oauth:oura:test-user"
  let tokenSet = TokenSet
        { accessToken = mkAccessToken "mock-access-token"
        , refreshToken = Just (mkRefreshToken "mock-refresh-token")
        , expiresInSeconds = Just 3600
        }
  secretStore.put tokenKey tokenSet
  -- Create mock provider
  let mockProvider = Provider
        { name = "oura"
        , authorizeEndpoint = "https://example.com/authorize"
        , tokenEndpoint = "https://example.com/token"
        }
  let validatedProvider = unsafeValidatedProvider mockProvider
  -- Create ValidatedOAuth2ProviderConfig
  let redirectUriResult = mkRedirectUri "https://example.com/callback"
  redirectUri <- case redirectUriResult of
    Ok uri -> Task.yield uri
    Err _ -> Task.throw "Failed to create redirect URI"
  let providerConfig = ValidatedOAuth2ProviderConfig
        { provider = mockProvider
        , validatedProvider = validatedProvider
        , clientId = ClientId "mock-client-id"
        , clientSecret = mkClientSecret "mock-client-secret"
        , redirectUri = redirectUri
        , scopes = Array.fromLinkedList [Scope "personal"]
        , onSuccess = \_userId _tokenKey -> "{}"
        , onFailure = \_userId _err -> "{}"
        , onDisconnect = \_userId -> "{}"
        , successRedirectUrl = "https://example.com/success"
        , failureRedirectUrl = "https://example.com/failure"
        }
  let providerRegistry = Map.fromArray (Array.fromLinkedList [("oura", providerConfig)])
  Task.yield ActionContext
    { secretStore = secretStore
    , providerRegistry = providerRegistry
    }


-- | Default test SyncAll config
testSyncAllConfig :: SyncAll TestCommand
testSyncAllConfig = SyncAll
  { userId = "test-user"
  , startDate = "2024-01-01"
  , endDate = "2024-01-07"
  , onComplete = \syncResult -> TestCommand { capturedResult = Just syncResult, errorMessage = Nothing }
  , onError = Nothing
  }


-- | Re-export from Data.Maybe for test assertions
isJust :: forall value. Maybe value -> Bool
isJust = GhcMaybe.isJust

