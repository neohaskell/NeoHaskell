module Integration.Oura.InternalSpec (spec) where

import Basics
import Test.Hspec
import Integration.Oura.Internal
  ( OuraHttpError (..)
  , HttpFetch
  , HttpFetchSingle
  , isUnauthorized
  , mapTokenError
  , urlEncodeDatetime
  , urlEncodeParam
  , getOuraProvider
  , executeDailySleepInternal
  , executeWorkoutInternal
  , executePersonalInfoInternal
  )
import Integration.Oura.Types (PaginatedResponse (..), SleepData (..), WorkoutData (..), PersonalInfoData (..))
import Integration (ActionContext (..), IntegrationError (..))
import Auth.OAuth2.TokenRefresh (TokenRefreshError (..))
import Auth.OAuth2.Types (OAuth2Error (TokenRequestFailed))
import Auth.SecretStore.InMemory qualified as InMemorySecretStore
import Auth.OAuth2.Provider (ValidatedOAuth2ProviderConfig)
import Task (Task)
import Task qualified
import Result (Result (..))
import Array (Array)
import Array qualified
import Text (Text)
import Text qualified
import Maybe (Maybe (..))
import Map qualified


spec :: Spec
spec = do
  describe "Integration.Oura.Internal" do
    describe "fetchAllPages (via executeDailySleepInternal)" do
      it "accumulates data across multiple pages" do
        -- Mock that returns next_token on first call, Nothing on second
        let mockPaginatedFetch :: HttpFetch SleepData
            mockPaginatedFetch _token url =
              case Text.contains "next_token" url of
                True -> Task.yield PaginatedResponse
                  { data_ = [mockSleepData2]
                  , nextToken = Nothing
                  }
                False -> Task.yield PaginatedResponse
                  { data_ = [mockSleepData1]
                  , nextToken = Just "page2token"
                  }

        result <- Task.runOrPanic do
          executeDailySleepInternal mockPaginatedFetch "test-token" "https://api.example.com/sleep"

        Array.length result `shouldBe` 2
        -- First page data comes first, then second page
        case Array.get 0 result of
          Just d -> d.id `shouldBe` "sleep1"
          Nothing -> expectationFailure "Expected first element"
        case Array.get 1 result of
          Just d -> d.id `shouldBe` "sleep2"
          Nothing -> expectationFailure "Expected second element"

      it "handles single page response" do
        let mockSinglePage :: HttpFetch SleepData
            mockSinglePage _token _url = Task.yield PaginatedResponse
              { data_ = [mockSleepData1]
              , nextToken = Nothing
              }

        result <- Task.runOrPanic do
          executeDailySleepInternal mockSinglePage "test-token" "https://api.example.com/sleep"

        Array.length result `shouldBe` 1
        case Array.get 0 result of
          Just d -> d.id `shouldBe` "sleep1"
          Nothing -> expectationFailure "Expected one element"

      it "accumulates all data from multiple pages" do
        -- Mock that returns 3 pages
        let callCount :: Text -> Int
            callCount url
              | Text.contains "page3" url = 3
              | Text.contains "page2" url = 2
              | True = 1

            mockThreePages :: HttpFetch SleepData
            mockThreePages _token url =
              case callCount url of
                1 -> Task.yield PaginatedResponse
                  { data_ = [mockSleepData1]
                  , nextToken = Just "page2"
                  }
                2 -> Task.yield PaginatedResponse
                  { data_ = [mockSleepData2]
                  , nextToken = Just "page3"
                  }
                _ -> Task.yield PaginatedResponse
                  { data_ = [mockSleepData3]
                  , nextToken = Nothing
                  }

        result <- Task.runOrPanic do
          executeDailySleepInternal mockThreePages "test-token" "https://api.example.com/sleep"

        Array.length result `shouldBe` 3

    describe "Error handling" do
      it "propagates Unauthorized error from httpFetch" do
        let mockUnauthorized :: HttpFetch SleepData
            mockUnauthorized _token _url = Task.throw Unauthorized

        let task :: Task Text (Result OuraHttpError (Array SleepData))
            task = executeDailySleepInternal mockUnauthorized "test-token" "https://api.example.com/sleep"
              |> Task.asResult
        result <- Task.runOrPanic task

        case result of
          Err Unauthorized -> pure ()
          _ -> expectationFailure "Expected Unauthorized error"

      it "missing provider throws UnexpectedError" do
        stubStore <- Task.runOrPanic InMemorySecretStore.new
        let emptyCtx = ActionContext
              { secretStore = stubStore
              , providerRegistry = Map.empty
              , fileAccess = Nothing
              }

        let task :: Task Text (Result IntegrationError ValidatedOAuth2ProviderConfig)
            task = getOuraProvider emptyCtx |> Task.asResult
        result <- Task.runOrPanic task

        case result of
          Err (UnexpectedError msg) ->
            Text.contains "Oura provider not configured" msg `shouldBe` True
          _ -> expectationFailure "Expected UnexpectedError with 'Oura provider not configured'"

    describe "mapTokenError" do
      it "maps TokenNotFound to AuthenticationError" do
        let err = TokenNotFound "user1" :: TokenRefreshError OuraHttpError
        case mapTokenError err of
          AuthenticationError msg -> Text.contains "user1" msg `shouldBe` True
          _ -> expectationFailure "Expected AuthenticationError"

      it "maps RefreshTokenMissing to AuthenticationError" do
        let err = RefreshTokenMissing :: TokenRefreshError OuraHttpError
        case mapTokenError err of
          AuthenticationError msg -> Text.contains "Refresh token missing" msg `shouldBe` True
          _ -> expectationFailure "Expected AuthenticationError"

      it "maps RefreshFailed to AuthenticationError" do
        let oauth2Err = TokenRequestFailed "network error"
        let err = RefreshFailed oauth2Err :: TokenRefreshError OuraHttpError
        case mapTokenError err of
          AuthenticationError msg -> Text.contains "Refresh failed" msg `shouldBe` True
          _ -> expectationFailure "Expected AuthenticationError"

      it "maps StorageError to UnexpectedError" do
        let err = StorageError "db connection lost" :: TokenRefreshError OuraHttpError
        case mapTokenError err of
          UnexpectedError msg -> Text.contains "Storage error" msg `shouldBe` True
          _ -> expectationFailure "Expected UnexpectedError"

      it "maps ActionFailed Unauthorized to AuthenticationError" do
        let err = ActionFailed Unauthorized :: TokenRefreshError OuraHttpError
        case mapTokenError err of
          AuthenticationError msg -> Text.contains "Unauthorized after refresh" msg `shouldBe` True
          _ -> expectationFailure "Expected AuthenticationError"

      it "maps ActionFailed OuraRateLimited to RateLimited" do
        let err = ActionFailed (OuraRateLimited 60) :: TokenRefreshError OuraHttpError
        case mapTokenError err of
          RateLimited seconds -> seconds `shouldBe` 60
          _ -> expectationFailure "Expected RateLimited"

      it "maps ActionFailed OtherHttpError to NetworkError" do
        let err = ActionFailed (OtherHttpError "connection refused") :: TokenRefreshError OuraHttpError
        case mapTokenError err of
          NetworkError msg -> Text.contains "connection refused" msg `shouldBe` True
          _ -> expectationFailure "Expected NetworkError"

    describe "urlEncodeDatetime" do
      it "encodes + as %2B in datetime" do
        let input = "2024-01-01T00:00:00+08:00"
        let expected = "2024-01-01T00:00:00%2B08:00"
        urlEncodeDatetime input `shouldBe` expected

      it "handles negative timezone offset (no encoding needed)" do
        let input = "2024-01-01T00:00:00-05:00"
        urlEncodeDatetime input `shouldBe` input

      it "handles Z timezone (no encoding needed)" do
        let input = "2024-01-01T00:00:00Z"
        urlEncodeDatetime input `shouldBe` input

    describe "urlEncodeParam" do
      it "encodes + as %2B" do
        urlEncodeParam "foo+bar" `shouldBe` "foo%2Bbar"

      it "encodes & as %26" do
        urlEncodeParam "foo&bar" `shouldBe` "foo%26bar"

      it "encodes = as %3D" do
        urlEncodeParam "foo=bar" `shouldBe` "foo%3Dbar"

      it "encodes % as %25" do
        urlEncodeParam "100%" `shouldBe` "100%25"

      it "encodes # as %23" do
        urlEncodeParam "foo#bar" `shouldBe` "foo%23bar"

      it "encodes ? as %3F" do
        urlEncodeParam "foo?bar" `shouldBe` "foo%3Fbar"

      it "encodes / as %2F" do
        urlEncodeParam "foo/bar" `shouldBe` "foo%2Fbar"

      it "encodes space as %20" do
        urlEncodeParam "foo bar" `shouldBe` "foo%20bar"

      it "handles already-encoded percent correctly (encodes first)" do
        -- If input contains %2B, the % gets encoded to %25, resulting in %252B
        -- This is correct behavior - we're encoding the raw token value
        urlEncodeParam "%2B" `shouldBe` "%252B"

      it "handles complex pagination token with multiple special chars" do
        -- Simulates a token like: abc+def&page=2
        let input = "abc+def&page=2"
        let expected = "abc%2Bdef%26page%3D2"
        urlEncodeParam input `shouldBe` expected

      it "leaves alphanumeric characters unchanged" do
        urlEncodeParam "abc123XYZ" `shouldBe` "abc123XYZ"

      it "leaves hyphen and underscore unchanged" do
        urlEncodeParam "foo-bar_baz" `shouldBe` "foo-bar_baz"

    describe "isUnauthorized" do
      it "returns True for Unauthorized" do
        isUnauthorized Unauthorized `shouldBe` True

      it "returns False for OuraRateLimited" do
        isUnauthorized (OuraRateLimited 60) `shouldBe` False

      it "returns False for OtherHttpError" do
        isUnauthorized (OtherHttpError "some error") `shouldBe` False

    describe "executeWorkoutInternal (paginated endpoint)" do
      it "accumulates workout data across multiple pages" do
        let mockPaginatedFetch :: HttpFetch WorkoutData
            mockPaginatedFetch _token url =
              case Text.contains "next_token" url of
                True -> Task.yield PaginatedResponse
                  { data_ = [mockWorkoutData2]
                  , nextToken = Nothing
                  }
                False -> Task.yield PaginatedResponse
                  { data_ = [mockWorkoutData1]
                  , nextToken = Just "page2token"
                  }

        result <- Task.runOrPanic do
          executeWorkoutInternal mockPaginatedFetch "test-token" "https://api.example.com/workout"

        Array.length result `shouldBe` 2
        case Array.get 0 result of
          Just d -> d.id `shouldBe` "workout1"
          Nothing -> expectationFailure "Expected first element"
        case Array.get 1 result of
          Just d -> d.id `shouldBe` "workout2"
          Nothing -> expectationFailure "Expected second element"

      it "handles single page workout response" do
        let mockSinglePage :: HttpFetch WorkoutData
            mockSinglePage _token _url = Task.yield PaginatedResponse
              { data_ = [mockWorkoutData1]
              , nextToken = Nothing
              }

        result <- Task.runOrPanic do
          executeWorkoutInternal mockSinglePage "test-token" "https://api.example.com/workout"

        Array.length result `shouldBe` 1
        case Array.get 0 result of
          Just d -> d.id `shouldBe` "workout1"
          Nothing -> expectationFailure "Expected one element"

    describe "executePersonalInfoInternal (non-paginated endpoint)" do
      it "fetches single PersonalInfo object directly" do
        let mockFetchSingle :: HttpFetchSingle PersonalInfoData
            mockFetchSingle _token _url = Task.yield mockPersonalInfo1

        result <- Task.runOrPanic do
          executePersonalInfoInternal mockFetchSingle "test-token" "https://api.example.com/personal_info"

        result.id `shouldBe` "pi1"
        result.email `shouldBe` Just "test@example.com"

      it "propagates Unauthorized error from non-paginated fetch" do
        let mockUnauthorized :: HttpFetchSingle PersonalInfoData
            mockUnauthorized _token _url = Task.throw Unauthorized

        let task :: Task Text (Result OuraHttpError PersonalInfoData)
            task = executePersonalInfoInternal mockUnauthorized "test-token" "https://api.example.com/personal_info"
              |> Task.asResult
        result <- Task.runOrPanic task

        case result of
          Err Unauthorized -> pure ()
          _ -> expectationFailure "Expected Unauthorized error"

    -- Note: realHttpFetch is tested in Http.ClientRawSpec for actual HTTP behavior
    -- Here we only document the expected behavior:
    -- - 401 response -> throws Unauthorized
    -- - 429 response -> throws OuraRateLimited with Retry-After value
    -- - Invalid JSON -> throws OtherHttpError with decode error message
    describe "realHttpFetch (behavior documentation)" do
      it "OuraRateLimited contains retry-after seconds" do
        -- This test documents the expected structure
        let err = OuraRateLimited 120
        case err of
          OuraRateLimited seconds -> seconds `shouldBe` 120
          _ -> expectationFailure "Expected OuraRateLimited"

      it "OtherHttpError contains error message" do
        let err = OtherHttpError "JSON decode failed: unexpected token"
        case err of
          OtherHttpError msg -> Text.contains "decode failed" msg `shouldBe` True
          _ -> expectationFailure "Expected OtherHttpError"


-- Mock SleepData for tests
mockSleepData1 :: SleepData
mockSleepData1 = SleepData
  { id = "sleep1"
  , day = "2024-01-01"
  , score = Just 85
  , timestamp = "2024-01-01T08:00:00Z"
  , contributors = Nothing
  }


mockSleepData2 :: SleepData
mockSleepData2 = SleepData
  { id = "sleep2"
  , day = "2024-01-02"
  , score = Just 90
  , timestamp = "2024-01-02T08:00:00Z"
  , contributors = Nothing
  }


mockSleepData3 :: SleepData
mockSleepData3 = SleepData
  { id = "sleep3"
  , day = "2024-01-03"
  , score = Just 88
  , timestamp = "2024-01-03T08:00:00Z"
  , contributors = Nothing
  }


-- Mock WorkoutData for tests
mockWorkoutData1 :: WorkoutData
mockWorkoutData1 = WorkoutData
  { id = "workout1"
  , day = "2024-01-01"
  , activity = Just "running"
  , calories = Just 350.0
  , distance = Just 5000.0
  , endDatetime = Just "2024-01-01T07:30:00Z"
  , intensity = Just "moderate"
  , label = Nothing
  , source = Just "manual"
  , startDatetime = Just "2024-01-01T07:00:00Z"
  }


mockWorkoutData2 :: WorkoutData
mockWorkoutData2 = WorkoutData
  { id = "workout2"
  , day = "2024-01-02"
  , activity = Just "cycling"
  , calories = Just 500.0
  , distance = Just 15000.0
  , endDatetime = Just "2024-01-02T08:00:00Z"
  , intensity = Just "high"
  , label = Nothing
  , source = Just "manual"
  , startDatetime = Just "2024-01-02T07:00:00Z"
  }


-- Mock PersonalInfoData for tests
mockPersonalInfo1 :: PersonalInfoData
mockPersonalInfo1 = PersonalInfoData
  { id = "pi1"
  , age = Just 30
  , weight = Just 75.0
  , height = Just 180.0
  , biologicalSex = Just "male"
  , email = Just "test@example.com"
  }
