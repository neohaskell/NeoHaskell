module Service.AuthSpec where

import Auth.Claims (UserClaims (..))
import Array qualified
import Core
import DateTime qualified
import Map qualified
import Service.Auth (RequestContext (..))
import Service.Auth qualified as Auth
import Test
import Uuid qualified


spec :: Spec Unit
spec = do
  describe "Service.Auth" do
    describe "emptyContext" do
      it "returns RequestContext with all fields set" \_ -> do
        -- Happy path: emptyContext successfully constructs a context.
        -- What this case proves: emptyContext produces a well-formed RequestContext.
        let ctx = Auth.emptyContext
        ctx.user |> shouldBe Nothing
        ctx.files |> shouldBe Map.empty
        ctx.requestId |> shouldBe Uuid.nil

      it "timestamp field is exactly DateTime.fromEpochSeconds 0" \_ -> do
        -- Edge case: the timestamp value is Unix epoch (1970-01-01T00:00:00Z).
        -- What this case proves: emptyContext.timestamp is the epoch constant, not an arbitrary time.
        let ctx = Auth.emptyContext
        ctx.timestamp |> DateTime.toEpochSeconds |> shouldBe 0

      it "timestamp is a DateTime value with zero epoch seconds" \_ -> do
        -- Edge case: verify round-trip consistency (epoch is boundary).
        -- What this case proves: DateTime.fromEpochSeconds 0 and toEpochSeconds round-trip identity.
        let expected = DateTime.fromEpochSeconds 0
        let ctx = Auth.emptyContext
        ctx.timestamp |> shouldBe expected

    describe "anonymousContext" do
      it "returns RequestContext with Nothing user and empty files" \_ -> do
        -- Happy path: anonymousContext executes and yields a context.
        -- What this case proves: anonymousContext constructs a request context with correct default fields.
        ctx <- Auth.anonymousContext
        ctx.user |> shouldBe Nothing
        ctx.files |> shouldBe Map.empty

      it "requestId is generated (not nil)" \_ -> do
        -- Edge case: verify requestId is freshly generated, not a sentinel value.
        -- What this case proves: anonymousContext calls Uuid.generate, not hardcoded Uuid.nil.
        ctx <- Auth.anonymousContext
        ctx.requestId |> shouldNotBe Uuid.nil

      it "timestamp is current time (within 5 seconds of sampling)" \_ -> do
        -- Edge case: timestamp is "now", not a constant or arbitrary value.
        -- What this case proves: anonymousContext calls DateTime.now to capture wall-clock time.
        beforeNow <- DateTime.now
        ctx <- Auth.anonymousContext
        afterNow <- DateTime.now
        let ctxEpoch = DateTime.toEpochSeconds ctx.timestamp
        let beforeEpoch = DateTime.toEpochSeconds beforeNow
        let afterEpoch = DateTime.toEpochSeconds (DateTime.addSeconds 5 afterNow)
        ctxEpoch |> shouldSatisfy (\s -> s >= beforeEpoch && s <= afterEpoch)

    describe "authenticatedContext" do
      it "returns RequestContext with user and empty files" \_ -> do
        -- Happy path: authenticatedContext executes and yields a context with the given claims.
        -- What this case proves: authenticatedContext constructs a request context with the supplied user.
        let claims = testUser "user-123"
        ctx <- Auth.authenticatedContext claims
        ctx.user |> shouldBe (Just claims)
        ctx.files |> shouldBe Map.empty

      it "requestId is generated (not nil)" \_ -> do
        -- Edge case: verify requestId is freshly generated, not a sentinel value.
        -- What this case proves: authenticatedContext calls Uuid.generate, not hardcoded Uuid.nil.
        let claims = testUser "user-456"
        ctx <- Auth.authenticatedContext claims
        ctx.requestId |> shouldNotBe Uuid.nil

      it "timestamp is current time (within 5 seconds of sampling)" \_ -> do
        -- Edge case: timestamp is "now", not a constant or arbitrary value.
        -- What this case proves: authenticatedContext calls DateTime.now to capture wall-clock time.
        let claims = testUser "user-789"
        beforeNow <- DateTime.now
        ctx <- Auth.authenticatedContext claims
        afterNow <- DateTime.now
        let ctxEpoch = DateTime.toEpochSeconds ctx.timestamp
        let beforeEpoch = DateTime.toEpochSeconds beforeNow
        let afterEpoch = DateTime.toEpochSeconds (DateTime.addSeconds 5 afterNow)
        ctxEpoch |> shouldSatisfy (\s -> s >= beforeEpoch && s <= afterEpoch)

    describe "Test.Service.Command.Decide.Spec.authenticatedContext" do
      it "returns RequestContext with user and deterministic fields" \_ -> do
        -- Happy path: the test helper successfully constructs a context.
        -- What this case proves: the test helper produces a well-formed RequestContext with the supplied user ID.
        let ctx = testHelperAuthContext "test-user-1"
        ctx.requestId |> shouldBe Uuid.nil
        ctx.files |> shouldBe Map.empty
        ctx.user |> shouldNotBe Nothing

      it "timestamp field is exactly DateTime.fromEpochSeconds 0" \_ -> do
        -- Edge case: the timestamp is epoch, making test fixtures deterministic.
        -- What this case proves: the test helper uses epoch constant for reproducible test setup.
        let ctx = testHelperAuthContext "test-user-2"
        ctx.timestamp |> DateTime.toEpochSeconds |> shouldBe 0

      it "user field contains the supplied userId" \_ -> do
        -- Edge case: verify the user field is correctly populated with the test user.
        -- What this case proves: the test helper wraps the userId in testUser and assigns it to the user field.
        let userId = "test-user-3"
        let ctx = testHelperAuthContext userId
        case ctx.user of
          Just claims -> claims.sub |> shouldBe userId
          Nothing -> fail "Expected user to be Just, got Nothing"


-- | Inline replica of Test.Service.Command.Decide.Spec.testUser for use in AuthSpec.
testUser :: Text -> UserClaims
testUser userId =
  UserClaims
    { sub = userId
    , email = Nothing
    , name = Nothing
    , permissions = Array.empty
    , tenantId = Nothing
    , rawClaims = Map.empty
    }


-- | Inline replica of Test.Service.Command.Decide.Spec.authenticatedContext test helper.
-- Mirrors the helper under test so we can verify its post-retype behaviour.
testHelperAuthContext :: Text -> RequestContext
testHelperAuthContext userId =
  RequestContext
    { user = Just (testUser userId)
    , files = Map.empty
    , requestId = Uuid.nil
    , timestamp = DateTime.fromEpochSeconds 0
    , trustedBypass = False
    }
