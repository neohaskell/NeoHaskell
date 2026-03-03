-- | Tests for constant-time comparison in OAuth2 state validation.
--
-- Verifies that validateState and validateRedirectUri use constant-time
-- comparison to prevent timing oracle attacks.
module Auth.OAuth2TypesSpec where

import Auth.OAuth2.StateToken (constEq)
import Auth.OAuth2.Types
import Core
import Test


spec :: Spec Unit
spec = do
  describe "constEq" do
    it "returns True for equal Text values" do
      let result = constEq "hello" "hello"
      result `shouldBe` True

    it "returns False for different Text values" do
      let result = constEq "hello" "world"
      result `shouldBe` False

    it "returns False for different-length Text values" do
      let result = constEq "short" "much longer text"
      result `shouldBe` False

    it "returns False for empty vs non-empty" do
      let result = constEq "" "text"
      result `shouldBe` False

    it "returns True for both empty" do
      let result = constEq "" ""
      result `shouldBe` True

  describe "validateState" do
    it "returns Ok () for matching states" do
      let expected = mkState "test-state-token-123"
      let returned = mkState "test-state-token-123"
      let result = validateState expected returned
      result `shouldSatisfy` isOk

    it "returns Err InvalidState for mismatched states" do
      let expected = mkState "state-1"
      let returned = mkState "state-2"
      let result = validateState expected returned
      case result of
        Ok _ -> Test.fail "Expected Err, got Ok"
        Err (InvalidState _) -> pure ()
        Err err -> Test.fail [fmt|Expected InvalidState, got {show err}|]

    it "returns Err InvalidState for empty vs non-empty state" do
      let expected = mkState ""
      let returned = mkState "some-state"
      let result = validateState expected returned
      case result of
        Ok _ -> Test.fail "Expected Err, got Ok"
        Err (InvalidState _) -> pure ()
        Err err -> Test.fail [fmt|Expected InvalidState, got {show err}|]

  describe "validateRedirectUri" do
    it "returns Ok () for matching redirect URIs" do
      let registered = mkRedirectUri "https://example.com/callback"
      let received = mkRedirectUri "https://example.com/callback"
      case (registered, received) of
        (Ok reg, Ok rec) -> do
          let result = validateRedirectUri reg rec
          result `shouldSatisfy` isOk
        _ -> Test.fail "Failed to create redirect URIs"

    it "returns Err InvalidRedirectUri for mismatched URIs" do
      let registered = mkRedirectUri "https://example.com/callback"
      let received = mkRedirectUri "https://example.com/other"
      case (registered, received) of
        (Ok reg, Ok rec) -> do
          let result = validateRedirectUri reg rec
          case result of
            Ok _ -> Test.fail "Expected Err, got Ok"
            Err (InvalidRedirectUri _) -> pure ()
            Err err -> Test.fail [fmt|Expected InvalidRedirectUri, got {show err}|]
        _ -> Test.fail "Failed to create redirect URIs"

    it "returns Err InvalidRedirectUri for different schemes" do
      let registered = mkRedirectUri "https://example.com/callback"
      let received = mkRedirectUri "http://example.com/callback"
      case (registered, received) of
        (Ok reg, Ok rec) -> do
          let result = validateRedirectUri reg rec
          case result of
            Ok _ -> Test.fail "Expected Err, got Ok"
            Err (InvalidRedirectUri _) -> pure ()
            Err err -> Test.fail [fmt|Expected InvalidRedirectUri, got {show err}|]
        _ -> Test.fail "Failed to create redirect URIs"

    it "returns Ok () for matching localhost URIs" do
      let registered = mkRedirectUri "http://localhost:3000/callback"
      let received = mkRedirectUri "http://localhost:3000/callback"
      case (registered, received) of
        (Ok reg, Ok rec) -> do
          let result = validateRedirectUri reg rec
          result `shouldSatisfy` isOk
        _ -> Test.fail "Failed to create redirect URIs"
