module Auth.OAuth2.TypesSpec where

import Auth.OAuth2.Types (
  TokenSet (..),
  mkAccessToken,
  mkAuthorizationCode,
  mkClientSecret,
  mkRefreshToken,
  mkState,
 )
import Core
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Auth.OAuth2.Types" do
    describe "Secret Redaction (security)" do
      -- CRITICAL: Secrets must never leak via Show instances
      -- This prevents accidental logging of sensitive data

      describe "ClientSecret" do
        it "Show instance does not reveal the secret value" \_ -> do
          let secret = mkClientSecret "super-secret-value-12345"
          let shown = toText secret
          -- The actual secret value should NOT appear in the output
          shown |> shouldSatisfy (\t -> not (Text.contains "super-secret-value-12345" t))
          -- Should show a redacted placeholder
          shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

        it "multiple secrets show the same redacted output" \_ -> do
          let secret1 = mkClientSecret "first-secret"
          let secret2 = mkClientSecret "second-secret"
          -- Both should show identically (no information leak via length etc)
          toText secret1 |> shouldBe (toText secret2)

      describe "AccessToken" do
        it "Show instance does not reveal the token value" \_ -> do
          let token = mkAccessToken "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.xxxxx"
          let shown = toText token
          shown |> shouldSatisfy (\t -> not (Text.contains "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9" t))
          shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

      describe "RefreshToken" do
        it "Show instance does not reveal the token value" \_ -> do
          let token = mkRefreshToken "refresh-token-value-xyz"
          let shown = toText token
          shown |> shouldSatisfy (\t -> not (Text.contains "refresh-token-value-xyz" t))
          shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

      describe "AuthorizationCode" do
        it "Show instance does not reveal the code value" \_ -> do
          let code = mkAuthorizationCode "secret-auth-code-xyz-12345"
          let shown = toText code
          -- The actual code value should NOT appear in the output
          shown |> shouldSatisfy (\t -> not (Text.contains "secret-auth-code-xyz-12345" t))
          -- Should show a redacted placeholder
          shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

      describe "State" do
        it "Show instance does not reveal the state value" \_ -> do
          let state = mkState "csrf-token-abc-98765"
          let shown = toText state
          -- The actual state value should NOT appear in the output
          shown |> shouldSatisfy (\t -> not (Text.contains "csrf-token-abc-98765" t))
          -- Should show a redacted placeholder
          shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

      describe "TokenSet" do
        it "Show instance does not reveal any token values" \_ -> do
          let tokens =
                TokenSet
                  { accessToken = mkAccessToken "access-secret-123"
                  , refreshToken = Just (mkRefreshToken "refresh-secret-456")
                  , expiresInSeconds = Just 3600
                  }
          let shown = toText tokens
          shown |> shouldSatisfy (\t -> not (Text.contains "access-secret-123" t))
          shown |> shouldSatisfy (\t -> not (Text.contains "refresh-secret-456" t))

    describe "Token Expiry Semantics" do
      -- CRITICAL: expires_in from OAuth2 is a DURATION in seconds,
      -- not a unix timestamp. The field name must reflect this.

      it "TokenSet has expiresInSeconds field (duration, not timestamp)" \_ -> do
        -- This test verifies the field is named correctly to prevent
        -- misuse. If someone renames it to expiresAt, they'll need
        -- to update this test and think about the semantics.
        let tokens =
              TokenSet
                { accessToken = mkAccessToken "test"
                , refreshToken = Nothing
                , expiresInSeconds = Just 3600 -- 1 hour duration
                }
        -- The field should be accessible with correct name
        tokens.expiresInSeconds |> shouldBe (Just 3600)



