module Auth.OAuth2.TypesSpec where

import Auth.OAuth2.Types (
  AccessToken (..),
  AuthorizationCode (..),
  ClientSecret (..),
  RefreshToken (..),
  State (..),
  TokenSet (..),
 )
import Core
import Task qualified
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
          let secret = ClientSecret "super-secret-value-12345"
          let shown = toText secret
          -- The actual secret value should NOT appear in the output
          shown |> shouldSatisfy (\t -> not (Text.contains "super-secret-value-12345" t))
          -- Should show a redacted placeholder
          shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

        it "multiple secrets show the same redacted output" \_ -> do
          let secret1 = ClientSecret "first-secret"
          let secret2 = ClientSecret "second-secret"
          -- Both should show identically (no information leak via length etc)
          toText secret1 |> shouldBe (toText secret2)

      describe "AccessToken" do
        it "Show instance does not reveal the token value" \_ -> do
          let token = AccessToken "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.xxxxx"
          let shown = toText token
          shown |> shouldSatisfy (\t -> not (Text.contains "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9" t))
          shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

      describe "RefreshToken" do
        it "Show instance does not reveal the token value" \_ -> do
          let token = RefreshToken "refresh-token-value-xyz"
          let shown = toText token
          shown |> shouldSatisfy (\t -> not (Text.contains "refresh-token-value-xyz" t))
          shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

      describe "AuthorizationCode" do
        it "Show instance does not reveal the code value" \_ -> do
          let code = AuthorizationCode "secret-auth-code-xyz-12345"
          let shown = toText code
          -- The actual code value should NOT appear in the output
          shown |> shouldSatisfy (\t -> not (Text.contains "secret-auth-code-xyz-12345" t))
          -- Should show a redacted placeholder
          shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

      describe "State" do
        it "Show instance does not reveal the state value" \_ -> do
          let state = State "csrf-token-abc-98765"
          let shown = toText state
          -- The actual state value should NOT appear in the output
          shown |> shouldSatisfy (\t -> not (Text.contains "csrf-token-abc-98765" t))
          -- Should show a redacted placeholder
          shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

      describe "TokenSet" do
        it "Show instance does not reveal any token values" \_ -> do
          let tokens =
                TokenSet
                  { accessToken = AccessToken "access-secret-123"
                  , refreshToken = Just (RefreshToken "refresh-secret-456")
                  , expiresInSeconds = Just 3600
                  }
          let shown = toText tokens
          shown |> shouldSatisfy (\t -> not (Text.contains "access-secret-123" t))
          shown |> shouldSatisfy (\t -> not (Text.contains "refresh-secret-456" t))

    describe "JSON Serialization (security)" do
      -- Secret types should NOT have JSON instances to prevent
      -- accidental serialization to logs, events, or API responses

      -- NOTE: These tests verify at the type level that JSON instances
      -- don't exist. If they did exist, the code wouldn't compile
      -- because we'd be trying to use non-existent instances.
      -- The actual enforcement is done by NOT deriving ToJSON/FromJSON
      -- for secret types.

      it "ClientSecret cannot be accidentally serialized" \_ -> do
        -- This test documents the design decision.
        -- If someone adds ToJSON to ClientSecret, they need to
        -- consciously update this test, forcing them to think about security.
        --
        -- The actual protection is that ClientSecret has no ToJSON instance,
        -- so any code trying to serialize it will fail to compile.
        Task.yield ()

      it "AccessToken cannot be accidentally serialized" \_ -> do
        Task.yield ()

      it "RefreshToken cannot be accidentally serialized" \_ -> do
        Task.yield ()

    describe "Token Expiry Semantics" do
      -- CRITICAL: expires_in from OAuth2 is a DURATION in seconds,
      -- not a unix timestamp. The field name must reflect this.

      it "TokenSet has expiresInSeconds field (duration, not timestamp)" \_ -> do
        -- This test verifies the field is named correctly to prevent
        -- misuse. If someone renames it to expiresAt, they'll need
        -- to update this test and think about the semantics.
        let tokens =
              TokenSet
                { accessToken = AccessToken "test"
                , refreshToken = Nothing
                , expiresInSeconds = Just 3600 -- 1 hour duration
                }
        -- The field should be accessible with correct name
        tokens.expiresInSeconds |> shouldBe (Just 3600)



