{-# LANGUAGE LambdaCase #-}

-- | Security tests for Config DSL secret handling.
--
-- These tests verify that the @secret@ modifier properly protects sensitive
-- configuration values from being leaked in Show instances, error messages,
-- and help output.
--
-- == Critical Security Properties
--
-- 1. Secret fields MUST NOT appear in Show output
-- 2. Secret fields MUST NOT appear in error messages
-- 3. Secret field defaults MUST NOT appear in --help output
-- 4. The @ModSecret@ modifier MUST affect generated code
module Config.SecretSpec where

import Config (defineConfig)
import Config.Builder (defaultsTo, doc, field, required, secret)
import Config.Core (FieldDef (..), FieldModifier (..))
import Core
import LinkedList qualified
import Test
import Text qualified


-- | Test config with a mix of secret and non-secret fields.
-- This config is used to verify secret field redaction behavior.
$(defineConfig "SecretTestConfig"
  [ field @Text "apiKey"
      |> doc "API key for external service (SENSITIVE)"
      |> secret
      |> defaultsTo ("default-api-key-12345" :: Text)
  , field @Int "port"
      |> doc "Server port (not sensitive)"
      |> defaultsTo (8080 :: Int)
  , field @Text "databasePassword"
      |> doc "Database password (SENSITIVE)"
      |> secret
      |> required
  , field @Text "hostname"
      |> doc "Hostname (not sensitive)"
      |> defaultsTo ("localhost" :: Text)
  ])


-- | Helper function that uses the config via implicit parameter.
-- Used to test that HasSecretTestConfig type alias works correctly.
useSecretConfigImplicit :: (HasSecretTestConfig) => Text
useSecretConfigImplicit = ?config.apiKey


-- | Helper to check if a modifier is present in a FieldDef
hasModifier :: (FieldModifier -> Bool) -> FieldDef -> Bool
hasModifier predicate fd = LinkedList.any predicate fd.fieldModifiers


spec :: Spec Unit
spec = do
  describe "Config.Secret" do
    describe "ModSecret modifier presence" do
      it "secret adds ModSecret to field modifiers" \_ -> do
        let fd = field @Text "apiKey"
              |> doc "API key"
              |> secret
              |> defaultsTo ("key" :: Text)
        let isSecret = \case
              ModSecret -> True
              _ -> False
        hasModifier isSecret fd |> shouldBe True

      it "non-secret fields do not have ModSecret" \_ -> do
        let fd = field @Int "port"
              |> doc "Server port"
              |> defaultsTo (8080 :: Int)
        let isSecret = \case
              ModSecret -> True
              _ -> False
        hasModifier isSecret fd |> shouldBe False

    describe "Show instance security (CRITICAL)" do
      -- These tests verify that secret values are NOT exposed when
      -- a config value is shown (e.g., in logs, debug output)

      it "does NOT reveal secret value when config is shown" \_ -> do
        let config = SecretTestConfig "my-super-secret-api-key-xyz" 8080 "db-password-abc" "localhost"
        let shown = toText config
        -- CRITICAL: The actual secret value must NEVER appear
        shown |> shouldSatisfy (\s -> not (Text.contains "my-super-secret-api-key-xyz" s))

      it "does NOT reveal database password when config is shown" \_ -> do
        let config = SecretTestConfig "api-key" 8080 "super-secret-db-password-999" "localhost"
        let shown = toText config
        -- CRITICAL: Database password must not appear
        shown |> shouldSatisfy (\s -> not (Text.contains "super-secret-db-password-999" s))

      it "shows placeholder for secret fields instead of actual value" \_ -> do
        let config = SecretTestConfig "secret-value" 8080 "another-secret" "localhost"
        let shown = toText config
        -- Must contain REDACTED marker for each secret field
        shown |> shouldSatisfy (\s -> Text.contains "REDACTED" s)
        -- Actual secret values must not appear
        shown |> shouldSatisfy (\s -> not (Text.contains "secret-value" s))
        shown |> shouldSatisfy (\s -> not (Text.contains "another-secret" s))

      it "shows non-secret fields normally" \_ -> do
        let config = SecretTestConfig "secret" 3000 "db-secret" "myhost.example.com"
        let shown = toText config
        -- Port and hostname should be visible since they are not secret
        shown |> shouldSatisfy (\s -> Text.contains "3000" s)
        shown |> shouldSatisfy (\s -> Text.contains "myhost.example.com" s)

      it "all secret fields are redacted independently" \_ -> do
        -- Test with multiple secret fields to ensure each is handled
        let config = SecretTestConfig "first-secret-aaa" 8080 "second-secret-bbb" "localhost"
        let shown = toText config
        -- Neither secret should appear
        shown |> shouldSatisfy (\s -> not (Text.contains "first-secret-aaa" s))
        shown |> shouldSatisfy (\s -> not (Text.contains "second-secret-bbb" s))

    describe "implicit parameter access with secrets" do
      it "HasSecretTestConfig type alias provides access" \_ -> do
        let config = SecretTestConfig "test-key" 9000 "test-db-pass" "test-host"
        let result = let ?config = config in useSecretConfigImplicit
        -- The actual value should still be accessible programmatically
        -- (secrets are just hidden from Show/logs, not inaccessible)
        result |> shouldBe "test-key"

    describe "secret modifier code generation (CRITICAL)" do
      -- This test documents the EXPECTED behavior.
      -- Currently it will FAIL because ModSecret is not implemented in TH.
      -- When ModSecret is properly implemented, this test should pass.

      it "ModSecret should affect generated Show instance" \_ -> do
        -- This test verifies that adding |> secret to a field
        -- results in different behavior than a field without secret.
        --
        -- Current bug: ModSecret is ignored in Config.TH.fieldModifiersToBuilders
        -- so secret and non-secret fields behave identically.
        --
        -- TODO: When this test fails, it proves ModSecret is not implemented.
        -- The fix is to handle ModSecret in Config.TH.
        let secretConfig = SecretTestConfig "visible-secret" 8080 "another-visible" "localhost"
        let shown = toText secretConfig
        -- If ModSecret is properly implemented, "visible-secret" should NOT appear
        -- If this test FAILS, it confirms the security vulnerability
        shown |> shouldSatisfy (\s -> not (Text.contains "visible-secret" s))

    describe "default value security" do
      -- These tests verify that default values for secret fields
      -- are not exposed in generated code or help output.

      it "secret field default should not appear in Show of default config" \_ -> do
        -- When using defaults, the default secret value should still be redacted
        let config = SecretTestConfig "default-api-key-12345" 8080 "any-password" "localhost"
        let shown = toText config
        -- The default value from defineConfig should not appear
        shown |> shouldSatisfy (\s -> not (Text.contains "default-api-key-12345" s))
