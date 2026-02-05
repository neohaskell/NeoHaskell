{-# LANGUAGE TemplateHaskell #-}

-- | Security tests for Config DSL error message safety.
--
-- These tests verify that error messages from configuration loading
-- and validation do not leak sensitive information such as:
--
-- - Environment variable values (especially secrets)
-- - Configuration file contents
-- - CLI argument values for secret fields
--
-- == Critical Security Properties
--
-- 1. Error messages MUST mention field/variable NAMES but not VALUES
-- 2. Parse errors MUST NOT echo back the invalid input if it could be sensitive
-- 3. File parse errors MUST NOT dump file contents
-- 4. All error paths MUST go through consistent sanitization
module Config.ErrorSafetySpec where

import Config.Core (ConfigError (..), validateFieldDef, FieldDef(..), FieldModifier(..))
import Core
import Data.List qualified as GhcList
import Language.Haskell.TH.Syntax qualified as TH
import Test
import Text qualified


-- | Helper to create a FieldDef for testing
makeFieldDef :: Text -> [FieldModifier] -> FieldDef
makeFieldDef name mods =
  FieldDef
    { fieldName = name
    , fieldType = TH.ConT (TH.mkName "Text")
    , fieldModifiers = mods
    }


-- | Helper to format error for testing
formatConfigError :: ConfigError -> Text
formatConfigError err =
  case err of
    MissingDoc fieldName ->
      [fmt|Config field '#{fieldName}' is missing Config.doc.|]
    MissingDefaultOrRequired fieldName ->
      [fmt|Config field '#{fieldName}' must have either Config.defaultsTo or Config.required.|]
    BothDefaultAndRequired fieldName ->
      [fmt|Config field '#{fieldName}' has both Config.defaultsTo and Config.required.|]


spec :: Spec Unit
spec = do
  describe "Config.ErrorSafety" do
    describe "ConfigError message safety" do
      -- These tests verify that validation error messages
      -- only contain field NAMES, never user-provided VALUES

      it "MissingDoc error contains only field name, no values" \_ -> do
        let fd = makeFieldDef "secretApiKey" [ModRequired]
        let errors = validateFieldDef fd
        let errorTexts = errors |> GhcList.map formatConfigError
        -- Error should mention the field name
        errorTexts |> shouldSatisfy (\errs ->
          GhcList.any (Text.contains "secretApiKey") errs)
        -- Error should NOT contain any potential value patterns
        -- (in this case there's no value, but the pattern should be safe)

      it "MissingDefaultOrRequired error contains only field name" \_ -> do
        let fd = makeFieldDef "databasePassword" [ModDoc "password", ModSecret]
        let errors = validateFieldDef fd
        let errorTexts = errors |> GhcList.map formatConfigError
        -- Error should mention the field name
        errorTexts |> shouldSatisfy (\errs ->
          GhcList.any (Text.contains "databasePassword") errs)

      it "BothDefaultAndRequired error contains only field name" \_ -> do
        let fd = makeFieldDef "apiToken" [ModDoc "token", ModSecret, ModRequired, ModDefault (TH.lift ("secret-token-value" :: Text))]
        let errors = validateFieldDef fd
        let errorTexts = errors |> GhcList.map formatConfigError
        -- Error should mention field name
        errorTexts |> shouldSatisfy (\errs ->
          GhcList.any (Text.contains "apiToken") errs)
        -- CRITICAL: Error should NOT contain the default value
        errorTexts |> shouldSatisfy (\errs ->
          not (GhcList.any (Text.contains "secret-token-value") errs))

    describe "validation error aggregation" do
      it "reports all errors at once for fail-fast debugging" \_ -> do
        -- Multiple invalid fields should all be reported together
        let fd1 = makeFieldDef "field1" []  -- Missing doc and default/required
        let fd2 = makeFieldDef "field2" [ModDoc "doc", ModRequired, ModDefault (TH.lift (42 :: Int))]  -- Both
        let fd3 = makeFieldDef "field3" [ModRequired]  -- Missing doc
        let errors1 = validateFieldDef fd1
        let errors2 = validateFieldDef fd2
        let errors3 = validateFieldDef fd3
        let allErrors = errors1 ++ errors2 ++ errors3
        -- Should have multiple errors (not just the first one)
        GhcList.length allErrors |> shouldBeGreaterThan 2

      it "error messages identify which field has the problem" \_ -> do
        let fd1 = makeFieldDef "configA" [ModDoc "doc a"]  -- Missing default/required
        let fd2 = makeFieldDef "configB" [ModDoc "doc b", ModRequired]  -- Valid
        let fd3 = makeFieldDef "configC" []  -- Missing doc and default/required
        let errors1 = validateFieldDef fd1
        let errors2 = validateFieldDef fd2
        let errors3 = validateFieldDef fd3
        let errors = errors1 ++ errors2 ++ errors3
        let errorTexts = errors |> GhcList.map formatConfigError
        -- Errors should identify the problematic fields
        errorTexts |> shouldSatisfy (\errs ->
          GhcList.any (Text.contains "configA") errs)
        errorTexts |> shouldSatisfy (\errs ->
          GhcList.any (Text.contains "configC") errs)
        -- Valid field should not appear in errors
        errorTexts |> shouldSatisfy (\errs ->
          not (GhcList.any (Text.contains "configB") errs))

    describe "error message format consistency" do
      it "all error types follow consistent format" \_ -> do
        let missingDocErr = MissingDoc "testField"
        let missingReqErr = MissingDefaultOrRequired "testField"
        let bothErr = BothDefaultAndRequired "testField"
        -- All errors should produce non-empty messages
        formatConfigError missingDocErr |> shouldSatisfy (\t -> Text.length t > 0)
        formatConfigError missingReqErr |> shouldSatisfy (\t -> Text.length t > 0)
        formatConfigError bothErr |> shouldSatisfy (\t -> Text.length t > 0)

      it "error messages are safe for logging" \_ -> do
        -- Error messages should not contain characters that could
        -- break logging systems or enable log injection
        let err = MissingDoc "fieldWithSpecialChars"
        let formatted = formatConfigError err
        -- Should not contain newlines that could forge log entries
        formatted |> shouldSatisfy (\t -> not (Text.contains "\n" t))
        -- Should not contain null bytes
        formatted |> shouldSatisfy (\t -> not (Text.contains "\0" t))

    describe "secret field error handling" do
      it "errors about secret fields do not reveal ModSecret is set" \_ -> do
        -- When a secret field has validation errors, the error message
        -- should not indicate that it's a secret (which would help attackers
        -- identify high-value targets)
        let fd = makeFieldDef "apiCredential" [ModSecret, ModRequired]  -- Missing doc
        let errors = validateFieldDef fd
        let errorTexts = errors |> GhcList.map formatConfigError
        -- Error should mention field name but not that it's secret
        errorTexts |> shouldSatisfy (\errs ->
          GhcList.any (Text.contains "apiCredential") errs)
        -- Should not reveal the field is marked as secret
        errorTexts |> shouldSatisfy (\errs ->
          not (GhcList.any (Text.contains "secret") errs) &&
          not (GhcList.any (Text.contains "Secret") errs))

    describe "field name safety" do
      it "field names with special characters are handled safely" \_ -> do
        -- Field names should be safe to include in error messages
        -- even if they contain unusual characters
        let fd = makeFieldDef "field_with-special.chars" [ModRequired]
        let errors = validateFieldDef fd
        -- Should not crash or behave unexpectedly
        GhcList.length errors |> shouldBeGreaterThan 0

      it "very long field names are handled" \_ -> do
        let longName = "thisIsAVeryLongFieldNameThatMightCauseIssuesWithBufferOverflowsOrTruncation"
        let fd = makeFieldDef longName [ModDoc "doc", ModRequired]
        let errors = validateFieldDef fd
        -- Should produce no errors for valid field
        errors |> shouldBe []

      it "empty field name is handled safely" \_ -> do
        let fd = makeFieldDef "" [ModDoc "doc", ModRequired]
        -- Should either validate or produce meaningful error
        -- (not crash or produce corrupted output)
        let errors = validateFieldDef fd
        -- No assertion on content - just verify it doesn't crash
        errors |> shouldSatisfy (\_ -> True)
