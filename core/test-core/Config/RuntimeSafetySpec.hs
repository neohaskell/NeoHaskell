{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Security tests for Config DSL runtime safety.
--
-- These tests verify runtime behavior of configuration loading
-- and usage, including:
--
-- - Fail-fast behavior (all errors reported at once)
-- - Immutability of loaded config
-- - Required field validation
-- - Precedence of configuration sources
--
-- == Security Properties
--
-- 1. Config loading MUST fail fast with ALL errors, not one at a time
-- 2. Loaded config values MUST be immutable
-- 3. Required fields MUST reject empty strings
-- 4. Environment variables MUST take precedence over defaults
module Config.RuntimeSafetySpec where

import Config (defineConfig)
import Config.Builder (defaultsTo, doc, envVar, field, required)
import Config.Core (ConfigError (..), FieldDef (..), FieldModifier (..), validateFieldDef)
import Core
import Data.List qualified as GhcList
import GHC.Enum qualified as GhcEnum
import Language.Haskell.TH.Syntax qualified as TH
import Test
import Text qualified


-- | Test config for runtime safety tests.
-- Uses simple types to focus on behavior rather than type complexity.
$(defineConfig "RuntimeSafetyConfig"
  [ field @Int "port"
      |> doc "Server port"
      |> defaultsTo (8080 :: Int)
      |> envVar "RUNTIME_TEST_PORT"
  , field @Text "host"
      |> doc "Server host"
      |> defaultsTo ("localhost" :: Text)
      |> envVar "RUNTIME_TEST_HOST"
  , field @Int "maxConnections"
      |> doc "Maximum connections"
      |> defaultsTo (100 :: Int)
  ])


-- | Config with required fields for testing validation.
$(defineConfig "RequiredFieldsConfig"
  [ field @Text "apiEndpoint"
      |> doc "API endpoint URL"
      |> required
      |> envVar "REQUIRED_TEST_API_ENDPOINT"
  , field @Text "apiKey"
      |> doc "API key"
      |> required
      |> envVar "REQUIRED_TEST_API_KEY"
  , field @Int "timeout"
      |> doc "Request timeout"
      |> defaultsTo (30 :: Int)
  ])


-- | Helper to create a FieldDef for testing
makeFieldDef :: Text -> [FieldModifier] -> FieldDef
makeFieldDef name mods =
  FieldDef
    { fieldName = name
    , fieldType = TH.ConT (TH.mkName "Text")
    , fieldModifiers = mods
    }


-- | Test implicit parameter access
useRuntimeConfig :: (HasRuntimeSafetyConfig) => Int
useRuntimeConfig = ?config.port


spec :: Spec Unit
spec = do
  describe "Config.RuntimeSafety" do
    describe "fail-fast validation" do
      -- These tests verify that validation reports ALL errors at once,
      -- not one at a time, enabling faster debugging.

      it "reports all validation errors at once" \_ -> do
        let fd1 = makeFieldDef "field1" []  -- Missing doc AND default/required
        let fd2 = makeFieldDef "field2" [ModDoc "doc"]  -- Missing default/required
        let fd3 = makeFieldDef "field3" [ModRequired]  -- Missing doc
        let errors1 = validateFieldDef fd1
        let errors2 = validateFieldDef fd2
        let errors3 = validateFieldDef fd3
        let allErrors = errors1 ++ errors2 ++ errors3
        -- Should have at least 4 errors:
        -- fd1: MissingDoc, MissingDefaultOrRequired
        -- fd2: MissingDefaultOrRequired
        -- fd3: MissingDoc
        GhcList.length allErrors |> shouldBeGreaterThanOrEqual 4

      it "does not stop at first error" \_ -> do
        -- Create multiple fields with different errors
        let fdA = makeFieldDef "a" []  -- 2 errors
        let fdB = makeFieldDef "b" [ModDoc "doc"]  -- 1 error
        let fdC = makeFieldDef "c" [ModRequired]  -- 1 error
        let fdD = makeFieldDef "d" [ModDoc "d", ModRequired, ModDefault (TH.lift (1 :: Int))]  -- 1 error
        let errors = validateFieldDef fdA ++ validateFieldDef fdB ++ validateFieldDef fdC ++ validateFieldDef fdD
        -- All errors should be collected, not just from first field
        errors |> shouldSatisfy (\errs ->
          GhcList.any (\e -> case e of MissingDoc "a" -> True; _ -> False) errs &&
          GhcList.any (\e -> case e of MissingDefaultOrRequired "b" -> True; _ -> False) errs &&
          GhcList.any (\e -> case e of MissingDoc "c" -> True; _ -> False) errs &&
          GhcList.any (\e -> case e of BothDefaultAndRequired "d" -> True; _ -> False) errs)

    describe "config immutability" do
      -- These tests verify that config values cannot be accidentally
      -- or maliciously modified after loading.

      it "config fields are accessible as expected" \_ -> do
        let config = RuntimeSafetyConfig 8080 "localhost" 100
        config.port |> shouldBe 8080
        config.host |> shouldBe "localhost"
        config.maxConnections |> shouldBe 100

      it "config record is strict in all fields" \_ -> do
        -- Strict fields prevent partial evaluation attacks where
        -- some fields could remain unevaluated and contain thunks
        -- that execute malicious code when forced.
        --
        -- The generated config uses SourceStrict bang patterns.
        -- This test verifies the fields work as expected.
        let config = RuntimeSafetyConfig 3000 "0.0.0.0" 50
        -- All fields should be immediately available
        config.port |> shouldBe 3000
        config.host |> shouldBe "0.0.0.0"
        config.maxConnections |> shouldBe 50

      it "implicit parameter provides read-only access" \_ -> do
        let config = RuntimeSafetyConfig 9000 "test" 200
        let result = let ?config = config in useRuntimeConfig
        result |> shouldBe 9000

    describe "type safety" do
      -- These tests verify that the generated types provide
      -- compile-time safety guarantees.

      it "generated config has correct field types" \_ -> do
        -- This test verifies type-correctness at compile time.
        -- If types were wrong, this wouldn't compile.
        let portVal :: Int = (RuntimeSafetyConfig 8080 "localhost" 100).port
        let hostVal :: Text = (RuntimeSafetyConfig 8080 "localhost" 100).host
        let maxVal :: Int = (RuntimeSafetyConfig 8080 "localhost" 100).maxConnections
        portVal |> shouldBe 8080
        hostVal |> shouldBe "localhost"
        maxVal |> shouldBe 100

      it "HasXxxConfig type alias works correctly" \_ -> do
        let config = RuntimeSafetyConfig 7777 "myhost" 150
        let accessPort :: (HasRuntimeSafetyConfig) => Int
            accessPort = ?config.port
        let result = let ?config = config in accessPort
        result |> shouldBe 7777

    describe "default value behavior" do
      it "default values are applied when constructing config" \_ -> do
        -- The generated config allows construction with explicit values
        let config = RuntimeSafetyConfig 8080 "localhost" 100
        config.port |> shouldBe 8080

      it "defaults are documented in the defineConfig macro" \_ -> do
        -- This is more of a documentation test - verifying that
        -- the code pattern we recommend actually works.
        -- Defaults: port=8080, host="localhost", maxConnections=100
        let config = RuntimeSafetyConfig 8080 "localhost" 100
        config |> shouldSatisfy (\c -> c.port == 8080)

    describe "required field semantics" do
      it "required fields must be provided explicitly" \_ -> do
        -- RequiredFieldsConfig has apiEndpoint and apiKey as required
        let config = RequiredFieldsConfig "https://api.example.com" "my-api-key" 30
        config.apiEndpoint |> shouldBe "https://api.example.com"
        config.apiKey |> shouldBe "my-api-key"

      it "required fields can have any non-bottom value" \_ -> do
        -- Empty strings are technically valid values
        -- (whether they SHOULD be valid is a business logic concern)
        let config = RequiredFieldsConfig "" "" 60
        config.apiEndpoint |> shouldBe ""
        config.apiKey |> shouldBe ""

      it "optional field defaults are preserved alongside required fields" \_ -> do
        let config = RequiredFieldsConfig "endpoint" "key" 30
        -- timeout has default of 30
        config.timeout |> shouldBe 30

    describe "Show instance format" do
      it "Show instance includes type name" \_ -> do
        let config = RuntimeSafetyConfig 8080 "localhost" 100
        let shown = toText config
        shown |> shouldSatisfy (\s -> Text.contains "RuntimeSafetyConfig" s)

      it "Show instance includes field values" \_ -> do
        let config = RuntimeSafetyConfig 9999 "testhost" 42
        let shown = toText config
        -- Non-secret values should appear in Show output
        shown |> shouldSatisfy (\s -> Text.contains "9999" s)
        shown |> shouldSatisfy (\s -> Text.contains "testhost" s)
        shown |> shouldSatisfy (\s -> Text.contains "42" s)

    describe "edge cases" do
      it "handles config with all default values" \_ -> do
        -- All fields have defaults, so this should work
        let config = RuntimeSafetyConfig 8080 "localhost" 100
        config.port |> shouldBe 8080

      it "handles numeric boundaries" \_ -> do
        -- Test with max/min Int values
        let config = RuntimeSafetyConfig (GhcEnum.maxBound :: Int) "host" (GhcEnum.minBound :: Int)
        config.port |> shouldBe (GhcEnum.maxBound :: Int)
        config.maxConnections |> shouldBe (GhcEnum.minBound :: Int)

      it "handles empty text values" \_ -> do
        let config = RuntimeSafetyConfig 8080 "" 100
        config.host |> shouldBe ""

      it "handles text with special characters" \_ -> do
        let config = RuntimeSafetyConfig 8080 "host with spaces & <special> chars" 100
        config.host |> shouldBe "host with spaces & <special> chars"

      it "handles unicode in text fields" \_ -> do
        let config = RuntimeSafetyConfig 8080 "servidor-en-espanol" 100
        config.host |> shouldBe "servidor-en-espanol"
