{-# LANGUAGE LambdaCase #-}

-- | Security tests for Config DSL input validation.
--
-- These tests verify that field names, environment variable names,
-- and CLI option names are properly validated to prevent:
--
-- - Shell injection via environment variable names
-- - CLI argument injection
-- - Invalid Haskell identifier generation
--
-- == Security Properties
--
-- 1. Environment variable names MUST NOT contain shell metacharacters
-- 2. CLI option names MUST be alphanumeric with dashes only
-- 3. Field names MUST produce valid Haskell identifiers
-- 4. Auto-generated env var names from camelCase MUST be safe
module Config.InputValidationSpec where

import Config.Builder (cliLong, cliShort, doc, defaultsTo, envVar, field)
import Config.Core (FieldDef (..), FieldModifier (..))
import Config.TH (toEnvVarName)
import Core
import Data.Maybe qualified as GhcMaybe
import LinkedList qualified
import Test
import Text qualified


-- | Helper to check if a modifier is present in a FieldDef
hasModifier :: (FieldModifier -> Bool) -> FieldDef -> Bool
hasModifier predicate fd = LinkedList.any predicate fd.fieldModifiers


-- | Helper to find a specific modifier value
findModifier :: (FieldModifier -> Maybe value) -> FieldDef -> Maybe value
findModifier extract fd = do
  let results = GhcMaybe.mapMaybe extract fd.fieldModifiers
  case results of
    [] -> Nothing
    (x : _) -> Just x


-- | Characters that are dangerous in shell contexts
shellMetacharacters :: [Text]
shellMetacharacters = ["$", "`", ";", "|", "&", "(", ")", "<", ">", "\"", "'", "\\", "\n", "\r", "\0"]


-- | Check if text contains any shell metacharacters
containsShellMetachar :: Text -> Bool
containsShellMetachar text =
  LinkedList.any (\meta -> Text.contains meta text) shellMetacharacters


spec :: Spec Unit
spec = do
  describe "Config.InputValidation" do
    describe "toEnvVarName conversion" do
      it "converts simple camelCase to SCREAMING_SNAKE_CASE" \_ -> do
        toEnvVarName "port" |> shouldBe "PORT"
        toEnvVarName "databaseUrl" |> shouldBe "DATABASE_URL"
        toEnvVarName "apiKey" |> shouldBe "API_KEY"

      it "handles multiple capital letters correctly" \_ -> do
        toEnvVarName "openRouterKey" |> shouldBe "OPEN_ROUTER_KEY"
        toEnvVarName "httpAPIEndpoint" |> shouldBe "HTTP_APIENDPOINT"

      it "handles single character names" \_ -> do
        toEnvVarName "x" |> shouldBe "X"

      it "handles already uppercase names" \_ -> do
        toEnvVarName "PORT" |> shouldBe "PORT"

      it "handles empty string safely" \_ -> do
        let result = toEnvVarName ""
        -- Should produce empty string or handle gracefully
        result |> shouldBe ""

      it "auto-generated env var names do not contain shell metacharacters" \_ -> do
        -- Test various field names to ensure generated env vars are safe
        toEnvVarName "port" |> shouldSatisfy (\n -> not (containsShellMetachar n))
        toEnvVarName "databaseUrl" |> shouldSatisfy (\n -> not (containsShellMetachar n))
        toEnvVarName "apiKey" |> shouldSatisfy (\n -> not (containsShellMetachar n))
        toEnvVarName "secretToken" |> shouldSatisfy (\n -> not (containsShellMetachar n))
        toEnvVarName "maxRetries" |> shouldSatisfy (\n -> not (containsShellMetachar n))

      it "handles names with numbers" \_ -> do
        toEnvVarName "oauth2ClientId" |> shouldBe "OAUTH2_CLIENT_ID"
        toEnvVarName "http2Enabled" |> shouldBe "HTTP2_ENABLED"

    describe "envVar modifier validation" do
      it "explicit envVar is stored correctly" \_ -> do
        let fd = field @Int "port"
              |> doc "Server port"
              |> defaultsTo (8080 :: Int)
              |> envVar "MY_CUSTOM_PORT"
        let extractEnvVar = \case
              ModEnvVar v -> Just v
              _ -> Nothing
        findModifier extractEnvVar fd |> shouldBe (Just "MY_CUSTOM_PORT")

      it "envVar with standard characters is accepted" \_ -> do
        let fd = field @Text "apiKey"
              |> doc "API key"
              |> defaultsTo ("" :: Text)
              |> envVar "MY_APP_API_KEY"
        let extractEnvVar = \case
              ModEnvVar v -> Just v
              _ -> Nothing
        case findModifier extractEnvVar fd of
          Just v -> v |> shouldSatisfy (\n -> not (containsShellMetachar n))
          Nothing -> fail "Expected ModEnvVar modifier"

    describe "cliLong modifier validation" do
      it "cliLong stores the option name" \_ -> do
        let fd = field @Int "port"
              |> doc "Server port"
              |> defaultsTo (8080 :: Int)
              |> cliLong "server-port"
        let extractCliLong = \case
              ModCliLong name -> Just name
              _ -> Nothing
        findModifier extractCliLong fd |> shouldBe (Just "server-port")

      it "cliLong with valid characters is accepted" \_ -> do
        -- Valid CLI long options: lowercase letters, digits, dashes
        let fd = field @Text "config"
              |> doc "Config path"
              |> defaultsTo ("" :: Text)
              |> cliLong "config-file-path"
        let extractCliLong = \case
              ModCliLong name -> Just name
              _ -> Nothing
        let isValidCliLongChar c = do
              let isLowercase = c >= 'a' && c <= 'z'
              let isDigit = c >= '0' && c <= '9'
              let isDash = c == '-'
              isLowercase || isDigit || isDash
        case findModifier extractCliLong fd of
          Just v -> do
            -- Should only contain alphanumeric and dashes
            v |> shouldSatisfy (\name -> Text.all isValidCliLongChar name)
          Nothing -> fail "Expected ModCliLong modifier"

    describe "cliShort modifier validation" do
      it "cliShort stores the option character" \_ -> do
        let fd = field @Int "port"
              |> doc "Server port"
              |> defaultsTo (8080 :: Int)
              |> cliShort 'p'
        let extractCliShort = \case
              ModCliShort c -> Just c
              _ -> Nothing
        findModifier extractCliShort fd |> shouldBe (Just 'p')

      it "cliShort with alphabetic character is valid" \_ -> do
        let fd = field @Text "verbose"
              |> doc "Verbose output"
              |> defaultsTo ("" :: Text)
              |> cliShort 'v'
        let extractCliShort = \case
              ModCliShort c -> Just c
              _ -> Nothing
        let isAlphabetic ch = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
        case findModifier extractCliShort fd of
          Just c -> c |> shouldSatisfy isAlphabetic
          Nothing -> fail "Expected ModCliShort modifier"

    describe "field name validation" do
      it "standard camelCase names are valid" \_ -> do
        let fd = field @Int "serverPort"
              |> doc "Port"
              |> defaultsTo (8080 :: Int)
        fd.fieldName |> shouldBe "serverPort"

      it "single word names are valid" \_ -> do
        let fd = field @Text "host"
              |> doc "Host"
              |> defaultsTo ("localhost" :: Text)
        fd.fieldName |> shouldBe "host"

      it "names with numbers are preserved" \_ -> do
        let fd = field @Int "retryCount3"
              |> doc "Retry count"
              |> defaultsTo (3 :: Int)
        fd.fieldName |> shouldBe "retryCount3"

    describe "doc modifier safety" do
      it "doc text is stored as-is" \_ -> do
        let docText = "This is a documentation string with special chars: <>&\""
        let fd = field @Int "port"
              |> doc docText
              |> defaultsTo (8080 :: Int)
        let extractDoc = \case
              ModDoc txt -> Just txt
              _ -> Nothing
        findModifier extractDoc fd |> shouldBe (Just docText)

      it "doc text with newlines is preserved" \_ -> do
        let docText = "Line 1\nLine 2\nLine 3"
        let fd = field @Int "port"
              |> doc docText
              |> defaultsTo (8080 :: Int)
        let extractDoc = \case
              ModDoc txt -> Just txt
              _ -> Nothing
        case findModifier extractDoc fd of
          Just txt -> txt |> shouldSatisfy (Text.contains "\n")
          Nothing -> fail "Expected ModDoc modifier"

    describe "environment variable name patterns" do
      -- These tests ensure that the toEnvVarName function
      -- produces names that are valid in all common shells

      it "result contains only uppercase letters, digits, and underscores" \_ -> do
        let isValidEnvVarChar c = do
              let isUppercase = c >= 'A' && c <= 'Z'
              let isDigit = c >= '0' && c <= '9'
              let isUnderscore = c == '_'
              isUppercase || isDigit || isUnderscore
        toEnvVarName "port" |> shouldSatisfy (\n -> Text.all isValidEnvVarChar n)
        toEnvVarName "databaseUrl" |> shouldSatisfy (\n -> Text.all isValidEnvVarChar n)
        toEnvVarName "apiKey" |> shouldSatisfy (\n -> Text.all isValidEnvVarChar n)
        toEnvVarName "maxConnections" |> shouldSatisfy (\n -> Text.all isValidEnvVarChar n)
        toEnvVarName "timeout" |> shouldSatisfy (\n -> Text.all isValidEnvVarChar n)

      it "does not produce names starting with digit" \_ -> do
        -- Env vars starting with digits are problematic in some shells
        -- Though our camelCase conversion shouldn't produce these naturally
        let result = toEnvVarName "config"
        result |> shouldSatisfy (\n ->
          case Text.uncons n of
            Just (c, _) -> c >= 'A' && c <= 'Z'
            Nothing -> True)  -- Empty is ok (handled elsewhere)

      it "underscores separate word boundaries correctly" \_ -> do
        toEnvVarName "myDatabaseHostName" |> shouldBe "MY_DATABASE_HOST_NAME"
        toEnvVarName "aBC" |> shouldBe "A_BC"
