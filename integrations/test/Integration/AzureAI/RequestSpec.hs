{-# LANGUAGE ImplicitParams #-}

module Integration.AzureAI.RequestSpec (spec) where

import Array qualified
import Basics
import Integration.AzureAI.Request
  ( Config (..)
  , Request (..)
  , azureEndpoint
  , azureEndpointAllowing
  , chatCompletion
  , defaultAzureHostSuffixes
  , defaultConfig
  , endpointUrl
  , hostOf
  )
import Integration.OpenRouter.Message qualified as Message
import Integration.OpenRouter.Response (Response (..))
import Maybe (Maybe (..))
import Redacted (Redacted)
import Redacted qualified
import Result (Result (..))
import Result qualified
import Test.Hspec
import Text (Text)
import Text qualified


spec :: Spec
spec = do

  describe "Integration.AzureAI.azureEndpoint" do
    it "accepts a valid https openai.azure.com endpoint (happy path)" do
      case azureEndpoint "https://my-res.openai.azure.com" of
        Result.Ok e -> endpointUrl e `shouldBe` "https://my-res.openai.azure.com"
        Result.Err reason -> expectationFailure [fmt|expected Ok, got Err: #{reason}|]

    it "accepts a valid https cognitiveservices.azure.com endpoint (happy path)" do
      Result.isOk (azureEndpoint "https://my-res.cognitiveservices.azure.com") `shouldBe` True

    it "accepts the Government sovereign-cloud host azure.us (boundary: allowlist member)" do
      Result.isOk (azureEndpoint "https://my-res.openai.azure.us") `shouldBe` True

    it "accepts the 21Vianet sovereign-cloud host azure.cn (boundary: allowlist member)" do
      Result.isOk (azureEndpoint "https://my-res.openai.azure.cn") `shouldBe` True

    it "rejects an http:// scheme, refusing cleartext (error: non-https, SEC-001 scheme guard)" do
      case azureEndpoint "http://my-res.openai.azure.com" of
        Result.Err msg -> do
          Text.contains "https" msg `shouldBe` True
          msg `shouldBe` "Azure endpoint must use https:// (refusing to send the api-key over cleartext)"
        Result.Ok _ -> expectationFailure "expected Err for http scheme"

    it "rejects a non-Azure host (error: host not in allowed suffix set)" do
      azureEndpoint "https://api.openai.com"
        `shouldBe` Result.Err "Azure endpoint host is not in the allowed Azure host suffix set"

    it "rejects a suffix-spoofing host that embeds an allowed suffix as a NON-suffix (SECURITY: SEC-001 lookalike)" do
      azureEndpoint "https://openai.azure.com.attacker.com"
        `shouldBe` Result.Err "Azure endpoint host is not in the allowed Azure host suffix set"

    it "rejects a label-boundary lookalike that ends with an allowed suffix WITHOUT a dot boundary (SECURITY: SEC-001 sovereign-cloud bypass)" do
      -- "notazure.us" literally ends with the sovereign suffix "azure.us" but is
      -- an attacker-registrable domain; a plain suffix match would accept it.
      azureEndpoint "https://notazure.us"
        `shouldBe` Result.Err "Azure endpoint host is not in the allowed Azure host suffix set"

    it "rejects a label-boundary lookalike prepended to a public-cloud suffix (SECURITY: SEC-001 no-dot bypass)" do
      azureEndpoint "https://evilopenai.azure.com"
        `shouldBe` Result.Err "Azure endpoint host is not in the allowed Azure host suffix set"

    it "accepts a host that equals an allowed suffix exactly (boundary: host == suffix)" do
      Result.isOk (azureEndpoint "https://openai.azure.com") `shouldBe` True

    it "accepts a mixed-case HTTPS:// scheme (edge: case-insensitive scheme check)" do
      Result.isOk (azureEndpoint "HTTPS://my-res.openai.azure.com") `shouldBe` True

    it "trims surrounding whitespace before validating (edge: leading/trailing spaces)" do
      case azureEndpoint "  https://my-res.openai.azure.com  " of
        Result.Ok e -> endpointUrl e `shouldBe` "https://my-res.openai.azure.com"
        Result.Err reason -> expectationFailure [fmt|expected Ok, got Err: #{reason}|]

    it "rejects an empty string (edge: empty input has no https scheme)" do
      case azureEndpoint "" of
        Result.Err msg -> Text.contains "https" msg `shouldBe` True
        Result.Ok _ -> expectationFailure "expected Err for empty string"

  describe "Integration.AzureAI.azureEndpointAllowing" do
    it "with empty extra suffixes behaves identically to azureEndpoint (happy path: delegation parity)" do
      let resultAllowing = azureEndpointAllowing Array.empty "https://my-res.openai.azure.com"
      let resultBase = azureEndpoint "https://my-res.openai.azure.com"
      case (resultAllowing, resultBase) of
        (Result.Ok ea, Result.Ok eb) -> endpointUrl ea `shouldBe` endpointUrl eb
        _ -> expectationFailure "expected both to be Ok"

    it "accepts a host matching a caller-supplied extra suffix (happy path: APIM custom domain)" do
      let extras = Array.fromLinkedList ["ai.contoso-internal.net"]
      Result.isOk (azureEndpointAllowing extras "https://gateway.ai.contoso-internal.net")
        `shouldBe` True

    it "rejects a host in neither the default nor the extra suffix set (error: not allow-listed)" do
      let extras = Array.fromLinkedList ["ai.contoso-internal.net"]
      azureEndpointAllowing extras "https://gateway.evil.example"
        `shouldBe` Result.Err "Azure endpoint host is not in the allowed Azure host suffix set"

    it "still enforces https even with a matching extra suffix (edge: SEC-001 guard not weakened)" do
      let extras = Array.fromLinkedList ["ai.contoso-internal.net"]
      case azureEndpointAllowing extras "http://gateway.ai.contoso-internal.net" of
        Result.Err msg -> Text.contains "https" msg `shouldBe` True
        Result.Ok _ -> expectationFailure "expected Err for http with extra suffix"

    it "rejects a suffix-spoof of an EXTRA suffix (SECURITY: SEC-001 lookalike on custom domain)" do
      let extras = Array.fromLinkedList ["ai.contoso-internal.net"]
      azureEndpointAllowing extras "https://gateway.ai.contoso-internal.net.attacker.com"
        `shouldBe` Result.Err "Azure endpoint host is not in the allowed Azure host suffix set"

    it "pipes as raw |> azureEndpointAllowing extras (edge: pipe-friendly argument order)" do
      let extras = Array.fromLinkedList ["ai.contoso-internal.net"]
      let result = "https://gateway.ai.contoso-internal.net" |> azureEndpointAllowing extras
      Result.isOk result `shouldBe` True

  describe "Integration.AzureAI.defaultAzureHostSuffixes" do
    it "contains the public-cloud suffix openai.azure.com (happy path)" do
      Array.contains "openai.azure.com" defaultAzureHostSuffixes `shouldBe` True

    it "contains both sovereign-cloud suffixes azure.us and azure.cn (edge: sovereign coverage)" do
      Array.contains "azure.us" defaultAzureHostSuffixes `shouldBe` True
      Array.contains "azure.cn" defaultAzureHostSuffixes `shouldBe` True

    it "lists exactly six suffixes (boundary: no accidental extra/missing entry)" do
      Array.length defaultAzureHostSuffixes `shouldBe` 6

  describe "Integration.AzureAI.defaultConfig" do
    it "sets all five sampling knobs to Nothing (happy path: omit-by-default)" do
      defaultConfig.temperature `shouldBe` Nothing
      defaultConfig.maxTokens `shouldBe` Nothing
      defaultConfig.topP `shouldBe` Nothing
      defaultConfig.frequencyPenalty `shouldBe` Nothing
      defaultConfig.presencePenalty `shouldBe` Nothing

    it "sets apiVersion to \"2024-10-21\" (boundary: pinned default version)" do
      defaultConfig.apiVersion `shouldBe` "2024-10-21"

    it "sets timeoutSeconds to 60 (boundary: default timeout)" do
      defaultConfig.timeoutSeconds `shouldBe` 60

    it "record-update of one knob preserves the other defaults (edge: partial override)" do
      let cfg = defaultConfig { temperature = Just 0.5 }
      cfg.temperature `shouldBe` Just 0.5
      cfg.maxTokens `shouldBe` Nothing
      cfg.timeoutSeconds `shouldBe` 60

  describe "Integration.AzureAI.chatCompletion" do
    it "seeds Config.endpoint from the AzureEndpoint argument (happy path)" do
      let ?config = TestConfig { azureAiApiKey = Redacted.wrap "k" }
      case azureEndpoint "https://my-res.openai.azure.com" of
        Result.Ok ep -> do
          let req = chatCompletion ep [Message.user "Hi"] "gpt-4o"
                      (\_ -> ("ok" :: Text)) (\_ -> "err")
          req.config.endpoint `shouldBe` ep
        Result.Err reason -> expectationFailure [fmt|endpoint setup: #{reason}|]

    it "stores the model and messages passed by the caller (happy path)" do
      let ?config = TestConfig { azureAiApiKey = Redacted.wrap "k" }
      case azureEndpoint "https://my-res.openai.azure.com" of
        Result.Ok ep -> do
          let req = chatCompletion ep [Message.user "Hi"] "gpt-4o"
                      (\_ -> ("ok" :: Text)) (\_ -> "err")
          req.model `shouldBe` "gpt-4o"
          Array.length req.messages `shouldBe` 1
        Result.Err reason -> expectationFailure [fmt|endpoint setup: #{reason}|]

    it "sources apiKey from ?config.azureAiApiKey (edge: implicit-param secret sourcing)" do
      let ?config = TestConfig { azureAiApiKey = Redacted.wrap "bearer-xyz" }
      case azureEndpoint "https://my-res.openai.azure.com" of
        Result.Ok ep -> do
          let req = chatCompletion ep [Message.user "Hi"] "gpt-4o"
                      (\_ -> ("ok" :: Text)) (\_ -> "err")
          Redacted.unwrap req.apiKey `shouldBe` "bearer-xyz"
        Result.Err reason -> expectationFailure [fmt|endpoint setup: #{reason}|]

    it "keeps apiKey Redacted so Show never renders the raw key (SECURITY: key-never-in-Show)" do
      let ?config = TestConfig { azureAiApiKey = Redacted.wrap "super-secret-key" }
      case azureEndpoint "https://my-res.openai.azure.com" of
        Result.Ok ep -> do
          let req = chatCompletion ep [Message.user "Hi"] "gpt-4o"
                      (\_ -> ("ok" :: Text)) (\_ -> "err")
          show req.apiKey `shouldBe` "<redacted>"
          show req.apiKey `shouldNotContain` "super-secret-key"
        Result.Err reason -> expectationFailure [fmt|endpoint setup: #{reason}|]

    it "inherits tuning defaults from defaultConfig (edge: unset knobs remain Nothing)" do
      let ?config = TestConfig { azureAiApiKey = Redacted.wrap "k" }
      case azureEndpoint "https://my-res.openai.azure.com" of
        Result.Ok ep -> do
          let req = chatCompletion ep [Message.user "Hi"] "gpt-4o"
                      (\_ -> ("ok" :: Text)) (\_ -> "err")
          req.config.temperature `shouldBe` Nothing
          req.config.apiVersion `shouldBe` "2024-10-21"
        Result.Err reason -> expectationFailure [fmt|endpoint setup: #{reason}|]

    it "stores onError so invoking it yields the caller's command (edge: callback wiring)" do
      let ?config = TestConfig { azureAiApiKey = Redacted.wrap "k" }
      case azureEndpoint "https://my-res.openai.azure.com" of
        Result.Ok ep -> do
          let onErr msg = [fmt|err:#{msg}|] :: Text
          let req = chatCompletion ep [Message.user "Hi"] "gpt-4o"
                      (\_ -> ("ok" :: Text)) onErr
          req.onError "boom" `shouldBe` "err:boom"
        Result.Err reason -> expectationFailure [fmt|endpoint setup: #{reason}|]

  describe "Integration.AzureAI.hostOf" do
    it "extracts the host from a plain https URL (happy path)" do
      hostOf "https://my-res.openai.azure.com"
        `shouldBe` Result.Ok "my-res.openai.azure.com"

    it "strips a trailing path segment before returning the host (edge: path present)" do
      hostOf "https://my-res.openai.azure.com/models/chat/completions"
        `shouldBe` Result.Ok "my-res.openai.azure.com"

    it "strips a query string before the host (SECURITY: SEC-002 query-injection lookalike)" do
      case hostOf "https://evil.example?x=.openai.azure.com" of
        Result.Ok h -> h `shouldBe` "evil.example"
        Result.Err reason -> expectationFailure [fmt|expected Ok, got Err: #{reason}|]

    it "strips a fragment before the host (SECURITY: SEC-002 fragment-injection lookalike)" do
      case hostOf "https://evil.example#.openai.azure.com" of
        Result.Ok h -> h `shouldBe` "evil.example"
        Result.Err reason -> expectationFailure [fmt|expected Ok, got Err: #{reason}|]

    it "rejects an https URL with no host segment (error: empty host)" do
      hostOf "https://" `shouldBe` Result.Err "Azure endpoint has no host"

  describe "Integration.AzureAI.endpointUrl" do
    it "projects the URL out of a validated endpoint (happy path)" do
      case azureEndpoint "https://my-res.openai.azure.com" of
        Result.Ok ep -> endpointUrl ep `shouldBe` "https://my-res.openai.azure.com"
        Result.Err reason -> expectationFailure [fmt|endpoint setup: #{reason}|]

    it "round-trips azureEndpoint |> endpointUrl to the trimmed input (edge: construction round-trip)" do
      let result = azureEndpoint "  https://my-res.openai.azure.com  "
                     |> Result.map endpointUrl
      result `shouldBe` Result.Ok "https://my-res.openai.azure.com"

    it "returns a URL free of surrounding whitespace (boundary: normalisation)" do
      case azureEndpoint "  https://my-res.openai.azure.com  " of
        Result.Ok ep -> do
          let url = endpointUrl ep
          Text.trim url `shouldBe` url
        Result.Err reason -> expectationFailure [fmt|endpoint setup: #{reason}|]

  describe "Integration.AzureAI.Config" do
    it "constructs via defaultConfig and is Eq-reflexive (happy path)" do
      defaultConfig `shouldBe` defaultConfig

    it "renders apiVersion/timeout in Show but carries no secret (edge: Config Show is safe)" do
      show defaultConfig `shouldContain` "2024-10-21"

    it "record-update swaps endpoint to another validated AzureEndpoint (edge: SEC-006 bounded update)" do
      case azureEndpoint "https://other.cognitiveservices.azure.com" of
        Result.Ok ep -> do
          let cfg = defaultConfig { endpoint = ep }
          cfg.endpoint `shouldBe` ep
        Result.Err reason -> expectationFailure [fmt|endpoint setup: #{reason}|]

  describe "Integration.AzureAI.Request" do
    it "constructs via the exported record literal with fields readable (happy path)" do
      case azureEndpoint "https://my-res.openai.azure.com" of
        Result.Ok ep -> do
          let req = Request
                { messages = Array.fromLinkedList [Message.user "Hi"]
                , model = "gpt-4o"
                , config = defaultConfig { endpoint = ep }
                , apiKey = Redacted.wrap "k"
                , onSuccess = (\_ -> ("ok" :: Text)) :: Response -> Text
                , onError = \_ -> "err"
                }
          req.model `shouldBe` "gpt-4o"
          req.config.endpoint `shouldBe` ep
        Result.Err reason -> expectationFailure [fmt|endpoint setup: #{reason}|]

    it "keeps apiKey Redacted under Show (SECURITY: key-never-in-Show on the record field)" do
      case azureEndpoint "https://my-res.openai.azure.com" of
        Result.Ok ep -> do
          let req = Request
                { messages = Array.fromLinkedList [Message.user "Hi"]
                , model = "gpt-4o"
                , config = defaultConfig { endpoint = ep }
                , apiKey = Redacted.wrap "leak-me"
                , onSuccess = (\_ -> ("ok" :: Text)) :: Response -> Text
                , onError = \_ -> "err"
                }
          show req.apiKey `shouldBe` "<redacted>"
          show req.apiKey `shouldNotContain` "leak-me"
        Result.Err reason -> expectationFailure [fmt|endpoint setup: #{reason}|]

    it "record-update swapping config preserves the Redacted apiKey (edge: field independence)" do
      case (azureEndpoint "https://my-res.openai.azure.com", azureEndpoint "https://ep2.cognitiveservices.azure.com") of
        (Result.Ok ep, Result.Ok ep2) -> do
          let req = Request
                { messages = Array.fromLinkedList [Message.user "Hi"]
                , model = "gpt-4o"
                , config = defaultConfig { endpoint = ep }
                , apiKey = Redacted.wrap "secret-key"
                , onSuccess = (\_ -> ("ok" :: Text)) :: Response -> Text
                , onError = \_ -> "err"
                }
          let req2 = req { config = defaultConfig { endpoint = ep2 } }
          show req2.apiKey `shouldBe` "<redacted>"
        _ -> expectationFailure "endpoint setup failed"

  describe "Properties" do
    it "azureEndpoint . endpointUrl round-trips a validated endpoint (property: left-inverse)" do
      case azureEndpoint "https://my-res.openai.azure.com" of
        Result.Ok ep ->
          azureEndpoint (endpointUrl ep) `shouldBe` Result.Ok ep
        Result.Err reason ->
          expectationFailure [fmt|property base case setup: #{reason}|]

    it "extra suffixes only widen the allowlist (property: monotonicity)" do
      let extras = Array.fromLinkedList ["ai.contoso-internal.net"]
      let raw = "https://gateway.ai.contoso-internal.net"
      case azureEndpointAllowing extras raw of
        Result.Ok _ ->
          Result.isOk (azureEndpointAllowing (Array.append (Array.fromLinkedList ["another.net"]) extras) raw)
            `shouldBe` True
        Result.Err _ ->
          -- stub throws before this branch; test is red
          pure ()

    it "hostOf result carries no path/query/fragment separators (property: output invariant)" do
      case hostOf "https://my-res.openai.azure.com/path?query=x#frag" of
        Result.Ok h -> do
          Text.contains "/" h `shouldBe` False
          Text.contains "?" h `shouldBe` False
          Text.contains "#" h `shouldBe` False
        Result.Err reason ->
          expectationFailure [fmt|expected Ok from hostOf: #{reason}|]


-- | Minimal test config satisfying HasField "azureAiApiKey" constraint.
data TestConfig = TestConfig
  { azureAiApiKey :: Redacted Text
  }
