module Service.Integration.AdapterSpec where

import Core
import Data.List qualified as GhcList
import Data.Typeable (typeRep, Proxy (..))
import Service.Integration.Adapter (Integration (..), Response)
import Service.Integration.IntegrationError (IntegrationError (..))
import Service.Integration.TestFixtures
import Task qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Service.Integration.Adapter" do
    it "resolves Response SendEmail to SendEmailResponse via the type family" \_ -> do
      let repResp = typeRep (Proxy @(Response SendEmail))
      let repDirect = typeRep (Proxy @SendEmailResponse)
      repResp |> shouldBe repDirect

    it "default runFake produces a parseable SendEmailResponse" \_ -> do
      let req = mkSendEmail "alice@example.com" "template-001"
      result <- runFake req |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected Ok, got Err: #{toText err}|]
        Ok _ -> pass

    it "overridden runFake on ChargeIntent returns the scripted captured branch" \_ -> do
      let req = mkChargeIntent "intent-42" 1000
      result <- runFake req |> Task.asResult
      result |> shouldBe (Ok (ChargeIntentResponse {status = "captured"}))

    it "default runFake handles an empty Array (Text,Text) in the request" \_ -> do
      let req = SendEmail { to = "bob@example.com", templateId = "t1", variables = [] }
      result <- runFake req |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected Ok, got Err: #{toText err}|]
        Ok _ -> pass

    it "default runFake handles a single-element variables collection" \_ -> do
      let req = SendEmail { to = "carol@example.com", templateId = "t2", variables = [("key", "val")] }
      result <- runFake req |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected Ok, got Err: #{toText err}|]
        Ok _ -> pass

    it "default runFake handles a multi-element variables collection of 1024 pairs" \_ -> do
      let pairs = GhcList.replicate 1024 ("k", "v")
      let req = SendEmail { to = "dave@example.com", templateId = "t3", variables = pairs }
      result <- runFake req |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected Ok, got Err: #{toText err}|]
        Ok _ -> pass

    it "default runFake handles Unicode / multi-byte to field" \_ -> do
      let req = mkSendEmail "用户@例子.广告" "tmpl"
      result <- runFake req |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected Ok, got Err: #{toText err}|]
        Ok _ -> pass

    it "default runFake handles an empty to Text" \_ -> do
      let req = mkSendEmail "" "tmpl"
      result <- runFake req |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected Ok, got Err: #{toText err}|]
        Ok _ -> pass

    it "handles Text.repeat 10000 a in templateId" \_ -> do
      let longTmpl = Text.repeat 10000 "a"
      let req = mkSendEmail "e@e.com" longTmpl
      result <- runFake req |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected Ok, got Err: #{toText err}|]
        Ok _ -> pass

    it "IntegrationError Show instance prints all five constructors with their payloads" \_ -> do
      toText (TransportFailure "reset") |> Text.contains "TransportFailure" |> shouldBe True
      toText AuthenticationFailure |> Text.contains "AuthenticationFailure" |> shouldBe True
      toText (PermanentFailure "rejected") |> Text.contains "PermanentFailure" |> shouldBe True
      toText (TransientFailure "503") |> Text.contains "TransientFailure" |> shouldBe True
      toText (ValidationFailure "missing") |> Text.contains "ValidationFailure" |> shouldBe True
      toText (TransportFailure "reset") |> Text.contains "reset" |> shouldBe True

    it "IntegrationError Eq instance treats AuthenticationFailure as singleton" \_ -> do
      (AuthenticationFailure == AuthenticationFailure) |> shouldBe True
      (AuthenticationFailure != TransportFailure "") |> shouldBe True

    it "every IntegrationError constructor is reachable (exhaustiveness check)" \_ -> do
      let variants :: [IntegrationError]
          variants =
            [ TransportFailure "a",
              AuthenticationFailure,
              PermanentFailure "b",
              TransientFailure "c",
              ValidationFailure "d"
            ]
      let count =
            GhcList.foldl'
              ( \acc variant -> case variant of
                  TransportFailure _ -> acc + 1
                  AuthenticationFailure -> acc + 1
                  PermanentFailure _ -> acc + 1
                  TransientFailure _ -> acc + 1
                  ValidationFailure _ -> acc + 1
              )
              (0 :: Int)
              variants
      count |> shouldBe 5

    it "override of runFake does not force Arbitrary (Response r) constraint on the instance" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/13 (requires test fixture module)"

    it "defining an instance without Arbitrary (Response r) and without an override is rejected at compile time" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/16 (requires -fdefer-type-errors submodule)"

    it "Response type family rejects a mismatched response on an override" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/17 (requires -fdefer-type-errors submodule)"

    it "default runFake invocation is non-blocking (returns within 2 s)" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/18 (requires test fixture module)"
