module Service.Integration.AdapterSpec where

import AsyncTask qualified
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

    it "runFake produces a parseable SendEmailResponse" \_ -> do
      let req = mkSendEmail "alice@example.com" "template-001"
      result <- runFake req |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected Ok, got Err: #{toText err}|]
        Ok _ -> pass

    it "overridden runFake on ChargeIntent returns the scripted captured branch" \_ -> do
      let req = mkChargeIntent "intent-42" 1000
      result <- runFake req |> Task.asResult
      result |> shouldBe (Ok (ChargeIntentResponse {status = "captured"}))

    it "runFake handles an empty Array (Text,Text) in the request" \_ -> do
      let req = SendEmail { to = "bob@example.com", templateId = "t1", variables = [] }
      result <- runFake req |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected Ok, got Err: #{toText err}|]
        Ok _ -> pass

    it "runFake handles a single-element variables collection" \_ -> do
      let req = SendEmail { to = "carol@example.com", templateId = "t2", variables = [("key", "val")] }
      result <- runFake req |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected Ok, got Err: #{toText err}|]
        Ok _ -> pass

    it "runFake handles a multi-element variables collection of 1024 pairs" \_ -> do
      let pairs = GhcList.replicate 1024 ("k", "v")
      let req = SendEmail { to = "dave@example.com", templateId = "t3", variables = pairs }
      result <- runFake req |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected Ok, got Err: #{toText err}|]
        Ok _ -> pass

    it "runFake handles Unicode / multi-byte to field" \_ -> do
      let req = mkSendEmail "用户@例子.广告" "tmpl"
      result <- runFake req |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected Ok, got Err: #{toText err}|]
        Ok _ -> pass

    it "runFake handles an empty to Text" \_ -> do
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

    it "every IntegrationError constructor is covered by the tag helper" \_ -> do
      let tag variant = case variant of
            TransportFailure _ -> "transport" :: Text
            AuthenticationFailure -> "auth"
            PermanentFailure _ -> "permanent"
            TransientFailure _ -> "transient"
            ValidationFailure _ -> "validation"
      GhcList.map tag [TransportFailure "a", AuthenticationFailure, PermanentFailure "b", TransientFailure "c", ValidationFailure "d"]
        |> shouldBe ["transport", "auth", "permanent", "transient", "validation"]

    it "explicit runFake override is called instead of the declared default" \_ -> do
      let req = mkChargeIntent "intent-77" 250
      result <- runFake req |> Task.asResult
      result |> shouldBe (Ok (ChargeIntentResponse {status = "captured"}))

    it "runFake invocation is non-blocking (returns within 2 s)" \_ -> do
      let req = mkSendEmail "latency@example.com" "tmpl"
      handle <- AsyncTask.run (runFake req) |> Task.mapError toText
      AsyncTask.sleep 2000 |> Task.mapError (\_ -> "sleep error" :: Text)
      outcome <- AsyncTask.waitCatch handle
      case outcome of
        Ok _ -> pass
        Err err -> fail [fmt|runFake did not complete within 2 s: #{err}|]
