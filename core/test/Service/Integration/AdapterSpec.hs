module Service.Integration.AdapterSpec where

import Core
import Test


spec :: Spec Unit
spec = do
  describe "Service.Integration.Adapter" do
    it "resolves Response SendEmail to SendEmailResponse via the type family" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/1"

    it "default runFake produces a parseable SendEmailResponse" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/2"

    it "overridden runFake on ChargeIntent returns the scripted captured branch" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/3"

    it "default runFake handles an empty Array (Text,Text) in the request" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/4"

    it "default runFake handles a single-element variables collection" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/5"

    it "default runFake handles a multi-element variables collection of 1024 pairs" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/6"

    it "default runFake handles Unicode / multi-byte to field" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/7"

    it "default runFake handles an empty to Text" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/8"

    it "handles Text.replicate 10000 a in templateId" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/9"

    it "IntegrationError Show instance prints all five constructors with their payloads" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/10"

    it "IntegrationError Eq instance treats AuthenticationFailure as singleton" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/11"

    it "every IntegrationError constructor is reachable (exhaustiveness check)" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/12"

    it "override of runFake does not force Arbitrary (Response r) constraint on the instance" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/13"

    it "InboundIntegration.runReal is typed correctly: receives callback and returns Task IntegrationError Void" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/14"

    it "InboundIntegration default runFake is Inbound.controllableHandle" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/15"

    it "defining an instance without Arbitrary (Response r) and without an override is rejected at compile time" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/16"

    it "Response type family rejects a mismatched response on an override" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/17"

    it "default runFake invocation is non-blocking (returns within 2 s)" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/18"
