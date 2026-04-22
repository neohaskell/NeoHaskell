module Service.Integration.AdapterSpec where

import Core
import Data.List qualified as GhcList
import Service.Integration.IntegrationError (IntegrationError (..))
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Service.Integration.Adapter" do
    it "resolves Response SendEmail to SendEmailResponse via the type family" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/1 (requires test fixtures TestFixtures/SendEmail.hs — deferred)"

    it "default runFake produces a parseable SendEmailResponse" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/2 (requires test fixture module)"

    it "overridden runFake on ChargeIntent returns the scripted captured branch" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/3 (requires test fixture module)"

    it "default runFake handles an empty Array (Text,Text) in the request" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/4 (requires test fixture module)"

    it "default runFake handles a single-element variables collection" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/5 (requires test fixture module)"

    it "default runFake handles a multi-element variables collection of 1024 pairs" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/6 (requires test fixture module)"

    it "default runFake handles Unicode / multi-byte to field" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/7 (requires test fixture module)"

    it "default runFake handles an empty to Text" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/8 (requires test fixture module)"

    it "handles Text.replicate 10000 a in templateId" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/9 (requires test fixture module)"

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

    it "InboundIntegration.runRealInbound is typed correctly" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/14 (requires test fixture module)"

    it "InboundIntegration default runFakeInbound is Inbound.controllableHandle" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/15 (requires test fixture module)"

    it "defining an instance without Arbitrary (Response r) and without an override is rejected at compile time" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/16 (requires -fdefer-type-errors submodule)"

    it "Response type family rejects a mismatched response on an override" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/17 (requires -fdefer-type-errors submodule)"

    it "default runFake invocation is non-blocking (returns within 2 s)" \_ ->
      pending "Phase 8 — ADR-0055 AdapterSpec/18 (requires test fixture module)"
