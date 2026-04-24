-- | End-to-end wire-up spec for ADR-0055 integrations.
--
-- Covers the path @Application.withIntegration@ → @buildIntegrationContext@
-- (via @IntegrationRegistrationEntry.registerInto@) → @DispatchRegistry@ →
-- @ShimEmit.emit@ for real, fake, hybrid, and unregistered cases.
module Service.Integration.WireupSpec where

import Array qualified
import Core
import Service.Application qualified as Application
import Service.Integration.DispatchRegistry (DispatchRegistry)
import Service.Integration.DispatchRegistry qualified as DispatchRegistry
import Service.Integration.IntegrationError (IntegrationError (..))
import Service.Integration.Selection (Selection (..))
import Service.Integration.ShimEmit qualified as ShimEmit
import Service.Integration.TestFixtures
import Task qualified
import Test
import Text qualified


-- | Mirrors Application.buildIntegrationContext's registry-building step
-- without parsing argv, so tests can drive it with a fixed Selection.
buildRegistry :: Application.Application -> Selection -> DispatchRegistry
buildRegistry app selection =
  app.integrationRegistrations
    |> Array.reduce
        (\entry reg -> entry.registerInto selection reg)
        DispatchRegistry.empty


spec :: Spec Unit
spec = do
  describe "Service.Integration wire-up (ADR-0055)" do
    describe "withIntegration + DispatchRegistry" do
      it "registers each declared integration exactly once" \_ -> do
        let app =
              Application.new
                |> Application.withIntegration @SendEmail
                |> Application.withIntegration @ChargeIntent
        let registry = buildRegistry app Real
        DispatchRegistry.size registry |> shouldBe 2

      it "ShimEmit.emit routes SendEmail through runReal in real mode" \_ -> do
        let app =
              Application.new
                |> Application.withIntegration @SendEmail
        let registry = buildRegistry app Real
        let req = mkSendEmail "alice@example.com" "template-001"
        result <- ShimEmit.emit registry req |> Task.asResult
        case result of
          Err (TransientFailure "not implemented in tests") -> pass
          Err other -> fail [fmt|Expected TransientFailure from real fixture, got: #{toText other}|]
          Ok _ -> fail "Expected runReal fixture to throw, but got Ok"

      it "ShimEmit.emit routes SendEmail through runFake in fake mode" \_ -> do
        let app =
              Application.new
                |> Application.withIntegration @SendEmail
        let registry = buildRegistry app Fake
        let req = mkSendEmail "alice@example.com" "template-001"
        result <- ShimEmit.emit registry req |> Task.asResult
        case result of
          Ok _ -> pass
          Err err -> fail [fmt|Expected runFake success, got: #{toText err}|]

      it "ShimEmit.emit honours hybrid mode — named integration fakes, others go real" \_ -> do
        let app =
              Application.new
                |> Application.withIntegration @SendEmail
                |> Application.withIntegration @ChargeIntent
        let registry =
              buildRegistry app (Hybrid (Array.fromLinkedList ["ChargeIntent"]))
        -- ChargeIntent is named: should run fake (returns "captured")
        chargeResult <-
          ShimEmit.emit registry (mkChargeIntent "intent-1" 100)
            |> Task.asResult
        chargeResult |> shouldBe (Ok (ChargeIntentResponse {status = "captured"}))
        -- SendEmail is NOT named: should run real (throws TransientFailure)
        emailResult <-
          ShimEmit.emit registry (mkSendEmail "e@e.com" "t")
            |> Task.asResult
        case emailResult of
          Err (TransientFailure _) -> pass
          _ -> fail "Expected SendEmail to run real in hybrid mode"

      it "ShimEmit.emit returns PermanentFailure for an unregistered integration" \_ -> do
        -- Registry contains ChargeIntent only; emit SendEmail to verify missing
        -- entries fail loudly per ADR-0055 §6 (no silent fallback to runReal).
        let app =
              Application.new
                |> Application.withIntegration @ChargeIntent
        let registry = buildRegistry app Real
        let req = mkSendEmail "missing@example.com" "t"
        result <- ShimEmit.emit registry req |> Task.asResult
        case result of
          Err (PermanentFailure msg) ->
            msg |> shouldSatisfy (\t -> Text.contains "Integration not registered" t)
          Err other -> fail [fmt|Expected PermanentFailure, got: #{toText other}|]
          Ok _ -> fail "Expected PermanentFailure for unregistered integration"

      it "ShimEmit.emit in hybrid mode falls through to runReal for non-named integration" \_ -> do
        let app =
              Application.new
                |> Application.withIntegration @SendEmail
        let registry = buildRegistry app (Hybrid (Array.fromLinkedList ["NonExistent"]))
        let req = mkSendEmail "alice@example.com" "template-001"
        result <- ShimEmit.emit registry req |> Task.asResult
        case result of
          Err (TransientFailure _) -> pass
          Err other -> fail [fmt|Expected TransientFailure from real fixture, got: #{toText other}|]
          Ok _ -> fail "Expected runReal fixture to throw, but got Ok"

      it "registering the same integration twice results in size 1 (last-write-wins)" \_ -> do
        let app =
              Application.new
                |> Application.withIntegration @SendEmail
                |> Application.withIntegration @SendEmail
        let registry = buildRegistry app Fake
        DispatchRegistry.size registry |> shouldBe 1

      it "Hybrid [] routes all integrations through runReal" \_ -> do
        let app =
              Application.new
                |> Application.withIntegration @SendEmail
        let registry = buildRegistry app (Hybrid (Array.fromLinkedList []))
        let req = mkSendEmail "alice@example.com" "template-001"
        result <- ShimEmit.emit registry req |> Task.asResult
        case result of
          Err (TransientFailure _) -> pass
          Err other -> fail [fmt|Expected TransientFailure from real fixture, got: #{toText other}|]
          Ok _ -> fail "Expected runReal fixture to throw, but got Ok"
