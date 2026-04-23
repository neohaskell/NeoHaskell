module Service.Integration.DispatchRegistrySpec where

import Core
import Service.Integration.Adapter (Integration (..))
import Service.Integration.DispatchRegistry qualified as DispatchRegistry
import Service.Integration.IntegrationError (IntegrationError (..))
import Service.Integration.TestFixtures
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "Service.Integration.DispatchRegistry" do
    it "lookup on empty returns Nothing" \_ -> do
      let result = DispatchRegistry.lookup @SendEmail DispatchRegistry.empty
      case result of
        Nothing -> pass
        Just _ -> fail "Expected Nothing, got Just"

    it "registers and retrieves a closure by TypeRep" \_ -> do
      let closure = runFake @SendEmail
      let reg = DispatchRegistry.register @SendEmail closure DispatchRegistry.empty
      case DispatchRegistry.lookup @SendEmail reg of
        Nothing -> fail "Expected Just closure, got Nothing"
        Just found -> do
          let req = mkSendEmail "x@y.com" "t"
          expected <- runFake req |> Task.asResult
          actual <- found req |> Task.asResult
          case (expected, actual) of
            (Ok _, Ok _) -> pass
            _ -> fail "closure did not return Ok"

    it "distinct TypeRep keys do not collide" \_ -> do
      let reg =
            DispatchRegistry.empty
              |> DispatchRegistry.register @SendEmail (runFake @SendEmail)
              |> DispatchRegistry.register @ChargeIntent (runFake @ChargeIntent)
      DispatchRegistry.size reg |> shouldBe 2

    it "re-registering the same TypeRep overwrites the previous closure" \_ -> do
      let always503 _req = Task.throw (TransientFailure "503")
      let reg =
            DispatchRegistry.empty
              |> DispatchRegistry.register @SendEmail (runFake @SendEmail)
              |> DispatchRegistry.register @SendEmail always503
      case DispatchRegistry.lookup @SendEmail reg of
        Nothing -> fail "Expected Just, got Nothing"
        Just found -> do
          result <- found (mkSendEmail "a@b.com" "t") |> Task.asResult
          case result of
            Err (TransientFailure "503") -> pass
            _ -> fail "Expected the second (overwritten) closure"

    it "two distinct registries do not share state" \_ -> do
      let reg1 = DispatchRegistry.register @SendEmail (runFake @SendEmail) DispatchRegistry.empty
      let reg2 = DispatchRegistry.empty
      DispatchRegistry.size reg1 |> shouldBe 1
      DispatchRegistry.size reg2 |> shouldBe 0

    it "lookup-miss returns Nothing for type present in sibling registry" \_ -> do
      let reg1 = DispatchRegistry.register @SendEmail (runFake @SendEmail) DispatchRegistry.empty
      let result = DispatchRegistry.lookup @ChargeIntent reg1
      case result of
        Nothing -> pass
        Just _ -> fail "Expected Nothing, got Just"

    it "the stored closure preserves its Response type (no unsafeCoerce slippage)" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/7 (requires -fdefer-type-errors submodule)"

    it "DispatchRegistry.lookup applied against a mismatched type witness returns Nothing" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/8 (requires test fixture module)"

    it "thread-safety: 16 concurrent register calls succeed" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/9 (requires concurrent test infrastructure)"

    it "thread-safety: 100 concurrent lookup calls never return a stale Nothing" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/10 (requires concurrent test infrastructure)"

    it "empty-registry size is 0" \_ -> do
      DispatchRegistry.size DispatchRegistry.empty |> shouldBe 0

    it "registering 1024 distinct types yields 1024 lookups all returning Just" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/12 (requires 1024 distinct Integration instances)"
