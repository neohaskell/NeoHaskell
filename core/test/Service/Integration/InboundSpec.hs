module Service.Integration.InboundSpec where

import Core
import Service.Integration.Inbound qualified as Inbound
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "Service.Integration.Inbound" do
    it "default controllableHandle exposes capacity = 1024" \_ -> do
      let handle = Inbound.controllableHandle @Int
      handle.capacity |> shouldBe 1024

    it "controllableHandleWith 10 exposes capacity = 10" \_ -> do
      let handle = Inbound.controllableHandleWith @Int 10
      handle.capacity |> shouldBe 10

    it "inject + drive delivers a trigger to the callback" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/3 (requires concurrency harness)"

    it "capacity 1: a single injected event is buffered and delivered" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/4 (requires concurrency harness)"

    it "tryRead returns Nothing on an empty channel" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/5 (requires concurrency harness)"

    it "write blocks when full (inject applies backpressure)" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/6 (requires concurrency harness)"

    it "inject succeeds immediately when drain keeps up" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/7 (requires concurrency harness)"

    it "simulate @StripeWebhook syntheticEvent reaches the callback" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/8 (requires test fixture module)"

    it "overriding runFake replaces the default controllableHandle" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/9 (requires test fixture module)"

    it "controllableHandleWith 0 throws ValidationFailure on use" \_ -> do
      let handle = Inbound.controllableHandleWith @Int 0
      result <- Task.asResult (Inbound.inject handle 1)
      case result of
        Err _ -> Task.yield ()
        Ok _ -> fail "expected ValidationFailure on zero capacity"

    it "negative capacity rejected" \_ -> do
      let handle = Inbound.controllableHandleWith @Int (-1)
      result <- Task.asResult (Inbound.inject handle 1)
      case result of
        Err _ -> Task.yield ()
        Ok _ -> fail "expected validation failure"

    it "concurrency: 8 producers injecting 100 events each are all observed exactly once" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/12 (requires concurrency harness)"

    it "drive exits with Err TransientFailure when the downstream callback throws" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/13 (requires concurrency harness)"
