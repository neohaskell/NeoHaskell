module Service.Integration.InboundSpec where

import Core
import Test


spec :: Spec Unit
spec = do
  describe "Service.Integration.Inbound" do
    it "default controllableHandle exposes capacity = 1024" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/1"

    it "controllableHandleWith 10 exposes capacity = 10" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/2"

    it "inject + drive delivers a trigger to the callback" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/3"

    it "capacity 1: a single injected event is buffered and delivered" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/4"

    it "tryRead returns Nothing on an empty channel" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/5"

    it "write blocks when full (inject applies backpressure)" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/6"

    it "inject succeeds immediately when drain keeps up" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/7"

    it "simulate @StripeWebhook syntheticEvent from Test.Integration.Simulate reaches the callback" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/8"

    it "overriding runFake replaces the default controllableHandle" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/9"

    it "controllableHandleWith 0 throws ValidationFailure on channel creation" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/10"

    it "negative capacity rejected" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/11"

    it "concurrency: 8 producers injecting 100 events each are all observed exactly once" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/12"

    it "drive exits with Err TransientFailure when the downstream callback throws, without losing subsequent events" \_ ->
      pending "Phase 8 — ADR-0055 InboundSpec/13"
