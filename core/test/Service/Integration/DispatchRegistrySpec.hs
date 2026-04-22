module Service.Integration.DispatchRegistrySpec where

import Core
import Test


spec :: Spec Unit
spec = do
  describe "Service.Integration.DispatchRegistry" do
    it "lookup @SendEmail empty returns Nothing" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/1"

    it "registers and retrieves a closure by TypeRep" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/2"

    it "distinct TypeRep keys do not collide" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/3"

    it "re-registering the same TypeRep overwrites the previous closure" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/4"

    it "two distinct registries do not share state" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/5"

    it "lookup-miss returns Nothing for a type present in a sibling registry" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/6"

    it "the stored closure preserves its Response type (no unsafeCoerce slippage)" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/7"

    it "DispatchRegistry.lookup applied against a mismatched type witness returns Nothing, not a coerced closure" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/8"

    it "thread-safety: 16 concurrent register calls for 16 distinct types all succeed" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/9"

    it "thread-safety: 100 concurrent lookup calls against a populated registry never return a stale Nothing" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/10"

    it "empty-registry Map.size is 0" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/11"

    it "registering 1024 distinct types yields 1024 lookups all returning Just" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/12"
