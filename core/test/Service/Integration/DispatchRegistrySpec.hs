module Service.Integration.DispatchRegistrySpec where

import Core
import Service.Integration.DispatchRegistry qualified as DispatchRegistry
import Test


spec :: Spec Unit
spec = do
  describe "Service.Integration.DispatchRegistry" do
    it "lookup on empty returns Nothing" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/1 (requires test fixture module with Integration instance)"

    it "registers and retrieves a closure by TypeRep" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/2 (requires test fixture module)"

    it "distinct TypeRep keys do not collide" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/3 (requires test fixture module)"

    it "re-registering the same TypeRep overwrites the previous closure" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/4 (requires test fixture module)"

    it "two distinct registries do not share state" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/5 (requires test fixture module)"

    it "lookup-miss returns Nothing for a type present in a sibling registry" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/6 (requires test fixture module)"

    it "the stored closure preserves its Response type (no unsafeCoerce slippage)" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/7 (requires test fixture module)"

    it "DispatchRegistry.lookup applied against a mismatched type witness returns Nothing" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/8 (requires test fixture module)"

    it "thread-safety: 16 concurrent register calls succeed" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/9 (requires test fixture module)"

    it "thread-safety: 100 concurrent lookup calls never return a stale Nothing" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/10 (requires test fixture module)"

    it "empty-registry size is 0" \_ -> do
      DispatchRegistry.size DispatchRegistry.empty |> shouldBe 0

    it "registering 1024 distinct types yields 1024 lookups all returning Just" \_ ->
      pending "Phase 8 — ADR-0055 DispatchRegistrySpec/12 (requires test fixture module)"
