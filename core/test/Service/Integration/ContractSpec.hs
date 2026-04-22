module Service.Integration.ContractSpec where

import Core
import Test


spec :: Spec Unit
spec = do
  describe "Service.Integration.Contract" do
    it "passes when fake's output parses under the current Response schema" \_ ->
      pending "Phase 8 — ADR-0055 ContractSpec/1"

    it "fails when fake produces a subset of the real schema (missing required field)" \_ ->
      pending "Phase 8 — ADR-0055 ContractSpec/2"

    it "fails when fake adds an extra required key not in Response" \_ ->
      pending "Phase 8 — ADR-0055 ContractSpec/3"

    it "skips sandbox assertion when NEOHASKELL_CONTRACT_SANDBOX unset" \_ ->
      pending "Phase 8 — ADR-0055 ContractSpec/4"

    it "exercises sandbox assertion when NEOHASKELL_CONTRACT_SANDBOX=1" \_ ->
      pending "Phase 8 — ADR-0055 ContractSpec/5"

    it "sandbox branch fails when runReal's response drifts" \_ ->
      pending "Phase 8 — ADR-0055 ContractSpec/6"

    it "contractTests @Ping is registered exactly once per instance (smoke)" \_ ->
      pending "Phase 8 — ADR-0055 ContractSpec/7"

    it "failure output names the integration (NameOf)" \_ ->
      pending "Phase 8 — ADR-0055 ContractSpec/8"

    it "contract property also checks structural compatibility via ToSchema" \_ ->
      pending "Phase 8 — ADR-0055 ContractSpec/9"
