module Service.Integration.SelectionSpec where

import Core
import Test


spec :: Spec Unit
spec = do
  describe "Service.Integration.Selection" do
    it "defaults to Real with no flag and no env var" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/1"

    it "returns Real when --integrations=real and no env var" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/2"

    it "aborts when --integrations=fake but env var absent" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/3"

    it "accepts --integrations=fake with env var = 1" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/4"

    it "accepts --integrations=hybrid --fake=Sendgrid --fake=Stripe with env var set" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/5"

    it "aborts hybrid when env var absent" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/6"

    it "error text mentions flag name --integrations=fake" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/7"

    it "error text mentions env var NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=1" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/8"

    it "error text mentions docs URL" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/9"

    it "treats NEOHASKELL_ALLOW_FAKE_INTEGRATIONS=0 as gate closed" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/10"

    it "treats env var with trailing whitespace (1 ) as gate closed" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/11"

    it "rejects --fake= name containing invalid chars with validation error" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/12"

    it "hybrid with an unknown-but-well-formed integration name logs WARN and keeps the name" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/13"

    it "isFakeByName Sendgrid (Hybrid [Sendgrid, Stripe]) is True" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/14"

    it "isFakeByName Other (Hybrid [Sendgrid, Stripe]) is False" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/15"

    it "isFakeByName _ Fake always returns True" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/16"

    it "validateOrThrow Fake logs one ERROR-level line naming active fakes" \_ ->
      pending "Phase 8 — ADR-0055 SelectionSpec/17"
