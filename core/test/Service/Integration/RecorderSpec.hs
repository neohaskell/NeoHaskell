module Service.Integration.RecorderSpec where

import Core
import Test


spec :: Spec Unit
spec = do
  describe "Service.Integration.Recorder" do
    it "record is a no-op when NEOHASKELL_RECORD_FIXTURES is unset" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/1"

    it "record writes to <projectRoot>/tests/fixtures/local/SendEmail/<hash>.json when env var is 1" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/2"

    it "record skips writing when NEOHASKELL_RECORD_FIXTURES is 0" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/3"

    it "record aborts before writing when entropy scan hits sk_live_..." \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/4"

    it "aborts on Bearer prefix" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/5"

    it "aborts on whsec_ prefix" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/6"

    it "aborts on JWT-shape eyJhbGci..." \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/7"

    it "aborts on 32+ char lowercase hex token" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/8"

    it "aborts on Base64 blob > 48 chars" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/9"

    it "aborts on high-Shannon-entropy string" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/10"

    it "RedactionRule.AllowEntropyPath $.fakeToken lets a flagged field pass" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/11"

    it "RedactJsonPath $.customer.email <redacted-email> applied before scan" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/12"

    it "defaultRedactionRules redacts an email-shaped field" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/13"

    it "defaultRedactionRules redacts a phone-number-shaped field" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/14"

    it "promote refuses without NEOHASKELL_PROMOTE_FIXTURES=1" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/15"

    it "promote moves file from local/ to committed tree when gate open" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/16"

    it "promote re-runs entropy scan and aborts if the local file now contains a secret" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/17"

    it "record under a response containing Redacted fields emits no Show of the redacted value" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/18"

    it "writes nothing when the input request array is empty" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/19"

    it "round-trip: record then lookup returns the same response for N random requests" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/20"
