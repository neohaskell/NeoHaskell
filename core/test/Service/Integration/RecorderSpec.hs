module Service.Integration.RecorderSpec where

import Array qualified
import Core
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Task qualified
import Test
import Test.Integration.EntropyScan qualified as EntropyScan
import Text qualified


-- Small helper for entropy-scan tests.
stringValue :: Text -> Aeson.Value
stringValue = Aeson.String


stringObject :: Text -> Text -> Aeson.Value
stringObject key val =
  Aeson.Object (AesonKeyMap.fromList [(AesonKey.fromText key, Aeson.String val)])


spec :: Spec Unit
spec = do
  describe "Service.Integration.Recorder" do
    it "record is a no-op when NEOHASKELL_RECORD_FIXTURES is unset" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/1 (requires filesystem test harness)"

    it "record writes to tests/fixtures/local/<integration>/<hash>.json when env var is 1" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/2 (requires filesystem test harness)"

    it "record skips writing when NEOHASKELL_RECORD_FIXTURES is 0" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/3 (requires filesystem test harness)"

    it "entropy scan rejects sk_live_ prefix" \_ -> do
      let val = stringObject "status" "sk_live_abcdef1234567890"
      case EntropyScan.scanForSecrets [] val of
        Err issues -> do
          let joined = Text.joinWith "; " (Array.fromLinkedList issues)
          Text.contains "sk_live" joined |> shouldBe True
        Ok _ -> fail "expected rejection"

    it "entropy scan rejects Bearer prefix" \_ -> do
      let val = stringObject "Authorization" "Bearer abc.def.ghi"
      case EntropyScan.scanForSecrets [] val of
        Err issues -> do
          let joined = Text.joinWith "; " (Array.fromLinkedList issues)
          Text.contains "Bearer" joined |> shouldBe True
        Ok _ -> fail "expected rejection"

    it "entropy scan rejects whsec_ prefix" \_ -> do
      let val = stringObject "token" "whsec_abc123"
      case EntropyScan.scanForSecrets [] val of
        Err _ -> Task.yield ()
        Ok _ -> fail "expected rejection"

    it "entropy scan rejects JWT-shape" \_ -> do
      let val = stringObject "token" "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJ0ZXN0In0.abc"
      case EntropyScan.scanForSecrets [] val of
        Err _ -> Task.yield ()
        Ok _ -> fail "expected rejection"

    it "entropy scan rejects 32+ char hex token" \_ -> do
      let val = stringObject "token" "abcdef0123456789abcdef0123456789"
      case EntropyScan.scanForSecrets [] val of
        Err _ -> Task.yield ()
        Ok _ -> fail "expected rejection"

    it "entropy scan rejects Base64 blob > 48 chars" \_ -> do
      let val = stringObject "token" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789++//"
      case EntropyScan.scanForSecrets [] val of
        Err _ -> Task.yield ()
        Ok _ -> fail "expected rejection"

    it "entropy scan rejects high-entropy string" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/10 (shannon-entropy threshold tuning deferred)"

    it "AllowEntropyPath lets a flagged field pass" \_ -> do
      let val = stringObject "fakeToken" "sk_live_ok"
      case EntropyScan.scanForSecrets ["$.fakeToken"] val of
        Ok _ -> Task.yield ()
        Err _ -> fail "expected bypass"

    it "RedactJsonPath replaces value in the recorded JSON" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/12 (uses Test.Integration.Fixture.record — filesystem harness)"

    it "defaultRedactionRules redacts an email-shaped field" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/13 (requires filesystem test harness)"

    it "defaultRedactionRules redacts a phone-number-shaped field" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/14 (requires filesystem test harness)"

    it "promote refuses without NEOHASKELL_PROMOTE_FIXTURES=1" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/15 (requires filesystem test harness)"

    it "promote moves file from local/ to committed tree when gate open" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/16 (requires filesystem test harness)"

    it "promote re-runs entropy scan and aborts if the local file now contains a secret" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/17 (requires filesystem test harness)"

    it "record under a response containing Redacted fields emits no Show of the redacted value" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/18 (requires filesystem test harness)"

    it "writes nothing when the input request array is empty" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/19 (requires filesystem test harness)"

    it "round-trip: record then lookup returns the same response for N random requests" \_ ->
      pending "Phase 8 — ADR-0055 RecorderSpec/20 (requires filesystem test harness)"


