module Service.Integration.FixtureSpec where

import Core
import Service.Integration.Fixture qualified as Fixture
import Service.Integration.FixtureKey qualified
import Task qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Service.Integration.Fixture" do
    it "returns the decoded response when a fixture exists" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/1 (requires filesystem test harness)"

    it "returns FixtureMiss FixtureFileAbsent when no fixture exists" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/2 (requires filesystem test harness)"

    it "rejects ../etc/passwd-style integration name with FixtureNameInvalid" \_ -> do
      case Fixture.validateIntegrationName "../etc/passwd" of
        Err txt -> txt |> Text.contains "[A-Za-z0-9_]" |> shouldBe True
        Ok _ -> fail "expected rejection"

    it "blocks .. traversal via assembled path escape" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/4 (requires filesystem test harness)"

    it "blocks symlink targets outside the fixtures root on read" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/5 (requires filesystem test harness)"

    it "blocks a path assembled from a non-descendant input" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/6 (requires filesystem test harness)"

    it "blocks Unicode homoglyph in integration name" \_ -> do
      case Fixture.validateIntegrationName "Sendm\x0430il" of
        Err txt -> txt |> Text.contains "[A-Za-z0-9_]" |> shouldBe True
        Ok _ -> fail "expected rejection of Cyrillic 'a'"

    it "validates integration name: rejects empty string" \_ -> do
      case Fixture.validateIntegrationName "" of
        Err txt -> txt |> Text.contains "[A-Za-z0-9_]" |> shouldBe True
        Ok _ -> fail "expected rejection"

    it "validates integration name: rejects space" \_ -> do
      case Fixture.validateIntegrationName "Send Email" of
        Err txt -> txt |> Text.contains "[A-Za-z0-9_]" |> shouldBe True
        Ok _ -> fail "expected rejection"

    it "accepts all-ASCII valid name" \_ -> do
      case Fixture.validateIntegrationName "SendEmail_v2" of
        Ok name -> name |> shouldBe "SendEmail_v2"
        Err _ -> fail "expected acceptance"

    it "hash is the full 64-char lowercase hex SHA-256 of the canonical-JSON request" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/11 (requires fixture test harness)"

    it "file path assembled is <root>/tests/fixtures/SendEmail/<hash>.json" \_ -> do
      let expectedPath = Fixture.fixturePath "/p" "SendEmail" (Service.Integration.FixtureKey.fromHash "abc")
      expectedPath |> shouldBe "/p/tests/fixtures/SendEmail/abc.json"

    it "resolves project root that contains a symlink in its own path" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/13 (requires filesystem test harness)"

    it "refuses to resolve without a project root (unset)" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/14 (requires Task runner)"

    it "treats corrupt-JSON fixture as FixtureDecodeError" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/15 (requires filesystem test harness)"

    it "returns a miss when the fixture filename has trailing whitespace" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/16 (requires filesystem test harness)"

    it "caller falls through to runFake on miss (end-to-end)" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/17 (requires filesystem test harness)"

    it "handles integration name at length 255" \_ -> do
      let name = Text.repeat 255 "a"
      case Fixture.validateIntegrationName name of
        Ok ok -> ok |> shouldBe name
        Err _ -> fail "expected acceptance"

    it "rejects integration name '.'" \_ -> do
      case Fixture.validateIntegrationName "." of
        Err _ -> Task.yield ()
        Ok _ -> fail "expected rejection"

    it "rejects integration name '..'" \_ -> do
      case Fixture.validateIntegrationName ".." of
        Err _ -> Task.yield ()
        Ok _ -> fail "expected rejection"


