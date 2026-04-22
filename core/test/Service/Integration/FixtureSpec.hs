module Service.Integration.FixtureSpec where

import Core
import Test


spec :: Spec Unit
spec = do
  describe "Service.Integration.Fixture" do
    it "returns the decoded response when a fixture exists at tests/fixtures/SendEmail/<hash>.json" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/1"

    it "returns FixtureMiss FixtureFileAbsent when no fixture exists" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/2"

    it "rejects ../etc/passwd-style integration name with FixtureNameInvalid" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/3"

    it "blocks .. traversal via assembled path escape" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/4"

    it "blocks symlink targets outside the fixtures root on read" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/5"

    it "blocks a path assembled from a non-descendant input" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/6"

    it "blocks Unicode homoglyph in integration name (e.g. Cyrillic Sendmаil)" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/7"

    it "validates integration name: rejects empty string" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/8"

    it "validates integration name: rejects space" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/9"

    it "accepts all-ASCII valid name" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/10"

    it "hash is the full 64-char lowercase hex SHA-256 of the canonical-JSON request" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/11"

    it "file path assembled is <root>/tests/fixtures/SendEmail/<hash>.json" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/12"

    it "resolves project root that contains a symlink in its own path" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/13"

    it "refuses to resolve without a project root (unset)" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/14"

    it "treats corrupt-JSON fixture as FixtureDecodeError" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/15"

    it "returns a miss when the fixture filename has trailing whitespace" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/16"

    it "caller falls through to runFake on miss (end-to-end)" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/17"

    it "handles integration name at length 255" \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/18"

    it "rejects integration name ." \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/19"

    it "rejects integration name .." \_ ->
      pending "Phase 8 — ADR-0055 FixtureSpec/20"
