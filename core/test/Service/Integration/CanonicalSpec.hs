module Service.Integration.CanonicalSpec where

import Core
import Test


spec :: Spec Unit
spec = do
  describe "Service.Integration.Canonical" do
    it "encodes RFC 8785 arrays vector byte-identical to the committed output" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/1"

    it "encodes RFC 8785 french vector byte-identical" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/2"

    it "encodes RFC 8785 structures vector byte-identical" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/3"

    it "encodes RFC 8785 unicode vector byte-identical" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/4"

    it "encodes RFC 8785 values vector byte-identical" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/5"

    it "encodes RFC 8785 weird vector byte-identical" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/6"

    it "sorts object keys by UTF-16 code units, not by UTF-8 bytes" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/7"

    it "formats integer 0 as 0" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/8"

    it "formats -0.0 as 0 (ECMA-262 ToString of -0 is 0)" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/9"

    it "formats 1e21 using exponential form 1e+21" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/10"

    it "formats 0.1 as 0.1 (shortest round-trippable)" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/11"

    it "formats 1.0 as 1 (no trailing zero after decimal)" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/12"

    it "rejects NaN encode with ValidationFailure text NaN not permitted" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/13"

    it "rejects Infinity encode with ValidationFailure text Infinity not permitted" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/14"

    it "rejects -Infinity encode symmetrically" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/15"

    it "escapes control character U+0000 as \\u0000" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/16"

    it "escapes control character U+001F as \\u001f" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/17"

    it "passes a non-BMP surrogate pair (U+1F600) through as UTF-8, not escaped" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/18"

    it "produces {} for an empty object" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/19"

    it "produces [] for an empty array" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/20"

    it "handles deeply nested object (32 levels)" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/21"

    it "preserves trailing decimal zeros in an input like 0.10 by formatting as 0.1" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/22"

    it "Canonical.version is 1" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/23"
