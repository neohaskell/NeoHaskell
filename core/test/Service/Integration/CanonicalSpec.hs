module Service.Integration.CanonicalSpec where

import Core
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.Text.Encoding qualified as GhcTextEncoding
import Service.Integration.Canonical qualified as Canonical
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Service.Integration.Canonical" do
    it "encodes RFC 8785 arrays vector byte-identical to the committed output" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/1 (RFC 8785 vectors not committed yet)"

    it "encodes RFC 8785 french vector byte-identical" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/2 (RFC 8785 vectors not committed yet)"

    it "encodes RFC 8785 structures vector byte-identical" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/3 (RFC 8785 vectors not committed yet)"

    it "encodes RFC 8785 unicode vector byte-identical" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/4 (RFC 8785 vectors not committed yet)"

    it "encodes RFC 8785 values vector byte-identical" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/5 (RFC 8785 vectors not committed yet)"

    it "encodes RFC 8785 weird vector byte-identical" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/6 (RFC 8785 vectors not committed yet)"

    it "sorts object keys lexicographically (UTF-16 code units)" \_ -> do
      let object1 = Aeson.object [("z", Aeson.Number 1), ("a", Aeson.Number 2)]
      let encoded = Canonical.encodeValue object1
      case encoded of
        Err _ -> fail "expected Ok encoding"
        Ok bytes -> do
          let decoded = GhcTextEncoding.decodeUtf8 bytes
          -- Keys should come out as a then z.
          decoded |> Text.contains "\"a\"" |> shouldBe True
          decoded |> shouldBe "{\"a\":2,\"z\":1}"

    it "formats integer 0 as 0" \_ -> do
      let val = Aeson.Number 0
      case Canonical.encodeValue val of
        Err _ -> fail "expected Ok"
        Ok bytes -> bytes |> shouldBe "0"

    it "formats -0.0 as 0 (ECMA-262 ToString of -0 is 0)" \_ -> do
      -- Scientific cannot represent -0.0 distinctly from 0; the sign is stripped.
      let val = Aeson.Number (-0)
      case Canonical.encodeValue val of
        Err _ -> fail "expected Ok"
        Ok bytes -> bytes |> shouldBe "0"

    it "formats 1e21 using exponential form" \_ -> do
      let val = Aeson.Number 1e21
      case Canonical.encodeValue val of
        Err _ -> fail "expected Ok"
        Ok bytes -> do
          let text = GhcTextEncoding.decodeUtf8 bytes
          text |> Text.contains "e" |> shouldBe True

    it "formats 0.1 as 0.1 (shortest round-trippable)" \_ -> do
      let val = Aeson.Number 0.1
      case Canonical.encodeValue val of
        Err _ -> fail "expected Ok"
        Ok bytes -> bytes |> shouldBe "0.1"

    it "formats 1.0 as 1 (no trailing zero after decimal)" \_ -> do
      let val = Aeson.Number 1.0
      case Canonical.encodeValue val of
        Err _ -> fail "expected Ok"
        Ok bytes -> bytes |> shouldBe "1"

    it "rejects NaN encode with ValidationFailure text NaN not permitted" \_ ->
      -- Aeson.Number uses Scientific which cannot hold NaN; this test is
      -- intentionally left pending because the reject path is not reachable
      -- via Aeson.Value construction.
      pending "Phase 8 — ADR-0055 CanonicalSpec/13 (NaN path unreachable via Aeson.Value)"

    it "rejects Infinity encode with ValidationFailure text Infinity not permitted" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/14 (Infinity path unreachable via Aeson.Value)"

    it "rejects -Infinity encode symmetrically" \_ ->
      pending "Phase 8 — ADR-0055 CanonicalSpec/15 (Infinity path unreachable via Aeson.Value)"

    it "escapes control character U+0000 as \\u0000" \_ -> do
      let val = Aeson.String "a\NULb"
      case Canonical.encodeValue val of
        Err _ -> fail "expected Ok"
        Ok bytes -> do
          let text = GhcTextEncoding.decodeUtf8 bytes
          text |> Text.contains "\\u0000" |> shouldBe True

    it "escapes control character U+001F as \\u001f" \_ -> do
      let val = Aeson.String "a\US b"
      case Canonical.encodeValue val of
        Err _ -> fail "expected Ok"
        Ok bytes -> do
          let text = GhcTextEncoding.decodeUtf8 bytes
          text |> Text.contains "\\u001f" |> shouldBe True

    it "passes a non-BMP surrogate pair (U+1F600) through as UTF-8, not escaped" \_ -> do
      let val = Aeson.String "\x1F600"
      case Canonical.encodeValue val of
        Err _ -> fail "expected Ok"
        Ok bytes -> do
          -- Should be 4 bytes of UTF-8 surrounded by quotes.
          ByteString.length bytes |> shouldBe 6

    it "produces {} for an empty object" \_ -> do
      let val = Aeson.object []
      case Canonical.encodeValue val of
        Err _ -> fail "expected Ok"
        Ok bytes -> bytes |> shouldBe "{}"

    it "produces [] for an empty array" \_ -> do
      let val = Aeson.toJSON ([] :: [Int])
      case Canonical.encodeValue val of
        Err _ -> fail "expected Ok"
        Ok bytes -> bytes |> shouldBe "[]"

    it "handles deeply nested object" \_ -> do
      let nested = buildNested 32
      case Canonical.encodeValue nested of
        Err _ -> fail "expected Ok"
        Ok bytes -> ByteString.length bytes |> shouldBeGreaterThan 32

    it "preserves trailing decimal zeros in an input like 0.10 by formatting as 0.1" \_ -> do
      let val = Aeson.Number 0.10
      case Canonical.encodeValue val of
        Err _ -> fail "expected Ok"
        Ok bytes -> bytes |> shouldBe "0.1"

    it "Canonical.version is 1" \_ ->
      Canonical.version |> shouldBe 1


buildNested :: Int -> Aeson.Value
buildNested depth =
  if depth <= 0
    then Aeson.Null
    else Aeson.object [("k", buildNested (depth - 1))]
