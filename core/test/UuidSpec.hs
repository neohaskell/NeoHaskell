module UuidSpec where

import Array qualified
import Core
import Maybe qualified
import Test
import Text qualified
import Uuid qualified


-- | The RFC 4122 DNS namespace — the standard fixture published v5 vectors use.
dnsNamespace :: Uuid
dnsNamespace =
  "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
    |> Uuid.fromText
    |> Maybe.getOrDie


-- | The RFC 4122 URL namespace, for proving namespaces separate the output.
urlNamespace :: Uuid
urlNamespace =
  "6ba7b811-9dad-11d1-80b4-00c04fd430c8"
    |> Uuid.fromText
    |> Maybe.getOrDie


-- | One dash-separated group of a UUID's canonical text form, zero-indexed.
-- Group 2 carries the version nibble, group 3 the variant bits.
group :: Int -> Uuid -> Text
group index uuid =
  uuid
    |> Uuid.toText
    |> Text.split "-"
    |> Array.get index
    |> Maybe.getOrDie


spec :: Spec Unit
spec = parallel do
  describe "Uuid" do
    describe "generateV5" do
      it "generateV5 is deterministic and matches the RFC 4122 v5 test vector" \_ -> do
        -- The canonical published vector: v5 of the DNS namespace over "python.org".
        -- Pinning it proves we interoperate with every other RFC 4122 v5
        -- implementation, not merely that we are self-consistent.
        Uuid.generateV5 dnsNamespace "python.org"
          |> Uuid.toText
          |> shouldBe "886313e1-3b8a-5372-9b90-0c9aee199e5d"

        -- Same inputs, same output — the whole point of the primitive.
        Uuid.generateV5 dnsNamespace "python.org"
          |> shouldBe (Uuid.generateV5 dnsNamespace "python.org")

        Uuid.generateV5 dnsNamespace "example.org"
          |> Uuid.toText
          |> shouldBe "aad03681-8b63-5304-89e0-8ca8f49461b5"

      it "generateV5 distinguishes names and namespaces" \_ -> do
        -- Different names under one namespace must not collide.
        Uuid.generateV5 dnsNamespace "python.org"
          |> shouldNotBe (Uuid.generateV5 dnsNamespace "example.org")

        -- And the SAME name under different namespaces must not collide —
        -- this is what makes an explicit namespace worth requiring.
        Uuid.generateV5 dnsNamespace "python.org"
          |> shouldNotBe (Uuid.generateV5 urlNamespace "python.org")

        -- The nil namespace is a namespace like any other, not a bypass.
        Uuid.generateV5 Uuid.nil "python.org"
          |> shouldNotBe (Uuid.generateV5 dnsNamespace "python.org")

      it "generateV5 sets the RFC 4122 version and variant bits" \_ -> do
        -- Version nibble is the first character of the third group.
        let derived = Uuid.generateV5 dnsNamespace "python.org"
        group 2 derived |> Text.startsWith "5" |> shouldBe True

        -- Variant bits: the first character of the fourth group is 8, 9, a or b.
        let variant = group 3 derived
        let isRfc4122Variant =
              ["8", "9", "a", "b"]
                |> Array.any (\prefix -> variant |> Text.startsWith prefix)
        isRfc4122Variant |> shouldBe True

        -- The bits are overwritten regardless of what the hash produced, so a
        -- second, unrelated name must satisfy the same invariant.
        let other = Uuid.generateV5 urlNamespace "some/other/name"
        group 2 other |> Text.startsWith "5" |> shouldBe True

      it "generateV5 handles unicode and empty names" \_ -> do
        -- The name is encoded as UTF-8 before hashing, so a multi-byte name
        -- matches what every other v5 implementation derives for it.
        Uuid.generateV5 dnsNamespace "café"
          |> Uuid.toText
          |> shouldBe "5e2e2331-a683-5e18-b56d-666e31574b41"

        -- Same name, different namespace — still UTF-8, still separated.
        Uuid.generateV5 urlNamespace "café"
          |> Uuid.toText
          |> shouldBe "79d4db4c-73a9-5cec-bd37-4fc862e2120d"

        -- The empty name is total and deterministic, not an error case.
        Uuid.generateV5 dnsNamespace ""
          |> Uuid.toText
          |> shouldBe "4ebd0208-8328-5d69-8c44-ec50939c0967"
