module CryptoSpec where

import Core
import Crypto (HmacKey)
import Crypto qualified
import Result qualified
import Test
import Text qualified


-- | RFC-style known-answer key: exactly 32 ASCII bytes.
knownKey :: HmacKey
knownKey = do
  let keyResult = Crypto.hmacKeyFromText "0123456789abcdef0123456789abcdef"
  case keyResult of
    Err _ -> panic "test key must be valid"
    Ok key -> key


spec :: Spec Unit
spec = parallel do
  describe "Crypto" do
    -- ==========================================================================
    -- HMAC Key Creation
    -- ==========================================================================
    describe "hmacKeyFromText" do
      it "can be created from a 32+ byte secret" \_ -> do
        let secret = "this-is-a-32-byte-secret-key!!!!"
        let keyResult = Crypto.hmacKeyFromText secret
        keyResult |> shouldSatisfy Result.isOk

      it "rejects secrets shorter than 32 bytes" \_ -> do
        let keyResult = Crypto.hmacKeyFromText "too-short"
        keyResult |> shouldSatisfy Result.isErr

      it "Show instance does not reveal the key" \_ -> do
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case Crypto.hmacKeyFromText secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            let shown = toText key
            shown |> shouldSatisfy (\t -> not (Text.contains "this-is-a-32-byte-secret-key" t))
            shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

    describe "hmacKeyFromBytes" do
      it "accepts 32 raw bytes" \_ -> do
        let secretBytes = Text.toBytes "0123456789abcdef0123456789abcdef"
        let keyResult = Crypto.hmacKeyFromBytes secretBytes
        keyResult |> shouldSatisfy Result.isOk

      it "rejects 31 raw bytes" \_ -> do
        let secretBytes = Text.toBytes "0123456789abcdef0123456789abcde"
        let keyResult = Crypto.hmacKeyFromBytes secretBytes
        keyResult |> shouldSatisfy Result.isErr

    -- ==========================================================================
    -- Signing
    -- ==========================================================================
    describe "signWith" do
      it "matches the HMAC-SHA256 known answer" \_ -> do
        -- Computed independently: hmac.new(key, msg, sha256).hexdigest()
        let message = Text.toBytes "The quick brown fox jumps over the lazy dog"
        let signature = message |> Crypto.signWith knownKey
        signature |> shouldBe "854e55263cca493bc884e2dc0f3b271fa072461cd0392bb6b5dc4797951e0295"

      it "matches the known answer for an empty message" \_ -> do
        let signature = Text.toBytes "" |> Crypto.signWith knownKey
        signature |> shouldBe "796cd3078af14636753d26b3b5555422ff55a3e261cf847b48e95371b9bd0aa2"

      it "is deterministic" \_ -> do
        let message = Text.toBytes "same message"
        let first = message |> Crypto.signWith knownKey
        let second = message |> Crypto.signWith knownKey
        first |> shouldBe second

      it "produces 64 lowercase hex characters" \_ -> do
        let signature = Text.toBytes "any message" |> Crypto.signWith knownKey
        Text.length signature |> shouldBe 64
        signature |> shouldBe (Text.toLower signature)

    -- ==========================================================================
    -- Verification
    -- ==========================================================================
    describe "verifyWith" do
      it "accepts a signature produced by signWith" \_ -> do
        let message = Text.toBytes "hello webhook"
        let signature = message |> Crypto.signWith knownKey
        let verified = message |> Crypto.verifyWith knownKey signature
        verified |> shouldBe True

      it "accepts an uppercase hex signature" \_ -> do
        let message = Text.toBytes "hello webhook"
        let signature = message |> Crypto.signWith knownKey |> Text.toUpper
        let verified = message |> Crypto.verifyWith knownKey signature
        verified |> shouldBe True

      it "rejects a signature for a different message" \_ -> do
        let signature = Text.toBytes "original message" |> Crypto.signWith knownKey
        let verified = Text.toBytes "tampered message" |> Crypto.verifyWith knownKey signature
        verified |> shouldBe False

      it "rejects a signature made with a different key" \_ -> do
        case Crypto.hmacKeyFromText "another-32-byte-secret-key!!!!!!" of
          Err _ -> fail "Failed to create key"
          Ok otherKey -> do
            let message = Text.toBytes "hello webhook"
            let signature = message |> Crypto.signWith otherKey
            let verified = message |> Crypto.verifyWith knownKey signature
            verified |> shouldBe False

      it "rejects a truncated signature" \_ -> do
        let message = Text.toBytes "hello webhook"
        let signature = message |> Crypto.signWith knownKey |> Text.dropRight 2
        let verified = message |> Crypto.verifyWith knownKey signature
        verified |> shouldBe False

      it "rejects garbage that is not hex at all" \_ -> do
        let message = Text.toBytes "hello webhook"
        let verified = message |> Crypto.verifyWith knownKey "not-a-signature"
        verified |> shouldBe False

    -- ==========================================================================
    -- Key Generation
    -- ==========================================================================
    describe "generateHmacKey" do
      it "generates a key that round-trips sign and verify" \_ -> do
        key <- Crypto.generateHmacKey
        let message = Text.toBytes "round trip"
        let signature = message |> Crypto.signWith key
        let verified = message |> Crypto.verifyWith key signature
        verified |> shouldBe True

      it "generates independent keys" \_ -> do
        firstKey <- Crypto.generateHmacKey
        secondKey <- Crypto.generateHmacKey
        let message = Text.toBytes "same message"
        let firstSignature = message |> Crypto.signWith firstKey
        let secondSignature = message |> Crypto.signWith secondKey
        firstSignature |> shouldNotBe secondSignature
