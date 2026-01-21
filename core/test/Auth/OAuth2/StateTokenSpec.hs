module Auth.OAuth2.StateTokenSpec where

import Auth.OAuth2.StateToken (
  StatePayload (..),
  StateTokenError (..),
  decodeStateToken,
  encodeStateToken,
  mkHmacKey,
 )
import Core
import Result qualified
import Task qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Auth.OAuth2.StateToken" do
    -- ==========================================================================
    -- HMAC Key Creation
    -- ==========================================================================
    describe "HmacKey" do
      it "can be created from a 32+ byte secret" \_ -> do
        -- 32 bytes minimum for HMAC-SHA256 security
        let secret = "this-is-a-32-byte-secret-key!!!!"
        let keyResult = mkHmacKey secret
        keyResult |> shouldSatisfy Result.isOk

      it "rejects secrets shorter than 32 bytes" \_ -> do
        let shortSecret = "too-short"
        let keyResult = mkHmacKey shortSecret
        keyResult |> shouldSatisfy Result.isErr

      it "Show instance does not reveal the key" \_ -> do
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case mkHmacKey secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            let shown = toText key
            shown |> shouldSatisfy (\t -> not (Text.contains "this-is-a-32-byte-secret-key" t))
            shown |> shouldSatisfy (\t -> Text.contains "REDACTED" t)

    -- ==========================================================================
    -- Roundtrip Encoding/Decoding
    -- ==========================================================================
    describe "Encode/Decode Roundtrip" do
      it "successfully roundtrips a valid payload" \_ -> do
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case mkHmacKey secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            let payload =
                  StatePayload
                    { provider = "oura"
                    , userId = "user-123"
                    , nonce = "random-nonce-abc"
                    , issuedAt = 1700000000
                    , expiresAt = 1700000300 -- 5 minutes later
                    }
            -- Encode
            encodedResult <- encodeStateToken key payload |> Task.asResult
            case encodedResult of
              Err _ -> fail "Encode failed"
              Ok stateToken -> do
                -- Decode with current time within validity window
                let currentTime = 1700000100 -- 100 seconds after issue
                decodedResult <- decodeStateToken key currentTime stateToken |> Task.asResult
                case decodedResult of
                  Err err -> fail [fmt|Decode failed: #{toText err}|]
                  Ok decoded -> do
                    decoded.provider |> shouldBe "oura"
                    decoded.userId |> shouldBe "user-123"
                    decoded.nonce |> shouldBe "random-nonce-abc"

      it "preserves all payload fields exactly" \_ -> do
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case mkHmacKey secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            let payload =
                  StatePayload
                    { provider = "github"
                    , userId = "user-with-long-id-0123456789"
                    , nonce = "nonce-12345-xyz"
                    , issuedAt = 1700000000
                    , expiresAt = 1700000600
                    }
            encodedResult <- encodeStateToken key payload |> Task.asResult
            case encodedResult of
              Err _ -> fail "Encode failed"
              Ok stateToken -> do
                let currentTime = 1700000100
                decodedResult <- decodeStateToken key currentTime stateToken |> Task.asResult
                case decodedResult of
                  Err _ -> fail "Decode failed"
                  Ok decoded -> do
                    decoded.provider |> shouldBe payload.provider
                    decoded.userId |> shouldBe payload.userId
                    decoded.nonce |> shouldBe payload.nonce
                    decoded.issuedAt |> shouldBe payload.issuedAt
                    decoded.expiresAt |> shouldBe payload.expiresAt

    -- ==========================================================================
    -- Tamper Detection (Security Critical)
    -- ==========================================================================
    describe "Tamper Detection" do
      it "rejects token with modified payload" \_ -> do
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case mkHmacKey secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            let payload =
                  StatePayload
                    { provider = "oura"
                    , userId = "user-123"
                    , nonce = "nonce"
                    , issuedAt = 1700000000
                    , expiresAt = 1700000300
                    }
            encodedResult <- encodeStateToken key payload |> Task.asResult
            case encodedResult of
              Err _ -> fail "Encode failed"
              Ok stateToken -> do
                -- Tamper with the token by flipping a character
                let tamperedToken = tamperWithToken stateToken
                let currentTime = 1700000100
                decodedResult <- decodeStateToken key currentTime tamperedToken |> Task.asResult
                case decodedResult of
                  Ok _ -> fail "Should have rejected tampered token"
                  Err err -> err |> shouldBe SignatureInvalid

      it "rejects token signed with different key" \_ -> do
        let secret1 = "this-is-a-32-byte-secret-key!!!!"
        let secret2 = "this-is-another-32-byte-secret!!"
        case (mkHmacKey secret1, mkHmacKey secret2) of
          (Ok key1, Ok key2) -> do
            let payload =
                  StatePayload
                    { provider = "oura"
                    , userId = "user-123"
                    , nonce = "nonce"
                    , issuedAt = 1700000000
                    , expiresAt = 1700000300
                    }
            encodedResult <- encodeStateToken key1 payload |> Task.asResult
            case encodedResult of
              Err _ -> fail "Encode failed"
              Ok stateToken -> do
                -- Try to decode with different key
                let currentTime = 1700000100
                decodedResult <- decodeStateToken key2 currentTime stateToken |> Task.asResult
                case decodedResult of
                  Ok _ -> fail "Should have rejected wrong-key token"
                  Err err -> err |> shouldBe SignatureInvalid
          _ -> fail "Failed to create keys"

    -- ==========================================================================
    -- Timestamp Validation
    -- ==========================================================================
    describe "Timestamp Validation" do
      it "rejects token with inverted timestamps (issuedAt > expiresAt)" \_ -> do
        -- Test that tokens with expiresAt < issuedAt are rejected
        -- This prevents malformed tokens from passing other validations
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case mkHmacKey secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            -- Create payload with inverted timestamps: issuedAt > expiresAt
            let payload =
                  StatePayload
                    { provider = "oura"
                    , userId = "user-123"
                    , nonce = "nonce"
                    , issuedAt = 1700000500 -- Issued at 500
                    , expiresAt = 1700000400 -- Expires at 400 (before issuedAt!)
                    }
            encodedResult <- encodeStateToken key payload |> Task.asResult
            case encodedResult of
              Err _ -> fail "Encode failed"
              Ok stateToken -> do
                -- Try to decode with a time between the inverted values
                let currentTime = 1700000450 -- Between 400 and 500
                decodedResult <- decodeStateToken key currentTime stateToken |> Task.asResult
                case decodedResult of
                  Ok _ -> fail "Should have rejected token with inverted timestamps"
                  Err err -> err |> shouldBe TokenInvalidTimestamps

    -- ==========================================================================
    -- TTL Enforcement
    -- ==========================================================================
    describe "TTL Enforcement" do
      it "rejects expired token" \_ -> do
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case mkHmacKey secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            let payload =
                  StatePayload
                    { provider = "oura"
                    , userId = "user-123"
                    , nonce = "nonce"
                    , issuedAt = 1700000000
                    , expiresAt = 1700000300 -- Expires at 300 seconds
                    }
            encodedResult <- encodeStateToken key payload |> Task.asResult
            case encodedResult of
              Err _ -> fail "Encode failed"
              Ok stateToken -> do
                -- Try to decode after expiry
                let currentTime = 1700000400 -- 400 seconds, past expiry
                decodedResult <- decodeStateToken key currentTime stateToken |> Task.asResult
                case decodedResult of
                  Ok _ -> fail "Should have rejected expired token"
                  Err err -> err |> shouldBe TokenExpired

      it "accepts token just before expiry" \_ -> do
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case mkHmacKey secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            let payload =
                  StatePayload
                    { provider = "oura"
                    , userId = "user-123"
                    , nonce = "nonce"
                    , issuedAt = 1700000000
                    , expiresAt = 1700000300
                    }
            encodedResult <- encodeStateToken key payload |> Task.asResult
            case encodedResult of
              Err _ -> fail "Encode failed"
              Ok stateToken -> do
                -- Exactly at expiry time should still work (<=)
                let currentTime = 1700000300
                decodedResult <- decodeStateToken key currentTime stateToken |> Task.asResult
                case decodedResult of
                  Err err -> fail [fmt|Should have accepted token at expiry boundary: #{toText err}|]
                  Ok decoded -> decoded.userId |> shouldBe "user-123"

      it "allows small clock skew for issuedAt" \_ -> do
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case mkHmacKey secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            let payload =
                  StatePayload
                    { provider = "oura"
                    , userId = "user-123"
                    , nonce = "nonce"
                    , issuedAt = 1700000100 -- Issued "in the future"
                    , expiresAt = 1700000400
                    }
            encodedResult <- encodeStateToken key payload |> Task.asResult
            case encodedResult of
              Err _ -> fail "Encode failed"
              Ok stateToken -> do
                -- Current time is 60 seconds before issuedAt (within 60s skew tolerance)
                let currentTime = 1700000040
                decodedResult <- decodeStateToken key currentTime stateToken |> Task.asResult
                case decodedResult of
                  Err err -> fail [fmt|Should allow 60s clock skew: #{toText err}|]
                  Ok decoded -> decoded.userId |> shouldBe "user-123"

      it "rejects token with excessive clock skew" \_ -> do
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case mkHmacKey secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            let payload =
                  StatePayload
                    { provider = "oura"
                    , userId = "user-123"
                    , nonce = "nonce"
                    , issuedAt = 1700000200 -- Issued "200 seconds in the future"
                    , expiresAt = 1700000500
                    }
            encodedResult <- encodeStateToken key payload |> Task.asResult
            case encodedResult of
              Err _ -> fail "Encode failed"
              Ok stateToken -> do
                -- Current time is 200 seconds before issuedAt (exceeds 60s skew)
                let currentTime = 1700000000
                decodedResult <- decodeStateToken key currentTime stateToken |> Task.asResult
                case decodedResult of
                  Ok _ -> fail "Should have rejected excessive clock skew"
                  Err err -> err |> shouldBe TokenNotYetValid

    -- ==========================================================================
    -- Provider Mix-up Defense
    -- ==========================================================================
    describe "Provider Validation" do
      it "payload contains provider for mix-up defense" \_ -> do
        -- This test ensures the provider is preserved in the token
        -- so the callback handler can verify it matches the route
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case mkHmacKey secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            let payload =
                  StatePayload
                    { provider = "oura"
                    , userId = "user-123"
                    , nonce = "nonce"
                    , issuedAt = 1700000000
                    , expiresAt = 1700000300
                    }
            encodedResult <- encodeStateToken key payload |> Task.asResult
            case encodedResult of
              Err _ -> fail "Encode failed"
              Ok stateToken -> do
                let currentTime = 1700000100
                decodedResult <- decodeStateToken key currentTime stateToken |> Task.asResult
                case decodedResult of
                  Err _ -> fail "Decode failed"
                  Ok decoded -> do
                    -- Verify provider is preserved for route validation
                    decoded.provider |> shouldBe "oura"

    -- ==========================================================================
    -- Malformed Input Handling
    -- ==========================================================================
    describe "Malformed Input Handling" do
      it "rejects empty token" \_ -> do
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case mkHmacKey secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            let emptyToken = ""
            let currentTime = 1700000100
            decodedResult <- decodeStateToken key currentTime emptyToken |> Task.asResult
            case decodedResult of
              Ok _ -> fail "Should have rejected empty token"
              Err err -> err |> shouldBe MalformedToken

      it "rejects non-base64 token" \_ -> do
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case mkHmacKey secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            let invalidToken = "not-valid-base64!@#$%"
            let currentTime = 1700000100
            decodedResult <- decodeStateToken key currentTime invalidToken |> Task.asResult
            case decodedResult of
              Ok _ -> fail "Should have rejected invalid base64"
              Err err -> err |> shouldBe MalformedToken

      it "rejects truncated token (too short for signature)" \_ -> do
        let secret = "this-is-a-32-byte-secret-key!!!!"
        case mkHmacKey secret of
          Err _ -> fail "Failed to create key"
          Ok key -> do
            -- A valid base64 string but too short to contain payload + 32-byte signature
            let shortToken = "YWJj" -- "abc" in base64
            let currentTime = 1700000100
            decodedResult <- decodeStateToken key currentTime shortToken |> Task.asResult
            case decodedResult of
              Ok _ -> fail "Should have rejected truncated token"
              Err err -> err |> shouldBe MalformedToken


-- | Helper to tamper with a token by modifying one character
tamperWithToken :: Text -> Text
tamperWithToken token = do
  let chars = Text.toLinkedList token
  case chars of
    [] -> token
    (c : rest) -> do
      -- Flip the first character
      let flipped = case c of
            'a' -> 'b'
            _ -> 'a'
      Text.fromLinkedList (flipped : rest)
