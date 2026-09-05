# Change 004: Add Crypto module with HMAC-SHA256 signWith/verifyWith

Applications that deliver signed webhooks (sender signs the raw request
body, receiver recomputes the HMAC and compares) currently have no public
signing primitive in nhcore: the only HMAC-SHA256 code is internal to
`Auth.OAuth2.StateToken`, shaped around a fixed `StatePayload` with TTL.
Per the philosophy that apps should not pull dependencies from Hackage
directly, expose a general-purpose, pipe-friendly signing primitive in
nhcore: `someBytes |> Crypto.signWith myHmacKey`, plus a constant-time
verify. This also serves the inbound half anticipated by ADR-0058
("public webhook receiver… signed with HMAC at the body level rather
than JWT").

```yaml spec
issue: adhoc:crypto-hmac-sign-verify
kind: feature
touches: [core-primitives]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

```diff signatures
+ Crypto: data HmacKey
+ Crypto: hmacKeyFromText :: Text -> Result Text HmacKey
+ Crypto: hmacKeyFromBytes :: Bytes -> Result Text HmacKey
+ Crypto: generateHmacKey :: Task err HmacKey
+ Crypto: signWith :: HmacKey -> Bytes -> Text
+ Crypto: verifyWith :: HmacKey -> Text -> Bytes -> Bool
+ Bytes: getRandom :: Int -> Task w Bytes
```

Review feedback (maintainer): `Crypto` must not reach for raw `ByteString`
primitives where `Bytes` can provide them. Key-length validation now goes
through `Bytes.length`, and secure random generation is exposed as
`Bytes.getRandom` (mirroring the `Int.getRandom` API), which
`Crypto.generateHmacKey` consumes. To let `Bytes` depend on `Task` without
an import cycle (`Task` → `Text` → `Bytes`), the `Bytes` newtype moved to
the new hidden internal module `Bytes.Internal` (listed under
`other-modules`, not part of the public API); the public `Bytes` API is
unchanged (`Bytes (..)` is re-exported as before).

## Criteria

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | Key creation accepts >= 32-byte secrets and rejects shorter ones (text and raw bytes) | `hspec:nhcore-test-core:core/test/CryptoSpec.hs#can be created from a 32+ byte secret`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#rejects secrets shorter than 32 bytes`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#accepts 32 raw bytes`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#rejects 31 raw bytes` | unit | none |
| C2 | Key never leaks through Show/toText | `hspec:nhcore-test-core:core/test/CryptoSpec.hs#Show instance does not reveal the key` | unit | none |
| C3 | signWith produces the correct HMAC-SHA256 (independently computed known answers, incl. empty message) as 64 lowercase hex chars | `hspec:nhcore-test-core:core/test/CryptoSpec.hs#matches the HMAC-SHA256 known answer`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#matches the known answer for an empty message`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#produces 64 lowercase hex characters` | unit | none |
| C4 | verifyWith accepts valid signatures (lower- and uppercase hex) and rejects tampered messages, wrong keys, truncated and non-hex input | `hspec:nhcore-test-core:core/test/CryptoSpec.hs#accepts a signature produced by signWith`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#accepts an uppercase hex signature`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#rejects a signature for a different message`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#rejects a signature made with a different key`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#rejects a truncated signature`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#rejects garbage that is not hex at all` | unit | none |
| C5 | generateHmacKey yields a usable key and independent keys per call | `hspec:nhcore-test-core:core/test/CryptoSpec.hs#generates a key that round-trips sign and verify`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#generates independent keys` | unit | none |
| C6 | Bytes.getRandom yields the requested number of bytes (clamping sizes below zero to empty) and independent values per call | `hspec:nhcore-test-core:core/test/BytesSpec.hs#generates the requested number of bytes`<br>`hspec:nhcore-test-core:core/test/BytesSpec.hs#generates empty bytes for size zero`<br>`hspec:nhcore-test-core:core/test/BytesSpec.hs#generates empty bytes for negative sizes`<br>`hspec:nhcore-test-core:core/test/BytesSpec.hs#generates independent values` | unit | none |

## User impact

None breaking. New public module `Crypto` and new `Bytes.getRandom`
primitive (secure random bytes, mirroring `Int.getRandom`); no existing
signatures change. The `Bytes` newtype now lives in the hidden internal
module `Bytes.Internal` purely to break an import cycle; it is not
importable by applications. The public `Bytes` API is unchanged (`Bytes
(..)` is re-exported as before). `Auth.OAuth2.StateToken` keeps its private
`HmacKey` for now — migrating it onto `Crypto.HmacKey` is a possible
follow-up refactor, deliberately out of scope here. Signature wire format
is lowercase hex (the common webhook header convention, e.g. GitHub/Stripe
style); `verifyWith` is case-insensitive on input.

## ADR

Not required — no trigger (breaking / new-dependency / new-capability /
new-extension-point all false). Related context: ADR-0058 motivates
body-level HMAC for public webhook receivers; this change provides the
primitive without deciding transport policy.
