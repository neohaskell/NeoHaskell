# Change 003: Add Crypto module with HMAC-SHA256 signWith/verifyWith

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
```

## Criteria

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | Key creation accepts >= 32-byte secrets and rejects shorter ones (text and raw bytes) | `CryptoSpec` "can be created from a 32+ byte secret" / "rejects secrets shorter than 32 bytes" / "accepts 32 raw bytes" / "rejects 31 raw bytes" | unit |
| C2 | Key never leaks through Show/toText | `CryptoSpec` "Show instance does not reveal the key" | unit |
| C3 | signWith produces the correct HMAC-SHA256 (independently computed known answers, incl. empty message) as 64 lowercase hex chars | `CryptoSpec` "matches the HMAC-SHA256 known answer" / "matches the known answer for an empty message" / "produces 64 lowercase hex characters" | unit |
| C4 | verifyWith accepts valid signatures (lower- and uppercase hex) and rejects tampered messages, wrong keys, truncated and non-hex input | `CryptoSpec` "accepts a signature produced by signWith" / "accepts an uppercase hex signature" / "rejects a signature for a different message" / "rejects a signature made with a different key" / "rejects a truncated signature" / "rejects garbage that is not hex at all" | unit |
| C5 | generateHmacKey yields a usable key and independent keys per call | `CryptoSpec` "generates a key that round-trips sign and verify" / "generates independent keys" | unit |

## User impact

None breaking. New public module `Crypto`; no existing signatures change.
`Auth.OAuth2.StateToken` keeps its private `HmacKey` for now — migrating it
onto `Crypto.HmacKey` is a possible follow-up refactor, deliberately out of
scope here. Signature wire format is lowercase hex (the common webhook
header convention, e.g. GitHub/Stripe style); `verifyWith` is
case-insensitive on input.

## ADR

Not required — no trigger (breaking / new-dependency / new-capability /
new-extension-point all false). Related context: ADR-0058 motivates
body-level HMAC for public webhook receivers; this change provides the
primitive without deciding transport policy.
