# Change 021: Add raw SHA-256, HMAC-SHA256 and hex primitives to Crypto

Code inside nhcore that speaks a signed protocol needs the plain building
blocks: an unkeyed SHA-256 digest, a raw HMAC-SHA256 that can be chained
(one MAC result becomes the next key), and lowercase hex encoding. Today
`Crypto` exposes only the webhook-shaped `signWith`/`verifyWith` (hex text,
32-byte minimum `HmacKey`), so modules such as the S3 `BlobStore` signer
(change 017) and `FileUpload.Web`'s content hash import `crypton` and
`Data.ByteArray` directly and unwrap `Bytes`. The maintainer asked on #748
to add the missing primitive to Core instead of unwrapping. This change does
that; `signWith` is re-expressed on top of the new functions so there is one
implementation.

```yaml spec
issue: adhoc:crypto-sha256-hmac-hex
kind: feature
touches: [core-primitives]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

Three pure, data-last functions on `Bytes`. `hmacSha256` deliberately takes an
arbitrary key (no 32-byte policy) because key-derivation chains need short
intermediate keys; `signWith`/`verifyWith` keep the policy and the
constant-time comparison and remain the right choice for a single webhook
secret.

```diff signatures
+ Crypto: sha256 :: Bytes -> Bytes
+ Crypto: hmacSha256 :: Bytes -> Bytes -> Bytes
+ Crypto: toHex :: Bytes -> Text
```

## Criteria

| ID | Criterion | Test | Level | Boundary |
|---|---|---|---|---|
| C1 | `sha256` matches the FIPS 180-4 vectors for `""` and `"abc"` | `hspec:nhcore-test-core:core/test/CryptoSpec.hs#hashes the empty input to the FIPS 180-4 vector`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#hashes abc to the FIPS 180-4 vector` | unit | none |
| C2 | `hmacSha256` matches RFC 4231 test case 2 | `hspec:nhcore-test-core:core/test/CryptoSpec.hs#matches RFC 4231 test case 2 (key Jefe)` | unit | none |
| C3 | `hmacSha256` accepts keys shorter than 32 bytes | `hspec:nhcore-test-core:core/test/CryptoSpec.hs#accepts keys shorter than 32 bytes, unlike signWith` | unit | none |
| C4 | `signWith` preserves the independent known answers for nonempty and empty messages | `hspec:nhcore-test-core:core/test/CryptoSpec.hs#matches the HMAC-SHA256 known answer`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#matches the known answer for an empty message` | unit | none |
| C5 | `toHex` is lowercase, two characters per byte, empty for empty | `hspec:nhcore-test-core:core/test/CryptoSpec.hs#encodes two lowercase characters per byte`<br>`hspec:nhcore-test-core:core/test/CryptoSpec.hs#encodes the empty input to the empty text` | unit | none |

## User impact

None breaking. `signWith` and `verifyWith` produce exactly the same output as
before (C4). New: `Crypto.sha256`, `Crypto.hmacSha256`, `Crypto.toHex` for
protocol code that needs the raw primitives. Jess does not need to change
anything.

## ADR

Not required — no trigger. Follow-up (not in this change):
`Service.FileUpload.Web.computeContentHash` can be rewritten on
`Crypto.sha256 |> Crypto.toHex` and drop its own `crypton` imports.
