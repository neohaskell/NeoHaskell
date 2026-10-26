---
group: platform
component: Framework
impact: compatible
category: Added
---

## Summary

`Crypto` now offers the raw building blocks behind signatures: `Crypto.sha256` (32-byte digest), `Crypto.hmacSha256 key message` (raw HMAC that can be chained, any key length) and `Crypto.toHex` (lowercase hexadecimal). Use them when a protocol tells you exactly which bytes to hash or sign; keep using `Crypto.signWith` and `Crypto.verifyWith` for webhook secrets, which are unchanged.

Existing applications do not need code changes. To verify locally, run `./dev test "Crypto" nhcore-test-core` and confirm the `sha256`, `hmacSha256` and `toHex` examples pass.
