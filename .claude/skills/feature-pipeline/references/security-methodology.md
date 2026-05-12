# Security review methodology

Scope: feature-level diffs (a new type, module, or protocol integration) in a NeoHaskell library/framework codebase. Each section: rule, reviewer check, sources.

## Contents

- [1. OWASP Top 10 (2025) for library authors](#1-owasp-top-10-2025-for-library-authors)
- [2. STRIDE per-feature reviewer questions](#2-stride-per-feature-reviewer-questions)
- [3. SLSA v1.1 supply chain](#3-slsa-v11-supply-chain)
- [4. NIST SSDF SP 800-218 (PW practices)](#4-nist-ssdf-sp-800-218-pw-practices)
- [5. Parse-don't-validate / illegal states unrepresentable](#5-parse-dont-validate--illegal-states-unrepresentable)
- [6. Constant-time comparison for secrets](#6-constant-time-comparison-for-secrets)
- [7. Cryptographically-secure randomness](#7-cryptographically-secure-randomness)
- [8. Secret handling (2025/2026 best practice)](#8-secret-handling-20252026-best-practice)
- [9. The overkill smell / proportional review](#9-the-overkill-smell--proportional-review)

## 1. OWASP Top 10 (2025) for library authors

Rule: A03:2025 (Software Supply Chain Failures) and A08:2025 (Software or Data Integrity Failures) are the library-author categories. A01 (Broken Access Control), A04 (Cryptographic Failures), A05 (Injection), A06 (Insecure Design), A07 (Authentication Failures), A09 (Logging Failures), A10 (Mishandling of Exceptional Conditions) are application-author categories that a library can only enable or undermine.

Reviewer check: does this diff introduce a dependency, build-step, or signed artifact (A03/A08)? If it only adds a pure type/function, only A06 and A10 apply.

Sources: <https://owasp.org/Top10/2025/> ; <https://owasp.org/Top10/2025/0x00_2025-Introduction/>

## 2. STRIDE per-feature reviewer questions

Rule: for every new trust boundary, data flow, or store introduced by the diff, ask one question per STRIDE letter. If the diff crosses no boundary, STRIDE collapses to Tampering + Information-disclosure only.

Reviewer check:

- **Spoofing**: does this code accept an identity claim without verifying it?
- **Tampering**: can a caller mutate state the type system says is immutable (`unsafeCoerce`, raw `IORef`, file write without integrity check)?
- **Repudiation**: does an event-sourced write skip the audit log or actor field?
- **Information disclosure**: does any `Show`, `ToJSON`, log, or error message render a secret, token, or PII?
- **Denial of service**: is there unbounded recursion, list, fold, or external call without a timeout or size limit?
- **Elevation of privilege**: does the diff expose a constructor, lens, or internal module that lets a caller forge a privileged value?

Sources: <https://owasp.org/www-community/Threat_Modeling_Process> ; <https://cheatsheetseries.owasp.org/cheatsheets/Threat_Modeling_Cheat_Sheet.html>

## 3. SLSA v1.1 supply chain

Rule: SLSA v1.1 defines Build L0–L3. Pinning by immutable reference is required from L1 upward; cryptographic-hash pinning is required when the dependency is fetched at build time and not already verified by a signed lockfile.

Reviewer check: does the diff add a new dependency? If yes, is it in a lockfile with hashes (`cabal.project.freeze`, `flake.lock`, `package-lock.json`)? Does any build step `curl | sh`, fetch from a mutable URL, or pull a Docker tag (`:latest`, branch name)? Pinning a hash on a dep already covered by a hash-verified lockfile is overkill; pinning on an inline GitHub Action, container, or shell-fetched script is mandatory.

Sources: <https://slsa.dev/spec/v1.1/levels> ; <https://www.kusari.dev/blog/pinning-dependencies>

## 4. NIST SSDF SP 800-218 (PW practices)

Rule: the PW (Produce Well-Secured Software) group covers feature-level coding. Diff-relevant practices: PW.1 (design to meet security requirements), PW.4 (reuse well-secured software vs reinvent), PW.5 (secure coding practices), PW.6 (secure compilation/build toolchain), PW.7 (review human-readable code), PW.8 (test executable code), PW.9 (secure defaults).

Reviewer check: was a known-good library reused instead of hand-rolling crypto/parsing (PW.4)? Are compiler warnings/strict flags on (PW.6)? Did review and tests cover the diff (PW.7/PW.8)? Are defaults safe-by-default (PW.9)?

Sources: <https://csrc.nist.gov/pubs/sp/800/218/final> ; <https://nvlpubs.nist.gov/nistpubs/specialpublications/nist.sp.800-218.pdf>

## 5. Parse-don't-validate / illegal states unrepresentable

Rule: refine input at the boundary into a type whose inhabitants are exactly the legal values; downstream code consumes the refined type and re-validation becomes dead code.

Reviewer check:

- Does the diff take a raw `Text`/`ByteString`/`Int` where a `newtype` with a smart constructor would have prevented the bug class?
- Does any function return `Maybe a` or `m ()` solely to report "input was invalid"? Promote the precondition into the argument type.
- Is `NonEmpty`, `Map`, `Set`, a refined range, or a sum type used where a list / `Maybe` / `Bool` flag would admit illegal combinations?
- Has the burden of proof been pushed upward but no further (no premature parsing of data that is still raw at the boundary)?

Sources: <https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/> ; <https://fsharpforfunandprofit.com/posts/designing-with-types-making-illegal-states-unrepresentable/>

## 6. Constant-time comparison for secrets

Rule: use a constant-time equality (`Data.ByteArray.constEq`, `SecureMem` Eq, `hmac.compare_digest`) whenever the comparand is a long-lived secret an attacker can repeatedly submit: API keys, session tokens, HMAC/MAC tags, password hashes, OTPs, webhook signatures, reset tokens.

Reviewer check: is the value compared a secret the attacker can probe many times? → constant-time required. UUIDs require it only when the UUID is itself a capability token (session, share-link); a UUID used as a non-secret identifier does not. Error codes, enum tags, lengths: not required. Length-leak: constant-time funcs still leak on length differences, so reject mismatched length up front.

Sources: <https://codahale.com/a-lesson-in-timing-attacks/> ; <https://paragonie.com/blog/2015/11/preventing-timing-attacks-on-string-comparison-with-double-hmac-strategy> ; <https://en.wikipedia.org/wiki/Timing_attack>

## 7. Cryptographically-secure randomness

Rule: a CSPRNG (`/dev/urandom`, `getrandom(2)`, `Crypto.Random.getRandomBytes`) is required whenever the output is unguessable-to-attacker by contract: keys, IVs/nonces requiring unpredictability, session IDs, password reset tokens, CSRF tokens, OAuth state, share links.

Reviewer check: does the produced value gate access, identify a session, or seed a crypto primitive? → CSPRNG required. Is it shuffling a deck for a test, picking retry jitter, or generating a non-security correlation ID? `System.Random` / `splitmix` is fine. False positives: UUIDv4 from a CSPRNG-backed library is already secure (do not double-wrap); jitter, exponential-backoff, load-balancer choice, sampling, fuzz-test seeds do not require CSPRNG.

Sources: <https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator> ; <https://www.zellic.io/blog/csprngs-how-to-properly-generate-random-numbers/>

## 8. Secret handling (2025/2026 best practice)

Rule: wrap every secret in a dedicated newtype that (a) hand-writes `Show` to print `"<redacted>"`, (b) does not derive `ToJSON`/`Generic`, (c) compares in constant time, (d) holds the payload in scrubbed/pinned memory (`ScrubbedBytes`, `SecureMem`) so the GC zeros on free, (e) is constructed only from a vetted source (env var read once at boot, secret-manager fetch, mounted file) and never from a config file checked into VCS.

Reviewer check: any `deriving (Show, ToJSON, Generic, FromJSON)` on a type carrying a secret? Reject. Does any log/trace/exception render the secret value? Reject (use `Redacted` or hand-written `Show`). Is the secret kept in a plain `Text`/`String` (immutable, copied on GC, never zeroed) instead of `ScrubbedBytes`? Flag for long-lived secrets. Is the secret sourced from an env var (acceptable, but prefer file-mount or secret manager for production per OWASP) or from a tracked config file (reject)? Is there a single read-at-boot site, or is the env var read repeatedly?

Sources: <https://cheatsheetseries.owasp.org/cheatsheets/Secrets_Management_Cheat_Sheet.html> ; <https://hackage.haskell.org/package/securemem> ; <https://blog.arcjet.com/storing-secrets-in-env-vars-considered-harmful/> ; <https://blog.gitguardian.com/secure-your-secrets-with-env/>

## 9. The overkill smell / proportional review

Rule: security controls must be proportional to the feature's blast radius; a cascade of controls on a feature that touches no secret, no trust boundary, and no external input is security theater (Schneier) and erodes reviewer credibility.

Reviewer heuristics for grounding intensity:

- If the diff adds no new trust boundary, no new data flow, and no new dependency, STRIDE collapses to T+I and SLSA/SSDF-PW.4 are the only relevant chapters.
- If the diff produces or compares a value whose secrecy gates access, sections 6/7/8 activate; otherwise they do not.
- **Blast-radius rule**: a finding is in scope only if a realistic attacker, given the diff's reachable surface, can convert it to a concrete impact (data exfil, integrity loss, DoS, EoP). If no path exists, log as "informational" and do not block.
- Reject control-cascades: do not require CSPRNG for a debug counter, do not require constant-time compare on a public ID, do not require `ScrubbedBytes` for a value already in a public log.

Sources: <https://www.schneier.com/essays/archives/2009/11/beyond_security_thea.html> ; <https://en.wikipedia.org/wiki/Security_theater> ; <https://www.softwaresecured.com/post/security-theater-when-is-a-critical-really-a-critical> ; <https://owasp.org/www-community/OWASP_Risk_Rating_Methodology>
