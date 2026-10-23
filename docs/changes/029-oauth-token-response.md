# Change 029: Return typed OAuth errors for token responses

OAuth token refresh currently attempts to decode a success token before checking
HTTP status. An owned HTTP 400 `invalid_grant` response throws an uncaught
`JSONException` and bypasses the existing OAuth and integration error paths.
This proposal needs a new explicit Gate 1; earlier fixture/CI approval does not
authorize this production change. No production implementation is included yet.

```yaml spec
issue: adhoc:oauth-token-response
kind: bug
touches: [auth, http-client]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

No new exports or changed signatures. Keep global JSON HTTP methods unchanged.
The private token-request path must retain HTTP status and bytes, classify status,
then decode either `TokenResponse` or `TokenErrorResponse`. Both validated and
per-request validated OAuth entrypoints share this decision. Existing URL/DNS
validation, form encoding and field values, TLS verification, proxy behavior,
10-second response timeout, and zero redirects remain unchanged.

```diff signatures
```

## Criteria

C1 uses a real port-zero loopback server through `refreshTokenValidated`, first
proving a valid token response on the same helper before issuing HTTP 400. It
must return typed `InvalidGrant`, not throw `JSONException`. Fixture teardown is
bracketed by Warp. Every response checks exactly one POST with the full expected
form multiset. The unsafe provider constructor is test-only for the owned loopback
URL; it does not weaken product validation. Proxy environment is cleared for the
native test process, not changed as product behavior.

| ID | Behavior | Proving test | Level | Boundary |
|----|----------|--------------|-------|----------|
| C1 | returns InvalidGrant for a real OAuth error after a valid token control | `hspec:nhcore-test-auth:core/test/Auth/OAuth2/TokenRefreshSpec.hs#returns InvalidGrant for a real OAuth error after a valid token control` | integration | http:real |
| C2 | returns InvalidClient for a real OAuth client error | `hspec:nhcore-test-auth:core/test/Auth/OAuth2/TokenRefreshSpec.hs#returns InvalidClient for a real OAuth client error` | integration | http:real |
| C3 | returns MalformedResponse for invalid JSON on token success | `hspec:nhcore-test-auth:core/test/Auth/OAuth2/TokenRefreshSpec.hs#returns MalformedResponse for invalid JSON on token success` | integration | http:real |
| C4 | returns MalformedResponse when token success lacks access_token | `hspec:nhcore-test-auth:core/test/Auth/OAuth2/TokenRefreshSpec.hs#returns MalformedResponse when token success lacks access_token` | integration | http:real |
| C5 | returns TokenRequestFailed for a non-JSON error response | `hspec:nhcore-test-auth:core/test/Auth/OAuth2/TokenRefreshSpec.hs#returns TokenRequestFailed for a non-JSON error response` | integration | http:real |
| C6 | decodes valid tokens and preserves their optional fields | `hspec:nhcore-test-auth:core/test/Auth/OAuth2/TokenRefreshSpec.hs#decodes valid tokens and preserves their optional fields` | integration | http:real |

## Error contract

- Successful HTTP status and valid token JSON yield the existing `TokenSet`.
- Successful status with malformed JSON or missing required fields yields existing
  `MalformedResponse`, with generic text rather than raw bytes/parser diagnostics.
- Non-success status with an OAuth error object uses existing `mapOAuthErrorCode`,
  preserving `InvalidGrant`, `InvalidClient`, `ScopeDenied`, and `TokenRequestFailed`.
- Non-success status without a valid OAuth error object yields `TokenRequestFailed`
  with generic status text; it must not expose response bytes.
- Transport failures remain sanitized `NetworkError`. Audit logging uses categories,
  never request credentials, raw response bodies or tokens. Provider error descriptions
  retain the existing mapping contract; they are not added to audit logs.

## Proposed implementation boundary

The maintainer must select one transport boundary at new Gate 1:

1. A private Auth byte-form helper copy-adapts `Http.Client.postFormIO` encoding,
   request options and sanitized transport handling. This keeps the empty API
   delta but duplicates transport plumbing inside Auth.
2. A narrowly scoped `Http.Client.Internal.postFormRaw` helper keeps transport
   ownership in HTTP and reuses existing private plumbing. Because Internal is an
   exposed module, this is an additive generated API change, not a private detail;
   choosing it requires an explicit signature amendment and applicable ADR review.

Both options use the same manager/TLS/proxy semantics and one shared private Auth
status-first decoder. Neither changes global JSON methods or catches global JSON
exceptions. No option is selected or implemented in this test/spec-only draft.
The exact transport and sanitization diff requires a fresh local security review
after new Gate 1 and before production implementation.

## User impact

OAuth refresh rejection reaches existing typed error handling instead of escaping
as a JSON exception. No application API or configuration migration is needed.
The owned Oura callback/error tests in prerequisite PR916 remain unchanged and
must pass against the integrated candidate before readiness. Full Auth and required
integration suites remain required; a targeted repro alone is not final readiness.

## ADR

Pending boundary choice. The no-export option has no trigger. Selecting the
exported raw form helper requires an explicit signature amendment and ADR review
before approval/implementation; the current empty API block does not authorize it.
