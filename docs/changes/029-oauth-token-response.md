# Change 029: Return typed OAuth errors for token responses

OAuth token requests decoded a success token before they checked the HTTP
status. An owned HTTP 400 `invalid_grant` response threw an uncaught
`JSONException` and bypassed the existing OAuth and integration error paths.
The maintainer approved the HTTP-owned `postFormRaw` helper and the shared OAuth
decoder (NickSeagull,
[Gate 1](https://github.com/neohaskell/NeoHaskell/pull/917#issuecomment-6852482246)).
The approval requires the existing transport behavior to stay the same.

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

`Http.Client.Internal` gains one export. `postFormRaw` sends the form request
that `Http.Client.postForm` sends, through the same connection manager. It
returns the status code, the headers, and the undecoded body of any response.
`Http.Client.postForm` and every other HTTP method keep their signatures and
their behavior.

The private token-request path in `Auth.OAuth2.Client` reads the status and the
bytes, classifies the status, then decodes either `TokenResponse` or
`TokenErrorResponse`. The six token entrypoints share this one decoder. Those
are `exchangeCode`, `exchangeCodeWithPkce`, `refreshToken`, and their
`Validated` variants. Existing URL and DNS validation, form encoding and field
values, TLS verification, proxy behavior, the 10-second response timeout, and
zero redirects remain unchanged.

```diff signatures
+ Http.Client.Internal: postFormRaw :: Request -> Array (Text, Text) -> Task Error (Response Bytes)
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
| C7 | `postFormRaw` returns every status with its undecoded body | `hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#postFormRaw returns a non-2xx status with its exact body bytes`<br>`hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#postFormRaw returns a 2xx body without decoding it` | integration | http:real |
| C8 | `postFormRaw` sends the form request that `Http.postForm` sends | `hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#postFormRaw POSTs the fields url-encoded in caller order`<br>`hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#postFormRaw sends the same method, content type and body as Http.postForm`<br>`hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#postFormRaw sends caller headers and no Accept header of its own` | integration | http:real |
| C9 | `postFormRaw` honors the redirect, size, and timeout options | `hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#postFormRaw returns a redirect status without contacting its target`<br>`hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#postFormRaw follows a redirect only when the request allows one`<br>`hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#postFormRaw enforces maxResponseBytes on the response body`<br>`hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#postFormRaw stops at the response timeout with a sanitized error` | integration | http:real |
| C10 | `postFormRaw` transport errors carry no form field, URL path, or URL query | `hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#postFormRaw keeps form fields and the URL query out of a connection error`<br>`hspec:nhcore-test-core:core/test/Http/ClientRawSpec.hs#postFormRaw reports an invalid URL without its path or query` | unit | none |
| C11 | Every token exchange entrypoint and the per-request refresh share the response contract | `hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#returns the tokens of a 200 response with their optional fields`<br>`hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#returns InvalidGrant for a 400 invalid_grant response`<br>`hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#returns InvalidClient for a 401 invalid_client response`<br>`hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#returns MalformedResponse for a 200 response that is not JSON`<br>`hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#returns MalformedResponse for a 200 response without access_token`<br>`hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#returns TokenRequestFailed for a non-JSON error status` | integration | http:real |
| C12 | The decoder classifies the status before it reads the body | `hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#does not accept tokens from an error status`<br>`hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#accepts tokens from any 2xx status`<br>`hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#returns a redirect status without following it`<br>`hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#returns TokenRequestFailed for an error status whose JSON is not an OAuth error` | integration | http:real |
| C13 | OAuth error objects keep the existing `mapOAuthErrorCode` mapping | `hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#returns ScopeDenied for an invalid_scope response`<br>`hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#returns TokenRequestFailed with the provider description for other OAuth error codes`<br>`hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#uses the error code when the provider sends no description` | integration | http:real |
| C14 | A refused connection is a sanitized `NetworkError` | `hspec:nhcore-test-auth:core/test/Auth/OAuth2/ClientSpec.hs#returns a sanitized NetworkError when the endpoint refuses the connection` | unit | none |

C7 to C9 and C11 to C13 send real HTTP over loopback to an in-process Warp
server on a port that Warp binds. `docs/changes/test-surfaces.json` attests each
selector. Each C11 selector matches five examples, one for each of
`exchangeCode`, `exchangeCodeWithPkce`, `exchangeCodeValidated`,
`exchangeCodeWithPkceValidated`, and `refreshToken`. C1 to C6 prove the same
contract for `refreshTokenValidated`. Every C11 to C13 example also checks that
the endpoint received exactly one POST with the expected form fields, the form
content type, and `Accept: application/json`. C10 and C14 stay `unit` and
`none` because no fixture server answers the request.

## Error contract

A successful status is any status from 200 to 299.

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

## Implementation boundary

The maintainer selected the HTTP-owned boundary at Gate 1.
`Http.Client.Internal.postFormRaw` keeps transport ownership in HTTP. The
rejected alternative was a private Auth helper that copied the form encoding,
the request options, and the sanitized transport handling out of
`Http.Client.postForm`.

`postFormRaw` builds its request in the order that `Http.Client.postForm` uses.
It sets the POST method, the form content type, and the url-encoded fields, then
applies the request options. It sends the request through the process-wide
connection manager that `Http.Client.postForm` uses. TLS verification and
environment-proxy handling are therefore unchanged for OAuth token requests.
`getRaw` and `sendRaw` use a separate manager that ignores environment proxies.
`postFormRaw` does not use that manager, because the switch would change the
proxy behavior of existing deployments.

The form encoding of `Http.Client.postForm` is private to `Http.Client`.
`postFormRaw` calls the same library encoder through a private helper, so that
`Http.Client` gains no export. A C8 test compares the method, the content type,
and the body bytes of both functions on the wire.

`postFormRaw` shares one bounded response reader with `sendRaw`. The reader
returns every status as a value, stops at `maxResponseBytes`, and closes the
response when the caller is cancelled. `sendRaw` and `getRaw` keep their request
construction, their manager, and their results.

The Auth decoder is private to `Auth.OAuth2.Client`. `postTokenRequest` sends
the request and `decodeTokenResponse` classifies the response. Both
`requestTokens` and `requestTokensValidated` call `postTokenRequest`, so no
token entrypoint has its own decoding. The decoder replaces a heuristic that
searched transport error messages for a JSON object. Transport error messages
never contain a response body, so that heuristic could not match.

The repair changes two details of a token request on the wire or at its limits:

- `Auth.OAuth2.Client` now sets `Accept: application/json` itself. The JSON
  helper that `Http.Client.postForm` uses added that header before. `postFormRaw`
  adds no `Accept` header, so the token request keeps the header it always sent.
- The default 10 MB `maxResponseBytes` limit now applies to a token response. A
  larger response returns a sanitized `NetworkError`. The JSON path had no limit.

Neither `Http.Client.postForm` nor any other global JSON method changes, and no
code catches the global JSON exceptions. The implementer reviewed the transport
and sanitization diff before the implementation was pushed. The review record
is local-only under [ADR-0069](../decisions/0069-security-reviews-are-local.md).

## User impact

A rejected token exchange or token refresh reaches the existing typed error
handling instead of escaping as a JSON exception. The OAuth2 callback route
now calls `onFailure` for a provider rejection, and `withValidToken` returns
`RefreshFailed`. No application API or configuration migration is needed.
The owned Oura callback/error tests in prerequisite PR916 remain unchanged and
must pass against the integrated candidate before readiness. Full Auth and required
integration suites remain required; a targeted repro alone is not final readiness.

No starter or testbed code imports `Http.Client.Internal` or calls
`Http.Client.postForm`, so no consumer needs a migration. Direct importers of
`Http.Client.Internal` gain `postFormRaw` and lose nothing.

## ADR

Not required. No trigger applies, because breaking, new-dependency,
new-capability, and new-extension-point are all false. The boundary choice
required an ADR review. The review found that the export follows an accepted
decision and an earlier change.
[ADR-0036](../decisions/0036-wave1-security-hardening.md) moved `getRaw` to
`Http.Client.Internal`, so that requests which do not enforce HTTPS need an
explicit import. Change 016 added `sendRaw` to the same module without a new
ADR. `postFormRaw` adds one more request of that kind to the same module.
