# ADR-0065: Azure Communication Services email integration

## Status

Proposed

## Context

### Current state

NeoHaskell ships exactly one transactional email integration: `Integration.Brevo`
(ADR-0015 two-persona model, `integrations/Integration/Brevo/`). Jess configures a
pure `Brevo.Request` record with smart constructors (`Brevo.sender`,
`Brevo.recipient`, `Brevo.HtmlBody`) and calls `Integration.outbound` in an
outbound handler; Nick's `Integration.Brevo.Internal.toHttpRequest` turns that
record into an `Http.Request`, unwraps the `Redacted` API key once, and dispatches
on the HTTP status code.

Teams already standardized on Microsoft Azure have no first-class path to send the
same transactional emails through Azure Communication Services (ACS). Today they
either pull in Brevo as a second vendor purely for email, or hand-roll an
`Integration.Http` request against the ACS REST endpoint — losing the
two-persona ergonomics, the `Redacted` secret discipline, and the
status-code sanitization that Brevo gives for free.

ACS authenticates email sends with a Microsoft Entra ID Bearer token
(`Authorization: Bearer <token>`). ACS also offers an HMAC-signed
connection-string scheme, but that path requires the caller to compute a
SHA-256 content hash and an HMAC-SHA256 signature per request — cryptography
vocabulary that fails the Jess test by construction. This ADR scopes the
connection-string/HMAC path **out** and supports Bearer-only, with the token
supplied by the caller as `Redacted Text` (Azure SDKs and the `az` CLI already
mint these tokens, so Jess never touches crypto).

### Use cases

- **Azure-native SaaS sign-up.** Jess runs her app on Azure Container Apps and
  already has an Entra-managed identity. She wants `OrderConfirmed` to send a
  receipt through her existing ACS Email resource instead of onboarding a second
  email vendor.
- **Single-vendor billing.** A team consolidating cloud spend under one Azure
  invoice replaces Brevo with ACS for password-reset and notification emails,
  changing only the smart-constructor module name in their outbound handler.
- **Async send with operation tracking.** ACS accepts an email and returns
  `202 Accepted` with an `operationId`; an operator wants that id surfaced in a
  domain event so a later phase can poll delivery status (polling itself is out
  of scope here, but the id must be captured).

### Design drivers

1. **Mirror Brevo exactly.** Jess already learned `Brevo.send (Brevo.sender …)
   (Brevo.recipient …) subject (Brevo.HtmlBody …) onSuccess onError |>
   Integration.outbound`. The ACS spelling must be identical apart from the
   module name and one extra `endpoint` argument, so she reuses one mental model.
2. **Two-persona split (ADR-0015).** Facade `Integration.Acs` exposes only pure
   records and smart constructors; all HTTP/IO/Task lives in
   `Integration.Acs.Internal`, which is not re-exported.
3. **One secret-unwrap site.** The Entra Bearer token is `Redacted Text`
   everywhere except a single `Redacted.unwrap` call inside
   `Internal.toHttpRequest`, matching Brevo's single-unwrap invariant.
4. **No cryptography.** Bearer-only. No HMAC, no connection-string parsing, no
   content hashing. Anything that would make Jess read a crypto doc is deferred.
5. **Sanitized error text.** A failed send never echoes the token or the raw
   response body back to the caller; `onError` receives a short,
   status-derived message.
6. **Pass the Jess 15-minute test.** Sending one ACS email must take ≤ 15 minutes
   from the moment Jess opens autocomplete, with no flag or pragma.

## Considered options

### Option A — Mirror `Integration.Brevo` module-for-module, Bearer-only (CHOSEN)

Add `Integration.Acs` (facade), `Integration.Acs.Request`,
`Integration.Acs.Response`, and `Integration.Acs.Internal`, one-for-one with the
Brevo layout. `Acs.Body` is a two-constructor sum (`HtmlBody Text | TextBody Text`),
`send` takes the same arguments as `Brevo.send` plus a leading ACS `endpoint`
`Text`, and reads the Bearer token from `?config.acsAccessToken`. The Internal
module builds the request to `POST {endpoint}/emails:send?api-version=2023-03-31`,
attaches `Http.Bearer` auth, and dispatches the `202` success and the
`401/403/429/other` error cases.

Verdict: **chosen.** Maximum reuse of an already-shipped, already-reviewed
pattern. Jess transfers her Brevo knowledge verbatim. The single-unwrap and
sanitized-error invariants come along for free because the code shape is
identical. The only genuinely new code is the ACS-specific JSON wire shape and
the `202`-with-`operationId` success branch.

### Option B — Generic "EmailProvider" abstraction over Brevo and ACS (REJECTED)

Introduce a provider-agnostic `Email.send` that dispatches to Brevo or ACS via a
typeclass or a sum of provider configs, so Jess writes one call and swaps
providers by config.

Rejected for three reasons:

1. **Premature.** With exactly two providers and no third on the roadmap, the
   abstraction is speculative. The two wire formats differ in body shape, success
   status (`201` vs `202`), and success payload (`messageId` vs `operationId`),
   so the "common" interface would leak provider specifics immediately.
2. **Worse Jess ergonomics.** A generic facade forces Jess to choose and
   configure a provider variant — an extra decision the per-provider modules
   avoid. The autocomplete path `Acs.send …` is shorter and more discoverable
   than `Email.send (Email.Acs …) …`.
3. **Out of scope.** ADR-0015 deliberately keeps each integration self-contained
   so it can later move to its own repo. A cross-provider abstraction couples
   them. If a real third provider arrives, that is the moment to extract a shared
   interface — a separate, evidence-driven ADR.

### Option C — Support the ACS connection-string / HMAC auth path (REJECTED)

Accept the ACS connection string and compute the per-request HMAC-SHA256
signature and SHA-256 content hash that the non-Entra ACS scheme requires.

Rejected because it fails the Jess test on contact: the caller (or the framework)
must understand content hashing, signing strings, and clock-skew windows. It also
multiplies the secret surface (connection string + derived signing key) and the
error modes (signature mismatch vs expired token). Entra Bearer tokens are minted
by the Azure tooling Jess already runs, so Bearer-only covers the use cases
without any cryptography. HMAC can be added later behind the same facade if a
concrete need appears.

## Decision

Adopt Option A. Concretely:

### 1. Module layout (mirrors Brevo)

| Module | Audience | Contents |
|---|---|---|
| `Integration.Acs` | Jess | Re-exports `Request (..)`, `Body (..)`, `Sender (..)`, `Recipient (..)`, smart constructors `send`/`sender`/`recipient`, and `Response (..)` |
| `Integration.Acs.Request` | Jess | `Request` record, `Body` sum, `Address`/`Sender`/`Recipient` types, smart constructors |
| `Integration.Acs.Response` | Jess | `Response { operationId :: Text }` |
| `Integration.Acs.Internal` | Nick | `toHttpRequest`, `encodeRequest`, `AcsResponseHandler`, status dispatch — **not re-exported** |

`Integration.Acs.Internal` is the single module that touches `Integration.Http`
and the single site that calls `Redacted.unwrap`.

### 2. Request types and smart constructors

The address shape mirrors Brevo so a later display-name addition is a
non-breaking change. ACS's wire contract names the recipient key `address`
(not `email`), so the hand-written encoder maps the field; the Haskell-facing
field stays `email` for Brevo parity.

```haskell
-- | A wire-format email address. The display name is optional and currently
-- always 'Nothing' from the smart constructors; kept for forward-compat.
data Address = Address
  { name :: Maybe Text
  , email :: Text
  }
  deriving (Eq, Show, Generic)


-- | The sender address. A newtype to stop accidental swaps with 'Recipient'.
newtype Sender = Sender Address deriving (Eq, Show)


-- | A recipient address. A newtype to stop accidental swaps with 'Sender'.
newtype Recipient = Recipient Address deriving (Eq, Show)


-- | Mutually-exclusive email body. ACS rejects a request that sets both
-- @html@ and @plainText@; the sum enforces the invariant at compile time.
data Body
  = HtmlBody Text
  | TextBody Text
  deriving (Eq, Show, Generic)


-- | A complete ACS email send. 'accessToken' is 'Redacted Text' so the Entra
-- Bearer token is never logged or serialized by accident.
data Request command = Request
  { endpoint :: Text
  , sender :: Sender
  , to :: Array Recipient
  , subject :: Text
  , body :: Body
  , accessToken :: Redacted Text
  , onSuccess :: Response -> command
  , onError :: Text -> command
  }
```

Smart constructors build the single-recipient happy path, matching
`Brevo.sender` / `Brevo.recipient`:

```haskell
-- | Build a sender from just an email address (no display name).
sender :: Text -> Sender
sender email =
  Sender (Address { name = Nothing, email })


-- | Build a recipient from just an email address (no display name).
recipient :: Text -> Recipient
recipient email =
  Recipient (Address { name = Nothing, email })
```

### 3. `send` reads the token from config

`send` takes the ACS resource `endpoint` as its first argument (it varies per
Azure resource and is not a secret), then the same arguments as `Brevo.send`.
The Bearer token comes from the project config via the implicit `?config`
parameter and a `HasField "acsAccessToken"` constraint, mirroring Brevo's
`brevoApiKey` read.

```haskell
send ::
  forall command config.
  ( ?config :: config
  , HasField "acsAccessToken" config (Redacted Text)
  ) =>
  Text ->
  Sender ->
  Recipient ->
  Text ->
  Body ->
  (Response -> command) ->
  (Text -> command) ->
  Request command
send endpointVal senderVal recipientVal subjectVal bodyVal onSuccess onError =
  Request
    { endpoint = endpointVal
    , sender = senderVal
    , to = Array.wrap recipientVal
    , subject = subjectVal
    , body = bodyVal
    , accessToken = ?config.acsAccessToken
    , onSuccess
    , onError
    }
```

### 4. `toHttpRequest` — the single unwrap site

`Internal.toHttpRequest` is the only place that unwraps the token and the only
place that constructs the HTTP request. It targets the ACS Email send route with
the pinned API version and attaches `Http.Bearer`, which produces
`Authorization: Bearer <token>`.

```haskell
toHttpRequest ::
  forall command.
  Request command ->
  Http.Request command
toHttpRequest req =
  do
    let url = [fmt|#{req.endpoint}/emails:send?api-version=2023-03-31|]
    let tokenValue = Redacted.unwrap req.accessToken
    let encodedBody = encodeRequest req
    let handler = AcsResponseHandler { onSuccess = req.onSuccess, onError = req.onError }
    Http.Request
      { method = Http.POST
      , url
      , headers = Array.empty
      , body = Http.raw "application/json" encodedBody
      , onSuccess = handleAcsResponse handler
      , onError = Just req.onError
      , auth = Http.Bearer tokenValue
      , retry = Http.noRetry
      , timeoutSeconds = 30
      }
```

### 5. Hand-built JSON wire shape

The encoder is hand-built (like `Brevo.encodeRequest`) so optional fields are
omitted entirely rather than serialized as `null`. The body variant chooses the
`html` or `plainText` key inside `content`; the recipient list is
`recipients.to` of `{ address }` objects; the sender is a plain
`senderAddress` string.

```haskell
-- Target shape:
-- { "senderAddress": "noreply@myapp.com"
-- , "content": { "subject": "Welcome", "html": "<h1>Hi</h1>" }
-- , "recipients": { "to": [ { "address": "user@example.com" } ] }
-- }
encodeRequest ::
  forall command.
  Request command ->
  Text
encodeRequest req =
  do
    let contentFields =
          encodeBodyField req.body
            |> Array.pushFront ("subject", Json.encode req.subject)
            |> Array.toLinkedList
    let toEntries =
          req.to
            |> Array.map encodeRecipientAddress
    let allFields =
          [ ("senderAddress", Json.encode (senderEmail req.sender))
          , ("content", Json.object contentFields)
          , ("recipients", Json.object [("to", Json.encode toEntries)])
          ]
    Json.encodeText (Json.object allFields)


-- | Pick the ACS body key for the chosen 'Body' variant.
encodeBodyField :: Body -> Array (Text, Json.Value)
encodeBodyField body =
  case body of
    HtmlBody html ->
      Array.fromLinkedList [("html", Json.encode html)]
    TextBody text ->
      Array.fromLinkedList [("plainText", Json.encode text)]
```

### 6. Status dispatch — `202` success, sanitized errors

`handleAcsResponse` mirrors `handleBrevoResponse` but treats `202 Accepted` as
success (ACS sends are asynchronous) and decodes the `operationId`. Auth failures
and throttling get explicit messages; every other `4xx`/`5xx` collapses to a
status-only message so neither the token nor the raw body can leak.

```haskell
handleAcsResponse ::
  forall command.
  AcsResponseHandler command ->
  Http.Response ->
  command
handleAcsResponse handler response =
  case response.statusCode of
    202 ->
      case Json.decode response.body of
        Result.Ok acsResponse ->
          handler.onSuccess acsResponse
        Result.Err _ ->
          handler.onError "Failed to parse ACS response"
    401 ->
      handler.onError "Authentication failed (HTTP 401): unauthorized"
    403 ->
      handler.onError "Authorization failed (HTTP 403): unauthorized"
    429 ->
      handler.onError "Rate limit exceeded (HTTP 429): throttled"
    status | status >= 400 && status < 500 ->
      handler.onError [fmt|ACS client error (HTTP #{status})|]
    status | status >= 500 ->
      handler.onError [fmt|ACS server error (HTTP #{status})|]
    status ->
      handler.onError [fmt|Unexpected HTTP status #{status}|]
```

### 7. Response

```haskell
-- | ACS's success payload for an accepted email send. ACS is asynchronous;
-- 'operationId' identifies the send for later delivery-status polling.
data Response = Response
  { operationId :: Text
  }
  deriving (Eq, Show, Generic)
```

### 8. Configuration

Jess adds one config field, exactly like `brevoApiKey`:

```haskell
Config.field @(Redacted Text) "acsAccessToken"
  |> Config.doc "Azure Communication Services Entra ID Bearer token"
  |> Config.required
  |> Config.envVar "ACS_ACCESS_TOKEN"
  |> Config.secret
```

### 9. Module placement

| Change | File |
|---|---|
| Facade re-exporting Request/Body/Sender/Recipient/constructors/Response | `integrations/Integration/Acs.hs` |
| Request record, `Body`, `Address`/`Sender`/`Recipient`, smart constructors | `integrations/Integration/Acs/Request.hs` |
| `Response { operationId }` | `integrations/Integration/Acs/Response.hs` |
| `toHttpRequest`, `encodeRequest`, `AcsResponseHandler`, status dispatch | `integrations/Integration/Acs/Internal.hs` |
| Expose the four modules under `exposed-modules` | `integrations/nhintegrations.cabal` |
| Register `Integration.Acs.RequestSpec`, `ResponseSpec`, `InternalSpec` | `integrations/nhintegrations.cabal` (test suite `other-modules`) |
| Request encoding / smart-constructor tests | `integrations/test/Integration/Acs/RequestSpec.hs` |
| Response decode test | `integrations/test/Integration/Acs/ResponseSpec.hs` |
| `toHttpRequest` + status-dispatch tests | `integrations/test/Integration/Acs/InternalSpec.hs` |

## Public API

```haskell
-- Integration.Acs (Jess's surface)

-- | Build a sender from an email address.
sender :: Text -> Sender

-- | Build a recipient from an email address.
recipient :: Text -> Recipient

-- | Mutually-exclusive email body.
data Body
  = HtmlBody Text
  | TextBody Text

-- | The accepted-send payload; carries the ACS operation id.
data Response = Response { operationId :: Text }

-- | Send one transactional email through an ACS Email resource.
-- Reads the Entra Bearer token from @?config.acsAccessToken@.
send ::
  forall command config.
  ( ?config :: config
  , HasField "acsAccessToken" config (Redacted Text)
  ) =>
  Text ->                    -- ^ ACS resource endpoint, e.g. "https://my-acs.communication.azure.com"
  Sender ->
  Recipient ->
  Text ->                    -- ^ subject
  Body ->
  (Response -> command) ->   -- ^ on 202 Accepted
  (Text -> command) ->       -- ^ on sanitized error
  Request command
```

Jess's call site, identical to Brevo apart from the module name and the leading
endpoint:

```haskell
Acs.send
  "https://my-acs.communication.azure.com"
  (Acs.sender "noreply@myapp.com")
  (Acs.recipient info.email)
  "Your order is confirmed"
  (Acs.HtmlBody "<h1>Thank you</h1>")
  (\response -> EmailSent { operationId = response.operationId })
  (\err -> EmailFailed { reason = err })
  |> Integration.outbound
```

## Consequences

### Positive

- **Brevo knowledge transfers verbatim.** Jess who has sent a Brevo email can
  send an ACS email in under 15 minutes; the only new things to learn are the
  module name and the `endpoint` argument, both visible from autocomplete.
- **Secret discipline is preserved.** The Entra token is `Redacted Text`
  end-to-end with one unwrap site, so it cannot be logged or serialized by
  accident, and a `Show` on the request prints `<redacted>`.
- **No cryptography reaches the user.** Bearer-only means Jess never sees a
  hash, signing string, or HMAC key.
- **Errors are safe by default.** `onError` receives a short, status-derived
  string; the token and raw response body are never echoed back.
- **Single-vendor Azure path.** Teams consolidating on Azure drop a second email
  vendor and keep the same outbound-handler ergonomics.

### Negative

- **A second email integration to maintain.** Brevo and ACS now both exist with
  duplicated-but-divergent encoders and dispatchers; a future bug fix to one
  does not automatically apply to the other.
- **Token lifecycle is the caller's job.** Entra Bearer tokens expire; Jess must
  refresh `ACS_ACCESS_TOKEN` out of band. An expired token surfaces only as a
  `401` at send time, not at config load.
- **No delivery confirmation.** A `202 Accepted` means ACS queued the email, not
  that it was delivered; the `operationId` is captured but polling is out of
  scope.

### Risks

- **Token expiry mistaken for a code bug.** A `401` from a stale token looks like
  an integration failure to an operator who does not know Entra tokens are
  short-lived.
- **Endpoint typo produces an opaque failure.** A malformed `endpoint` argument
  (wrong region, missing scheme) yields a generic client/connection error rather
  than a guided message.
- **ACS API-version drift.** The `2023-03-31` API version is pinned in
  `toHttpRequest`; a future ACS breaking change would require a code update.

### Mitigations

- The `401`/`403` branch returns the explicit text `unauthorized`, and the ADR
  and module haddock state that the token is caller-managed and short-lived, so
  the most common operational failure has a documented cause.
- The `endpoint` is a plain function argument (not buried in config), so a typo
  is visible at the call site Jess is already editing.
- The pinned API version lives at the single `toHttpRequest` URL site, so a
  future bump is a one-line change with a regression test already in place.
- HMAC/connection-string auth, delivery-status polling, attachments, and
  cc/bcc/reply-to remain explicitly out of scope; each can be added later behind
  the same facade as evidence-driven follow-up ADRs.

## References

- [#698: Azure Communication Services email integration](https://github.com/neohaskell/NeoHaskell/issues/698)
- [ADR-0015: HTTP outbound integration two-persona model](0015-http-outbound-integration.md)
- [ADR-0008: Integration pattern](0008-integration-pattern.md)
- [integrations/Integration/Brevo.hs](../../integrations/Integration/Brevo.hs)
- [integrations/Integration/Brevo/Request.hs](../../integrations/Integration/Brevo/Request.hs)
- [integrations/Integration/Brevo/Response.hs](../../integrations/Integration/Brevo/Response.hs)
- [integrations/Integration/Brevo/Internal.hs](../../integrations/Integration/Brevo/Internal.hs)
- [integrations/Integration/Http/Auth.hs](../../integrations/Integration/Http/Auth.hs)
</content>
</invoke>
