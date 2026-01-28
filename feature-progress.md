# Feature Progress: HTTP Outbound Integration (#317)

**Branch**: `feature/317-http-outbound-integration`
**Started**: 2026-01-28
**Status**: Complete - PR #318 Open

---

## Phase Checklist

- [x] **Phase 1: API Design**
  - [x] Review issue/ADR
  - [x] DevEx Lead proposes Jess-facing API
  - [x] API spec documented below

- [x] **Phase 2: Testbed Implementation**
  - [x] Dummy feature in testbed/src/Testbed/Examples/HttpIntegration.hs
  - [x] Compiles successfully

- [x] **Phase 3: Test Spec**
  - [x] Unit tests cover all public API and internal functions
  - [x] 72 tests in nhintegration-http-test, 11 tests in nhcore IntSpec

- [x] **Phase 4: Architecture Plan**
  - [x] Three-layer architecture (Public API, Core Logic, Infrastructure)
  - [x] Configurability ensured (env vars, retry config, auth options)

- [x] **Phase 5-8: Implementation Loop**
  - [x] Layer 1: Public API (Types & Helpers)
  - [x] Layer 2: Core Logic (ToAction & Retry)
  - [x] Layer 3: Infrastructure (no changes needed for v1)
  - [x] nhcore extensions: `Bytes.toBase64`, `Json.null`, `KnownSymbol`, `Int.powerOf`

- [x] **Phase 9: Validation**
  - [x] Package builds successfully
  - [x] All packages build together
  - [x] Testbed example compiles

- [x] **Phase 10: PR Review**
  - [x] pr-bot-review executed
  - [x] Critical issues fixed
  - [x] PR submitted: https://github.com/neohaskell/NeoHaskell/pull/318

---

## Known Limitations (v1)

These are documented and acceptable for the initial release:

| Limitation | Status | Future Issue |
|------------|--------|--------------|
| PUT/PATCH/DELETE unsupported | Returns "Unsupported method" error | TBD |
| Response.statusCode placeholder | Always returns 200 | TBD |
| Response.headers placeholder | Always returns empty | TBD |
| No streaming support | Large responses loaded in memory | TBD |
| No OAuth2 token refresh | Manual token management required | TBD |

---

## API Design

### Two-Persona Model (ADR-0015)

- **Jess** (Integration User): Configures HTTP requests with pure records
- **Nick** (Integration Developer): Implements ToAction with retries, auth, error handling

### Jess-facing API

```haskell
import Integration.Http qualified as Http

Integration.outbound Http.Request
  { method = Http.POST
  , url = "https://api.shippo.com/v1/shipments"
  , headers = []
  , body = Http.json
      { tracking_number = info.trackingNumber
      , carrier = info.carrier
      }
  , onSuccess = \response -> NotifyCustomer
      { orderId = order.id
      , trackingUrl = response.body
          |> Json.get "tracking_url_provider"
          |> Maybe.withDefault ""
      }
  , onError = Just (\err -> LogShippingError
      { orderId = order.id
      , error = err
      })
  , auth = Http.ApiKey "X-Api-Key" "${SHIPPO_API_KEY}"
  , retry = Http.defaultRetry
  , timeoutSeconds = 30
  }
```

---

## Testbed Examples

**File**: `testbed/src/Testbed/Examples/HttpIntegration.hs`

1. `orderShippingIntegration` - JSON body, API key auth, retries, error handling
2. `paymentRefundIntegration` - Form-encoded body, Bearer auth, custom retry count
3. `slackNotificationIntegration` - Fire-and-forget webhook, no retries

---

## Architecture

### Layer 1: Public API
- `Integration.Http` - Re-exports (Jess's entry point)
- `Integration.Http.Request` - Request, Method, Body types
- `Integration.Http.Response` - Response type
- `Integration.Http.Auth` - Auth type + helpers
- `Integration.Http.Retry` - Retry type + helpers

### Layer 2: Core Logic
- `Integration.Http.Internal` - ToAction instance (hidden from Jess)
  - Environment variable expansion (`${VAR_NAME}`)
  - Authentication header building
  - Exponential backoff with jitter
  - HTTP error to IntegrationError mapping

### Layer 3: Infrastructure
- `Http.Client` - Existing, no changes needed for v1

---

## Loop History

| Loop | Trigger | Changes Made |
|------|---------|--------------|
| 1 | Initial | Starting implementation |
| 2 | Build fix | Added nhcore extensions, fixed fmt syntax, replaced Task.attempt with Task.asResult |
| 3 | Example fix | Updated testbed example to use sum types, added Internal module import |
| 4 | Security fix | Redacted Show instance for Auth, removed ToJSON/FromJSON, fixed retry count |
| 5 | Unit tests | Added test-suite with 79 unit tests |
| 6 | Refactor | Moved powerOfTwo to nhcore as `Int.powerOf`, added 11 tests |

---

## Test Coverage

- **72 unit tests** in nhintegration-http (RetrySpec, RequestSpec, AuthSpec, InternalSpec)
- **11 unit tests** in nhcore for `Int.powerOf`

---

## Commits

| Hash | Message | Phase |
|------|---------|-------|
| ff0ed0f | feat(integrations): add HTTP outbound integration (ADR-0015) | 1-10 |
