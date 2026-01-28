# Feature Progress: HTTP Outbound Integration (#317)

**Branch**: `feature/317-http-outbound-integration`
**Started**: 2026-01-28
**Status**: Complete - Ready for Merge

---

## Phase Checklist

- [x] **Phase 1: API Design**
  - [x] Review issue/ADR
  - [x] DevEx Lead proposes Jess-facing API
  - [x] API spec documented below

- [x] **Phase 2: Testbed Implementation**
  - [x] Dummy feature in testbed/src/Testbed/Examples/HttpIntegration.hs
  - [x] Add examples using PUT/PATCH/DELETE methods
  - [x] Add example checking real statusCode
  - [x] Add example using real headers
  - [x] Add example with raw body

- [x] **Phase 3: Test Spec**
  - [x] Unit tests cover all public API and internal functions
  - [x] 72 tests in nhintegration-http-test, 11 tests in nhcore IntSpec (792 total)
  - [x] All existing tests pass with new Response type

- [x] **Phase 4: Architecture Plan**
  - [x] Three-layer architecture (Public API, Core Logic, Infrastructure)
  - [x] Configurability ensured (env vars, retry config, auth options)
  - [x] Plan for Http.Client extensions (Loop 7) - see IMPLEMENTATION_PLAN.md

- [x] **Phase 5-8: Implementation Loop**
  - [x] Layer 1: Public API (Types & Helpers)
  - [x] Layer 2: Core Logic (ToAction & Retry)
  - [x] Layer 3: Infrastructure (Http.Client extensions)
    - [x] Add `Response` type with statusCode/headers/body
    - [x] Update `get`, `post`, `postForm` to return `Response`
    - [x] Add `put`, `patch`, `delete` methods
    - [x] Add `postRaw` for raw body support
  - [x] nhcore extensions: `Bytes.toBase64`, `Json.null`, `KnownSymbol`, `Int.powerOf`

- [x] **Phase 9: Validation**
  - [x] Package builds successfully
  - [x] All packages build together (nhcore, nhintegration-http, testbed)
  - [x] Testbed example compiles
  - [x] All 792 nhcore tests pass
  - [x] All 72 nhintegration-http tests pass

- [x] **Phase 10: PR Review**
  - [x] pr-bot-review executed (Loop 6)
  - [x] Final review after workaround fixes (Loop 7)
    - Security & Correctness: APPROVE (0 blocking, 3 warnings)
    - Code Quality: APPROVE (0 blocking, 5 warnings)
    - All warnings are non-blocking stylistic improvements

---

## Workarounds Fixed (Loop 7)

| Issue | Previous Behavior | Fix |
|-------|------------------|-----|
| PUT/PATCH/DELETE unsupported | Returned `IntegrationError` | Added `put`, `patch`, `delete` to Http.Client |
| Response.statusCode placeholder | Always 200 | Now extracts real status from HTTP response |
| Response.headers placeholder | Always empty | Now extracts real headers from HTTP response |
| Raw body not supported | Returned error | Added `postRaw` to Http.Client |

### Acceptable Limitations (Not Changed)
| Limitation | Reason |
|------------|--------|
| No streaming support | Requires significant Http.Client redesign |
| No OAuth2 token refresh | Complex state management, out of scope |

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

### Original Examples (Loop 1-6)
1. `orderShippingIntegration` - JSON body, API key auth, retries, error handling
2. `paymentRefundIntegration` - Form-encoded body, Bearer auth, custom retry count
3. `slackNotificationIntegration` - Fire-and-forget webhook, no retries

### New Examples (Loop 7)
4. `updateUserProfileIntegration` - PUT method for full resource replacement
5. `partialUpdateOrderIntegration` - PATCH method for partial updates
6. `cancelSubscriptionIntegration` - DELETE method for resource removal
7. `conditionalUpdateIntegration` - Uses real statusCode and headers (ETag)
8. `xmlOrderIntegration` - Raw body with custom content type (XML)

---

## Architecture

### Layer 1: Public API
- `Integration.Http` - Re-exports (Jess's entry point)
- `Integration.Http.Request` - Request, Method, Body types
- `Integration.Http.Response` - Response type with statusCode/headers/body
- `Integration.Http.Auth` - Auth type + helpers
- `Integration.Http.Retry` - Retry type + helpers

### Layer 2: Core Logic
- `Integration.Http.Internal` - ToAction instance (hidden from Jess)
  - Environment variable expansion (`${VAR_NAME}`)
  - Authentication header building
  - Exponential backoff with jitter
  - HTTP error to IntegrationError mapping
  - All HTTP methods (GET, POST, PUT, PATCH, DELETE)

### Layer 3: Infrastructure (Extended in Loop 7)
- `Http.Client` - Extended with:
  - `Response` type with statusCode/headers/body
  - `put`, `patch`, `delete` methods
  - `postRaw` for raw body support
  - All methods now return `Response` instead of just body

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
| 7 | Fix workarounds | Added Response type with real statusCode/headers, added PUT/PATCH/DELETE/postRaw |

---

## Test Coverage

- **72 unit tests** in nhintegration-http (RetrySpec, RequestSpec, AuthSpec, InternalSpec)
- **11 unit tests** in nhcore for `Int.powerOf`

---

## Commits

| Hash | Message | Phase |
|------|---------|-------|
| ff0ed0f | feat(integrations): add HTTP outbound integration (ADR-0015) | 1-10 |
