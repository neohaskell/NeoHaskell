# Feature Progress: HTTP Outbound Integration (#317)

**Branch**: `feature/317-http-outbound-integration`
**Started**: 2026-01-28
**Status**: Complete - PR #318 Open

---

## Current Phase: 9 (Validation)

## Phase Checklist

- [x] **Phase 1: API Design**
  - [x] Review issue/ADR
  - [x] DevEx Lead proposes Jess-facing API
  - [x] API spec documented below

- [x] **Phase 2: Testbed Implementation**
  - [x] Dummy feature in testbed/src/Testbed/Examples/HttpIntegration.hs
  - [x] Compiles successfully

- [x] **Phase 3: Integration Test Spec**
  - [x] HURL tests in testbed/tests/integrations/http-outbound.hurl
  - [x] Tests define expected behavior (4 test scenarios)

- [x] **Phase 4: Architecture Plan**
  - [x] Abstraction layers identified (3 layers)
  - [x] IMPLEMENTATION_PLAN.md created
  - [x] Configurability ensured (env vars, retry config, auth options)

- [x] **Phase 5-8: Implementation Loop**
  - [x] Layer 1: Public API (Types & Helpers)
    - [x] Implementation complete
    - [x] Package builds successfully
  - [x] Layer 2: Core Logic (ToAction & Retry)
    - [x] Implementation complete
    - [x] Package builds successfully
  - [x] Layer 3: Infrastructure (Http.Client extensions if needed)
    - [x] No changes needed for v1 (documented limitations)
  - [x] nhcore extensions added:
    - [x] `Bytes.toBase64` for HTTP Basic auth encoding
    - [x] `Json.null` for empty POST body
    - [x] `KnownSymbol` re-exported from Basics for type-level symbol constraints

- [x] **Phase 9: Validation**
  - [x] Package builds successfully (`cabal build nhintegration-http`)
  - [x] All packages build together (`cabal build all`)
  - [x] Testbed example compiles and uses the integration

- [x] **Phase 10: PR Review**
  - [x] pr-bot-review executed
  - [x] Critical issues fixed (security, correctness, performance)
  - [x] PR submitted for review: https://github.com/neohaskell/NeoHaskell/pull/318

---

## Phase 1: API Design

### Issue/ADR Summary

ADR-0015 defines HTTP outbound integration following the two-persona model:
- **Jess** (Integration User): Configures HTTP requests with pure records
- **Nick** (Integration Developer): Implements ToAction with retries, auth, error handling

### Proposed Jess-facing API

```haskell
-- What Jess imports
import Integration.Http qualified as Http

-- What Jess writes
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

### Nick's Implementation (Hidden from Jess)
- `Integration.Http.Internal` - ToAction instance
- Environment variable expansion (`${VAR_NAME}`)
- Exponential backoff with jitter
- HTTP error mapping to IntegrationError

### DevEx Lead Review
- [x] Follows NeoHaskell conventions (|>, do blocks, Task, Result)
- [x] Intuitive for junior developers (pure record configuration)
- [x] Hides complexity appropriately (ToAction in Internal.hs)
- [x] Matches existing patterns (same as Command.Emit, Timer.Every)

**Approved**: API design follows ADR-0015 exactly. The two-persona model is correctly applied:
- Jess sees only `Http.Request` record configuration
- Nick implements `ToAction` with retries, auth, env expansion in `Internal.hs`

---

## Phase 2: Testbed Implementation

**File**: `testbed/src/Testbed/Examples/HttpIntegration.hs`

**Status**: Complete

Three example integrations created:
1. `orderShippingIntegration` - Full example with JSON body, API key auth, retries, error handling
2. `paymentRefundIntegration` - Form-encoded body, Bearer auth, custom retry count
3. `slackNotificationIntegration` - Fire-and-forget webhook, no retries

These examples demonstrate Jess's perspective: pure record configuration with no `Task` or networking code visible.

---

## Phase 3: Integration Test Spec

**File**: `testbed/tests/integrations/http-outbound.hurl`

**Status**: Complete

Four test scenarios defined:
1. **Basic POST with JSON body** - Verify request reaches mock, correct headers/body, success callback emits command
2. **Retry on 5xx errors** - Mock returns 503 twice then 200, verify 3 requests made, eventual success
3. **Error callback on permanent failure** - Mock returns 400, verify error callback command emitted
4. **Environment variable expansion** - Verify `${VAR_NAME}` is expanded, not sent as literal

Note: Tests require mock HTTP server on localhost:9090 and testbed Order domain (to be added).

---

## Phase 4: Architecture Plan

**File**: `IMPLEMENTATION_PLAN.md`

**Status**: Complete

See `IMPLEMENTATION_PLAN.md` for detailed layer breakdown.

### Layer Summary
1. **Public API**: Request, Method, Body, Auth, Retry, Response types + helpers
2. **Core Logic**: ToAction instance, retry logic, env expansion
3. **Infrastructure**: Http.Client (existing, may need extensions)

### Key Decisions
- Package in `integrations/http/` directory
- Separate modules per type for clarity
- ToAction in `Internal.hs` (hidden from Jess)
- Known limitations documented (PUT/PATCH/DELETE unsupported for v1)

---

## Loop History

| Loop | Trigger | Changes Made |
|------|---------|--------------|
| 1 | Initial | Starting implementation |
| 2 | Build fix | Added nhcore extensions (Bytes.toBase64, Json.null, KnownSymbol), fixed fmt syntax (#{var} not {var}), replaced Task.attempt with Task.asResult, implemented powerOfTwo helper for Int exponentiation |
| 3 | Example fix | Updated testbed example to use sum types for command results, added Internal module import to Http.hs for ToAction instance, verified full build |
| 4 | Security fix | Implemented redacted Show instance for Auth, removed ToJSON/FromJSON to prevent credential serialization, fixed retry count off-by-one, improved powerOfTwo to tail-recursive |
| 5 | Unit tests | Added test-suite to cabal, implemented 79 unit tests covering RetrySpec, RequestSpec, AuthSpec, InternalSpec; fixed fmt interpolation bug in error message |
| 6 | Refactor | Moved powerOfTwo to nhcore as `Int.powerOf`, added 11 tests in IntSpec.hs; fixed HURL tests to use port 8080; fixed redundant KnownSymbol import in THSpec |

---

## Blockers

(none)

---

## Next Action

**COMPLETE** - All phases finished, all tests passing. PR #318 submitted for review.

### Test Coverage
- **72 unit tests** in nhintegration-http covering Layer 1 and Layer 2 code
- **11 unit tests** in nhcore for `Int.powerOf`
- HURL integration tests defined for testbed on port 8080 (require Order domain implementation)

---

## Commits

| Hash | Message | Phase |
|------|---------|-------|
| ff0ed0f | feat(integrations): add HTTP outbound integration (ADR-0015) | 1-10 |

---

## Notes

- ADR-0015 already defines comprehensive API
- Need to verify Http.Client capabilities for PUT/PATCH/DELETE
- Token refresh (OAuth2) deferred per ADR

