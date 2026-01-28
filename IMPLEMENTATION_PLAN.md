# Implementation Plan: OpenRouter Integration

## Overview

OpenRouter integration piggybacks on the existing HTTP integration.
Jess configures `OpenRouter.Request`, which transforms to `Http.Request` via `toHttpRequest`.

## Layers (Top to Bottom)

### Layer 1: Public API (Jess sees this)

**Modules:**
- `Integration.OpenRouter` - Re-exports all public types and functions
- `Integration.OpenRouter.Message` - Message and Role types with smart constructors
- `Integration.OpenRouter.Request` - Request and Config types
- `Integration.OpenRouter.Response` - Response, Choice, Usage, FinishReason types

**Status:** Implemented

### Layer 2: Core Logic (Nick's code)

**Modules:**
- `Integration.OpenRouter.Internal` - toHttpRequest transformation

**Responsibilities:**
- Transform OpenRouter.Request → Http.Request
- Build JSON request body
- Build headers (Referer, X-Title)
- Parse API response
- Route success/error callbacks

**Status:** Implemented

### Layer 3: nhcore Extensions

**Modules:**
- `core/json/Json.hs` - Extended to export JSON parsing/encoding helpers

**New Exports:**
- `withText`, `withObject` - Parsing combinators
- `.:`, `.:?`, `.!=` - Field extraction
- `yield`, `fail` - Parser return/error
- `object`, `.=` - JSON object building
- `FromJSON (..)`, `ToJSON (..)` - Class methods

**Status:** Implemented

## Task Checklist

### Layer 1: Public API
- [x] Message.hs - Role type, Message type, smart constructors
- [x] Request.hs - Config type, Request type, defaultConfig, chatCompletion
- [x] Response.hs - Response, Choice, Usage, FinishReason types
- [x] OpenRouter.hs - Re-export module

### Layer 2: Core Logic
- [x] Internal.hs - toHttpRequest function
- [x] Internal.hs - buildRequestBody function
- [x] Internal.hs - buildHeaders function
- [x] Internal.hs - handleSuccess function
- [x] Internal.hs - handleError function
- [x] Internal.hs - RequestBody ToJSON instance

### Layer 3: nhcore Extensions
- [x] Json.hs - Add parsing helpers (withText, withObject, etc.)
- [x] Json.hs - Add encoding helpers (object, .=)
- [x] Json.hs - Export FromJSON/ToJSON with methods

### Integration
- [x] Merge http/ and openrouter/ into single integrations/ package
- [x] Update cabal.project
- [x] Update testbed dependency (nhintegrations)
- [x] Testbed example compiles

### Tests
- [ ] MessageSpec.hs - Message JSON encoding/decoding
- [ ] ResponseSpec.hs - Response JSON decoding
- [ ] InternalSpec.hs - toHttpRequest transformation

### Validation
- [ ] `cabal build all` passes
- [ ] `cabal test nhintegrations-test` passes
- [ ] hlint passes

## Design Decisions

1. **Piggyback pattern**: OpenRouter returns Http.Request, reusing Http integration
2. **No streaming v1**: Simpler implementation first
3. **Environment-based auth**: `${OPENROUTER_API_KEY}` placeholder
4. **Callback pattern**: Matches Http integration (onSuccess/onError)
5. **Optional fields in JSON**: Only include when Just, use getJusts
6. **Smart constructor**: `chatCompletion` for simple cases, full `Request` for advanced

## Files Modified

### New Files
- `integrations/Integration/OpenRouter.hs`
- `integrations/Integration/OpenRouter/Internal.hs`
- `integrations/Integration/OpenRouter/Message.hs`
- `integrations/Integration/OpenRouter/Request.hs`
- `integrations/Integration/OpenRouter/Response.hs`
- `testbed/src/Testbed/Examples/OpenRouterIntegration.hs`
- `testbed/tests/integrations/openrouter.hurl`

### Modified Files
- `cabal.project` - Single integrations package
- `core/json/Json.hs` - Extended exports
- `testbed/nhtestbed.cabal` - New example module
- `integrations/nhintegrations.cabal` - Merged package
