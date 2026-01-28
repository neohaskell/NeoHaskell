# Implementation Plan: HTTP Workaround Fixes (Loop 7)

## Overview

Fix v1 workarounds in the HTTP outbound integration:
1. Add PUT/PATCH/DELETE methods to Http.Client
2. Return real statusCode and headers from HTTP responses
3. Add raw body support

## Architecture Changes

### Layer 3: Http.Client (Infrastructure)

The core changes are in `core/http/Http/Client.hs`. Currently:

```haskell
-- Current: Only returns parsed response body
get :: (Json.FromJSON response) => Request -> Task Error response
post :: (Json.FromJSON response, Json.ToJSON body) => Request -> body -> Task Error response
```

**Change to:**

```haskell
-- New: Response type with metadata
data Response body = Response
  { statusCode :: Int
  , headers :: Array (Text, Text)
  , body :: body
  }

-- Functions return Response instead of just body
get :: (Json.FromJSON response) => Request -> Task Error (Response response)
post :: (Json.FromJSON response, Json.ToJSON body) => Request -> body -> Task Error (Response response)
-- etc.
```

### New Http.Client Functions

| Function | Method | Body Type |
|----------|--------|-----------|
| `get` | GET | None |
| `post` | POST | JSON |
| `postForm` | POST | Form-urlencoded |
| `postRaw` | POST | Raw bytes + content-type |
| `put` | PUT | JSON |
| `putRaw` | PUT | Raw bytes |
| `patch` | PATCH | JSON |
| `delete` | DELETE | None |
| `deleteWithBody` | DELETE | JSON (for APIs that require it) |

### Layer 2: Integration.Http.Internal

Update `executeHttpRequest` to:
1. Use new `Response` type from Http.Client
2. Map to `Integration.Http.Response` with real values
3. Add cases for PUT/PATCH/DELETE
4. Add case for raw body

```haskell
-- Before
handleHttpResult result = case result of
  Ok jsonValue ->
    Ok Response
      { statusCode = 200  -- TODO: placeholder
      , body = jsonValue
      , headers = []      -- TODO: placeholder
      }

-- After
handleHttpResult result = case result of
  Ok httpResponse ->
    Ok Response
      { statusCode = httpResponse.statusCode
      , body = httpResponse.body
      , headers = httpResponse.headers
      }
```

## Task Checklist

### Http.Client Changes
- [x] Add `Response` type with statusCode, headers, body
- [x] Update `get` to return `Response`
- [x] Update `post` to return `Response`
- [x] Update `postForm` to return `Response`
- [x] Add `postRaw` function
- [x] Add `put` function
- [x] Add `patch` function
- [x] Add `delete` function
- [x] Update exports
- [x] Add `case-insensitive` dependency for header extraction

### Http.Client Breaking Changes Fixed
- [x] Updated Auth.OAuth2.Client to use `response.body.*`
- [x] Updated Auth.Discovery to use `response.body`

### Integration.Http.Internal Changes
- [x] Update `executeHttpRequest` for new Response type
- [x] Add PUT case (with JsonBody and NoBody)
- [x] Add PATCH case (with JsonBody and NoBody)
- [x] Add DELETE case
- [x] Add raw body case (using postRaw)
- [x] Remove TODO comments - now using real statusCode/headers

### Testbed Examples Added
- [x] PUT example (updateUserProfileIntegration)
- [x] PATCH example (partialUpdateOrderIntegration)
- [x] DELETE example (cancelSubscriptionIntegration)
- [x] Status code/headers example (conditionalUpdateIntegration)
- [x] Raw body example (xmlOrderIntegration)

## Breaking Changes

**Http.Client return type changes from `body` to `Response body`.**

This is a breaking change for any code that directly uses Http.Client. Consumers need to extract `.body` from the response.

```haskell
-- Before
result <- Http.get request
process result

-- After
response <- Http.get request
process response.body
```

## Implementation Order

1. **Http.Client Response type** - Add type, update existing functions
2. **Http.Client new methods** - Add put, patch, delete, postRaw
3. **Integration.Http.Internal** - Wire up new Response type and methods
4. **Tests** - Add tests for all new functionality
5. **Testbed verification** - Ensure examples compile
