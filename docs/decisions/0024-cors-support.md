# ADR-0024: CORS Support for WebTransport

## Status

Accepted

## Context

NeoHaskell's `WebTransport` server (WAI/Warp) does not include CORS headers in HTTP responses. This means browser-based frontends served from a different origin are blocked by the Same-Origin Policy from making requests to a NeoHaskell backend.

For example, a frontend running on `localhost:5173` (via Vite) cannot call a NeoHaskell backend on `localhost:8080` without getting:

```text
Cross-Origin Request Blocked: The Same Origin Policy disallows reading the remote resource.
(Reason: CORS header 'Access-Control-Allow-Origin' missing).
```

The current workaround is a dev proxy (e.g., Vite's `server.proxy`), but this does not work in production deployments where frontend and backend are on different domains or subdomains.

### Use Cases

- **Local development**: Frontend dev server on one port, NeoHaskell backend on another
- **Production SPA**: Frontend served from a CDN or different subdomain, calling NeoHaskell API
- **Mobile/third-party clients**: Any browser-based client on a different origin

### GitHub Issue

- [#363: Add CORS support to WebTransport](https://github.com/neohaskell/NeoHaskell/issues/363)

## Decision

### 1. CorsConfig Type

Add a CORS configuration type to `Service.Transport.Web`:

```haskell
data CorsConfig = CorsConfig
  { allowedOrigins :: Array Text
  , allowedMethods :: Array Text
  , allowedHeaders :: Array Text
  , maxAge :: Maybe Int
  }
```

### 2. WebTransport Extension

Add `corsConfig :: Maybe CorsConfig` to the `WebTransport` record. Default is `Nothing` (no CORS headers).

### 3. Application Builder

Add `Application.withCors` following the existing `withAuth`/`withFileUpload` builder pattern:

```haskell
withCors :: CorsConfig -> Application -> Application
```

### 4. CORS Middleware

CORS is applied in `runTransport` as a WAI middleware wrapper around the application:

- **Origin matching**: Compare request `Origin` header against `allowedOrigins`
- **Response headers**: Add `Access-Control-Allow-Origin`, `Access-Control-Allow-Methods`, `Access-Control-Allow-Headers`, and optionally `Access-Control-Max-Age`
- **Preflight handling**: Intercept `OPTIONS` requests and return `204 No Content` with CORS headers
- **Origin reflection**: When a specific origin matches, reflect that exact origin (not `"*"`) to support credentials
- **Wildcard support**: `"*"` in `allowedOrigins` matches any origin
- **Cache safety**: When reflecting a specific origin (not `"*"`), a `Vary: Origin` header is included to prevent intermediate caches from serving incorrect responses

### 5. Example Usage

```haskell
app =
  Application.new
    |> Application.withTransport WebTransport.server
    |> Application.withCors CorsConfig
        { allowedOrigins = ["http://localhost:5173"]
        , allowedMethods = ["GET", "POST", "OPTIONS"]
        , allowedHeaders = ["Content-Type", "Authorization"]
        , maxAge = Just 3600
        }
    |> Application.withService MyService.service
```

### 6. Files Modified

| File | Change |
|------|--------|
| `core/service/Service/Transport/Web.hs` | `CorsConfig` type, `corsConfig` field on `WebTransport`, CORS header injection, `OPTIONS` handling |
| `core/service/Service/Application.hs` | `withCors` builder function |
| `core/service/Service/Application/Transports.hs` | Pass `corsConfig` through to `WebTransport` |

### 7. Security Considerations

- **No hardcoded origins**: All origins are user-configured
- **No credentials with wildcard**: When `"*"` is used, `Access-Control-Allow-Credentials` is never set (browser-enforced)
- **Header sanitization**: CORS header values are filtered for CRLF to prevent header injection
- **Origin validation**: Case-insensitive matching to prevent bypass via case variations
- **Documentation**: Security best practices for CORS configuration will be documented

### 8. Performance Considerations

- CORS middleware is lightweight: string comparison + header injection
- `OPTIONS` preflight short-circuits before body processing
- No impact on the 50k req/s target
- `INLINE` pragmas on hot-path matching functions

## Consequences

### Positive

1. **Enables cross-origin frontends**: Browser-based SPAs can call NeoHaskell backends without a proxy.
2. **Consistent API**: Follows the existing `withAuth`/`withFileUpload` builder pattern.
3. **Zero overhead when disabled**: `corsConfig = Nothing` by default; no headers added unless configured.
4. **Flexible configuration**: Supports wildcard, specific origins, and configurable methods/headers.

### Negative

1. **Security misconfiguration risk**: Overly permissive CORS (e.g., wildcard in production) could expose APIs to unintended origins.
2. **Additional configuration burden**: Users must understand CORS to configure it correctly.

### Risks

1. **Wildcard in production**: Users may copy development configs with `"*"` into production. Mitigate with documentation.
2. **Header size increase**: Negligible (a few hundred bytes per response).
