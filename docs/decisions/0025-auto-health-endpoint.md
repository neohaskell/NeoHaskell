# ADR-0025: Auto Health Endpoint for WebTransport Apps

## Status

Proposed

## Context

NeoHaskell apps using `WebTransport` need to integrate with modern deployment platforms like Fly.io, Railway, and Render. These platforms require health check endpoints to monitor application readiness and route traffic appropriately.

Without an automatic health endpoint, apps deployed to these platforms show warnings like:

```text
Health check on port 8080 is in a 'warning' state
```

Currently, there is no built-in way to respond to health probes without manually implementing custom routes or middleware. This creates deployment friction and requires boilerplate code in every application.

### Use Cases

- **Fly.io deployments**: Platform expects `GET /health` to return 200 when the app is ready
- **Railway/Render**: Similar health check requirements for load balancing and zero-downtime deploys
- **Kubernetes readiness probes**: Standard pattern for container orchestration
- **Local development**: Quick sanity check that the server is running

### GitHub Issue

- [#386: Auto health endpoint for WebTransport apps](https://github.com/neohaskell/NeoHaskell/issues/386)

## Decision

### 1. HealthCheckConfig Type

Add a health check configuration type to `Service.Transport.Web`:

```haskell
-- | Health check configuration for WebTransport.
-- When enabled, GET requests to the health path return 200 OK with a JSON body.
-- Enabled by default with path "/health". Can be disabled or customized.
data HealthCheckConfig = HealthCheckConfig
  { healthPath :: Text
  -- ^ URL path for the health endpoint (default: "health")
  }
```

### 2. WebTransport Extension

Add `healthCheck :: Maybe HealthCheckConfig` to the `WebTransport` record. **Default is `Just` (enabled)** — unlike CORS which defaults to `Nothing`. This is the key design choice: health checks are ON by default to follow convention over configuration.

### 3. Default Server Change

The `server` function defaults to:

```haskell
server :: WebTransport
server =
  WebTransport
    { port = 8080
    , maxBodySize = 1048576
    , authEnabled = Nothing
    , oauth2Config = Nothing
    , fileUploadEnabled = Nothing
    , apiInfo = Nothing
    , corsConfig = Nothing
    , healthCheck = Just HealthCheckConfig { healthPath = "health" }
    }
```

### 4. Application Builder

Two functions for customizing health check behavior:

```haskell
-- | Customize the health check path
withHealthCheck :: Text -> Application -> Application

-- | Disable the health check entirely
withoutHealthCheck :: Application -> Application
```

### 5. Route Handling

In `assembleTransport`, add a case BEFORE the catch-all `_ -> notFound`:

```haskell
[healthPath] | isHealthCheckPath healthPath webTransport -> 
  respond (Wai.responseLBS HTTP.status200 
    [(HTTP.hContentType, "application/json")] 
    "{\"status\":\"ok\"}")
```

The health check:

- Returns HTTP 200 with `{"status":"ok"}` JSON body
- Content-Type: `application/json`
- Does NOT require authentication (bypasses auth middleware)
- Typically not subject to CORS (platform health probes are server-to-server and do not send Origin headers, though CORS middleware wraps the entire WAI application)
- Is handled at the route level, not as middleware

### 6. Files Modified

| File | Change |
|------|--------|
| `core/service/Service/Transport/Web.hs` | `HealthCheckConfig` type, `healthCheck` field on `WebTransport`, health route in `assembleTransport` |
| `core/service/Service/Application.hs` | `withHealthCheck`, `withoutHealthCheck` builder functions, `healthCheckConfig` field |
| `core/service/Service/Application/Transports.hs` | Pass `healthCheckConfig` through to `WebTransport` |

### 7. Security Considerations

- Health endpoint does NOT expose internal state (no DB status, no version info)
- Returns only `{"status":"ok"}` — minimal information disclosure
- No authentication required (health probes come from platform infrastructure)
- Path is configurable to avoid conflicts with application routes

### 8. Performance Considerations

- Zero overhead: simple string comparison on path + static response
- No allocation beyond the static response bytes
- Short-circuits before any body reading or auth checking
- No impact on 50k req/s target

### 9. Example Usage

```haskell
-- Default: health endpoint at /health (no config needed)
app = do
  let service = Application.new
        |> Application.withTransport WebTransport.server
        |> Application.withService myService
  service

-- Custom path:
app = do
  let service = Application.new
        |> Application.withTransport WebTransport.server
        |> Application.withHealthCheck "/_health"
        |> Application.withService myService
  service

-- Disabled:
app = do
  let service = Application.new
        |> Application.withTransport WebTransport.server
        |> Application.withoutHealthCheck
        |> Application.withService myService
  service
```

## Consequences

### Positive

1. **Zero-config deployment**: Apps work out-of-the-box on Fly.io, Railway, Render, etc.
2. **Convention over configuration**: Enabled by default with sensible path (`/health`)
3. **Platform compatibility**: Follows industry standard for health check endpoints
4. **Flexibility**: Can be disabled or customized for apps with specific requirements

### Negative

1. **Path collision risk**: Occupies the `/health` path by default (mitigated by configurability)
2. **Limited readiness reporting**: Does not report actual application readiness (e.g., DB connectivity) — just "server is listening"

### Risks

1. **Path collision with user routes**: If an app already uses `/health`, there will be a conflict. Mitigated by making the path configurable via `withHealthCheck`.
2. **False positive health**: Server may be up but unable to serve requests (e.g., DB connection lost). Future enhancement could add readiness checks that query dependencies.
