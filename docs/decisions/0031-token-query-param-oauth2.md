# ADR-0031: Token Query Parameter for OAuth2 Browser Redirects

## Status

Proposed

## Context

The `/connect/{provider}` OAuth2 endpoint requires JWT authentication via the `Authorization: Bearer <token>` header (see ADR-0009, ADR-0010). This works for API clients but fails for browser-based SPAs that need to initiate OAuth2 flows via redirect (`window.location.href`), because browser redirects cannot send custom HTTP headers.

### Use Cases

- React/Vue/Angular SPA with Keycloak auth needs to redirect users to `/connect/oura` to initiate OAuth2 provider connection
- Any browser-based frontend that authenticates via JWT and needs to trigger OAuth2 provider flows
- Mobile webviews that use redirect-based navigation

### Requirements

- The `/connect/{provider}` route must accept a `token` query parameter as fallback when no `Authorization` header is present
- Header-based auth remains the primary and preferred mechanism
- Query parameter fallback is scoped to `/connect` routes only — NOT global
- Existing behavior for all other endpoints is unchanged
- Token size limits and validation apply equally to query-param tokens
- `Referrer-Policy: no-referrer` must be set on the redirect response to prevent token leakage to the OAuth2 provider

### Workarounds Attempted (by users)

1. `fetch()` with `redirect: 'manual'` — cannot read Location header due to CORS opaque-redirect
2. `fetch()` with `redirect: 'follow'` — headers not preserved on cross-origin redirects
3. Popup/iframe approaches — complex and poor UX

## Decision

Add a new `extractTokenFromQuery` helper in `Auth.Middleware` and use it as a scoped fallback in the `/connect/{provider}` route handler in `Service.Transport.Web`. The general `extractToken` function remains unchanged (header-only).

### Design: Scoped Fallback (NOT Global)

**Why not modify `extractToken` globally?**

Per RFC 6750 §2.3, URI query parameter tokens "SHOULD NOT be used" unless the Authorization header is infeasible. Making it global would mean every authenticated endpoint silently accepts `?token=`, which:
- Expands attack surface to all endpoints unnecessarily
- Exposes tokens in server/proxy logs for every request type
- Leaks tokens via `Referer` headers on any endpoint that returns HTML with external links

The `/connect/{provider}` endpoint is the **only** route where the header is genuinely infeasible (browser redirect).

### New Helper Function

```haskell
-- | Extract token from ?token= query parameter.
-- SECURITY: Only use for endpoints where Authorization header is infeasible
-- (e.g., browser redirect flows). Prefer extractToken (header-based) for all
-- other endpoints.
extractTokenFromQuery :: Wai.Request -> Maybe Text
```

### Integration in Web.hs `/connect` Handler

The `/connect` handler tries the Authorization header first, then falls back to the query parameter:

```haskell
["connect", providerName] -> do
  -- Try Authorization header first, fall back to ?token= query param
  let maybeToken = case Middleware.extractToken request of
        Just token -> Just token
        Nothing -> Middleware.extractTokenFromQuery request
  -- Use token to validate auth...
```

### Referrer Protection

The 302 redirect response from `/connect` to the OAuth2 provider **must** include `Referrer-Policy: no-referrer` to prevent the JWT from leaking via the `Referer` header.

### Frontend Usage

```javascript
window.location.href = `/connect/oura?token=${encodeURIComponent(jwtToken)}`;
```

## Consequences

### Positive

- SPAs can initiate OAuth2 flows without complex workarounds
- No breaking changes to existing header-based auth
- Scoped to `/connect` only — principle of least privilege
- All existing token validation (size limits, JWT verification, JWKS) applies equally
- `Referrer-Policy: no-referrer` prevents token leakage to OAuth2 provider

### Negative

- Token appears in URL for `/connect` requests, which may be logged by:
  - Server access logs
  - Reverse proxies (nginx, Cloudflare)
  - Browser history
- Slightly more complex auth flow in the `/connect` handler

### Risks

- **Token leakage via logs**: Mitigated by short-lived JWTs (typical 5-15 min expiry). The connect endpoint immediately redirects, so the token URL is transient.
- **Referrer leakage**: Mitigated by `Referrer-Policy: no-referrer` header on the 302 redirect response.
- **Browser history**: The URL with token is saved in browser history. Mitigated by short-lived tokens — by the time someone accesses history, the token is expired.

### Security Model

This is the same security model used by:
- OAuth2 `access_token` query parameter (RFC 6750 Section 2.3, deprecated but widely used)
- OAuth2 `code` parameter in callbacks (already used in this codebase at `/callback/{provider}`)
- The existing state token passed via query parameter in the OAuth2 flow

The PKCE + state token already provide CSRF protection for the OAuth2 flow itself.
