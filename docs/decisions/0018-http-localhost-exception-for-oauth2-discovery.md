# ADR-0018: HTTP Localhost Exception for OAuth2 Discovery URLs

## Status

Proposed

## Context

### Problem

The OAuth2 discovery flow (`Auth.Discovery.discoverConfig`) strictly requires HTTPS for all URLs, including localhost development environments. This blocks local development with identity providers like Keycloak running on `http://localhost:8180`.

When attempting to use OAuth2 with a local Keycloak instance:

```
Auth discovery failed: UrlValidationFailed "URL must use HTTPS: http://localhost:8180/realms/my-realm/.well-known/openid-configuration"
```

### Current Inconsistency

The codebase has **inconsistent localhost handling** between two validation paths:

| Component | HTTP for localhost? | Location |
|-----------|---------------------|----------|
| **Redirect URIs** | Allowed | `Auth/OAuth2/Types.hs:201-225` |
| **Discovery URL** | Blocked | `Auth/UrlValidation.hs:99-121` |
| **JWKS URL** | Blocked | Same as above |

**Redirect URI validation (allows localhost):**

```haskell
-- Auth/OAuth2/Types.hs:217-225
"http:" -> do
  case URI.uriAuthority uri of
    Nothing -> Err (InvalidRedirectUri "Redirect URI must use HTTPS")
    Just auth -> do
      let host = URI.uriRegName auth
      case isLocalhost host of
        True -> Ok (RedirectUri text)  -- HTTP allowed for localhost
        False -> Err (InvalidRedirectUri "Redirect URI must use HTTPS (http allowed only for localhost)")
```

**Discovery URL validation (blocks localhost):**

```haskell
-- Auth/UrlValidation.hs:99-121
case scheme of
  "https:" -> ...  -- proceed
  _ -> Err (NotHttps urlText)  -- No localhost exception
```

### Why This Matters

Developers building applications with NeoHaskell need to test OAuth2 flows locally. Common local identity provider setups:

- **Keycloak**: `http://localhost:8180/realms/dev`
- **ORY Hydra**: `http://localhost:4444`
- **Dex**: `http://localhost:5556/dex`
- **Mock IdP for testing**: `http://localhost:9999`

Requiring HTTPS for local development creates friction:
1. Developers must set up local TLS certificates
2. Requires certificate trust configuration
3. Many IdP Docker images run HTTP by default
4. Adds complexity without security benefit on localhost

### Standards Compliance

**RFC 8252 Section 7.3 (OAuth 2.0 for Native Apps)** explicitly permits HTTP for loopback addresses:

> The authorization server MUST allow any port to be specified at the time of the request for loopback IP redirect URIs, to accommodate ephemeral port listeners.

While RFC 8252 specifically addresses redirect URIs, the security rationale applies equally to discovery endpoints:

- **Loopback traffic never leaves the local machine**
- **No network interception is possible on 127.0.0.1 or ::1**
- **The threat model for localhost is fundamentally different from remote hosts**

## Decision

### Modify `validateSecureUrl` to Allow HTTP for Localhost

Add a localhost exception to `Auth.UrlValidation.validateSecureUrl`, matching the existing pattern in `Auth.OAuth2.Types.mkRedirectUri`:

```haskell
validateSecureUrl :: Text -> Result ValidationError Text
validateSecureUrl urlText = do
  let urlString = Text.toLinkedList urlText
  case URI.parseURI urlString of
    Nothing -> Err (MalformedUrl urlText)
    Just uri -> do
      let scheme = URI.uriScheme uri
      case scheme of
        "https:" -> -- existing HTTPS handling (unchanged)
          case URI.uriAuthority uri of
            Nothing -> Err (MissingHostname urlText)
            Just auth -> do
              let host = URI.uriRegName auth
              case host of
                [] -> Err (MissingHostname urlText)
                _ -> do
                  let normalizedHost = LinkedList.map toLowerChar host
                  case isSingleLabelHostname normalizedHost of
                    True -> Err (SingleLabelHostname urlText)
                    False ->
                      case isPrivateOrLoopback normalizedHost of
                        True -> Err (PrivateIpBlocked urlText)
                        False -> Ok urlText
        "http:" -> do
          -- Allow http ONLY for localhost (development)
          case URI.uriAuthority uri of
            Nothing -> Err (NotHttps urlText)
            Just auth -> do
              let host = URI.uriRegName auth
              case isLocalhostHost host of
                True -> Ok urlText
                False -> Err (NotHttps urlText)
        _ -> Err (NotHttps urlText)
```

### Extract `isLocalhost` to Shared Location

Move the `isLocalhost` helper from `Auth.OAuth2.Types` to `Auth.UrlValidation` as `isLocalhostHost` (using `[Char]` type for consistency with existing functions in UrlValidation):

```haskell
-- | Check if a hostname is localhost (for development URLs).
-- Matches: localhost, 127.0.0.1, [::1], and variants with ports.
isLocalhostHost :: [Char] -> Bool
isLocalhostHost host =
  host == "localhost"
    || host == "127.0.0.1"
    || host == "[::1]"
    || LinkedList.any (\prefix -> GhcList.isPrefixOf prefix host) ["localhost:", "127.0.0.1:", "[::1]:"]
```

Then update `Auth.OAuth2.Types` to import and use this shared helper:

```haskell
import Auth.UrlValidation (isLocalhostHost)

-- Update mkRedirectUri to use shared helper
isLocalhost :: [Char] -> Bool
isLocalhost = isLocalhostHost
```

### Affected Functions

The change to `validateSecureUrl` automatically applies to:

1. **`validateSecureUrlWithDns`** - Calls `validateSecureUrl` first, so localhost URLs will pass the initial check and skip DNS resolution (as they should - localhost doesn't need DNS validation)

2. **OAuth2 Discovery** - Any code using `validateSecureUrl` for discovery endpoints

3. **JWKS URL validation** - Any code validating JWKS endpoints

## Consequences

### Positive

- **Consistent behavior**: Discovery URLs and redirect URIs follow the same rules
- **Improved DX**: Local development with OAuth2 "just works"
- **Standards-aligned**: Follows RFC 8252 security rationale for loopback addresses
- **No production impact**: Only affects localhost; all other hosts still require HTTPS
- **Minimal code change**: ~15 lines of implementation

### Negative

- **Slightly larger attack surface**: An attacker who already has code execution on the local machine could potentially intercept HTTP traffic to localhost (but they already have code execution, so this is moot)

### Trade-offs

| Aspect | Before | After |
|--------|--------|-------|
| Local dev setup | Complex (TLS required) | Simple (HTTP works) |
| Security for localhost | Maximum (HTTPS only) | Practical (HTTP allowed) |
| Code consistency | Inconsistent | Consistent |
| RFC compliance | Partial | Full |

### Migration Path

**No migration required.** This is a relaxation of validation rules. Existing configurations using HTTPS will continue to work. Developers who want to use HTTP localhost will simply be unblocked.

## Security Analysis

### Threat Model for Localhost

| Threat | Applicable? | Reasoning |
|--------|-------------|-----------|
| Network interception | No | 127.0.0.1 traffic never hits the network |
| DNS rebinding | No | Literal localhost/127.0.0.1 don't use DNS |
| SSRF to internal services | No | This is about localhost, not private networks |
| Malicious IdP responses | Same | HTTP vs HTTPS doesn't change IdP trustworthiness |

### What Stays Protected

1. **Private network IPs remain blocked** - 10.x.x.x, 172.16.x.x, 192.168.x.x still blocked
2. **DNS rebinding protection remains** - validateSecureUrlWithDns still validates DNS responses
3. **HTTPS required for all non-localhost** - Production URLs must use HTTPS
4. **Single-label hostname protection** - "db" or "internal" still blocked

### Localhost Addresses Allowed

- `localhost`
- `127.0.0.1`
- `[::1]`
- With any port: `localhost:8180`, `127.0.0.1:4444`, `[::1]:5556`

## Implementation Checklist

- [x] Add `isLocalhost` function to `Auth.Hostname` module (shared helper)
- [x] Modify `validateSecureUrl` to check for localhost on HTTP
- [x] Modify `validateSecureUrlWithDns` to skip DNS for localhost
- [x] Update `Auth.OAuth2.Types.isLocalhost` to use shared helper
- [x] Add unit tests for HTTP localhost URLs (19 tests in HostnameSpec, 54 in UrlValidationSpec)
- [x] Add unit tests ensuring non-localhost HTTP still rejected
- [x] Add security regression tests (localhost subdomain attacks, alternate loopback IPs)

## References

- [Issue #356](https://github.com/neohaskell/neohaskell/issues/356) - Original feature request
- [RFC 8252 Section 7.3](https://datatracker.ietf.org/doc/html/rfc8252#section-7.3) - OAuth 2.0 for Native Apps, loopback redirect URI
- [ADR-0010](0010-oauth2-provider-architecture.md) - OAuth2 Provider Integration Architecture
- [ADR-0009](0009-jwt-authentication-middleware.md) - JWT Authentication Middleware
