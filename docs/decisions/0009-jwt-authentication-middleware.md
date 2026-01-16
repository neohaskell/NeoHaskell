# ADR-0009: JWT Authentication Middleware

## Status

Proposed

## Context

NeoHaskell's event-sourced architecture needs to support secure authentication for real-world applications. After analyzing the requirements, we identified two possible approaches:

### The Integration Problem

Real-world applications need secure authentication, but OAuth2 involves significant complexity:

- Authorization code flows with redirects
- State parameter management (CSRF protection)
- Token exchange with external providers
- Refresh token handling
- Session management
- Secure cookie handling

Implementing full OAuth flows in NeoHaskell would require:

1. Custom HTTP routes for `/authorize`, `/callback`, `/logout`
2. Query parameter parsing (not currently supported by WebTransport)
3. HTTP redirects (302 responses)
4. Form-encoded POST bodies (currently JSON-only)
5. State storage for CSRF tokens
6. Session management infrastructure

### Two Personas

Similar to the Integration pattern (ADR-0008), authentication involves two user types:

**Infrastructure Admin (DevOps/Platform)** - Configures external OAuth provider:

- Sets up Keycloak/Auth0/Okta
- Configures OAuth clients for external providers
- Manages JWKS endpoints and token lifetimes
- Handles the complex OAuth machinery

**Application Developer (Jess)** - Uses declarative auth in NeoHaskell:

- Specifies `AuthOptions` for endpoints
- Accesses `UserClaims` in command handlers
- Never deals with tokens, keys, or OAuth flows directly

### Industry Standard Pattern

The industry standard pattern for authentication in event-sourced applications is:

1. **External OAuth provider** (Keycloak/Auth0/Okta) handles all OAuth complexity
2. **Application validates JWT tokens** from `Authorization: Bearer` headers
3. **JWKS endpoint** provides public keys for token verification
4. **Declarative auth options** on endpoints specify requirements

This pattern separates concerns effectively: OAuth specialists handle the hard parts; applications just verify tokens.

### Current Architecture Gap

The current WebTransport (ADR-0002) explicitly notes:

> "No middleware: No built-in support for authentication, rate limiting, or logging middleware"

And the TODO.md documents planned authorization infrastructure:

> - `AuthOptions`: Everyone, Authenticated, RequireRoles, RequireClaims, Custom
> - `UserSecurity` type with User ID/Subject, Roles/Claims, Tenant memberships

This ADR implements that planned infrastructure using JWT validation.

## Decision

We will implement JWT authentication middleware that validates tokens from external OAuth providers. NeoHaskell will NOT implement OAuth flows directly.

### 1. External Dependency: jose Library

Add the `jose` library (frasertweedale/hs-jose) to nhcore.cabal:

```cabal
build-depends:
  ...
  jose,
```

The `jose` library is the standard Haskell implementation for JOSE (JSON Object Signing and Encryption), supporting:

- JWS (JSON Web Signature) validation
- JWK and JWKS parsing
- RS256, ES256, and other algorithms
- Claims validation

### 2. Core Types

#### UserClaims (What Jess Sees)

```haskell
-- Auth/Claims.hs
module Auth.Claims
  ( UserClaims (..)
  ) where

-- | User identity extracted from a validated JWT.
-- Available to command handlers when authentication is required.
-- This is the primary type Jess interacts with - everything else is internal.
data UserClaims = UserClaims
  { sub :: Text,                    -- Subject (user ID from OAuth provider)
    email :: Maybe Text,            -- User email (if provided)
    name :: Maybe Text,             -- Display name (if provided)
    permissions :: Array Text,      -- Scopes/permissions/roles
    tenantId :: Maybe Text,         -- For multi-tenant apps
    rawClaims :: Map Text Json.Value -- All claims for custom access
  }
  deriving (Generic, Show, Eq)

instance Json.FromJSON UserClaims
instance Json.ToJSON UserClaims
```

#### AuthError

```haskell
data AuthError
  = TokenMissing                     -- No Authorization header
  | TokenMalformed Text              -- Not a valid JWT format
  | TokenExpired                     -- exp claim in the past
  | TokenNotYetValid                 -- nbf claim in the future
  | SignatureInvalid                 -- Signature doesn't verify
  | IssuerMismatch Text Text         -- Expected vs actual issuer
  | AudienceMismatch Text (Array Text) -- Expected vs actual audience
  | InsufficientPermissions (Array Text) -- Required vs available
  | KeyFetchFailed Text              -- JWKS endpoint error
  deriving (Generic, Show, Eq)

instance Json.FromJSON AuthError
instance Json.ToJSON AuthError
```

#### AuthOptions (What Jess Configures)

```haskell
-- Auth/Options.hs
module Auth.Options
  ( AuthOptions (..)
  ) where

-- | Declarative authentication requirements for an endpoint.
data AuthOptions
  = Everyone                        -- No authentication required
  | Authenticated                   -- Valid JWT required, any permissions
  | RequireAllPermissions (Array Text) -- JWT + must have ALL listed permissions
  | RequireAnyPermission (Array Text)  -- JWT + must have at least ONE permission
  | Custom (UserClaims -> Result AuthError Unit) -- Custom validation logic
  deriving (Generic)
```

### 3. JWT Validation Module

```haskell
-- Auth/Jwt.hs (internal implementation, not exposed to Jess)

import Crypto.JOSE qualified as Jose
import Crypto.JWT qualified as JWT

-- | Validate a JWT token against the auth configuration.
-- This is called by the middleware, not by application code.
-- Returns UserClaims on success, AuthError on failure.
validateToken ::
  AuthConfig ->              -- Configuration from OpenID Connect Discovery
  Text ->                    -- Raw JWT token (without "Bearer " prefix)
  Task AuthError UserClaims
validateToken config token = do
  -- 1. Parse the compact JWT format
  jwt <- token
    |> JWT.decodeCompact
    |> Task.fromEither
    |> Task.mapError (\_ -> TokenMalformed "Invalid JWT format")

  -- 2. Create validation settings from discovered config
  let audienceCheck = case config.audience of
        Nothing -> const True
        Just expectedAud -> \aud -> aud == expectedAud

  let validationSettings = JWT.defaultJWTValidationSettings audienceCheck
        |> JWT.issuerPredicate (\iss -> iss == config.issuer)

  -- 3. Verify signature and validate claims
  claims <- jwt
    |> JWT.verifyClaims validationSettings (Jose.JWKSet config.publicKeys)
    |> Task.fromEither
    |> Task.mapError mapJoseError

  -- 4. Extract UserClaims from validated JWT claims
  extractUserClaims config claims

-- | Extract user claims from validated JWT, using configured claim names.
extractUserClaims ::
  AuthConfig ->
  JWT.ClaimsSet ->
  Task AuthError UserClaims
extractUserClaims config claims = do
  let sub = claims |> JWT.claimSub |> Maybe.map JWT.stringOrUri
  case sub of
    Nothing -> Task.throw (TokenMalformed "Missing sub claim")
    Just subject -> do
      let rawClaims = claims |> JWT.unregisteredClaims
      let permissions = rawClaims
            |> Map.get config.permissionsClaim
            |> Maybe.andThen Json.decodeValue
            |> Maybe.withDefault Array.empty
      let tenantId = config.tenantIdClaim
            |> Maybe.andThen (\claim -> Map.get claim rawClaims)
            |> Maybe.andThen Json.decodeValue

      Task.yield UserClaims
        { sub = subject,
          email = rawClaims |> Map.get "email" |> Maybe.andThen Json.decodeValue,
          name = rawClaims |> Map.get "name" |> Maybe.andThen Json.decodeValue,
          permissions = permissions,
          tenantId = tenantId,
          rawClaims = rawClaims
        }
```

Note: Key fetching is now handled internally by `Auth.Discovery.discoverConfig`, not exposed as a separate function.

### 4. AuthConfig and OpenID Connect Discovery

The key insight is that **Jess shouldn't need to think about JWKs, issuers, or claim names**. Modern OAuth providers support OpenID Connect Discovery, which exposes a `.well-known/openid-configuration` endpoint containing all the necessary configuration.

#### OpenID Connect Discovery

When Jess provides an auth server URL like `https://auth.example.com`, the framework:

1. Fetches `https://auth.example.com/.well-known/openid-configuration`
2. Extracts `jwks_uri` and `issuer` from the response
3. Fetches public keys from `jwks_uri`
4. Uses `issuer` for token validation

Example discovery response:
```json
{
  "issuer": "https://auth.example.com",
  "authorization_endpoint": "https://auth.example.com/oauth/authorize",
  "token_endpoint": "https://auth.example.com/oauth/token",
  "jwks_uri": "https://auth.example.com/.well-known/jwks.json",
  "userinfo_endpoint": "https://auth.example.com/userinfo"
}
```

#### AuthConfig (Internal, Auto-Populated)

```haskell
-- Auth/Config.hs
module Auth.Config
  ( AuthConfig (..)
  , AuthOverrides (..)
  , defaultOverrides
  , discoverConfig
  ) where

-- | Internal configuration populated from OpenID Connect Discovery.
-- This is NOT exposed to Jess - it's created automatically from the auth server URL.
data AuthConfig = AuthConfig
  { issuer :: Text,                  -- From discovery: iss claim validation
    jwksUri :: Text,                 -- From discovery: where to fetch public keys
    publicKeys :: Array Jose.JWK,    -- Cached public keys
    audience :: Maybe Text,          -- Optional: aud claim validation
    permissionsClaim :: Text,        -- Claim name for permissions (default: "permissions")
    tenantIdClaim :: Maybe Text,     -- Claim name for tenant ID (optional)
    clockSkewSeconds :: Int          -- Tolerance for exp/nbf validation (default: 60)
  }
  deriving (Generic, Show)

-- | Optional overrides for advanced users.
-- Most fields use sensible defaults; only override if you have special requirements.
data AuthOverrides = AuthOverrides
  { audience :: Maybe Text,          -- Override aud claim validation (default: None)
    permissionsClaim :: Maybe Text,  -- Override permissions claim name (default: "permissions")
    tenantIdClaim :: Maybe Text,     -- Claim name for tenant ID (default: None)
    clockSkewSeconds :: Maybe Int    -- Override clock skew tolerance (default: 60)
  }
  deriving (Generic, Show)

instance Json.FromJSON AuthOverrides
instance Json.ToJSON AuthOverrides

-- | Default overrides (all Nothing - use discovered/default values).
defaultOverrides :: AuthOverrides
defaultOverrides =
  AuthOverrides
    { audience = Nothing,
      permissionsClaim = Nothing,
      tenantIdClaim = Nothing,
      clockSkewSeconds = Nothing
    }
```

#### Discovery Implementation

```haskell
-- Auth/Discovery.hs
module Auth.Discovery
  ( discoverConfig
  , DiscoveryError (..)
  ) where

import Http.Client qualified as Http

-- | OpenID Connect Discovery document structure.
-- We only parse the fields we need.
data DiscoveryDocument = DiscoveryDocument
  { issuer :: Text,
    jwks_uri :: Text
  }
  deriving (Generic, Show)

instance Json.FromJSON DiscoveryDocument

data DiscoveryError
  = FetchFailed Text              -- Failed to fetch discovery document
  | ParseFailed Text              -- Failed to parse discovery document
  | KeysFetchFailed Text          -- Failed to fetch JWKS
  | KeysParseFailed Text          -- Failed to parse JWKS
  deriving (Generic, Show)

-- | Discover auth configuration from an OAuth provider's base URL.
-- Fetches the OpenID Connect discovery document and JWKS keys.
discoverConfig ::
  Text ->                         -- Auth server base URL (e.g., "https://auth.example.com")
  AuthOverrides ->                -- Optional overrides
  Task DiscoveryError AuthConfig
discoverConfig authServerUrl overrides = do
  -- 1. Fetch discovery document
  let discoveryUrl = [fmt|{authServerUrl}/.well-known/openid-configuration|]

  discovery <- Http.request
    |> Http.withUrl discoveryUrl
    |> Http.get @DiscoveryDocument
    |> Task.mapError (\err -> FetchFailed [fmt|Discovery fetch failed: {err}|])

  -- 2. Fetch JWKS keys
  keys <- Http.request
    |> Http.withUrl discovery.jwks_uri
    |> Http.get @Jose.JWKSet
    |> Task.mapError (\err -> KeysFetchFailed [fmt|JWKS fetch failed: {err}|])

  -- 3. Build config with discovered values + overrides
  let config =
        AuthConfig
          { issuer = discovery.issuer,
            jwksUri = discovery.jwks_uri,
            publicKeys = keys |> Jose.jwkSetKeys |> Array.fromList,
            audience = overrides.audience,
            permissionsClaim = overrides.permissionsClaim |> Maybe.withDefault "permissions",
            tenantIdClaim = overrides.tenantIdClaim,
            clockSkewSeconds = overrides.clockSkewSeconds |> Maybe.withDefault 60
          }

  Task.yield config
```

### 5. WebTransport Middleware Integration

The authentication check happens in `assembleTransport` before routing to command handlers:

```haskell
-- Service/Transport/Web.hs (modified)

assembleTransport ::
  Endpoints WebTransport ->
  Wai.Request ->
  (Wai.Response -> Task Text Wai.ResponseReceived) ->
  Task Text Wai.ResponseReceived
assembleTransport endpoints request respond = do
  -- ... existing code ...

  case Wai.pathInfo request of
    ["commands", commandNameKebab] -> do
      let commandName = commandNameKebab |> Text.toPascalCase

      case Map.get commandName endpoints.commandEndpoints of
        Maybe.Just (handler, authOptions) -> do
          -- NEW: Check authentication before executing handler
          authResult <- checkAuth endpoints.authConfig authOptions request

          case authResult of
            Result.Err authError ->
              respondWithAuthError authError respond
            Result.Ok maybeUserClaims -> do
              -- Pass UserClaims to handler (if authenticated)
              bodyResult <- readBodyWithLimit maxBodySize request
              case bodyResult of
                Result.Err errorMessage -> payloadTooLarge errorMessage
                Result.Ok bodyBytes ->
                  handler bodyBytes maybeUserClaims respond
        Maybe.Nothing ->
          notFound [fmt|Command not found: #{commandName}|]
```

#### Authentication Check Logic

```haskell
-- Auth/Middleware.hs
module Auth.Middleware
  ( checkAuth
  , respondWithAuthError
  ) where

-- | Check authentication based on AuthOptions.
checkAuth ::
  Maybe AuthConfig ->        -- Auth configuration (Nothing = auth disabled)
  AuthOptions ->             -- Requirements for this endpoint
  Wai.Request ->             -- Incoming HTTP request
  Task Text (Result AuthError (Maybe UserClaims))
checkAuth maybeConfig authOptions request = do
  case authOptions of
    Everyone ->
      Task.yield (Result.Ok Nothing)

    Authenticated -> do
      userClaims <- validateFromRequest maybeConfig request
      Task.yield (userClaims |> Result.map Just)

    RequireAllPermissions required -> do
      userClaims <- validateFromRequest maybeConfig request
      case userClaims of
        Result.Err err -> Task.yield (Result.Err err)
        Result.Ok claims ->
          if required |> Array.all (\p -> claims.permissions |> Array.contains p)
            then Task.yield (Result.Ok (Just claims))
            else Task.yield (Result.Err (InsufficientPermissions required))

    RequireAnyPermission required -> do
      userClaims <- validateFromRequest maybeConfig request
      case userClaims of
        Result.Err err -> Task.yield (Result.Err err)
        Result.Ok claims ->
          if required |> Array.any (\p -> claims.permissions |> Array.contains p)
            then Task.yield (Result.Ok (Just claims))
            else Task.yield (Result.Err (InsufficientPermissions required))

    Custom validator -> do
      userClaims <- validateFromRequest maybeConfig request
      case userClaims of
        Result.Err err -> Task.yield (Result.Err err)
        Result.Ok claims ->
          case validator claims of
            Result.Ok () -> Task.yield (Result.Ok (Just claims))
            Result.Err err -> Task.yield (Result.Err err)

-- | Extract and validate JWT from Authorization header.
validateFromRequest ::
  Maybe AuthConfig ->
  Wai.Request ->
  Task Text (Result AuthError UserClaims)
validateFromRequest maybeConfig request = do
  case maybeConfig of
    Nothing ->
      Task.yield (Result.Err (TokenMalformed "Auth not configured"))
    Just config -> do
      let authHeader = request
            |> Wai.requestHeaders
            |> Array.find (\(name, _) -> name == "Authorization")

      case authHeader of
        Nothing ->
          Task.yield (Result.Err TokenMissing)
        Just (_, value) -> do
          let headerText = value |> Text.fromBytes
          case Text.stripPrefix "Bearer " headerText of
            Nothing ->
              Task.yield (Result.Err (TokenMalformed "Expected 'Bearer <token>'"))
            Just token ->
              Jwt.validateToken config.publicKeys config.jwtConfig token
                |> Task.asResult

-- | Convert AuthError to appropriate HTTP response.
respondWithAuthError ::
  AuthError ->
  (Wai.Response -> Task Text Wai.ResponseReceived) ->
  Task Text Wai.ResponseReceived
respondWithAuthError authError respond = do
  let (status, message) = case authError of
        TokenMissing ->
          (HTTP.status401, "Authorization header required")
        TokenMalformed reason ->
          (HTTP.status401, [fmt|Invalid token: {reason}|])
        TokenExpired ->
          (HTTP.status401, "Token expired")
        TokenNotYetValid ->
          (HTTP.status401, "Token not yet valid")
        SignatureInvalid ->
          (HTTP.status401, "Invalid token signature")
        IssuerMismatch expected actual ->
          (HTTP.status401, [fmt|Invalid issuer: expected {expected}, got {actual}|])
        AudienceMismatch expected actual ->
          (HTTP.status401, [fmt|Invalid audience|])
        InsufficientPermissions required ->
          (HTTP.status403, [fmt|Insufficient permissions. Required: {required}|])
        KeyFetchFailed reason ->
          (HTTP.status500, [fmt|Authentication service unavailable: {reason}|])

  let responseBody = Json.encodeText (Map.fromArray [("error", message)])
  respond (Wai.responseLBS status [(HTTP.hContentType, "application/json")] (Text.toBytes responseBody |> Bytes.toLazyLegacy))
```

### 6. Command Handler Integration

Update the Command typeclass and handler to receive UserClaims:

```haskell
-- Updated EndpointHandler signature
type EndpointHandler =
  Bytes ->
  Maybe UserClaims ->  -- NEW: User claims (Nothing if Everyone)
  ((CommandResponse, Bytes) -> Task Text Unit) ->
  Task Text Unit
```

Commands can access the authenticated user:

```haskell
-- Example: CreateOrder command that uses UserClaims
module Order.Commands.CreateOrder where

data CreateOrder = CreateOrder
  { items :: Array OrderItem
  }
  deriving (Generic, Typeable, Show)

instance Json.FromJSON CreateOrder

type instance EntityOf CreateOrder = OrderEntity
type instance AuthOf CreateOrder = Authenticated  -- NEW: Auth requirement

getEntityId :: CreateOrder -> Maybe UserClaims -> Maybe Uuid
getEntityId _ _ = Nothing  -- New order, no existing entity

decide :: CreateOrder -> Maybe OrderEntity -> Maybe UserClaims -> Decision OrderEvent
decide cmd entity maybeUser = case maybeUser of
  Nothing ->
    Decision.reject "Authentication required"
  Just user ->
    Decision.acceptNew
      [ OrderCreated
          { orderId = generatedId,
            userId = user.sub,  -- Use authenticated user's ID
            items = cmd.items
          }
      ]
```

### 7. Application Layer Configuration

The configuration API is designed for **progressive disclosure**: the simple case is trivial, and advanced options are available but optional.

#### Simple Case (95% of users)

```haskell
-- Just provide your auth server URL - that's it!
app :: Application
app =
  Application.new
    |> Application.withAuth "https://auth.example.com"
    |> Application.withService orderService
    |> Application.withService userService
    |> Application.withTransport WebTransport.server
    |> Application.withEventStore postgresConfig
```

The framework automatically:
1. Fetches `https://auth.example.com/.well-known/openid-configuration`
2. Extracts `jwks_uri` and `issuer` from the discovery document
3. Fetches public keys from `jwks_uri`
4. Validates tokens against the discovered issuer

#### Advanced Case (custom claim names, audience validation)

```haskell
-- For special requirements, provide overrides
app :: Application
app =
  Application.new
    |> Application.withAuthOverrides "https://auth.example.com" authOverrides
    |> Application.withService orderService
    |> Application.withTransport WebTransport.server
    |> Application.withEventStore postgresConfig

authOverrides :: Auth.AuthOverrides
authOverrides =
  Auth.defaultOverrides
    { audience = Just "my-api",              -- Require specific audience
      permissionsClaim = Just "roles",       -- Use "roles" instead of "permissions"
      tenantIdClaim = Just "org_id"          -- Extract tenant from "org_id" claim
    }
```

#### Application.withAuth Implementation

```haskell
-- Service/Application.hs

-- | Enable JWT authentication using OpenID Connect Discovery.
--
-- This is the simplest way to add authentication. Just provide your OAuth
-- provider's base URL, and the framework handles everything else:
--
-- 1. Discovers JWKS endpoint and issuer from .well-known/openid-configuration
-- 2. Fetches and caches public keys
-- 3. Validates tokens on protected endpoints
--
-- Example:
--
-- @
-- app = Application.new
--   |> Application.withAuth "https://auth.example.com"
--   |> Application.withService myService
-- @
withAuth ::
  Text ->                         -- Auth server base URL
  Application ->
  Application
withAuth authServerUrl app =
  app |> withAuthOverrides authServerUrl Auth.defaultOverrides

-- | Enable JWT authentication with custom overrides.
--
-- Use this when you need to customize audience validation, claim names,
-- or other advanced settings. For most cases, prefer 'withAuth'.
--
-- Example:
--
-- @
-- overrides = Auth.defaultOverrides { audience = Just "my-api" }
-- app = Application.new
--   |> Application.withAuthOverrides "https://auth.example.com" overrides
-- @
withAuthOverrides ::
  Text ->                         -- Auth server base URL
  Auth.AuthOverrides ->           -- Custom overrides
  Application ->
  Application
withAuthOverrides authServerUrl overrides app =
  app {authSetup = Just (Auth.discoverConfig authServerUrl overrides)}
```

#### Runtime Initialization

When `Application.run` is called, the auth setup task runs:

```haskell
-- In Application.run (simplified)
run :: Application -> Task Text Unit
run app = do
  -- ... create event store ...

  -- Initialize auth if configured
  maybeAuthConfig <- case app.authSetup of
    Nothing -> Task.yield Nothing
    Just setupTask -> do
      config <- setupTask
        |> Task.mapError (\err -> [fmt|Auth setup failed: {err}|])
      Task.yield (Just config)

  -- Pass auth config to transports
  runWith eventStore maybeAuthConfig app
```

### 8. Module Structure

```text
core/
  auth/                              -- NEW: Auth module directory
    Auth.hs                          -- Re-export wrapper (public API)
    Auth/
      Config.hs                      -- AuthConfig, AuthOverrides, defaultOverrides
      Discovery.hs                   -- discoverConfig (OpenID Connect Discovery)
      Jwt.hs                         -- JWT validation (validateToken)
      Options.hs                     -- AuthOptions (Everyone, Authenticated, etc.)
      Middleware.hs                  -- checkAuth, respondWithAuthError
      Claims.hs                      -- UserClaims type
      Error.hs                       -- AuthError, DiscoveryError types
```

Add to nhcore.cabal:

```cabal
exposed-modules:
  ...
  Auth
  Auth.Config
  Auth.Discovery
  Auth.Jwt
  Auth.Options
  Auth.Middleware
  Auth.Claims
  Auth.Error

hs-source-dirs:
  ...
  auth
```

#### What Jess Sees (Public API via Core)

```haskell
-- In Core module re-exports:
-- Only the types Jess actually uses
module Core
  ( ...
    -- Auth (minimal surface area)
  , Auth.AuthOptions (..)           -- Everyone, Authenticated, RequireAllPermissions, etc.
  , Auth.AuthOverrides (..)         -- For advanced configuration (rarely needed)
  , Auth.defaultOverrides           -- Starting point for overrides
  , Auth.UserClaims (..)            -- Access authenticated user info
  ) where
```

The discovery mechanism, internal config, and validation logic are **not** exposed through Core - Jess never needs them.

## Consequences

### Positive

1. **Zero-config for common case**: `Application.withAuth "https://auth.example.com"` is all Jess needs. No JWKS URIs, no issuer strings, no claim name configuration.

2. **OpenID Connect Discovery**: Automatic configuration from industry-standard `.well-known/openid-configuration` endpoint. Works with Keycloak, Auth0, Okta, Azure AD, Google, and any OIDC-compliant provider.

3. **Progressive disclosure**: Simple API for 95% of users; advanced overrides available for special requirements.

4. **Clean separation of concerns**: OAuth complexity stays with specialist services (Keycloak/Auth0); NeoHaskell just validates tokens.

5. **Minimal cognitive load**: Jess doesn't need to understand JWKS, JWTs, or token validation internals.

6. **Declarative API**: Jess writes `AuthOf CreateOrder = Authenticated` and gets auth enforcement automatically.

7. **Type-safe claims access**: `UserClaims` provides typed access to common fields with `rawClaims` for custom data.

8. **Flexible permissions**: Supports role-based (`RequireAllPermissions`), capability-based (`RequireAnyPermission`), and custom auth logic.

9. **Multi-tenant ready**: `tenantId` claim enables tenant isolation in command handlers.

10. **Standard compliance**: Uses JOSE/JWT standards; works with any OAuth2/OIDC provider.

11. **Production-proven pattern**: Follows the industry standard pattern used by production event-sourced systems.

12. **Future-proof**: Can add more auth options (API keys, mTLS) without changing the `AuthOptions` interface.

### Negative

1. **External dependency**: Adds `jose` library to nhcore dependencies. However, it's a mature, well-maintained library.

2. **Startup network call**: Application startup requires fetching discovery document and JWKS. Adds ~100-500ms to startup time.

3. **Key management**: Applications must handle JWKS caching and refresh. Future work may add automatic key rotation.

4. **No OAuth flows**: Applications cannot initiate OAuth flows directly. They must use an external provider or proxy.

5. **Eventually consistent keys**: If JWKS keys rotate while cached, there may be a brief period of invalid validations.

6. **Bearer tokens only**: No support for cookie-based sessions or API keys in this ADR. Those can be added later.

7. **OIDC requirement**: Auth provider must support OpenID Connect Discovery. Legacy OAuth2-only providers would need manual configuration via `withAuthOverrides`.

### Trade-offs

1. **Simplicity over completeness**: We deliberately exclude OAuth flows, which limits what applications can do but dramatically reduces complexity.

2. **External provider dependency**: Applications require a separate auth service. For simple apps, this adds infrastructure. For complex apps, this is standard practice.

3. **JWT-centric**: Design optimized for JWT tokens. Other token formats (opaque tokens, PASETO) would need additional work.

4. **Discovery over configuration**: We fetch config at runtime rather than requiring compile-time configuration. This trades startup time for simpler developer experience.

## Alternatives Considered

### Path B: Implement Full OAuth in NeoHaskell

**Would require:**
- Custom HTTP routes for OAuth endpoints
- Query parameter parsing
- HTTP redirects (302 responses)
- Form-encoded POST support
- State storage for CSRF protection
- Session management
- Cookie handling

**Rejected because:**
- Much more implementation work (~1000+ lines)
- Security-critical code better left to specialists
- Reinvents what Keycloak/Auth0/Okta do well
- Would need ongoing maintenance for OAuth spec changes

### Path C: No Auth in Framework

Let applications implement their own authentication.

**Rejected because:**
- Every application would reimplement the same logic
- Inconsistent security practices across applications
- Doesn't align with NeoHaskell's "batteries included" philosophy

## Implementation Plan

### Phase 1: Core Types and Discovery (~2 days)
- [ ] Add `jose` dependency to nhcore.cabal
- [ ] Create `Auth.Claims` with `UserClaims` type
- [ ] Create `Auth.Error` with `AuthError` and `DiscoveryError` types
- [ ] Create `Auth.Config` with `AuthConfig` and `AuthOverrides`
- [ ] Create `Auth.Discovery` with `discoverConfig` (OpenID Connect Discovery)
- [ ] Write unit tests for discovery parsing

### Phase 2: JWT Validation (~1 day)
- [ ] Create `Auth.Jwt` with `validateToken`
- [ ] Write unit tests for token validation (mock keys)

### Phase 3: Middleware Integration (~2 days)
- [ ] Create `Auth.Options` with `AuthOptions` type
- [ ] Create `Auth.Middleware` with `checkAuth`
- [ ] Update `EndpointHandler` type signature
- [ ] Update `WebTransport.assembleTransport` to check auth
- [ ] Add HTTP 401/403 response handling
- [ ] Write integration tests

### Phase 4: Application Layer (~1 day)
- [ ] Add `Application.withAuth` (simple API)
- [ ] Add `Application.withAuthOverrides` (advanced API)
- [ ] Add `authSetup` field to `Application` record
- [ ] Update `Application.run` to initialize auth via discovery
- [ ] Write end-to-end tests

### Phase 5: Documentation (~1 day)
- [ ] Add minimal `Auth` types to Core re-exports
- [ ] Document auth patterns in website
- [ ] Add example with Keycloak/Auth0 integration
- [ ] Update testbed with authenticated endpoints

## Future Work

1. **Automatic JWKS refresh**: Background task to periodically refresh public keys.

2. **API key authentication**: Alternative auth method for service-to-service calls.

3. **Rate limiting middleware**: Companion middleware using similar pattern.

4. **Audit logging**: Log all authentication failures for security monitoring.

5. **mTLS support**: Client certificate authentication for high-security scenarios.

6. **Query endpoint auth**: Extend auth to Query endpoints (currently command-only).

## References

- [ADR-0002: WebAPI Adapter Architecture](0002-webapi-adapter-architecture.md) - Current transport implementation
- [ADR-0008: Integration Pattern](0008-integration-pattern.md) - Two-persona design pattern
- [jose library (Haskell)](https://hackage.haskell.org/package/jose) - JOSE/JWT implementation
- [RFC 7519: JSON Web Token](https://tools.ietf.org/html/rfc7519)
- [RFC 7517: JSON Web Key](https://tools.ietf.org/html/rfc7517)
- [OpenID Connect Discovery](https://openid.net/specs/openid-connect-discovery-1_0.html) - JWKS endpoint standard
