# Authorization

Inbound authentication validates who calls the application. Command and query policies decide what that caller may do. External OAuth2 account connection is a separate feature. See JWT middleware in `core/auth/Auth/Middleware.hs`, access control in `core/service/Service/AccessControl.hs`, and [external OAuth2](advanced-oauth2.md).

## Inbound identity

`Application.withAuth` takes a factory from application configuration to an identity provider's base URL. `withAuthOverrides` also accepts `AuthOverrides`. Startup uses OpenID Connect discovery and starts a JWKS manager. See Application in `core/service/Service/Application.hs`, discovery in `core/auth/Auth/Discovery.hs`, and JWKS in `core/auth/Auth/Jwks.hs`.

The identity provider must expose `/.well-known/openid-configuration` with an issuer and `jwks_uri`. Discovery and key fetches validate secure URLs. A provider that only offers external OAuth2 authorization and token endpoints does not satisfy this prerequisite. See DiscoveryDocument and URL validation in `core/auth/Auth/Discovery.hs`.

## Commands

With authentication configured, a command without a custom `canAccess` uses `authenticatedAccess`. A `canAccess` function declared before `command` overrides that policy. An invalid Web token fails before command authorization. A missing token can reach an explicit public policy. See Command in `core/service/Service/Command/Core.hs`, the marker in `core/service/Service/CommandExecutor/TH.hs`, and Web claim resolution in `core/service/Service/Transport/Web.hs`.

A Web deployment without `withAuth` supplies a trusted context that bypasses the command access gate. The generated starter app in `neo/starter/src/App.hs` uses this configuration. Do not describe the default command policy as protection for an application that has no authentication configured. See trustedContext in `core/service/Service/Auth.hs`.

`decide` still receives `RequestContext`. Inspect `ctx.user` and the loaded entity to enforce ownership or other transition rules. Never accept an owner identifier from a request as proof of caller identity. The context type in `core/service/Service/Auth.hs` and command contract in `core/service/Service/Command/Core.hs` distinguish those inputs.

## Queries

A query defines both `canAccess` and `canView`. `authenticatedAccess`, `requirePermission`, and `publicAccess` apply before reads. `ownerOnly`, `tenantOnly`, and `publicView` apply to rows. See AccessControl in `core/service/Service/AccessControl.hs` and the query endpoint in `core/service/Service/Query/Endpoint.hs`.

For authorization tests, use anonymous and authenticated contexts. `emptyContext` has no user and does not bypass the command gate. Reserve trusted context for the owning transport configuration. The context constructors in `core/service/Service/Auth.hs` and command authorization specs in `core/test/Service/Command/CanAccessSpec.hs` define these distinctions.
