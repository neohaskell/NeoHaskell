# ADR-0066: Static SPA Asset Serving for WebTransport

## Status

Proposed

## Context

A NeoHaskell `WebTransport` serves the command/query API (plus health, readiness,
OAuth2, file upload, and OpenAPI docs) from a single WAI/Warp server. It has no
way to serve a directory of static frontend assets. Today a developer who has
built a Single-Page Application (SPA) — a Vite/React/Svelte build folder of
`index.html` plus content-hashed JS/CSS bundles — must stand up a *second*
server (nginx, a CDN, `wai-app-static`, or `python -m http.server`) just to
hand those files to the browser, then wire CORS (ADR-0024) so the SPA can reach
the API on a different origin.

For the common case — a small app where the same NeoHaskell process should serve
both the UI and the API on one port — that is disproportionate operational
overhead. It also reintroduces the cross-origin problem that CORS exists to
paper over: if the assets and the API share an origin, no CORS is needed at all.

### Current State

- `WebTransport` (`core/service/Service/Transport/Web.hs`) resolves each request
  through a fixed set of route branches (commands, queries, `/health`, `/ready`,
  OAuth2, file upload, `/openapi.yaml`, `/docs`). Anything unmatched falls
  through to a terminal `notFound "Not found"` at `Web.hs:1026`.
- The framework already has a well-established family of *web-transport-tied
  Application combinators* — `withAuth`, `withAuthOverrides`, `withCors`,
  `withHealthCheck` / `withoutHealthCheck`, `withApiInfo`, `withFileUpload`,
  `withOAuth2Provider`, and `useReadinessEndpoint`. Each is an `Application`
  line that sets a `Maybe` field on the `WebTransport` record, resolved at
  `Application.run` time and injected into the transport.
- WAI can already stream a file off disk with `responseFile`, and Warp handles
  the body natively. No file-serving dependency is present or needed.

### Use Cases

- **Single-process full-stack app**: One NeoHaskell binary serves `static/` at
  `/` and the API at `/commands/*` and `/queries/*` — no second server, no CORS.
- **SPA deep links**: A user reloads `/orders/42` (a client-side route with no
  matching file on disk). The server returns `index.html` so the SPA's router
  can take over, instead of a 404.
- **Hot rebuild during development**: The dev edits the frontend, the bundler
  rewrites `static/`, and the browser must pick up the *new* `index.html` (never a
  stale cached one) while still caching content-hashed bundles aggressively.

### Design Goals

1. **Consistency over novelty** — the feature must look and behave exactly like
   the other nine web-transport combinators so a developer who knows `withCors`
   already knows this. No new configuration idiom.
2. **API always wins** — turning on static serving must never change the
   behaviour of an existing API route. Static files are a *last resort*, not a
   competing router.
3. **Opt-in** — adding a `WebTransport` must not silently start serving disk
   contents; that would change 404 semantics for existing API-only apps and
   expose the filesystem by surprise.
4. **Correct caching out of the box** — the developer should get
   hot-rebuild-safe caching without knowing what `Cache-Control` is, because
   getting it wrong makes local development mysteriously stale.
5. **No new dependencies** — reuse WAI's `responseFile`; do not pull in
   `wai-app-static`.

### GitHub Issue

- [#707: Serve static SPA assets from WebTransport](https://github.com/neohaskell/NeoHaskell/issues/707)

## Decision

### 1. An Application-Level Combinator, Consistent with the Existing Family

Add `Application.withStaticAssets`, an `Application` combinator that sets an
optional `StaticAssets` configuration on the `WebTransport`. It joins the
existing family (`withCors`, `withApiInfo`, `withHealthCheck`, …) and is used
identically: one pipe line between `withTransport` and `withService`.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| **Application-level `Application.withStaticAssets`** | **Chosen** | Matches the nine existing web combinators exactly. Zero new idioms for the developer to learn. `Maybe StaticAssets` field, factory resolution, and record-update threading are a copy of the `withCors` path. |
| Transport-level `WebTransport.withStaticAssets :: StaticAssets -> WebTransport -> WebTransport`, nested inside `withTransport (WebTransport.server \|> ...)` | Rejected | Would *found* a brand-new convention: `WebTransport` currently exposes **zero** combinators of its own, so this lone transport-level builder would sit inconsistently beside nine decoupled `Application`-level ones until (and unless) they all migrate. Smaller-diff and coupled-by-construction, but the whole point of this feature is proportionate consistency, not starting a migration. Revisit only if the family is deliberately moved as a set. |
| Depend on `wai-app-static` | Rejected | WAI's `responseFile` already streams files; a dependency buys nothing but supply-chain surface. This is why the feature is `moderate`, not `complex`. |
| Auto-serve a convention dir (`./public` / `./static`) with zero config | Rejected | Silently serving disk contents the moment a `WebTransport` is added changes 404 semantics for every existing API-only app and is an exposure footgun. Opt-in with good internal defaults is safer. |

### 2. `StaticAssets` Configuration Type

Add a configuration type next to `CorsConfig` / `HealthCheckConfig` in
`Service.Transport.Web`:

```haskell
-- | Static asset serving configuration for WebTransport.
-- When set, unmatched non-API GET requests are served from 'root' on disk.
-- Set via Application.withStaticAssets.
data StaticAssets = StaticAssets
  { root :: Text
  -- ^ Directory whose contents are served (e.g., "static").
  , spaFallback :: Maybe Text
  -- ^ Document served for unmatched non-API paths so client-side routing
  --   works on deep links (e.g., Just "index.html"). Nothing = no fallback.
  }
```

### 3. `WebTransport` Field and Default

Add a `staticAssets :: Maybe StaticAssets` field to the `WebTransport` record
(`Web.hs:149`), defaulting to `Nothing` in `server` (`Web.hs:189`) — off by
default (Design Goal 3), exactly as `corsConfig` defaults to `Nothing`.

```haskell
server :: WebTransport
server =
  WebTransport
    { -- ...existing fields...
      corsConfig = Nothing,
      staticAssets = Nothing,   -- new: opt-in via Application.withStaticAssets
      -- ...
    }
```

### 4. Factory, Resolution, and Threading (Mirror of `withCors`)

`withStaticAssets` stores a factory on the `Application`, following the
established `CorsFactory` / `ApiInfoFactory` pattern (`Application.hs:309`). This
supports both the `@()`-immediate form and the config-deferred form with a
single signature — identical ergonomics to `withApiInfo`:

```haskell
withStaticAssets ::
  forall config.
  (Typeable config) =>
  (config -> Web.StaticAssets) ->
  Application ->
  Application
```

Resolution mirrors `withCors` end to end:

- `Application.run` resolves the factory into a `Maybe StaticAssets`
  (alongside `resolvedCorsConfig` at `Application.hs:849`), then threads it
  through `runTransports` (the `app.corsConfig` argument list at
  `Application.hs:1329`) as one additional parameter.
- `Transports.runWebTransport` adds `staticAssets = maybeStaticAssets` to the
  `webTransportWithConfig` record-update clobber list
  (`Transports.hs:128-139`), exactly where `corsConfig = maybeCors` already sits.

No new resolution *shape* is introduced — this is the same plumbing the family
already uses.

### 5. Terminal Fallback — API Routes Always Win

Static serving replaces the terminal `_ -> notFound "Not found"` at `Web.hs:1026`.
Every existing branch — commands, queries, `/health`, `/ready`, OAuth2, file
upload, `/openapi.yaml`, `/docs` — is matched *first* and is unaffected. Only a
request that matches nothing reaches the static fallback:

```haskell
_ -> case endpoints.transport.staticAssets of
  Nothing -> notFound "Not found"          -- unchanged behaviour when disabled
  Just assets -> serveStaticAsset assets request respond
```

This satisfies Design Goal 2: enabling static assets can never shadow an API
route, because the router only reaches it after every API branch has declined.

### 6. SPA Deep-Link Fallback

Inside `serveStaticAsset`, when the normalized request path resolves to a real
file under `root`, that file is streamed. When it does not (a client-side route
like `/orders/42`) and `spaFallback` is `Just doc`, the fallback document is
served with the entry-document cache policy so the SPA router can handle the
route. When `spaFallback` is `Nothing`, an unmatched path is a plain `404`.

### 7. Cache-Control Policy (Load-Bearing)

The framework picks the correct `Cache-Control` so the developer never has to
(Design Goal 4):

| Asset | `Cache-Control` | Why |
|-------|-----------------|-----|
| Content-hashed bundle (`/assets/*.<hash>.js`, `.css`, …) | `public, max-age=31536000, immutable` | The hash *is* the version; the URL never serves different bytes, so caching forever is safe and fast. |
| `index.html` and other non-hashed entry documents (incl. the SPA fallback) | `no-cache, must-revalidate` | The entry document points at the current hashed bundles; it must be re-fetched so a hot rebuild is picked up instead of a stale shell. |

Hashed assets are recognized by the bundler's `name.<hash>.ext` filename shape.
Getting this split right is what makes hot-rebuild-during-dev usable and is not
optional.

### 8. Path-Traversal Safety

The requested path is normalized and guarded against `..` segments before being
joined to `root`, so no request can escape the configured directory. A path that
would resolve outside `root` is rejected as `404` (never `403` — do not confirm
existence). This guard runs before any disk access.

### 9. No New Dependencies

Files are streamed with WAI's `responseFile`; Warp handles the body. No
`wai-app-static`, no new package in `nhcore.cabal`.

### 10. Example Usage

```haskell
app :: Application
app =
  Application.new
    |> Application.withTransport WebTransport.server
    |> Application.withStaticAssets @()
        (\_ -> StaticAssets { root = "static", spaFallback = Just "index.html" })
    |> Application.withService MyService.service
```

One line, same shape as `withApiInfo @() (\_ -> ...)` in the testbed. The
developer names a directory and (optionally) a fallback document; the caching,
traversal guard, and route ordering are handled by the framework.

### 11. Files Modified

| File | Change |
|------|--------|
| `core/service/Service/Transport/Web.hs` | `StaticAssets` type; `staticAssets :: Maybe StaticAssets` field on `WebTransport` (default `Nothing`); `serveStaticAsset` helper; terminal fallback at `:1026`. |
| `core/service/Service/Application.hs` | `StaticAssetsFactory`; `withStaticAssets` builder; factory resolution in `run`; extra argument through the `runTransports` call. |
| `core/service/Service/Application/Transports.hs` | `Maybe StaticAssets` parameter threaded through `runTransports` / `runWebTransport`; `staticAssets = maybeStaticAssets` in the `runWebTransport` record-update. |

### 12. Security Considerations

- **Path traversal** is blocked by normalization + `..` guard (§8); the served
  set is strictly the contents of `root`.
- **Opt-in only** — no directory is served unless `withStaticAssets` is present,
  so existing apps' 404 semantics and filesystem exposure are unchanged.
- **API precedence** — static serving is the terminal branch, so it cannot
  shadow or intercept an authenticated API route (§5).
- **Unauthenticated** — this initial version serves assets without authn/authz
  (typical for public SPA shells); gating static assets behind auth is
  explicitly out of scope (see below).

### 13. Performance Considerations

- `responseFile` streams from disk via Warp's native sendfile path; bodies are
  not buffered into memory.
- The static branch is reached only for requests that match no API route, so the
  API hot path is untouched — zero overhead when a request is an API call, and
  zero overhead for all requests when `staticAssets = Nothing`.
- Aggressive `immutable` caching of hashed bundles means repeat visits are served
  from the browser cache, not the server.

## Consequences

### Positive

- **One process, one origin**: The same NeoHaskell server serves UI and API, so
  the common small-app case needs no second server and no CORS.
- **Deep links just work**: The SPA fallback makes reloads of client-side routes
  return the app shell instead of a 404.
- **Familiar**: A developer who knows any of the nine existing web combinators
  uses this one correctly from autocomplete in well under 15 minutes.
- **Correct caching for free**: Hot rebuilds are picked up; hashed bundles are
  cached forever — without the developer touching `Cache-Control`.
- **Zero cost when unused**: `Nothing` default means no behavioural or
  performance change for existing apps.

### Negative

- **Chose consistency over the "better" long-term shape**: keeping this at the
  `Application` level (rather than a transport-level combinator) preserves the
  existing decoupled convention but perpetuates it; if the family is ever moved
  transport-level, this must move with it.
- **A little Application/Transports plumbing**: one field, one factory, one extra
  threaded argument — the standard cost of matching the family.

### Risks

- **Serving a directory that contains secrets**: If a developer points `root` at
  a directory with sensitive files, those become public. Mitigation: it is
  opt-in and documented as "serve your built SPA output (e.g. `static`)", and the
  traversal guard prevents escaping `root` — but the developer still chooses
  `root`.
- **Bundler hash naming variance**: A bundler that does not emit
  `name.<hash>.ext` would miss the `immutable` classification and fall back to
  the safe `no-cache` policy (correct but less cached). Acceptable: the default
  degrades to safe, never to stale.

### Mitigations

- Path normalization + `..` guard (§8) contains serving to `root`.
- Unknown-shaped filenames default to the revalidating (`no-cache`) policy, so a
  misclassification can only cost caching, never serve stale entry documents.
- Documentation steers `root` at build output, not source or config directories.

## Out of Scope

- **Server → client push** (SSE / WebSockets) — a separate transport concern.
- **Authn/authz for static assets** — the initial version serves assets
  unauthenticated; gating them behind auth is a future decision.

## References

- [#707: Serve static SPA assets from WebTransport](https://github.com/neohaskell/NeoHaskell/issues/707)
- [ADR-0024: CORS Support for WebTransport](0024-cors-support.md)
- [ADR-0025: Auto Health Endpoint for WebTransport Apps](0025-auto-health-endpoint.md)
- [ADR-0011: File Upload Architecture](0011-file-upload-architecture.md)
- [ADR-0035: Config-Dependent Application Builders](0035-config-dependent-application-builders.md)
- [core/service/Service/Transport/Web.hs](../../core/service/Service/Transport/Web.hs)
- [core/service/Service/Application.hs](../../core/service/Service/Application.hs)
- [core/service/Service/Application/Transports.hs](../../core/service/Service/Application/Transports.hs)
