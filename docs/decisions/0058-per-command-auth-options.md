# ADR-0058: Per-command access control via `canAccess`

## Status

Proposed

## Context

### Current state

NeoHaskell treats command authentication as all-or-nothing at the transport layer. When `Application.withAuth` is wired, every command endpoint requires a valid JWT regardless of which command. `Service.Transport.Web.hs:583` hard-codes `Authenticated` when calling `Middleware.checkAuth` for command dispatch:

```haskell
authResult <-
  Middleware.checkAuth (Just auth.jwksManager) auth.authConfig Authenticated request
```

The `AuthOptions` ADT in `core/auth/Auth/Options.hs` already supports both modes (`Everyone | Authenticated`) and `Middleware.checkAuth` already branches correctly: `Everyone` returns an empty `AuthContext`, `Authenticated` validates the token and rejects on any `AuthError`. The gap is purely on the command-registration side. `Service.Command.Core` has no auth-related method on the `Command` typeclass, so there is no way for a command to opt into a different policy.

Queries already solved exactly this problem. `Service.Query.Core.hs:77-88` defines a `Query` typeclass with `canAccessImpl :: Maybe UserClaims -> Maybe QueryAuthError`. The user writes a top-level `canAccess` next to the query definition and the TH macro (`deriveQuery`) wires it into the instance. `Service.Query.Auth` already provides the helpers `publicAccess`, `authenticatedAccess`, `requirePermission`, `requireAnyPermission`, `requireAllPermissions`, with the `QueryAuthError` shape `Unauthenticated | Forbidden | InsufficientPermissions [Text]`. The web layer mirrors this on the query path at `Service/Transport/Web.hs:665-679`: missing token degrades to anonymous context (and the query's own `canAccessImpl` decides), but any *invalid* token still rejects.

This ADR aligns commands with the query pattern. The first draft proposed a `type family AuthOptionsOf` plus an `EndpointHandler` record refactor; maintainer review rejected that direction in favour of the typeclass-method approach used by queries — see "Considered options".

GitHub issue: [#638](https://github.com/neohaskell/NeoHaskell/issues/638).

### Use cases

- **Anonymous feedback form.** Jess builds a SaaS dashboard with one public command `SubmitFeedback` so visitors can leave notes without signing up, alongside many authenticated commands.
- **Newsletter signup.** A public landing page sends `SignupForNewsletter` to the same service that handles authenticated user commands.
- **Admin-only delete.** A `PurgeAccount` command must require the `admin:delete` permission, not just any authenticated user.
- **Public webhook receiver.** An external system posts to a public command endpoint signed with HMAC at the body level rather than JWT.

### Design drivers

1. **Mirror the query auth pattern exactly.** Jess already learned `canAccess = authenticatedAccess` on queries. The command-side spelling must be identical, so she only learns one mental model.
2. **Whitelist by default.** A command with no override blocks unauthenticated requests at the framework level. Going public must be a deliberate, single-line declaration colocated with `decide`.
3. **Override colocated with `decide`.** The opt-out lives in the command module next to the business logic, not in a separate `type instance` declaration somewhere else.
4. **Permission helpers reused verbatim.** `requirePermission`, `requireAnyPermission`, `requireAllPermissions` already exist in `Service.Query.Auth`. The command side must consume them without any wrapper or re-export ceremony.
5. **Backwards compatible.** Existing services with no `canAccess` keep behaving exactly as today (private by default).
6. **Pass the Jess 15-minute test.** Making `SubmitFeedback` public must take ≤ 15 minutes from the moment Jess opens the file.

## Considered options

### Option A — `canAccessImpl` method on the `Command` typeclass (CHOSEN)

Add a method `canAccessImpl :: Maybe UserClaims -> Maybe CommandAuthError` to the `Command` typeclass with a default of `authenticatedAccess`. The user writes a top-level `canAccess` next to `decide` and TH wires it in just like it already wires `decideImpl`. Reuse `Service.Query.Auth.QueryAuthError` (constructors `Unauthenticated | Forbidden | InsufficientPermissions [Text]`) by re-exporting it under the alias `CommandAuthError` — the error shape is identical and duplicating the type would force every helper (`publicAccess`, `authenticatedAccess`, `requirePermission`, `requireAnyPermission`, `requireAllPermissions`) to be duplicated too. The dispatcher in `Service.CommandExecutor.Core.execute` calls `canAccessImpl @command (requestContext.user)` before invoking `decideImpl`; the Web transport falls through to anonymous context on missing token (mirroring the query path), and the dispatcher decides whether anonymous is acceptable for this command.

Verdict: **chosen.** Mirrors the query precedent verbatim. Whitelist by default is enforced at the typeclass head — a command author who writes nothing gets the secure behaviour. The override is a single top-level function colocated with `decide`. Permission helpers transfer for free.

### Option B — `AuthOptionsOf` type family + `EndpointHandler` record (REJECTED)

The first draft of this ADR proposed a closed knob: `type family AuthOptionsOf (commandType :: Type) :: AuthOptions` with a head default of `'Authenticated`, plus promoting `EndpointHandler` from a function alias to a record carrying the resolved `AuthOptions`. The Web transport would branch on the resolved value at dispatch time.

Rejected for four reasons:

1. **Does not mirror the query precedent it claims to.** Queries use a typeclass method, not a type family. The whole stated goal "one mental model for commands and queries" is undercut the moment commands use a different mechanism.
2. **`AuthOptions = Everyone | Authenticated` cannot express permission-based access.** Adding `requirePermission "admin:delete"` would require either growing the ADT (`Permission Text`, `AnyPermission [Text]`, …) — at which point it becomes a poor reinvention of the existing `Maybe QueryAuthError` shape — or layering a second check inside `decide`, which is exactly what this ADR is trying to remove.
3. **`EndpointHandler` record refactor is large and gratuitous.** It touches every transport (Web, Cli, Mcp, Internal) and every consumer of the handler map. The typeclass-method approach needs zero changes to `EndpointHandler` because the policy travels with the command type, not with the handler value.
4. **Override is not colocated with `decide`.** The user has to write `type instance AuthOptionsOf SubmitFeedback = 'Everyone` as a separate top-level type declaration. The maintainer wants the policy to live next to the business logic, the way `canAccess` already does on the query side.

### Option C — In-decider check + one-site Web.hs fallthrough (REJECTED)

A smaller change: drop the hardcoded `Authenticated` in `Service.Transport.Web.hs:583` so missing tokens fall through to anonymous, and rely on each command's `decide` function to check `ctx.user` and `Decider.reject "Unauthorized"` when it cares.

Rejected because it is not a whitelist. Forgetting to check `ctx.user` in a freshly-written decider silently makes that command public. The whole reason this ADR exists is to make "I forgot to think about auth" produce a 401, not a 200. The Jess test fails by construction: an autocomplete-driven junior writes the decider body and ships, never knowing she had to add an auth guard.

## Decision

Adopt Option A. Concretely:

### 1. Add `canAccessImpl` to the `Command` typeclass

In `core/service/Service/Command/Core.hs` (the class declaration starts at line 111), add a third method alongside `getEntityIdImpl` and `decideImpl`:

```haskell
class Command command where
  type IsMultiTenant command :: Bool
  type IsMultiTenant command = False

  getEntityIdImpl :: GetEntityIdFunction (IsMultiTenant command) command (EntityIdType (EntityOf command))
  decideImpl     :: DecideFunction (IsMultiTenant command) command (EntityOf command) (EventOf (EntityOf command))

  -- | Authorization check run before 'decideImpl'.
  --
  -- Returns 'Nothing' if the caller may execute this command, or
  -- 'Just' a 'CommandAuthError' explaining why not. Defaults to
  -- 'authenticatedAccess' so a command with no 'canAccess' definition
  -- blocks unauthenticated callers — going public is an explicit opt-in.
  canAccessImpl :: Maybe UserClaims -> Maybe CommandAuthError
  canAccessImpl = authenticatedAccess
```

The default `canAccessImpl = authenticatedAccess` is the whitelist guarantee. A command with no `canAccess` definition in its module inherits the default and rejects unauthenticated callers — the only way to reach public access is to write `canAccess = publicAccess` (or another helper) next to `decide`.

### 2. Reuse `QueryAuthError` as `CommandAuthError`

`Service.Query.Auth.QueryAuthError` already has the exact constructor set commands need (`Unauthenticated | Forbidden | InsufficientPermissions (Array Text)`). Duplicating it as a sibling `Service.Command.Auth.CommandAuthError` would force duplicating every helper too (`publicAccess`, `authenticatedAccess`, `requirePermission`, `requireAnyPermission`, `requireAllPermissions`), each with an identical body. Instead, add a small `Service.Command.Auth` module that re-exports the relevant names:

```haskell
module Service.Command.Auth (
  -- * Error type (alias of Service.Query.Auth.QueryAuthError)
  CommandAuthError,
  pattern Unauthenticated,
  pattern Forbidden,
  pattern InsufficientPermissions,

  -- * Pre-dispatch helpers (canAccess)
  publicAccess,
  authenticatedAccess,
  requirePermission,
  requireAnyPermission,
  requireAllPermissions,
) where

import Service.Query.Auth (
  QueryAuthError (..),
  publicAccess,
  authenticatedAccess,
  requirePermission,
  requireAnyPermission,
  requireAllPermissions,
  )

type CommandAuthError = QueryAuthError
```

The alias name keeps the command-side type signature self-documenting (`canAccess :: Maybe UserClaims -> Maybe CommandAuthError`) without forking a second copy of the error model. If a future ADR needs command-specific error constructors, `CommandAuthError` becomes a newtype with explicit conversions, but until then the alias is the right call.

### 3. TH wiring in `Service.CommandExecutor.TH`

The `command` macro at `core/service/Service/CommandExecutor/TH.hs:253` already wires `getEntityIdImpl` from a top-level `getEntityId` and `decideImpl` from a top-level `decide`. Add one more lookup for `canAccess` — but unlike `getEntityId`/`decide`, this one is **optional**. When the user defines `canAccess`, the macro emits a `canAccessImpl = canAccess` binding inside the `Command` instance. When the user defines nothing, the binding is omitted and the typeclass default (`authenticatedAccess`) applies. No user action is required for the secure default.

Sketch:

```haskell
-- Inside the 'command' macro, after the existing lookups:
maybeCanAccess <- TH.lookupValueName "canAccess"

let canAccessBinding = case maybeCanAccess of
      Just canAccessName ->
        [TH.ValD (TH.VarP (TH.mkName "canAccessImpl")) (TH.NormalB (TH.VarE canAccessName)) []]
      Nothing ->
        []  -- Typeclass default applies.

let commandInstance =
      TH.InstanceD
        Nothing
        []
        (TH.ConT commandClassName `TH.AppT` TH.ConT someName)
        ( multiTenancyDecl
            ++ [ TH.ValD (TH.VarP (TH.mkName "getEntityIdImpl")) (TH.NormalB (TH.VarE getEntityId)) [],
                 TH.ValD (TH.VarP (TH.mkName "decideImpl")) (TH.NormalB (TH.VarE decide)) []
               ]
            ++ canAccessBinding
        )
```

No signature validation step is added: if the user writes `canAccess` with a wrong type, GHC reports the instance-body type error against `canAccessImpl`. The error sites it on the user's `canAccess` definition, which is exactly what Jess needs.

### 4. Dispatcher calls `canAccessImpl` before `decideImpl`

`Service.CommandExecutor.Core.execute` at line 134 is the single dispatcher entry point. Before any tenant-id extraction or entity-fetching, it consults `canAccessImpl` with the request context's user claims and translates a `Just authErr` into the same HTTP status that queries use today. The query endpoint at `Service/Query/Endpoint.hs:50-54` shows the pattern:

```haskell
case canAccessImpl @query userClaims of
  Just authErr -> Task.throw (Auth.AuthorizationError authErr)
  Nothing -> ...
```

Commands need the same shape, but `execute` returns an `ExecutionResult` rather than throwing a typed endpoint error, so the equivalent is a new `CommandUnauthorized` constructor (or a typed exception consumed by the Web transport — see consequences). The proposed shape:

```haskell
execute eventStore entityFetcher entityName requestContext command = do
  case canAccessImpl @command (requestContext.user) of
    Just authErr ->
      Task.yield (CommandUnauthorized authErr)
    Nothing ->
      case sbool @(IsMultiTenant command) of
        SFalse -> ...  -- unchanged
        STrue  -> ...  -- unchanged
```

The Web transport maps `CommandUnauthorized authErr` to status codes identically to the query side (see `Service/Transport/Web.hs:646-651`):

| `CommandAuthError` constructor | HTTP status | Body message |
|---|---|---|
| `Unauthenticated` | 401 | `Authentication required` |
| `Forbidden` | 403 | `Access denied` |
| `InsufficientPermissions _` | 403 | `Insufficient permissions` |

**Audit logging.** The dispatcher is the single site where `CommandUnauthorized` is produced, so it is also the single site where rejected commands are logged. Every `Just authErr` branch above emits one `Log.warn` record with three fields and three fields only: the command type name (already known from the typeclass dispatch), a boolean `claims_present` (derived from `requestContext.user`, never the claims themselves), and the `CommandAuthError` constructor name (`Unauthenticated` / `Forbidden` / `InsufficientPermissions`). Permission strings, claim contents, token bytes, and JWT subjects are never logged — they would leak sensitive identifiers into the operator's log pipeline and turn the audit trail into a secondary credential surface. Logging at this site (rather than per-transport) gives operators a single grep target for brute-force detection while keeping transports stateless.

### 5. Web.hs fallthrough at line 583

`Service/Transport/Web.hs:583` currently hardcodes `Authenticated` and rejects on every `AuthError`. The new shape mirrors the query path at lines 665-679:

```haskell
case webTransport.authEnabled of
  Maybe.Nothing -> do
    requestContext <- Auth.anonymousContext
    processCommandWithContext requestContext
  Maybe.Just auth -> do
    -- The command's canAccessImpl decides if authentication is required.
    -- We still preserve checkAuth's AuthOptions arg by passing Authenticated:
    -- it controls whether an invalid token gets rejected here, but missing
    -- tokens are now allowed through as anonymous.
    authResult <-
      Middleware.checkAuth (Maybe.Just auth.jwksManager) auth.authConfig Authenticated request
    case authResult of
      Result.Err authErr -> case authErr of
        TokenMissing -> do
          -- Missing token: build anonymous context, let the dispatcher's
          -- canAccessImpl decide whether this command tolerates that.
          requestContext <- Auth.anonymousContext
          processCommandWithContext requestContext
        _ ->
          -- Any other AuthError (malformed, expired, bad signature, infra)
          -- still rejects. An invalid token is never silently accepted.
          Middleware.respondWithAuthError authErr respond
      Result.Ok authContext -> do
        requestContext <- case authContext.claims of
          Maybe.Just claims -> Auth.authenticatedContext claims
          Maybe.Nothing    -> Auth.anonymousContext
        processCommandWithContext requestContext
```

Two invariants hold by construction:

- **Missing token + public command → 200 anonymous.** The Web layer constructs `Auth.anonymousContext` and the dispatcher's `canAccessImpl` (which is `publicAccess` for that command) returns `Nothing`, so execution proceeds.
- **Invalid token (any reason) → 401, always.** Malformed, expired, bad-signature, key-not-found, and infra-unavailable errors are rejected at the middleware before they ever reach the dispatcher. An attacker cannot use a public command as a JWT-validity oracle, and a buggy client that ships a stale token cannot silently downgrade to anonymous.

### 6. Out-of-scope call sites

`Service/Transport/Web.hs:783` is the **OAuth2 disconnect** route — it consumes `claims.sub` directly to look up the user's OAuth account, so it is genuinely framework-managed and not a user-defined command. `Service/Transport/Web.hs:823` is the **file-upload** route, which sets `ownerHash = claims.sub` for object-store partitioning, also framework-managed. Both remain hardcoded `Authenticated` in this ADR. They are not on the command-dispatch code path, so the policy change at line 583 has no effect on them. If we later want to thread per-route policy through these endpoints, that is a separate, smaller ADR.

### 7. Module placement

| Change | File |
|---|---|
| Add `canAccessImpl` method with `authenticatedAccess` default to `Command` typeclass | `core/service/Service/Command/Core.hs` |
| New module re-exporting `QueryAuthError` as `CommandAuthError` + helpers | `core/service/Service/Command/Auth.hs` |
| Wire optional top-level `canAccess` into `canAccessImpl` | `core/service/Service/CommandExecutor/TH.hs` |
| Call `canAccessImpl` before `decideImpl`; add `CommandUnauthorized` to `ExecutionResult` | `core/service/Service/CommandExecutor/Core.hs` |
| Replace hardcoded `Authenticated` at line 583 with the missing-token fallthrough; map `CommandUnauthorized` to 401/403 | `core/service/Service/Transport/Web.hs` |
| TH-resolution test: command with no `canAccess` resolves to default; command with `canAccess = publicAccess` resolves correctly | `core/test/Service/Command/CanAccessSpec.hs` |
| Web-middleware dispatch test: missing/invalid/valid token × default/`publicAccess`/`requirePermission` | `core/test/Service/Transport/Web/CommandAuthSpec.hs` |

## Public API

### The `Command` typeclass and its default

```haskell
-- In Service.Command.Core (new method on the existing class)
class Command command where
  type IsMultiTenant command :: Bool
  type IsMultiTenant command = False

  getEntityIdImpl :: GetEntityIdFunction (IsMultiTenant command) command (EntityIdType (EntityOf command))
  decideImpl     :: DecideFunction (IsMultiTenant command) command (EntityOf command) (EventOf (EntityOf command))

  canAccessImpl :: Maybe UserClaims -> Maybe CommandAuthError
  canAccessImpl = authenticatedAccess
```

### Example 1 — public command (`SubmitFeedback`)

```haskell
module App.Feedback.SubmitFeedback where

import Core
import Service.Command.Auth (publicAccess)

data SubmitFeedback = SubmitFeedback
  { message :: Text,
    submittedAt :: DateTime
  }

type instance EntityOf SubmitFeedback = FeedbackEntity

getEntityId :: SubmitFeedback -> Maybe Uuid
getEntityId _ = Nothing

canAccess :: Maybe UserClaims -> Maybe CommandAuthError
canAccess = publicAccess

decide :: SubmitFeedback -> Maybe FeedbackEntity -> RequestContext -> Decision FeedbackEvent
decide submitFeedback _entity _ctx =
  [FeedbackSubmitted submitFeedback.message submitFeedback.submittedAt]
    |> Decision.acceptNew

command ''SubmitFeedback
```

Three lines — `canAccess :: ...`, `canAccess = publicAccess`, and the import — are everything Jess writes to make the endpoint public. The macro at the bottom of the file picks up `canAccess` automatically; if she deletes those three lines, the command falls back to the typeclass default and rejects unauthenticated callers.

### Example 2 — permission-gated command (`PurgeAccount`)

```haskell
module App.Admin.PurgeAccount where

import Core
import Service.Command.Auth (requirePermission)

data PurgeAccount = PurgeAccount { accountId :: Uuid }

type instance EntityOf PurgeAccount = AccountEntity

getEntityId :: PurgeAccount -> Maybe Uuid
getEntityId purgeAccount = Just purgeAccount.accountId

canAccess :: Maybe UserClaims -> Maybe CommandAuthError
canAccess = requirePermission "admin:delete"

decide :: PurgeAccount -> Maybe AccountEntity -> RequestContext -> Decision AccountEvent
decide _command entity _ctx = case entity of
  Maybe.Nothing -> Decision.reject "Account not found"
  Maybe.Just _  -> Decision.acceptExisting [AccountPurged]

command ''PurgeAccount
```

`requirePermission`, `requireAnyPermission`, and `requireAllPermissions` are imported verbatim from the existing query helpers — no command-specific wrapper, no new helper module beyond the re-export. A non-admin caller (claims present, `"admin:delete"` not in `claims.permissions`) gets `Just (InsufficientPermissions ["admin:delete"])`, which the Web transport maps to HTTP 403 `Insufficient permissions`. This is the strongest argument for the typeclass-method design over the rejected type-family design: permission helpers light up for free.

### Example 3 — Web.hs fallthrough sketch

The relevant slice of `Service/Transport/Web.hs:583` after this ADR lands:

```haskell
case webTransport.authEnabled of
  Maybe.Nothing -> do
    requestContext <- Auth.anonymousContext
    processCommandWithContext requestContext
  Maybe.Just auth -> do
    authResult <-
      Middleware.checkAuth (Maybe.Just auth.jwksManager) auth.authConfig Authenticated request
    case authResult of
      Result.Err authErr -> case authErr of
        TokenMissing -> do
          requestContext <- Auth.anonymousContext
          processCommandWithContext requestContext
        _ ->
          Middleware.respondWithAuthError authErr respond
      Result.Ok authContext -> do
        requestContext <- case authContext.claims of
          Maybe.Just claims -> Auth.authenticatedContext claims
          Maybe.Nothing    -> Auth.anonymousContext
        processCommandWithContext requestContext
```

The dispatcher inside `processCommandWithContext` (now reaching `Service.CommandExecutor.Core.execute`) calls `canAccessImpl @command (requestContext.user)` first, and on `Just authErr` short-circuits with `CommandUnauthorized` which the transport renders as 401/403 using the same mapping as queries.

### Resulting matrix for `POST /commands/submit-feedback`

| Request | Default (`canAccess` omitted) | `canAccess = publicAccess` | `canAccess = requirePermission "admin:delete"` |
|---|---|---|---|
| No `Authorization` header | 401 `Authentication required` | 200, anonymous `RequestContext` | 401 `Authentication required` |
| Valid JWT, no required perm | 200, authenticated context | 200, authenticated context | 403 `Insufficient permissions` |
| Valid JWT, required perm present | 200, authenticated context | 200, authenticated context | 200, authenticated context |
| Expired JWT | 401 `Authentication failed` | 401 `Authentication failed` | 401 `Authentication failed` |
| Malformed JWT | 401 `Authentication failed` | 401 `Authentication failed` | 401 `Authentication failed` |
| Bad signature | 401 `Authentication failed` | 401 `Authentication failed` | 401 `Authentication failed` |
| JWKS unavailable | 503 `Service temporarily unavailable` | 503 `Service temporarily unavailable` | 503 `Service temporarily unavailable` |

### Jess 15-minute walkthrough

1. Jess copies `SubmitFeedback` from the example. Autocomplete on `Service.Command.Auth.` lists `publicAccess`, `authenticatedAccess`, `requirePermission`.
2. She writes `canAccess = publicAccess` next to her existing `decide`.
3. `cabal build` — the `command` macro picks up `canAccess`; no other change needed.
4. `curl -X POST http://localhost:8080/commands/submit-feedback -d '{"message":"hi"}'` succeeds with no `Authorization` header.
5. To require admin instead, she changes one line: `canAccess = requirePermission "admin:delete"`. Same import, same shape.

No pragma, no config flag, no second type-instance declaration somewhere else in the file. The override lives where the decision lives.

## Consequences

### Positive

- **One mental model for commands and queries.** `canAccess = publicAccess` is the same incantation on both sides; Jess never has to remember two spellings.
- **Permission helpers transfer for free.** `requirePermission`, `requireAnyPermission`, `requireAllPermissions` already exist in `Service.Query.Auth` and produce the same `Maybe QueryAuthError` shape. Re-exporting them under `Service.Command.Auth` is a five-line module — no new error type, no new helper bodies, no per-command boilerplate. This is the largest concrete win over the rejected type-family design, which could only express `Everyone | Authenticated`.
- **Whitelist by default at the framework layer.** A junior developer who writes a new command and forgets `canAccess` cannot accidentally ship a public endpoint. The typeclass default forces them through `authenticatedAccess`.
- **Override colocated with `decide`.** No separate `type instance` declaration; the policy is a top-level function in the same module as the business logic, where reviewers will find it.
- **No `EndpointHandler` refactor.** Every existing transport (Cli, Mcp, Internal) is untouched. The Web transport changes one site (line 583). The dispatcher in `CommandExecutor.Core.execute` gains one `case` and a new `CommandUnauthorized` variant.
- **Hardcoded `Authenticated` literal at `Service.Transport.Web.hs:583` is gone.** Auth policy now lives next to the command definition, not in the transport.

### Negative

- **`ExecutionResult` gains a new constructor (`CommandUnauthorized CommandAuthError`).** Every consumer that pattern-matches on `ExecutionResult` must handle it. The blast radius is small (`Service.CommandExecutor.Core`, `Service.Transport.Web`, the existing test suites) and the alternative — throwing a typed exception through the `Task Text` channel — would be lossy (the `Text` shape erases the constructor needed for the 401/403 split).
- **`CommandAuthError` is an alias for `QueryAuthError`.** A future need for command-specific error constructors (e.g. `RateLimited`) would require promoting the alias to a real type with explicit conversions. This is a forward-looking risk, not a current cost: the constructors `Unauthenticated | Forbidden | InsufficientPermissions` cover every case in scope for #638.

### Risks

- **Accidental public exposure.** Mitigated by the typeclass default: a command that does not define `canAccess` inherits `authenticatedAccess` from the class declaration. There is no implicit path to public.
- **JWT-validity oracle on public commands.** Mitigated by the Web.hs fallthrough only treating `TokenMissing` as anonymous-eligible. Every other `AuthError` (malformed, expired, bad signature, key not found, infra unavailable) rejects at the middleware before reaching the dispatcher, exactly as on the query side. An attacker cannot probe token validity by hitting a public command.
- **Public commands reading `ctx.user`.** A command that opts into `publicAccess` will receive `requestContext.user = Maybe.Nothing` when the caller is unauthenticated. If `decide` reads `ctx.user` without handling `Nothing`, behaviour matches the existing internal-transport path (ADR-0050): the command rejects via `Decider.reject`. This is documentation-level guidance, not a framework gap.
- **TH macro silent on `canAccess` typos.** If the user writes `caAccess = publicAccess` (typo) the macro will not find `canAccess` and silently fall back to the default. Mitigation: the integration test for "default applies when `canAccess` absent" exercises this path; a hlint rule could later flag suspicious near-misses, but it is out of scope here.

### Mitigations

- TH-resolution test at `core/test/Service/Command/CanAccessSpec.hs`: confirms a command with no top-level `canAccess` resolves `canAccessImpl` to `authenticatedAccess`, and a command with `canAccess = publicAccess` resolves to `publicAccess`.
- Web-middleware dispatch test at `core/test/Service/Transport/Web/CommandAuthSpec.hs`: covers all rows of the matrix above (default / `publicAccess` / `requirePermission`, missing / valid / invalid / expired token).
- Document the invariant "invalid token is never silently accepted" in the `canAccessImpl` docstring next to the class declaration, so anyone touching the Web fallthrough later sees the contract.

## References

- [#638: feat: Per-command AuthOptions — let commands declare Everyone vs Authenticated](https://github.com/neohaskell/NeoHaskell/issues/638)
- [ADR-0003: Command Abstraction and Flow](0003-command-abstraction-and-flow.md)
- [ADR-0007: Queries (Read Models)](0007-queries-read-models.md)
- [ADR-0009: JWT Authentication Middleware](0009-jwt-authentication-middleware.md)
- [ADR-0050: Internal Command Transport](0050-internal-command-transport.md)
- [ADR-0054: Multi-Tenant Support for Commands and Queries](0054-multi-tenant-command-query-support.md)
- [core/auth/Auth/Options.hs](../../core/auth/Auth/Options.hs)
- [core/auth/Auth/Middleware.hs](../../core/auth/Auth/Middleware.hs)
- [core/service/Service/Auth.hs](../../core/service/Service/Auth.hs)
- [core/service/Service/Command/Core.hs](../../core/service/Service/Command/Core.hs)
- [core/service/Service/CommandExecutor/Core.hs](../../core/service/Service/CommandExecutor/Core.hs)
- [core/service/Service/CommandExecutor/TH.hs](../../core/service/Service/CommandExecutor/TH.hs)
- [core/service/Service/Query/Core.hs](../../core/service/Service/Query/Core.hs)
- [core/service/Service/Query/Auth.hs](../../core/service/Service/Query/Auth.hs)
- [core/service/Service/Query/Endpoint.hs](../../core/service/Service/Query/Endpoint.hs)
- [core/service/Service/Transport/Web.hs](../../core/service/Service/Transport/Web.hs)
