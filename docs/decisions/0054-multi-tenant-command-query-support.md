# ADR-0054: Multi-Tenant Support for Commands and Queries

## Status

Accepted

## Context

### Current State

The service framework has **partial multi-tenant infrastructure** that is ready at the type level but not wired through execution. The `IsMultiTenant` type family and associated `GetEntityIdFunction`/`DecideFunction` type families are defined in `Service.Command.Core`, and the `command` TH macro validates signatures correctly for both single-tenant and multi-tenant modes. `UserClaims.tenantId` is extracted from JWT claims and available in `RequestContext`.

However, the actual execution path in `CommandExecutor.execute` carries a hard constraint `IsMultiTenant command ~ 'False` (line 124), making it impossible to execute any command with `IsMultiTenant = True`. The same constraint exists in `ServiceDefinition.Core`'s `CommandInspect` instance (line 353) and `buildCommandResponseHandler` (line 294).

On the query side, there is no tenant awareness at all. `QueryObjectStore.getAll` returns all tenants' data, `Query.Endpoint` filters by `canView` but has no tenant concept, and `QueryOf.queryId`/`combine` have no tenant parameter.

This means a developer cannot build a multi-tenant service today without manually threading tenant IDs through every command and filtering every query by hand.

### Use Cases

- **SaaS platform**: Jess builds a project management SaaS where each company (tenant) has isolated projects, tasks, and members. Commands like `CreateProject` need the tenant ID to scope the entity, and the `ProjectSummary` query must only return projects belonging to the requesting user's tenant.

- **White-label service**: A restaurant ordering platform where each restaurant chain is a tenant. `PlaceOrder` commands must be scoped to the tenant, and `OrderHistory` queries must be filtered to the authenticated tenant's data only.

- **Multi-org internal tool**: An enterprise tool where employees belong to different organizations. The same service handles all orgs, but data must be isolated per org via the tenant claim in SSO tokens.

### Design Goals

1. **Zero impact on single-tenant services**: The default remains `IsMultiTenant = False`. Existing services compile and run without changes.

2. **Type-safe tenant threading**: Multi-tenant commands receive the tenant UUID as a first argument to `getEntityId` and `decide` (already designed). Multi-tenant queries enforce tenant scoping at the type level.

3. **Stream isolation by default**: Multi-tenant commands produce tenant-prefixed `StreamId`s, ensuring event streams are physically isolated per tenant at the storage layer.

4. **Fail-fast on missing tenant**: If a command or query requires multi-tenancy but the JWT lacks a `tenantId` claim, the framework rejects the request immediately with a clear error, before any business logic runs.

5. **Jess-friendly API**: Adding multi-tenancy to a command or query requires exactly two changes: `type MultiTenancy = True` and adjusting function signatures. No boilerplate, no manual tenant threading.

### GitHub Issue

- [#602: feat(service): complete multi-tenant support for commands and queries](https://github.com/neohaskell/NeoHaskell/issues/602)

## Decision

### 1. CommandExecutor: Lift the `IsMultiTenant ~ False` Constraint

Remove the `IsMultiTenant command ~ 'False` constraint from `CommandExecutor.execute`, `ServiceDefinition.buildCommandResponseHandler`, and the `CommandInspect` instance. Replace the single `execute` function with a type-class-based dispatch that handles both modes.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Two separate `execute` functions (`executeSingleTenant`, `executeMultiTenant`) | Rejected | Doubles the API surface, requires callers to choose the right function, and `ServiceDefinition` would need separate code paths. |
| Single `execute` with `SBool`-based dispatch on `IsMultiTenant` | **Chosen** | One function, one code path. A singleton boolean (`SBool`) reifies the type-level `IsMultiTenant` flag at runtime, allowing a single `case` branch to extract the tenant ID and call the correct function shape. Callers don't change. |
| Class-based dispatch with `ExecuteCommand` typeclass | Rejected | Over-engineered for a boolean dispatch. `SBool` is simpler and keeps all logic in one place. |

The new `execute` function will:

1. Pattern match on `sbool @(IsMultiTenant command)` to determine the mode
2. For `STrue`: extract `tenantId` from `RequestContext.user`, reject if missing, parse to `Uuid`, then call `getEntityIdImpl tenantId command` and `decideImpl tenantId command entity requestContext`
3. For `SFalse`: call `getEntityIdImpl command` and `decideImpl command entity requestContext` (current behavior, unchanged)

```haskell
-- Singleton boolean for type-level dispatch
data SBool (b :: Bool) where
  STrue :: SBool 'True
  SFalse :: SBool 'False

class KnownMultiTenant (b :: Bool) where
  sbool :: SBool b

instance KnownMultiTenant 'True where
  sbool = STrue

instance KnownMultiTenant 'False where
  sbool = SFalse
```

### 2. Tenant ID Extraction and Validation

When `IsMultiTenant ~ 'True`, the executor extracts the tenant ID before any business logic:

```haskell
-- Inside execute, when STrue:
case requestContext.user of
  Nothing ->
    Task.yield CommandRejected { reason = "Unauthorized" }
  Just claims ->
    case claims.tenantId of
      Nothing ->
        Task.yield CommandRejected { reason = "Forbidden" }
      Just tenantIdText ->
        case Uuid.fromText tenantIdText of
          Err _ ->
            Task.yield CommandRejected { reason = "Forbidden" }
          Ok tenantUuid -> do
            Log.debug "Multi-tenant command: tenant extracted from token" |> Task.ignoreError
            -- Proceed with tenant-scoped execution
            let maybeEntityId = (getEntityIdImpl @command) tenantUuid command
            ...
```

Error messages are intentionally generic to avoid revealing multi-tenant internals. Detailed reasons (missing tenant, invalid format) are logged server-side at debug level only.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Accept `Text` tenant IDs as-is | Rejected | The type families define `Uuid` as the tenant parameter type. Accepting raw `Text` would break the type contract and allow arbitrary strings. |
| Parse `Text` to `Uuid` at extraction | **Chosen** | `UserClaims.tenantId` is `Maybe Text` (for OAuth provider flexibility), but the command signatures expect `Uuid`. Parsing at the framework boundary catches format errors early. |

### 3. Stream Isolation

Multi-tenant commands will produce tenant-prefixed `StreamId`s to ensure physical isolation:

```
tenant-{tenantUuid}/{entityStreamId}
```

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| No stream isolation (filter at application layer) | Rejected | A bug in filtering could leak data across tenants. Defense in depth requires isolation at the storage layer. |
| Separate event stores per tenant | Rejected | Dramatically increases operational complexity. Would require dynamic store creation and routing. |
| Tenant prefix in StreamId | **Chosen** | Simple, effective, no operational overhead. StreamId is already `Text`-based with a 1024-char limit. A UUID prefix adds 44 characters (`tenant-{36-char-uuid}/`), well within limits. Events from different tenants are physically separated in the stream. |

A new helper will be added to `Service.Event.StreamId`:

```haskell
-- | Create a tenant-scoped StreamId by prefixing with the tenant UUID.
withTenant :: Uuid -> StreamId -> StreamId
withTenant tenantUuid (StreamId streamIdText) =
  StreamId [fmt|tenant-#{Uuid.toText tenantUuid}/#{streamIdText}|]
```

The `CommandExecutor` will automatically apply this prefix when `IsMultiTenant ~ 'True`.

### 4. Query Tenant Support

Extend the query side to mirror the command-side multi-tenancy pattern.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Add `IsMultiTenant` to `Query` typeclass (matching commands) | Rejected | Queries don't have the same `getEntityId`/`decide` signature polymorphism. A simpler approach is better. |
| Add `tenantOnly` auth helper (post-fetch filter) | **Chosen** | Leverages the existing two-phase authorization. `tenantOnly` is a post-fetch filter (like `ownerOnly`) that compares the query instance's tenant field to the requesting user's `tenantId` claim. No type-level changes to `Query` or `QueryOf` needed. |

A new auth helper in `Service.Query.Auth`:

```haskell
-- | Restrict viewing to instances belonging to the requesting user's tenant.
--
-- Takes a function that extracts the tenant ID from the query instance
-- and compares it to the authenticated user's tenantId claim.
--
-- @
-- canView = tenantOnly (\query -> query.tenantId)
-- @
tenantOnly ::
  forall query.
  (query -> Text) ->
  Maybe UserClaims ->
  query ->
  Maybe QueryAuthError
tenantOnly getTenantId user query = case user of
  Nothing -> Just Unauthenticated
  Just claims ->
    case claims.tenantId of
      Nothing -> Just Forbidden
      Just claimTenantId ->
        -- Normalize both sides to lowercase for case-insensitive comparison.
        -- OAuth providers may return UUIDs in different casing.
        case Text.toLower claimTenantId == Text.toLower (getTenantId query) of
          True -> Nothing
          False -> Just Forbidden
```

This approach:
- Requires no changes to `Query`, `QueryOf`, `QueryObjectStore`, or `QuerySubscriber`
- Is opt-in per query (single-tenant queries don't use it)
- Composes with existing helpers (`requirePermission` + `tenantOnly`)

### 5. Query `combine` Tenant Context

For multi-tenant queries, `combine` needs access to the tenant ID from the event's stream context to populate the query instance's `tenantId` field.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Add tenant parameter to `QueryOf.combine` | Rejected | Changes the typeclass signature for all queries, breaking backward compatibility. |
| Extract tenant from entity fields | **Chosen** | Multi-tenant entities should store `tenantId` as a field (populated by the `decide` function which receives it). The `combine` function reads it from the entity. This requires no framework changes. |

Example:

```haskell
-- Entity stores tenant ID (set by decide function)
data ProjectEntity = ProjectEntity
  { projectEntityId :: Uuid
  , tenantId :: Text
  , name :: Text
  }

-- Query instance includes tenant ID for filtering
data ProjectSummary = ProjectSummary
  { projectSummaryId :: Uuid
  , tenantId :: Text
  , name :: Text
  }

-- combine copies tenantId from entity to query
instance QueryOf ProjectEntity ProjectSummary where
  queryId project = project.projectEntityId
  combine project _existing = Update ProjectSummary
    { projectSummaryId = project.projectEntityId
    , tenantId = project.tenantId
    , name = project.name
    }
```

### 6. TH Macro: No Changes Required

The `command` TH macro already correctly:
- Detects `type MultiTenancy = True` declarations
- Validates `getEntityId` and `decide` signatures for both modes
- Generates the `IsMultiTenant` type instance

The `deriveQuery` TH macro requires no changes because tenant filtering is handled via auth helpers, not type-level machinery.

### 7. Module Placement

| Change | File |
|--------|------|
| `SBool` / `KnownMultiTenant` | `core/service/Service/Command/Core.hs` |
| Multi-tenant `execute` | `core/service/Service/CommandExecutor/Core.hs` |
| Remove `IsMultiTenant ~ False` constraints | `core/service/Service/ServiceDefinition/Core.hs` |
| `StreamId.withTenant` | `core/service/Service/Event/StreamId.hs` |
| `tenantOnly` helper | `core/service/Service/Query/Auth.hs` |
| Tests | `core/test/Service/MultiTenantSpec.hs` |

No new modules are created. All changes are within existing files.

### 8. Public API

```haskell
-- Service.Command.Core (new exports)
data SBool (b :: Bool) where
  STrue :: SBool 'True
  SFalse :: SBool 'False

class KnownMultiTenant (b :: Bool) where
  sbool :: SBool b

-- Service.Event.StreamId (new export)
withTenant :: Uuid -> StreamId -> StreamId

-- Service.Query.Auth (new export)
tenantOnly :: (query -> Text) -> Maybe UserClaims -> query -> Maybe QueryAuthError

-- CommandExecutor.execute: signature unchanged, constraint relaxed
-- (IsMultiTenant command ~ 'False removed, KnownMultiTenant (IsMultiTenant command) added)
```

## Consequences

### Positive

- Jess can enable multi-tenancy with `type MultiTenancy = True` and adjusted signatures — the framework handles tenant extraction, validation, and stream isolation automatically
- Single-tenant services are completely unaffected (default remains `False`, no constraint changes observable)
- Event streams are physically isolated per tenant, preventing cross-tenant data leaks at the storage layer
- The `tenantOnly` query auth helper follows the same pattern as `ownerOnly`, making it immediately familiar
- No changes to TH macros — the existing compile-time validation already works for multi-tenant signatures

### Negative

- `tenantId` in `UserClaims` is `Maybe Text` but command signatures expect `Uuid` — there's a parse step at the boundary that could fail at runtime if the OAuth provider sends non-UUID tenant IDs
- Multi-tenant queries rely on entities storing `tenantId` as a field — the framework doesn't enforce this at the type level, so forgetting to include it would be a runtime issue (filtered out by `tenantOnly`, but no compile-time guarantee)

### Risks

- Stream prefix format (`tenant-{uuid}/`) becomes a permanent contract once events are persisted. Changing the format later would require event migration.
- `getAll` in `QueryObjectStore` still returns all tenants' data before `canView` filtering. For very large multi-tenant deployments, this could become a performance bottleneck.

### Mitigations

- The stream prefix format is simple and well-defined. A future ADR can add store-level tenant filtering without changing the prefix format.
- `QueryObjectStore` can be extended with a `getByTenant` operation in a follow-up ADR if `getAll` + filter proves insufficient at scale. The current approach is correct for the initial implementation and works for moderate tenant counts.

## References

- [#602: feat(service): complete multi-tenant support for commands and queries](https://github.com/neohaskell/NeoHaskell/issues/602)
- [ADR-0003: Command Abstraction and Flow](0003-command-abstraction-and-flow.md)
- [ADR-0007: Queries (Read Models)](0007-queries-read-models.md)
- [ADR-0009: JWT Authentication Middleware](0009-jwt-authentication-middleware.md)
- [Service/Command/Core.hs](../../core/service/Service/Command/Core.hs)
- [Service/CommandExecutor/Core.hs](../../core/service/Service/CommandExecutor/Core.hs)
- [Service/ServiceDefinition/Core.hs](../../core/service/Service/ServiceDefinition/Core.hs)
- [Service/Query/Auth.hs](../../core/service/Service/Query/Auth.hs)
- [Service/Event/StreamId.hs](../../core/service/Service/Event/StreamId.hs)
