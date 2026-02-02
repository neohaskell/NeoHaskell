# ADR-0020: OpenAPI Grouping and Complete Endpoint Coverage

## Status

Proposed

## Context

ADR-0014 established automatic OpenAPI documentation for WebTransport applications. However, the current implementation has two significant gaps identified in [Issue #362](https://github.com/neohaskell/neohaskell/issues/362):

### 1. Missing Logical Grouping

The generated OpenAPI spec presents all endpoints as a flat list:

```yaml
paths:
  /commands/add-item:
    post: ...
  /commands/remove-item:
    post: ...
  /queries/cart-summary:
    get: ...
  /queries/inventory-level:
    get: ...
```

For applications with multiple services, this becomes difficult to navigate. API consumers cannot easily identify which endpoints belong to which bounded context or service.

### 2. Missing Endpoint Types

Several endpoint categories are completely absent from the generated OpenAPI schema:

| Endpoint Category | Routes | Current Status |
|-------------------|--------|----------------|
| OAuth2 Authentication | `/connect/{provider}`, `/callback/{provider}`, `/disconnect/{provider}` | Missing |
| File Upload | `POST /files/upload`, `GET /files/{fileRef}` | Missing |
| Documentation | `/openapi.json`, `/openapi.yaml`, `/docs` | Missing |

This incomplete documentation leads to:
- API consumers discovering endpoints through trial-and-error or code reading
- Incomplete client SDK generation
- Security audit gaps (undocumented endpoints may be overlooked)

### Design Goals

1. **Automatic grouping**: Endpoints should be grouped by their logical service without manual configuration
2. **Complete coverage**: All WebTransport endpoints should appear in the OpenAPI spec
3. **Backward compatibility**: Existing applications should work without changes
4. **Standard compliance**: Use OpenAPI 3.0 tags for grouping (industry standard)

## Decision

### 1. OpenAPI Tags for Logical Grouping

Use OpenAPI tags to group endpoints. Tags are the standard mechanism for grouping in OpenAPI 3.0:

```yaml
tags:
  - name: Cart
    description: Shopping cart service commands and queries
  - name: Inventory
    description: Inventory management commands and queries
  - name: Authentication
    description: OAuth2 authentication flows
  - name: Files
    description: File upload and download
```

Each endpoint references its tag:

```yaml
paths:
  /commands/add-item:
    post:
      tags: [Cart]
      ...
```

### 2. Service Name from Entity

Since each service is tied to exactly one entity (1 service = 1 entity), the service name is derived from `NameOf entity`. This is already available in `CommandDefinition`:

```haskell
-- Service/ServiceDefinition/Core.hs (existing)
data CommandDefinition name transport cmd transportName event entity entityName entityIdType
  = CommandDefinition { ... }

-- entityName ~ NameOf entity (already constrained in command registration)
```

Extend `EndpointSchema` to capture the entity name as the service/tag name:

```haskell
-- Service/Transport.hs
data EndpointSchema = EndpointSchema
  { requestSchema :: Maybe Schema
  , responseSchema :: Schema
  , description :: Text
  , deprecated :: Bool
  , entityName :: Maybe Text  -- NEW: NameOf entity for grouping
  }
```

The entity name is captured during `buildEndpointsByTransport`:

```haskell
-- In ServiceDefinition/Core.hs buildEndpointsByTransport
let endpointSchema = Service.Transport.EndpointSchema
      { requestSchema = Just schema
      , responseSchema = commandResponseSchema
      , description = ""
      , deprecated = False
      , entityName = Just (getSymbolText (Record.Proxy @entityName))  -- NameOf entity
      }
```

For commands registered directly (not via a service), `entityName` is `Nothing` and they appear in a default "API" group. All queries use the fixed tag "Queries" regardless of their entities.

### 3. Built-in Endpoint Schemas

Add schema definitions for WebTransport's built-in endpoints:

```haskell
-- Service/Transport/Web/OpenApi.hs

-- | OAuth2 endpoint schemas
oauth2Schemas :: Maybe OAuth2Config -> Map Text EndpointSchema
oauth2Schemas Nothing = Map.empty
oauth2Schemas (Just _) = Map.fromEntries
  [ ("connect", EndpointSchema
      { requestSchema = Nothing
      , responseSchema = SObject []  -- 302 redirect
      , description = "Initiate OAuth2 connection flow"
      , deprecated = False
      , entityName = Just "Authentication"
      })
  , ("callback", EndpointSchema { ... })
  , ("disconnect", EndpointSchema { ... })
  ]

-- | File upload endpoint schemas
fileUploadSchemas :: Maybe FileUploadEnabled -> Map Text EndpointSchema
fileUploadSchemas Nothing = Map.empty
fileUploadSchemas (Just _) = Map.fromEntries
  [ ("upload", EndpointSchema
      { requestSchema = Just multipartFormDataSchema
      , responseSchema = fileRefResponseSchema
      , description = "Upload a file"
      , deprecated = False
      , entityName = Just "Files"
      })
  , ("download", EndpointSchema { ... })
  ]
```

### 4. Extended OpenAPI Generation

Modify `Schema.OpenApi.toOpenApiSpec` to:

1. Accept additional endpoint maps (OAuth2, files)
2. Generate tags from unique entity names
3. Assign tags to each operation

```haskell
-- Schema/OpenApi.hs

toOpenApiSpec ::
  ApiInfo ->
  Map Text EndpointSchema ->     -- Commands (tagged by entity name)
  Map Text EndpointSchema ->     -- Queries (all tagged "Queries")
  Map Text EndpointSchema ->     -- OAuth2 endpoints (tagged "Authentication")
  Map Text EndpointSchema ->     -- File endpoints (tagged "Files")
  OpenApi.OpenApi
toOpenApiSpec apiInfo commands queries oauth2 files = do
  -- Collect entity names from commands only
  let commandEntityNames = commands
        |> Map.values
        |> Array.map (.entityName)
        |> Array.takeIf Maybe.isJust
        |> Array.map Maybe.unwrap
        |> Set.fromArray

  -- Fixed tags for other endpoint categories
  let fixedTags = ["Queries", "Authentication", "Files"]
        |> Array.takeIf (\tag -> case tag of
             "Queries" -> not (Map.isEmpty queries)
             "Authentication" -> not (Map.isEmpty oauth2)
             "Files" -> not (Map.isEmpty files))

  -- Generate all tag definitions
  let allTagNames = commandEntityNames |> Set.toArray |> Array.append fixedTags
  let tags = allTagNames
        |> Array.map (\name -> OpenApi.Tag name Nothing Nothing)

  -- Generate paths: commands use entity name, queries use "Queries"
  let commandPaths = commands |> Map.map makeCommandPath  -- uses entityName
  let queryPaths = queries |> Map.map (makeQueryPath "Queries")  -- fixed tag
  ...
```

### 5. Complete Endpoint Paths

Add all endpoint categories to the generated spec:

| Category | Path Pattern | Method | Tag |
|----------|--------------|--------|-----|
| Commands | `/commands/{kebab-name}` | POST | Entity name (`NameOf entity`) |
| Queries | `/queries/{kebab-name}` | GET | "Queries" (all queries grouped together) |
| OAuth2 Connect | `/connect/{provider}` | GET | "Authentication" |
| OAuth2 Callback | `/callback/{provider}` | GET | "Authentication" |
| OAuth2 Disconnect | `/disconnect/{provider}` | DELETE | "Authentication" |
| File Upload | `/files/upload` | POST | "Files" |
| File Download | `/files/{fileRef}` | GET | "Files" |

Documentation endpoints (`/openapi.json`, `/openapi.yaml`, `/docs`) are intentionally excluded as they are meta-endpoints about the API itself, not part of the API.

### 6. Parameter Schemas

OAuth2 and file endpoints have path parameters that need proper schema definitions:

```yaml
# OAuth2 provider parameter
parameters:
  - name: provider
    in: path
    required: true
    schema:
      type: string
      enum: [google, github, azure]  # From configured providers
    description: OAuth2 provider identifier

# File reference parameter
parameters:
  - name: fileRef
    in: path
    required: true
    schema:
      type: string
      format: uuid
    description: Unique file reference from upload response
```

### 7. Module Structure

```
core/
  schema/
    Schema/
      OpenApi.hs              -- Modified: tag generation, extended toOpenApiSpec
  service/
    Service/
      Transport.hs            -- Modified: entityName field in EndpointSchema
      Transport/
        Web.hs                -- Modified: pass OAuth2/file schemas to OpenAPI
        Web/
          OpenApi.hs          -- NEW: Built-in endpoint schema definitions
```

## Consequences

### Positive

1. **Improved API discoverability**: Swagger UI groups endpoints by service, making large APIs navigable
2. **Complete documentation**: All endpoints appear in the spec, enabling complete client generation
3. **Security visibility**: Auditors can see all endpoints including auth and file upload
4. **Zero configuration**: Grouping happens automatically based on service definitions

### Negative

1. **Breaking change to EndpointSchema**: Adding `entityName` field requires updates to existing code that constructs `EndpointSchema` values. Mitigation: Provide smart constructor with default `Nothing`.
2. **Larger OpenAPI spec**: Including OAuth2 and file endpoints increases spec size. Acceptable given the discoverability benefit.

### Trade-offs

1. **Automatic vs explicit grouping**: We chose automatic grouping from entity names (via `NameOf entity`). Alternative was explicit `@Tag` annotations on commands. Automatic is simpler and follows convention-over-configuration.

2. **Provider enumeration in OAuth2**: Including configured provider names in the enum exposes what providers are available. This is acceptable as the routes themselves reveal this information.

### Migration Path

Existing applications require no changes. The new `entityName` field defaults to `Nothing`, placing endpoints in a generic "API" group. To get proper grouping, ensure commands are registered via `Application.withService` rather than directly.

## Performance Considerations

Based on performance review targeting 50,000 req/s throughput, the following optimizations are required:

### 1. Server-Side Spec Caching (Required)

The current implementation regenerates the OpenAPI spec on every `/openapi.json` and `/openapi.yaml` request (10-50ms overhead). This MUST be cached:

```haskell
-- Cache spec at transport assembly time
data CachedSpecs = CachedSpecs
  { cachedJson :: Bytes
  , cachedYaml :: Bytes
  }

-- In Endpoints or WebTransport
cachedOpenApiSpec :: Maybe CachedSpecs
```

**Implementation**: Generate and cache the spec once when `assembleTransport` is called. Serve cached bytes directly.

**Impact**: Reduces documentation endpoint latency from 10-50ms to <1ms.

### 2. Pre-Encoded Representations

Pre-encode both JSON and YAML at cache time to eliminate all computation from documentation endpoint handlers.

### 3. Optimized Tag Generation

Use `Set` for unique entity name extraction instead of `Array.unique`:

```haskell
let entityNames = allSchemas
      |> Map.values
      |> Array.foldl (\acc schema ->
           case schema.entityName of
             Just name -> Set.insert name acc
             Nothing -> acc
         ) Set.empty
      |> Set.toArray
```

### 4. Rate Limiting Documentation Endpoints

Even with caching, documentation endpoints should have separate rate limits to prevent abuse/DoS.

### 5. Benchmarks (Required)

Add criterion benchmarks for:
- `toOpenApiSpec` with 10, 100, 1000 endpoints
- Schema traversal depth impact
- Cached vs uncached serving performance

Target: Spec generation <5ms for 100 endpoints; cached serving <1ms.

## Security Considerations

Based on security review (OWASP, NIST 800-53, GDPR/NIS2), the following security measures are required:

### 1. OpenAPI Security Schemes (Required)

The generated OpenAPI spec MUST include security scheme definitions and apply them per-endpoint:

```yaml
components:
  securitySchemes:
    BearerAuth:
      type: http
      scheme: bearer
      bearerFormat: JWT
      description: JWT token from authentication provider

paths:
  /connect/{provider}:
    get:
      security:
        - BearerAuth: []
  /callback/{provider}:
    get:
      security: []  # Explicitly public (browser redirect)
  /disconnect/{provider}:
    delete:
      security:
        - BearerAuth: []
  /files/upload:
    post:
      security:
        - BearerAuth: []
```

### 2. Documentation Endpoint Protection (Optional)

Add configuration to protect documentation endpoints in production:

```haskell
data WebTransport = WebTransport
  { ...
  , docsAuthenticated :: Bool  -- Default: False
  }
```

When `docsAuthenticated = True`:
- `/openapi.json`, `/openapi.yaml`, `/docs` require valid JWT
- Recommended for internal/sensitive APIs

### 3. Provider Enumeration Mitigation

The provider enum in OAuth2 endpoints may expose authentication infrastructure. Two options:

**Option A (Default)**: Accept exposure since routes reveal the same information
**Option B (High-security)**: Use dynamic provider parameter without enum constraint:

```yaml
parameters:
  - name: provider
    in: path
    required: true
    schema:
      type: string
    description: OAuth2 provider identifier
```

### 4. Privacy Annotations (GDPR)

For EU deployments, add privacy metadata to schemas handling personal data:

```yaml
x-privacy-category: personal-data
x-retention-policy: "Unconfirmed files expire after 6 hours"
x-data-controller: "Application deployer"
```

### 5. HTTPS Enforcement

OpenAPI spec should declare HTTPS-only servers for production:

```yaml
servers:
  - url: https://api.example.com
    description: Production (HTTPS required)
```

### 6. Error Response Standardization

Document standardized error responses to prevent information leakage:

```yaml
responses:
  '401':
    description: Authentication required
    content:
      application/json:
        schema:
          $ref: '#/components/schemas/Error'
```

Error messages MUST be generic (per ADR-0009) and NOT reveal internal details.

## References

- [Issue #362](https://github.com/neohaskell/neohaskell/issues/362)
- [ADR-0014: WebTransport OpenAPI Integration](0014-webtransport-openapi-integration.md)
- [ADR-0013: Automatic Schema Generation](0013-automatic-schema-generation.md)
- [OpenAPI 3.0 Tags](https://spec.openapis.org/oas/v3.0.3#tag-object)
