# ADR-0014: WebTransport OpenAPI Integration

## Status

Proposed

## Context

ADR-0013 introduced a library-agnostic `Schema` ADT and `ToSchema` typeclass for automatic type introspection. However, the actual OpenAPI integration with WebTransport remains unspecified. Currently:

1. WebTransport exposes commands at `/commands/<name>` and queries at `/queries/<name>` but provides no discoverability
2. API consumers must read code or external documentation to understand available endpoints
3. No machine-readable API specification exists for code generation, API gateways, or tooling

The core design principle for this feature is:

> **OpenAPI should be automatic when WebTransport is used - not a separate feature.**

OpenAPI generation is the documentation generation of the transport. It should not require separate wiring, opt-in flags, or additional `Application.withOpenApi` calls. When you use WebTransport, you get OpenAPI documentation automatically.

### Requirements

1. **Zero configuration**: OpenAPI spec available at `/openapi.json` and `/openapi.yaml` without any additional setup
2. **Interactive documentation**: Swagger UI at `/docs` for API exploration
3. **Type accuracy**: Generated specs must match actual endpoint behavior
4. **CI integration**: Support for build-time spec generation and golden testing
5. **Runtime validation**: Option to validate incoming requests against schemas

### Design Constraints

- Must build on ADR-0012's `Schema` ADT (no parallel schema system)
- Must not change the public API of `Service.command` or `Service.query`
- Must not require changes to existing application code
- Should follow NeoHaskell's "batteries included" philosophy

## Decision

### 1. Application Configuration Additions

Add optional API metadata fields to `Application` with sensible defaults. API info describes the application, not the transport:

```haskell
-- core/service/Service/Application.hs
data ApiInfo = ApiInfo
  { apiTitle :: Text,
    apiVersion :: Text,
    apiDescription :: Text
  }

defaultApiInfo :: ApiInfo
defaultApiInfo = ApiInfo
  { apiTitle = "API",
    apiVersion = "1.0.0",
    apiDescription = ""
  }
```

Add a builder function for setting API info on the application:

```haskell
withApiInfo :: Text -> Text -> Text -> Application -> Application
withApiInfo title version description app =
  app { apiInfo = ApiInfo { apiTitle = title, apiVersion = version, apiDescription = description } }
```

The `ApiInfo` is passed to transports when they are assembled, allowing any transport that supports documentation (like WebTransport) to use it.

### 2. Automatic Documentation Routes

The `assembleTransport` function adds three documentation routes automatically - no opt-in required:

| Route | Content-Type | Description |
|-------|--------------|-------------|
| `GET /openapi.json` | `application/json` | OpenAPI 3.0 specification in JSON |
| `GET /openapi.yaml` | `application/x-yaml` | OpenAPI 3.0 specification in YAML |
| `GET /docs` | `text/html` | Swagger UI pointing to `/openapi.json` |

These routes are added to the existing path matching in `assembleTransport`:

```haskell
assembleTransport endpoints request respond = do
  case Wai.pathInfo request of
    -- Existing routes...
    ["commands", commandNameKebab] -> ...
    ["queries", queryNameKebab] -> ...
    
    -- NEW: Documentation routes (always available)
    ["openapi.json"] -> serveOpenApiJson endpoints
    ["openapi.yaml"] -> serveOpenApiYaml endpoints
    ["docs"] -> serveSwaggerUi endpoints
    
    _ -> notFound "Not found"
```

### 3. Schema Capture Flow

Schema information must be captured at handler registration time, before type information is erased. The flow is:

```
Service.command @AddItem
      ↓
toSchema @AddItem captured (ToSchema constraint already required)
      ↓
CommandDefinition stores schema alongside handler
      ↓
ServiceRunner.getEndpointsByTransport collects endpoints + schemas
      ↓
WebTransport.assembleTransport receives Endpoints with schemas
      ↓
/openapi.json generates spec from collected schemas
```

The `Endpoints` type gains schema information:

```haskell
-- Service/Transport.hs
data EndpointSchema = EndpointSchema
  { requestSchema :: Maybe Schema,   -- Commands have request body
    responseSchema :: Schema,        -- All endpoints have response
    description :: Text,             -- From Documented instance
    deprecated :: Bool               -- From Documented instance
  }

data Endpoints transport = Endpoints
  { transport :: transport,
    commandEndpoints :: Map Text EndpointHandler,
    queryEndpoints :: Map Text QueryHandler,
    -- NEW: schema information for OpenAPI generation
    commandSchemas :: Map Text EndpointSchema,
    querySchemas :: Map Text EndpointSchema
  }
```

### 4. Schema to OpenAPI Conversion

A new module converts the library-agnostic `Schema` ADT to OpenAPI 3.0 types:

```haskell
-- core/schema/Schema/OpenApi.hs
module Schema.OpenApi (
  toOpenApiSchema,
  toOpenApiSpec,
) where

import Schema (Schema (..), FieldSchema (..))
import Data.OpenApi qualified as OpenApi

-- | Convert Schema ADT to OpenAPI Schema
toOpenApiSchema :: Schema -> OpenApi.Schema
toOpenApiSchema schema = case schema of
  SNull -> OpenApi.nullSchema
  SBool -> OpenApi.boolSchema
  SInt -> OpenApi.integerSchema
  SNumber -> OpenApi.numberSchema
  SText -> OpenApi.stringSchema
  SArray inner -> OpenApi.arraySchema (toOpenApiSchema inner)
  SOptional inner -> toOpenApiSchema inner  -- Required-ness is on field
  SObject fields -> do
    let properties = fields |> Array.map toOpenApiProperty
    let required = fields |> Array.takeIf (.fieldRequired) |> Array.map (.fieldName)
    OpenApi.objectSchema properties required
  SEnum variants -> OpenApi.enumSchema variants
  SUnion cases -> OpenApi.oneOfSchema (cases |> Array.map toOpenApiCase)
  SRef name -> OpenApi.refSchema name

-- | Generate complete OpenAPI 3.0 spec from endpoints
toOpenApiSpec ::
  Text ->                        -- API title
  Text ->                        -- API version
  Text ->                        -- API description
  Map Text EndpointSchema ->     -- Command endpoints
  Map Text EndpointSchema ->     -- Query endpoints
  OpenApi.OpenApi
```

### 5. Endpoint Documentation Patterns

Commands and queries follow consistent OpenAPI patterns:

**Commands** (`POST /commands/{kebab-name}`):

```yaml
paths:
  /commands/add-item:
    post:
      summary: AddItem
      description: (from Documented instance)
      requestBody:
        required: true
        content:
          application/json:
            schema: (from ToSchema)
      responses:
        '200':
          description: Command accepted
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/CommandResponse.Accepted'
        '400':
          description: Command rejected or invalid
        '401':
          description: Unauthorized (when auth enabled)
        '500':
          description: Internal server error
```

**Queries** (`GET /queries/{kebab-name}`):

```yaml
paths:
  /queries/cart-summary:
    get:
      summary: CartSummary
      description: (from Documented instance)
      responses:
        '200':
          description: Query result
          content:
            application/json:
              schema:
                type: array
                items: (from ToSchema on query type)
        '401':
          description: Unauthorized
        '403':
          description: Forbidden
        '500':
          description: Internal server error
```

### 6. Swagger UI Embedding

Swagger UI is served at `/docs` using one of two strategies:

**Option A: CDN-based (default)**

Serve a minimal HTML page that loads Swagger UI from a CDN:

```haskell
serveSwaggerUi :: ApiInfo -> Task Text Wai.ResponseReceived
serveSwaggerUi apiInfo = do
  let html = [fmt|
    <!DOCTYPE html>
    <html>
    <head>
      <title>{apiInfo.apiTitle} - API Documentation</title>
      <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui.css">
    </head>
    <body>
      <div id="swagger-ui"></div>
      <script src="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui-bundle.js"></script>
      <script>
        SwaggerUIBundle({
          url: "/openapi.json",
          dom_id: '#swagger-ui'
        });
      </script>
    </body>
    </html>
  |]
  respond (htmlResponse html)
```

**Option B: Embedded (future consideration)**

Use `file-embed` to embed Swagger UI assets at compile time for offline/airgapped deployments. This would require adding `file-embed` as a dependency.

The default is CDN-based because:
- Zero additional dependencies
- Smaller binary size
- Automatic updates to Swagger UI
- Most production deployments have internet access

### 7. CI Integration

For build-time spec generation and golden testing, provide a function to generate the spec without starting a server:

```haskell
-- core/schema/Schema/OpenApi.hs

-- | Generate OpenAPI spec from an Application configuration.
-- Use this for CI golden tests or static spec file generation.
generateOpenApiSpec :: Application -> OpenApi.OpenApi
generateOpenApiSpec app = do
  let endpoints = Application.collectEndpoints app
  let info = Application.getApiInfo app
  toOpenApiSpec
    info.apiTitle
    info.apiVersion
    info.apiDescription
    endpoints.commandSchemas
    endpoints.querySchemas
```

Usage in tests:

```haskell
-- Generate spec at test time
spec :: Test.Spec
spec = do
  Test.describe "OpenAPI spec" do
    Test.it "matches golden file" do
      let generatedSpec = generateOpenApiSpec myApp
      let expectedSpec = File.readText "openapi.golden.json"
      Json.encodeText generatedSpec `Test.shouldBe` expectedSpec
```

Usage in CI:

```bash
# Generate spec and compare to committed version
cabal run generate-openapi > openapi.json
diff openapi.json openapi.committed.json
```

### 8. Module Structure

```text
core/
  schema/
    Schema.hs                    -- (ADR-0013) Schema ADT, ToSchema
    Schema/
      OpenApi.hs                 -- NEW: Schema → OpenAPI conversion
  service/
    Service/
      Application.hs             -- Modified: ApiInfo, withApiInfo
      Transport/
        Web.hs                   -- Modified: /openapi.json, /docs routes
        Web/
          OpenApi.hs             -- NEW: OpenAPI spec assembly from endpoints
          SwaggerUI.hs           -- NEW: Swagger UI HTML generation
```

### 9. Usage Example

No extra wiring required - documentation "just works":

```haskell
-- Application setup
Application.new
  |> Application.withApiInfo "Cart API" "1.0.0" "Shopping cart service"
  |> Application.withEventStore postgresConfig
  |> Application.withService cartService
  |> Application.withQuery @CartSummary
  |> Application.withTransport WebTransport.server
  |> Application.run

-- Automatically serves:
-- POST /commands/add-item        (documented in OpenAPI)
-- POST /commands/remove-item     (documented in OpenAPI)
-- GET  /queries/cart-summary     (documented in OpenAPI)
-- GET  /openapi.json             (full OpenAPI 3.0 spec)
-- GET  /openapi.yaml             (full OpenAPI 3.0 spec)
-- GET  /docs                     (Swagger UI)
```

### 10. Dependencies

This ADR requires adding the following packages to `nhcore.cabal`:

| Package | Purpose | Notes |
|---------|---------|-------|
| `openapi3` | OpenAPI 3.0 types | Mature, actively maintained |
| `yaml` | YAML encoding for `/openapi.yaml` | Lightweight, already a transitive dependency |

Optional for future work:
- `file-embed` for embedded Swagger UI assets

## Consequences

### Positive

1. **Zero-effort documentation**: Every WebTransport application gets OpenAPI docs automatically. No configuration, no opt-in, no forgetting to update.

2. **Single source of truth**: Schemas come from types (ADR-0012). The OpenAPI spec cannot drift from implementation.

3. **Developer experience**: New team members can explore APIs via Swagger UI at `/docs` immediately.

4. **Ecosystem integration**: Generated specs work with:
   - Code generators (TypeScript, Python, etc.)
   - API gateways (Kong, AWS API Gateway)
   - Documentation portals (Redoc, Stoplight)
   - Testing tools (Postman, Insomnia)

5. **CI-friendly**: `generateOpenApiSpec` enables golden tests to catch unintended API changes.

6. **Consistent with NeoHaskell philosophy**: "Batteries included" - documentation is not a separate concern to configure.

### Negative

1. **New dependencies**: `openapi3` and `yaml` packages are required. These are well-maintained but add to the dependency footprint.

2. **Cannot disable**: There's no way to hide `/openapi.json` or `/docs` routes. If this becomes a security concern (unlikely), we'd need to add an opt-out flag.

3. **CDN dependency for Swagger UI**: Default configuration requires internet access to load Swagger UI. Airgapped environments need the embedded option (future work).

4. **Schema limitations**: Custom `ToJSON`/`FromJSON` instances that differ from Generic derivation will produce inaccurate schemas. This is inherited from ADR-0012.

### Trade-offs

1. **Automatic over explicit**: Routes appear without opt-in. This follows NeoHaskell's "sensible defaults" philosophy but may surprise developers expecting explicit configuration.

2. **CDN over embedded**: Default Swagger UI loads from CDN (smaller binary, automatic updates) vs embedded assets (works offline, slower updates).

3. **Always-on documentation**: No "production mode" that hides docs. If this becomes a requirement, it would need a future ADR.

## Future Considerations

1. **Request validation**: Use schemas to validate incoming JSON before deserialization, providing better error messages.

2. **Redoc alternative**: Offer Redoc as an alternative to Swagger UI at `/redoc`.

3. **Versioning**: Support multiple API versions with `/v1/openapi.json`, `/v2/openapi.json`.

4. **Authentication documentation**: Document auth requirements in OpenAPI security schemes.

5. **Embedded Swagger UI**: Add `file-embed` option for airgapped deployments.

6. **Schema caching**: Cache generated OpenAPI spec to avoid regeneration on every request.

7. **Spectral integration**: Provide linting rules for the generated spec.

## References

- [ADR-0013: Automatic Schema Generation](0013-automatic-schema-generation.md) - Schema ADT foundation
- [ADR-0002: WebAPI Adapter Architecture](0002-webapi-adapter-architecture.md) - WebTransport architecture
- [core/service/Service/Transport/Web.hs](../../core/service/Service/Transport/Web.hs) - Current WebTransport implementation
- [OpenAPI 3.0 Specification](https://spec.openapis.org/oas/v3.0.3)
- [Swagger UI](https://swagger.io/tools/swagger-ui/)
- [openapi3 Hackage package](https://hackage.haskell.org/package/openapi3)
