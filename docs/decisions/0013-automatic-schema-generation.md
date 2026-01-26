# ADR-0013: Automatic Schema Generation

## Status

Proposed

## Context

NeoHaskell's WebTransport exposes commands at `/commands/<name>` endpoints, but provides no mechanism for documenting these APIs. Developers must:

1. Manually write OpenAPI specs separately from code (gets out of sync)
2. Use external tools that don't understand NeoHaskell's type system
3. Go without documentation, making API discovery difficult

Real applications need:

- **OpenAPI specs** for frontend code generation, API gateways, and documentation portals
- **JSON Schema** for request validation and IDE autocompletion
- **CLI argument schemas** for future CLI transports
- **Interactive documentation** (Swagger UI, Redoc) for API exploration

We need a system that:

1. Extracts schema information automatically from Haskell types
2. Produces a library-agnostic intermediate representation
3. Converts to multiple output formats (OpenAPI, JSON Schema, etc.)
4. Integrates seamlessly with WebTransport without extra wiring

### Prior Art

The `autodocodec` library proves that a single schema definition can target multiple formats (OpenAPI, JSON Schema, YAML Schema). GHC.Generics provides all metadata needed: field names (`selName`), type names (`datatypeName`), constructor names (`conName`), and record detection (`conIsRecord`).

Every major Haskell schema library (Aeson, swagger2, openapi3, optparse-generic, beam) uses identical Generic patterns with `selName (undefined :: S1 s (K1 i t) p)` - this is the universal standard for extracting field names.

## Decision

### 1. Library-Agnostic Schema ADT

We define a `Schema` ADT that captures type structure without committing to any output format:

```haskell
-- core/schema/Schema.hs
module Schema (
  Schema (..),
  FieldSchema (..),
) where

import Array (Array)
import Text (Text)

-- | Library-agnostic schema representation.
-- Can be converted to OpenAPI, JSON Schema, CLI args, etc.
data Schema
  = SNull                           -- Unit, ()
  | SBool                           -- Bool
  | SInt                            -- Int, Integer
  | SNumber                         -- Double, Float
  | SText                           -- Text, String
  | SArray Schema                   -- Array a, [a]
  | SOptional Schema                -- Maybe a (marks field as non-required)
  | SObject (Array FieldSchema)     -- Record types
  | SEnum (Array Text)              -- Sum types without fields (data Status = Active | Inactive)
  | SUnion (Array (Text, Schema))   -- Sum types with fields (data Shape = Circle {radius :: Int} | ...)
  | SRef Text                       -- Reference for recursive types
  deriving (Show, Eq)

data FieldSchema = FieldSchema
  { fieldName :: Text,
    fieldSchema :: Schema,
    fieldRequired :: Bool,          -- False for Maybe fields
    fieldDescription :: Text        -- From Documented instance
  }
  deriving (Show, Eq)
```

This ADT is intentionally minimal. It captures what's needed for documentation without imposing OpenAPI-specific concepts like `discriminator` or JSON Schema concepts like `$defs`.

### 2. ToSchema Typeclass with Generic Default

The `ToSchema` typeclass converts Haskell types to `Schema`:

```haskell
-- core/schema/Schema.hs (continued)
module Schema (
  -- ... previous exports
  ToSchema (..),
) where

import GHC.Generics qualified as Generics

class ToSchema value where
  toSchema :: Schema
  
  default toSchema :: 
    (Generics.Generic value, GToSchema (Generics.Rep value)) => 
    Schema
  toSchema = gToSchema @(Generics.Rep value)
```

Types with `Generic` instances get `ToSchema` for free:

```haskell
data AddItem = AddItem
  { cartId :: Uuid,
    productId :: Text,
    quantity :: Int
  }
  deriving (Generic)

instance ToSchema AddItem
-- Automatically generates: SObject [FieldSchema "cartId" ..., FieldSchema "productId" ..., ...]
```

### 3. GToSchema (Generic Schema Derivation)

The `GToSchema` class walks the Generic representation:

```haskell
-- core/schema/Schema.hs (continued)
class GToSchema (rep :: Type -> Type) where
  gToSchema :: Schema

-- Product types (records)
instance (GToSchema left, GToSchema right) => GToSchema (left Generics.:*: right) where
  gToSchema = -- combine fields from left and right

-- Field with selector name
instance 
  ( Generics.Selector selector
  , ToSchema fieldType
  ) => GToSchema (Generics.S1 selector (Generics.K1 index fieldType)) where
  gToSchema = do
    let fieldName = Generics.selName (undefined :: Generics.S1 selector (Generics.K1 index fieldType) p)
    -- ... build FieldSchema

-- Special case: Maybe fields are non-required
instance 
  ( Generics.Selector selector
  , ToSchema inner
  ) => GToSchema (Generics.S1 selector (Generics.K1 index (Maybe inner))) where
  gToSchema = do
    let fieldName = Generics.selName (undefined :: Generics.S1 selector (Generics.K1 index (Maybe inner)) p)
    -- ... build FieldSchema with fieldRequired = False
```

The `selName (undefined :: ...)` pattern is the universal standard in Haskell - Aeson, swagger2, openapi3, optparse-generic, and beam all use identical code.

### 4. Documented Typeclass (Optional Metadata)

The `Documented` typeclass adds human-readable metadata:

```haskell
-- core/traits/Documented.hs
module Documented (
  Documented (..),
) where

import Array (Array)
import Array qualified
import Text (Text)
import TypeName qualified

-- | Optional documentation metadata for types.
-- All fields have defaults - implement only what you need.
class (TypeName.Inspectable value) => Documented value where
  -- | Human-readable name. Defaults to type name via TypeName.reflect.
  name :: Text
  name = TypeName.reflect @value
  
  -- | Description of what this type represents. Defaults to empty.
  description :: Text
  description = ""
  
  -- | Example values for documentation. Defaults to empty array.
  examples :: Array value
  examples = Array.empty
  
  -- | Whether this type is deprecated. Defaults to False.
  deprecated :: Bool
  deprecated = False
```

Design notes:

- **All fields defaulted**: Users implement only what they need
- **Empty string for description**: Not `Maybe Text` - simpler API, empty means "no description"
- **Uses existing TypeName.Inspectable**: Leverages NeoHaskell's existing type reflection infrastructure
- **Examples are typed**: IDE can validate example values match the type

Usage:

```haskell
instance Documented AddItem where
  description = "Add an item to a shopping cart"
  examples = 
    [ AddItem { cartId = exampleUuid, productId = "SKU-123", quantity = 2 }
    ]
```

### 5. OpenAPI Integration with WebTransport

OpenAPI documentation is a feature of WebTransport, not a separate wiring step:

```haskell
-- core/service/Service/Transport/Web.hs (additions)
data WebTransport = WebTransport
  { port :: Int,
    maxBodySize :: Int,
    authEnabled :: Maybe AuthEnabled,
    oauth2Config :: Maybe OAuth2Config,
    fileUploadEnabled :: Maybe FileUploadEnabled,
    -- NEW: API documentation metadata
    apiTitle :: Text,
    apiVersion :: Text,
    apiDescription :: Text
  }

server :: WebTransport
server = WebTransport
  { port = 8080,
    maxBodySize = 1048576,
    authEnabled = Nothing,
    oauth2Config = Nothing,
    fileUploadEnabled = Nothing,
    -- NEW: sensible defaults
    apiTitle = "API",
    apiVersion = "1.0.0",
    apiDescription = ""
  }
```

The `assembleTransport` function adds documentation routes automatically:

```haskell
-- In assembleTransport implementation
-- After routing command and query endpoints...

-- Documentation routes (always available when using WebTransport)
["openapi.json"] -> serveOpenApiJson endpoints transport
["openapi.yaml"] -> serveOpenApiYaml endpoints transport  
["docs"] -> serveSwaggerUi transport
```

### 6. Schema Capture at Handler Creation

Schema information must be captured at `createHandler` time in `Service.ServiceDefinition.Core`, before type information is erased:

```haskell
-- Service/ServiceDefinition/Core.hs changes
data CommandDefinition name transport cmd transportName event entity entityName entityIdType = CommandDefinition
  { -- existing fields...
    commandExecutor :: CommandExecutorConfig,
    -- NEW: schema captured at definition time
    commandSchema :: Schema,
    commandDocumentation :: Maybe DocumentedInfo
  }

data DocumentedInfo = DocumentedInfo
  { docName :: Text,
    docDescription :: Text,
    docDeprecated :: Bool
  }
```

When `Service.command @AddItem` is called:

```haskell
command ::
  forall cmd.
  ( Command cmd,
    ToSchema cmd,
    -- ... other constraints
  ) =>
  ServiceBuilder a -> ServiceBuilder b
command builder = do
  -- Capture schema while type is still available
  let schema = toSchema @cmd
  let docs = captureDocumentation @cmd  -- If Documented instance exists
  -- ... rest of command registration
```

### 7. OpenAPI Conversion Module

A dedicated module converts `Schema` to OpenAPI:

```haskell
-- core/schema/Schema/OpenApi.hs
module Schema.OpenApi (
  toOpenApiSpec,
  toOpenApiSchema,
) where

import Schema (Schema (..), FieldSchema (..))
-- Uses openapi3 library types

toOpenApiSchema :: Schema -> OpenApi.Schema
toOpenApiSchema schema = case schema of
  SNull -> OpenApi.nullSchema
  SBool -> OpenApi.boolSchema
  SInt -> OpenApi.intSchema
  SNumber -> OpenApi.numberSchema
  SText -> OpenApi.stringSchema
  SArray inner -> OpenApi.arraySchema (toOpenApiSchema inner)
  SOptional inner -> toOpenApiSchema inner  -- Required-ness is on field, not schema
  SObject fields -> OpenApi.objectSchema (fields |> Array.map toOpenApiProperty)
  SEnum variants -> OpenApi.enumSchema variants
  SUnion cases -> OpenApi.oneOfSchema (cases |> Array.map toOpenApiCase)
  SRef name -> OpenApi.refSchema name

toOpenApiSpec :: 
  Text ->           -- title
  Text ->           -- version
  Text ->           -- description
  Array Endpoint -> -- command and query endpoints
  OpenApi.OpenApi
toOpenApiSpec title version description endpoints = -- ...
```

### 8. Module Structure

```text
core/
  traits/
    Documented.hs           -- NEW: Documented typeclass with defaults
  schema/
    Schema.hs               -- NEW: Schema ADT + ToSchema + GToSchema
    Schema/
      OpenApi.hs            -- NEW: Schema → OpenAPI conversion
```

The `Core.hs` module re-exports:

```haskell
-- In Core.hs
import Schema (Schema, ToSchema, toSchema)
import Schema qualified
import Documented (Documented (..))
import Documented qualified
```

## Consequences

### Positive

1. **Zero-effort documentation**: Types with `Generic` get schemas automatically. No manual OpenAPI writing.

2. **Single source of truth**: Schema comes from types. Can't get out of sync with implementation.

3. **Library agnostic**: `Schema` ADT can target OpenAPI 3.0, JSON Schema, CLI args, or future formats.

4. **Leverages existing infrastructure**: Uses `TypeName.Inspectable` (existing), `Default` pattern (existing), and `Generic` (GHC).

5. **Progressive enhancement**: Basic schema is free. Add `Documented` instance only when you want descriptions/examples.

6. **Automatic WebTransport integration**: No `Application.withOpenApi` wiring - routes appear automatically.

7. **Compile-time schema capture**: Schema extracted at `Service.command` time ensures type information isn't lost.

### Negative

1. **New dependency on openapi3**: The `Schema/OpenApi.hs` module requires the `openapi3` Haskell package.

2. **Generic constraint on commands**: All commands must derive `Generic` for schema generation. This is already common practice.

3. **No custom serialization support**: If a type has custom `ToJSON`/`FromJSON` that differs from Generic derivation, the schema may not match the actual JSON.

### Trade-offs

1. **Automatic over explicit**: Routes like `/openapi.json` appear without opt-in. Some may prefer explicit registration.

2. **Empty string over Maybe**: `description = ""` instead of `Maybe Text` - simpler but can't distinguish "no description" from "empty description" (unlikely to matter in practice).

3. **GHC.Generics over Template Haskell**: Generics are more portable and don't require TH, but may have longer compile times for very large types.

## Future Considerations

1. **JSON Schema output**: Add `Schema/JsonSchema.hs` for JSON Schema Draft 7 output.

2. **CLI argument generation**: Use `Schema` for `optparse-applicative` generation in future CLI transport.

3. **Validation**: Use `Schema` for request validation (reject invalid JSON before command execution).

4. **Custom schema overrides**: Allow `ToSchema` instances to customize schema beyond what Generic provides.

5. **Schema caching**: Cache generated OpenAPI spec to avoid regeneration on every request.

6. **Redoc support**: Alternative to Swagger UI for `/docs` endpoint.

## References

- [core/meta/TypeName.hs](../../core/meta/TypeName.hs) - Existing type reflection infrastructure
- [core/traits/Default.hs](../../core/traits/Default.hs) - Default value pattern
- [core/service/Service/Transport/Web.hs](../../core/service/Service/Transport/Web.hs) - WebTransport implementation
- [core/service/Service/ServiceDefinition/Core.hs](../../core/service/Service/ServiceDefinition/Core.hs) - Service definition and command registration
- [ADR-0002: WebAPI Adapter Architecture](0002-webapi-adapter-architecture.md) - WebTransport routing foundation
- [autodocodec](https://hackage.haskell.org/package/autodocodec) - Prior art for single-schema-multiple-formats
