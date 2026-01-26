# OpenAPI Generation for NeoHaskell

**Date**: 2026-01-26  
**Status**: Design (v2 - Simplified)  
**Authors**: Brainstorm session with user feedback

## Design Principles

1. **Use existing infrastructure** - `NameOf`, `TypeName.reflect`, `Typeable`, `Default`
2. **All fields defaulted** - Users can just derive, no required overrides
3. **OpenAPI is automatic** - Built into WebTransport, not a separate feature
4. **Documentation decoupled from OpenAPI** - Reusable for other doc formats

## Core Insight

OpenAPI generation is the **documentation feature of WebTransport**, not a separate module. When you use `WebTransport`, you automatically get:
- `/openapi.json` 
- `/openapi.yaml`
- `/docs` (Swagger UI)

No extra wiring needed.

## Existing Infrastructure We Leverage

| Primitive | Location | Purpose |
|-----------|----------|---------|
| `NameOf t` | `Service.Command.Core` | Type → Symbol at compile-time |
| `TypeName.reflect @a` | `TypeName` | Type → Text at runtime via `Typeable` |
| `Default` | `Default` | Provides `def` for default values |
| `Generic` | GHC | Automatic derivation |
| `ToJSON`/`FromJSON` | `Json` | Already on all commands/queries |

## The `Documented` Typeclass

**All fields have defaults.** Users can derive it or write minimal instances.

```haskell
module Documented (Documented (..)) where

import Basics
import Text (Text)
import TypeName qualified
import Array (Array)
import Array qualified

-- | Typeclass for documentable types.
-- 
-- All methods have defaults - just derive it:
-- 
-- @
-- data CreateOrder = CreateOrder { ... }
--   deriving (Generic, Documented)
-- @
--
-- Or customize what you need:
--
-- @
-- instance Documented CreateOrder where
--   description = "Creates a new order for a customer"
-- @
class (TypeName.Inspectable a) => Documented a where
  -- | Human-readable name. Default: type name from Typeable.
  name :: Text
  name = TypeName.reflect @a
  
  -- | Description of what this type represents. Default: empty.
  description :: Text
  description = ""
  
  -- | Example values. Default: empty array.
  examples :: Array a
  examples = Array.empty
  
  -- | Whether this type is deprecated. Default: False.
  deprecated :: Bool
  deprecated = False
```

### Why This Works

1. **`name`** - Uses existing `TypeName.reflect` which wraps `Typeable`
2. **`description`** - Defaults to `""` (not `Maybe Text` - simpler)
3. **`examples`** - Defaults to empty (can be populated for rich docs)
4. **`deprecated`** - Defaults to `False`

### Usage Patterns

```haskell
-- Minimal: Just derive, get type name automatically
data CreateCart = CreateCart
  deriving (Generic, Typeable, Documented)

-- With description only
instance Documented ReserveStock where
  description = "Reserves stock for an order"

-- Full customization
instance Documented CreateOrder where
  description = "Creates a new order for a customer"
  examples = Array.fromLinkedList
    [ CreateOrder { customerId = Uuid.nil, items = Array.empty }
    ]
```

## Schema Generation

Uses `Generic` + `Aeson` patterns already in place:

```haskell
module Schema (Schema (..), ToSchema (..)) where

import Basics
import Json qualified
import Documented (Documented)
import TypeName qualified

-- | OpenAPI-compatible schema representation
data Schema = Schema
  { schemaType :: Text
  , schemaProperties :: Map Text Schema
  , schemaRequired :: Array Text
  , schemaDescription :: Text
  , schemaExample :: Maybe Json.Value
  , schemaDeprecated :: Bool
  }
  deriving (Generic, ToJSON)

-- | Generate schema from a type
class (Documented a, Json.ToJSON a, Generic a) => ToSchema a where
  toSchema :: Schema
  default toSchema :: (GToSchema (Rep a)) => Schema
  toSchema = do
    let baseSchema = genericToSchema @a
    -- Enrich with Documented metadata
    baseSchema
      { schemaDescription = description @a
      , schemaExample = examples @a |> Array.first |> Maybe.map Json.toJSON
      , schemaDeprecated = deprecated @a
      }
```

**Key**: `ToSchema` combines `Generic` (for structure) + `Documented` (for metadata).

## WebTransport Integration

OpenAPI is **built into WebTransport**, not separate:

```haskell
-- In Service/Transport/Web.hs

-- WebTransport configuration gains optional API metadata
data WebTransport = WebTransport
  { port :: Int
  , maxBodySize :: Int
  , authEnabled :: Bool
  , -- NEW: API documentation metadata (all defaulted)
    apiTitle :: Text        -- Default: "API"
    apiVersion :: Text      -- Default: "1.0.0"
    apiDescription :: Text  -- Default: ""
  }

-- Default configuration
server :: WebTransport
server = WebTransport
  { port = 8080
  , maxBodySize = 1024 * 1024
  , authEnabled = False
  , apiTitle = "API"
  , apiVersion = "1.0.0"
  , apiDescription = ""
  }

-- Builder for customization
withApiInfo :: Text -> Text -> Text -> WebTransport -> WebTransport
withApiInfo title version desc transport = transport
  { apiTitle = title
  , apiVersion = version
  , apiDescription = desc
  }
```

### Routes Added Automatically

When `WebTransport` assembles routes, it **automatically** adds:

```haskell
-- In assembleTransport, before the catch-all:

["openapi.json"] -> do
  let spec = generateOpenApiSpec endpoints webTransport
  respondJson spec

["openapi.yaml"] -> do
  let spec = generateOpenApiSpec endpoints webTransport
  respondYaml spec

["docs"] -> do
  respondHtml (swaggerUIHtml "/openapi.json")
```

### Spec Generation

```haskell
generateOpenApiSpec :: Endpoints WebTransport -> WebTransport -> OpenApiSpec
generateOpenApiSpec endpoints config = OpenApiSpec
  { openapi = "3.0.3"
  , info = Info
      { title = config.apiTitle
      , version = config.apiVersion
      , description = config.apiDescription
      }
  , paths = do
      let commandPaths = endpoints.commandEndpoints
            |> Map.toList
            |> Array.fromLinkedList
            |> Array.map commandToPath
      let queryPaths = endpoints.queryEndpoints
            |> Map.toList
            |> Array.fromLinkedList
            |> Array.map queryToPath
      Map.fromList (Array.toLinkedList (commandPaths ++ queryPaths))
  , components = Components
      { schemas = collectSchemas endpoints
      }
  }
```

## What Changes

### Files Modified

| File | Change |
|------|--------|
| `core/traits/Documented.hs` | **NEW** - `Documented` typeclass |
| `core/service/Schema.hs` | **NEW** - `ToSchema` + schema types |
| `core/service/Service/Transport/Web.hs` | Add `/openapi.json`, `/docs` routes |
| `core/service/Service/Transport/Web.hs` | Add `apiTitle`, `apiVersion`, `apiDescription` fields |

### Files NOT Needed (Removed from Previous Design)

- ~~`OpenApi/Api.hs`~~ - No separate DSL needed
- ~~`OpenApi/Auto.hs`~~ - Built into transport
- ~~`OpenApi/Application.hs`~~ - No separate wiring
- ~~`OpenApi/Validation.hs`~~ - Can add later if needed

## Usage Example (Simplified)

```haskell
module MyApp where

-- Commands automatically get OpenAPI docs
data CreateOrder = CreateOrder
  { customerId :: Uuid
  , items :: Array OrderItem
  }
  deriving (Generic, Typeable, FromJSON, ToJSON, Documented)

-- Optional: Add description
instance Documented CreateOrder where
  description = "Creates a new order for a customer"

-- Application - NO EXTRA WIRING for OpenAPI
main :: Task Never ()
main = do
  Application.new
    |> Application.withService orderService
    |> Application.withQuery @OrderSummary
    |> Application.withTransport 
        (WebTransport.server 
          |> WebTransport.withApiInfo "Order API" "1.0.0" "Order management")
    |> Application.run

-- Result: 
--   /commands/create-order  (POST)
--   /queries/order-summary  (GET)
--   /openapi.json           (GET) <- Automatic!
--   /openapi.yaml           (GET) <- Automatic!
--   /docs                   (GET) <- Automatic!
```

## Type Information Flow

**Problem**: At `assembleTransport` time, type info is erased.

**Solution**: Store schema info during handler creation in `CommandDefinition`:

```haskell
data CommandDefinition ... = CommandDefinition
  { commandName :: Text
  , transportName :: Text
  , -- NEW: Schema stored at definition time
    commandSchema :: Schema  -- Captured when handler is built
  , responseSchema :: Schema
  }
```

The TH macro `command ''CreateOrder` would also generate:
- `type instance NameOf CreateOrder = "CreateOrder"` (existing)
- Schema derivation via `ToSchema` (new)

## Open Questions (Reduced)

1. **Schema for `Array a`** - How to represent parameterized types?
2. **Auth documentation** - Mark endpoints as requiring JWT?
3. **Field descriptions** - Do we need per-field docs or is type-level enough?

## Summary of Changes from v1

| Aspect | v1 Design | v2 Design |
|--------|-----------|-----------|
| Type name | New `typeName` method | Use `TypeName.reflect` |
| Description | `Maybe Text` | `Text` with `""` default |
| OpenAPI wiring | `Application.withOpenApi` | Automatic in `WebTransport` |
| File structure | 6 new modules | 2 new modules |
| DSL | Endpoint builder DSL | None needed |
| Configuration | Separate `OpenApiConfig` | Fields on `WebTransport` |

**Result**: Simpler, more integrated, leverages existing NeoHaskell patterns.
