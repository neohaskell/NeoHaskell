# ADR-0034: CliTransport — Command-Line Interface Transport

## Status

Proposed

## Context

NeoHaskell's `WebTransport` (ADR-0002, ADR-0014) exposes commands and queries over HTTP, auto-generates OpenAPI docs from schemas, and routes requests to the appropriate handlers. For CLI applications (e.g., the planned `neo` CLI), we need an equivalent **CliTransport** that exposes commands and queries as CLI subcommands.

The existing `Transport` typeclass (`Service/Transport.hs`) is designed for this:

```haskell
class Transport transport where
  type Request transport
  type Response transport
  type RunnableTransport transport
  assembleTransport :: Endpoints transport -> RunnableTransport transport
  runTransport :: transport -> RunnableTransport transport -> Task Text Unit
  buildHandler :: ... -> EndpointHandler
```

However, two architectural constraints must be resolved first:

1. **`TransportOf` type family is single-valued**: Each command declares `type instance TransportOf AddItem = WebTransport`, locking it to one transport. A command cannot serve both HTTP and CLI from the same codebase.

2. **`EndpointHandler` takes raw `Bytes`**: The handler signature `RequestContext -> Bytes -> ((CommandResponse, Bytes) -> Task Text Unit) -> Task Text Unit` expects JSON-encoded bytes, which requires a strategy for CLI-to-Bytes conversion.

### Requirements

1. Commands become CLI subcommands grouped by entity
2. Auto-generated help text from `ToSchema` instances
3. A single command can serve multiple transports simultaneously
4. Type-safe CLI argument parsing with environment variable and config file support
5. Integration with `SimpleEventStore` (ADR-0032) for local persistence
6. Security hardening for local file-based operation

## Decision

### 1. `TransportOf` Redesign: Type-Level List

**Breaking change.** Replace the single-valued `TransportOf` type family with a type-level list:

```haskell
-- Before (Command/Core.hs:57):
type family TransportOf (commandType :: Type) :: Type

-- After:
type family TransportsOf (commandType :: Type) :: [Type]
```

Command definitions change from:

```haskell
type instance TransportOf AddItem = WebTransport
```

To:

```haskell
type instance TransportsOf AddItem = '[WebTransport, CliTransport]
```

This allows a single command to be exposed via multiple transports simultaneously. The `ServiceDefinition` endpoint-building logic (`buildEndpointsByTransport` in `ServiceDefinition/Core.hs`) already groups handlers by transport name — it iterates the list instead of using a single value.

**Migration path**: A type alias `type TransportOf cmd = '[t]` or TH macro update can ease the transition for existing commands.

### 2. CliTransport Data Type

```haskell
data CliTransport = CliTransport
  { programName :: Text
  , version :: Text
  , description :: Text
  }

instance Transport CliTransport where
  type Request CliTransport = [Text]        -- CLI args (from getArgs)
  type Response CliTransport = Text          -- stdout output
  type RunnableTransport CliTransport = Task Text Unit
```

Default constructor:

```haskell
cli :: CliTransport
cli = CliTransport
  { programName = "app"
  , version = "0.0.0"
  , description = ""
  }
```

### 3. CLI Subcommand Generation

Subcommand structure is derived from the `EntityOf` type family and `NameOf`, both of which are already captured in `EndpointSchema.entityName` at registration time.

**Algorithm:**

1. Get entity name from `EntityOf cmd` via `EndpointSchema.entityName` (e.g., `CartEntity` → `"CartEntity"`)
2. Strip the `"Entity"` suffix → `"Cart"`
3. Get command name from `NameOf cmd` (e.g., `"AddItem"`)
4. Strip the entity prefix from the command name → `"AddItem"` minus `"Cart"` prefix = `"AddItem"` (no match, keep as-is). `"CreateCart"` minus `"Cart"` suffix = `"Create"`
5. Kebab-case the result

**Concrete examples from testbed:**

| Command Type | `EntityOf` | Entity Name | Stripped Command | CLI |
|---|---|---|---|---|
| `CreateCart` | `CartEntity` | `Cart` | `Create` → `create` | `myapp cart create` |
| `AddItem` | `CartEntity` | `Cart` | `AddItem` → `add-item` | `myapp cart add-item` |
| `InitializeStock` | `StockEntity` | `Stock` | `Initialize` → `initialize` | `myapp stock initialize` |
| `ReserveStock` | `StockEntity` | `Stock` | `Reserve` → `reserve` | `myapp stock reserve` |
| `CreateDocument` | `DocumentEntity` | `Document` | `Create` → `create` | `myapp document create` |

The entity name is stripped from the **suffix** of the command name only when the command name ends with it. If the entity name does not appear in the command name (e.g., `AddItem` for `CartEntity`), the full command name is used as the subcommand.

**Queries** are top-level (not grouped by entity):

```bash
myapp query cart-summary
myapp query stock-level
```

### 4. CLI Argument Mapping from Schema

The `Schema` ADT (`core/schema/Schema.hs`) is used to generate CLI argument parsers. Mapping rules:

| Schema Variant | CLI Representation | Example |
|---|---|---|
| `SText` | `--field-name VALUE` | `--name "John"` |
| `SInt` | `--field-name N` | `--quantity 5` |
| `SNumber` | `--field-name N.N` | `--price 9.99` |
| `SBool` | `--field-name` (flag) | `--active` |
| `SOptional inner` | Flag is optional | `[--email TEXT]` |
| `SEnum variants` | `--field-name VARIANT` | `--status Active` |
| `SArray inner` | Repeated flag | `--tags foo --tags bar` |
| `SObject`, `SUnion`, `SRef` | JSON string value | `--address '{"street":"...","city":"..."}'` |

**Design principle**: Flat scalar types get native CLI flags. Complex types (nested objects, unions, recursive refs) fall back to JSON string values parsed via `FromJSON`. This keeps the mapping simple and predictable — users can always pass JSON for any field.

**Field naming**: Record field names are converted to kebab-case for CLI flags. `cartId` → `--cart-id`, `stockId` → `--stock-id`.

**Required vs optional**: Fields with `SOptional` wrapper are optional flags. All other fields are required. opt-env-conf enforces this and produces clear error messages.

### 5. Parsing with opt-env-conf

CLI argument parsing uses [opt-env-conf](https://hackage.haskell.org/package/opt-env-conf) instead of raw optparse-applicative. This provides:

- **CLI arguments**: `--cart-id abc-123`
- **Environment variables**: `MYAPP_CART_ID=abc-123` (auto-derived from flag name)
- **Configuration files**: `~/.config/myapp/config.yaml`

The existing `OptionsParser` module (`core/options-parser/Command.hs`) already has a comment noting future migration to opt-env-conf (line 299: `-- Helper function to make porting to opt-env-conf in the future easily`).

opt-env-conf handles:
- Type validation with CLI-friendly error messages ("expected integer, got 'abc'")
- Missing required argument detection
- Help text generation from field descriptions
- Shell completion generation

### 6. EndpointHandler Integration: Schema-Driven JSON Construction

The `EndpointHandler` signature takes `Bytes`:

```haskell
type EndpointHandler = RequestContext -> Bytes -> ((CommandResponse, Bytes) -> Task Text Unit) -> Task Text Unit
```

CliTransport's `assembleTransport` bridges CLI args to this interface:

1. **opt-env-conf parses CLI args** → produces typed values with CLI-friendly errors (catches "missing required flag", "invalid integer" at this layer)
2. **Construct `Aeson.Value`** from parsed values using Aeson builders (`Aeson.object`, `Aeson.String`, etc.) — **never string interpolation** (prevents JSON injection)
3. **`Json.encode`** the `Value` to `Bytes`
4. **Pass `Bytes` to `EndpointHandler`** → standard `FromJSON` deserialization

This preserves:
- **Same validation path as WebTransport**: all inputs go through `FromJSON`
- **CLI-friendly errors**: opt-env-conf catches type/missing errors before JSON conversion
- **No injection risk**: Aeson `Value` construction handles escaping
- **Negligible overhead**: JSON encode/decode roundtrip is sub-millisecond for CLI payloads

The opt-env-conf parser is generated from `commandSchemas` (available in `Endpoints`) at `assembleTransport` time. Each `EndpointSchema.requestSchema` provides the field structure.

### 7. RequestContext for CLI

CLI applications construct `RequestContext` from available OS-level data:

- **User identity**: OS username (`System.Posix.User.getEffectiveUserName` / equivalent)
- **No JWT/OAuth2**: Authentication is out of scope for CliTransport
- **No permission checks**: `RequestContext` carries OS user info only; commands that check permissions against JWT claims will see no claims

This is appropriate because:
- CLI apps run with the user's own filesystem permissions
- The local user is implicitly trusted (they can read/write the event store directly)
- Network-level auth (JWT, OAuth2) is a WebTransport concern

### 8. Output Formatting

| Flag | Format | Use Case |
|---|---|---|
| (default) | JSON | Machine-readable, scripting |
| `--pretty` | Pretty-printed JSON | Human inspection |
| `--quiet` | Entity ID only (or nothing) | Scripting pipelines |

`CommandResponse` variants map to:
- `Accepted { entityId, events }` → JSON with entity ID and event count. Exit code 0.
- `Rejected { reason }` → Error message to stderr. Exit code 1.
- `Failed { error }` → Error message to stderr. Exit code 1.

### 9. Process Lifecycle

CLI apps are **run-once-and-exit**, unlike WebTransport's long-running server.

`Application.run` startup sequence for CLI mode:

1. Set stdout/stderr to LineBuffering
2. Load .env file
3. Load config (if registered)
4. Create EventStore (SimpleEventStore with `persistent = True` by default)
5. Wire query definitions
6. Rebuild queries from historical events
7. **Skip**: Live event subscriptions (not needed for single invocation)
8. Parse CLI args → dispatch to command/query → print result
9. Shut down via an **Exit integration** that terminates the process

The Exit integration is a new outbound integration pattern:

```haskell
-- After command execution completes, shut down cleanly
Application.new
  |> Application.withTransport CliTransport.cli
  |> Application.withIntegration Exit.afterCommand
```

This reuses the existing integration pattern (ADR-0008) rather than adding lifecycle special-casing to the transport layer.

### 10. Query Handling

Queries return all results as-is (no filtering, no parameters). Query parameter support will be addressed by the NeoQL proposal in the future.

```bash
myapp query cart-summary        # Returns all CartSummary entries as JSON array
myapp query stock-level         # Returns all StockLevel entries as JSON array
```

### 11. Security Hardening

#### 11.1 No Secrets as CLI Arguments

Fields using the `Redacted` type (ADR-0016) are **refused** as CLI flag arguments. They must be provided via:
- Environment variables: `MYAPP_API_KEY=sk_live_...`
- Configuration files: `~/.config/myapp/credentials.yaml` (mode 0600)

opt-env-conf natively supports env var and config file sources, making this the default path for sensitive fields.

#### 11.2 SimpleEventStore File Security

When using `SimpleEventStore` with `persistent = True` (ADR-0032):

| Threat | Mitigation |
|---|---|
| **Permission leak** | Create store files with mode `0600` (owner-only read/write) |
| **Symlink attacks** | Open files with `O_NOFOLLOW` to prevent symlink traversal |
| **TOCTOU races** | Use `O_EXCL \| O_CREAT` for atomic create-or-fail |
| **Path traversal** | Entity IDs are validated as UUIDs before use as path components (already enforced by `getEntityId :: cmd -> Maybe Uuid`) |
| **Event tampering** | Append-only file mode. Each event includes a SHA-256 hash of the previous event, forming a hash chain. Tampering with any event invalidates all subsequent hashes. |

**Note**: `SimpleEventStore` with file persistence is intended for **development and single-user CLI tools**. Production deployments should use `PostgresEventStore` with proper database access controls.

#### 11.3 Default Storage Location

Event store files default to `XDG_DATA_HOME` (`~/.local/share/<programName>/events/`) for per-user isolation. Shared storage paths require explicit configuration.

### 12. Module Structure

```text
core/
  service/
    Service/
      Command/
        Core.hs              -- Modified: TransportsOf type-level list
      Transport/
        Cli.hs               -- NEW: CliTransport type + Transport instance
        Cli/
          ArgParser.hs        -- NEW: Schema → opt-env-conf parser generation
          Output.hs           -- NEW: CommandResponse → CLI output formatting
  options-parser/
    Command.hs               -- Modified: migrate to opt-env-conf
```

### 13. Usage Example

```haskell
-- App.hs
app :: Application
app =
  Application.new
    |> Application.withConfig @AppConfig
    |> Application.withEventStore simpleStore
    |> Application.withTransport CliTransport.cli
        { programName = "myapp"
        , version = "1.0.0"
        , description = "My CLI application"
        }
    |> Application.withService User.service
    |> Application.withQuery @UserSummary
    |> Application.withIntegration Exit.afterCommand
    |> Application.run
```

Commands serve both transports:

```haskell
-- User/Commands/CreateUser.hs
type instance TransportsOf CreateUser = '[WebTransport, CliTransport]
```

Generated CLI:

```bash
myapp user create --name "John" --email "john@example.com"
myapp user create --help

# Usage: myapp user create [OPTIONS]
#
# Create a new user.
#
# Options:
#   --name TEXT       User's full name (required)
#   --email TEXT      Email address (required)
#   --age INT         Age in years
#   -h, --help        Show this help text
#
# Environment variables:
#   MYAPP_USER_CREATE_NAME
#   MYAPP_USER_CREATE_EMAIL
#   MYAPP_USER_CREATE_AGE

myapp query user-summary
myapp query user-summary --pretty
```

### 14. Dependencies

| Package | Purpose | Notes |
|---|---|---|
| `opt-env-conf` | CLI args + env vars + config files | Replaces raw optparse-applicative usage |

No additional dependencies beyond opt-env-conf. The existing `optparse-applicative` dependency (used by `Command.hs`) is a transitive dependency of opt-env-conf.

### 15. Out of Scope

- **Stdin/pipe support**: Not in initial implementation. May be added later for scripting (`echo '{}' | myapp cart create`).
- **Query parameters/filtering**: Handled by the NeoQL proposal.
- **CLI authentication (JWT/OAuth2)**: Out of scope. CLI trusts the local OS user.
- **Interactive mode / REPL**: Future consideration.
- **Custom output formats** (`--format table|yaml`): Future consideration. Initial implementation supports JSON and pretty JSON only.

## Consequences

### Positive

1. **Multi-transport commands**: The `TransportsOf` type-level list enables commands to serve HTTP, CLI, and future transports (WebSocket, gRPC) simultaneously from the same codebase.

2. **Batteries-included CLI**: Like WebTransport auto-generates OpenAPI docs, CliTransport auto-generates help text and argument parsers from schemas. Zero manual CLI wiring.

3. **Same validation path**: CLI inputs go through the same `FromJSON` deserialization as HTTP inputs. Business logic is transport-agnostic.

4. **Secure by default**: Redacted fields refused as CLI args, event store files with restrictive permissions, hash-chain integrity for event tampering detection.

5. **Consistent DX**: `Application.withTransport CliTransport.cli` follows the same pattern as `Application.withTransport WebTransport.server`.

6. **Environment variable support**: opt-env-conf provides env var and config file support for free, enabling 12-factor app patterns for CLI tools.

### Negative

1. **Breaking change**: `TransportOf` → `TransportsOf` requires updating every command definition. Migration can be eased with TH macro updates.

2. **New dependency**: `opt-env-conf` adds to the dependency footprint. It is well-maintained and actively developed.

3. **Complex types require JSON flags**: Nested objects, unions, and recursive types fall back to `--field '{"json":"..."}'`, which is less ergonomic than native flags.

4. **JSON roundtrip overhead**: CLI args are parsed, serialized to JSON, then deserialized via FromJSON. This is negligible for CLI use but architecturally inelegant.

### Trade-offs

1. **Safety over ergonomics for complex types**: Rather than attempting to flatten arbitrary nested structures into CLI flags (error-prone, ambiguous), complex types use JSON string values. This is predictable and correct, at the cost of verbosity.

2. **Hash chain over simplicity**: SHA-256 hash chain for event integrity adds complexity to SimpleEventStore but provides tamper detection for local file storage.

3. **Exit integration over lifecycle special-casing**: Using an integration to shut down after command execution is slightly indirect, but avoids adding CLI-specific lifecycle logic to the transport abstraction.

## References

- [ADR-0002: WebAPI Adapter Architecture](0002-webapi-adapter-architecture.md) — Transport abstraction origin
- [ADR-0014: WebTransport OpenAPI Integration](0014-webtransport-openapi-integration.md) — Schema-driven documentation pattern
- [ADR-0016: Redacted Type for Sensitive Data](0016-redacted-type-for-sensitive-data.md) — Sensitive field handling
- [ADR-0032: SimpleEventStore](0032-simple-event-store.md) — Local file persistence
- [Issue #412](https://github.com/neohaskell/NeoHaskell/issues/412) — CliTransport proposal
- [opt-env-conf on Hackage](https://hackage.haskell.org/package/opt-env-conf) — CLI + env + config parsing
- [core/service/Service/Transport.hs](../../core/service/Service/Transport.hs) — Transport typeclass
- [core/service/Service/Transport/Web.hs](../../core/service/Service/Transport/Web.hs) — WebTransport reference implementation
- [core/options-parser/Command.hs](../../core/options-parser/Command.hs) — Existing CLI parser wrapper
