# ADR-0017: ToSchema Auto-Derivation in Command TH

## Status

Proposed

## Context

NeoHaskell's command system uses Template Haskell (`command ''CommandName`) to generate boilerplate for commands. Currently, the `command` splice generates:

1. `type instance NameOf Command = "CommandName"`
2. `instance Command Command` (with `getEntityIdImpl` and `decideImpl`)
3. `instance KnownHash "CommandName"`

However, ADR-0013 introduced the `ToSchema` typeclass for automatic OpenAPI schema generation. Commands exposed via WebTransport need `ToSchema` instances to appear in the generated API documentation.

**Current situation**: Users must manually add `instance ToSchema CommandName` in each command module:

```haskell
data AddItem = AddItem
  { cartId :: Uuid
  , stockId :: Uuid
  , quantity :: Int
  }
  deriving (Generic, Typeable, Show)

-- Manual boilerplate users must remember to add
instance ToSchema AddItem

instance Json.FromJSON AddItem

-- ... rest of command definition ...
command ''AddItem
```

**Problems with current approach**:

1. **Easy to forget**: Users can forget to add the `ToSchema` instance, causing runtime errors when the WebTransport tries to generate OpenAPI specs
2. **Inconsistent**: Some commands have it, some don't, leading to incomplete documentation
3. **Redundant**: Since commands already derive `Generic`, the `ToSchema` instance is trivial boilerplate
4. **Poor developer experience**: Adds friction to command creation without adding value

## Decision

### Extend the `command` TH splice to automatically derive `ToSchema`

The `command` splice in `Service.CommandExecutor.TH` will be extended to generate:

```haskell
instance ToSchema CommandName
```

alongside the existing generated instances.

### 1. Implementation Changes

Modify `Service/CommandExecutor/TH.hs` to:

1. Look up the `ToSchema` type class name
2. Generate an empty `ToSchema` instance (relies on default method from Generic)
3. Include the instance in the returned declarations

```haskell
-- In the command function, add:
toSchemaClassName <-
  TH.lookupTypeName "ToSchema"
    >>= orError
      "ERROR: ToSchema type class not found. Please ensure Core is imported."

let toSchemaInstance =
      TH.InstanceD
        Nothing
        []
        (TH.ConT toSchemaClassName `TH.AppT` TH.ConT someName)
        []  -- Empty body, uses default implementation from Generic

pure ([nameOfInstance, commandInstance, toSchemaInstance] ++ knownHashInstance)
```

### 2. Prerequisites Validation

The TH splice should verify that the command type:

1. Has a `Generic` instance (already required for `FromJSON`)
2. All field types have `ToSchema` instances (checked at use-site, not TH time)

If `Generic` is missing, the existing error messages will guide users.

### 3. Migration Path

**Backwards compatible**: Existing code with explicit `instance ToSchema CommandName` will cause a "duplicate instance" compile error. Users should remove the manual instances.

A one-time migration script can be provided:

```bash
# Find and remove redundant ToSchema instances
grep -rn "instance ToSchema" --include="*.hs" | grep -E "Commands/|Queries/"
```

### 4. Final Command Definition (After Change)

```haskell
data AddItem = AddItem
  { cartId :: Uuid
  , stockId :: Uuid
  , quantity :: Int
  }
  deriving (Generic, Typeable, Show)

instance Json.FromJSON AddItem

type instance EntityOf AddItem = CartEntity
type instance TransportOf AddItem = WebTransport

-- Single splice generates: Command, NameOf, KnownHash, AND ToSchema
command ''AddItem
```

## Consequences

### Positive

1. **Less boilerplate**: Users no longer need to remember to add `ToSchema` instances
2. **Guaranteed schema coverage**: All commands automatically get schemas, ensuring complete API documentation
3. **Better onboarding**: New developers can't accidentally create undocumented commands
4. **Consistency**: All commands follow the same pattern
5. **Single source of truth**: The `command` splice is the single place defining what a command needs

### Negative

1. **Breaking change**: Existing code with explicit `instance ToSchema` will fail to compile (easy fix: remove the duplicate)
2. **Less flexibility**: Users who want custom `ToSchema` behavior must use a different approach (rare case)
3. **Increased TH complexity**: The `command` splice now generates one more instance

### Trade-offs

1. **Convention over configuration**: We assume all commands want schemas, which is true for WebTransport but may not be for hypothetical non-HTTP transports. This trade-off favors the common case.

2. **Compile-time validation vs runtime flexibility**: By generating instances at TH time, we catch schema issues at compile time. Users wanting dynamic schemas must use alternative approaches.

### Migration Path

1. Search for explicit `instance ToSchema` in command modules
2. Remove them (the TH splice now handles this)
3. Rebuild to verify no duplicate instance errors

## References

- [Issue #353](https://github.com/neohaskell/neohaskell/issues/353)
- [ADR-0003: Command Abstraction and Flow](0003-command-abstraction-and-flow.md) - Command TH design
- [ADR-0013: Automatic Schema Generation](0013-automatic-schema-generation.md) - ToSchema typeclass
- [core/service/Service/CommandExecutor/TH.hs](../../core/service/Service/CommandExecutor/TH.hs) - TH implementation
- [testbed/src/Testbed/Cart/Commands/AddItem.hs](../../testbed/src/Testbed/Cart/Commands/AddItem.hs) - Example command
