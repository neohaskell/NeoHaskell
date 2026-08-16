# Change 007: Make `NameOf` the single type identity and add the `Descriptor` primitive

Issue #801 asks for two things that are really one thing. First, a type must
have **one** identity: today `NameOf` (type-level, drives `emitCommand`,
`makeCommandPayload`, `buildHandler`, `withTransport` and the `KnownHash`
command registry) and `Documented.name` (value-level `Text`, defaulting to
`TypeName.reflect`) both claim to name a type, and they already disagree —
`Integration.Agent.TestFixtures.AddItemCommand` has
`type instance NameOf AddItemCommand = "AddItem"`, so `Documented.name` would
answer `"AddItemCommand"` while dispatch expects `"AddItem"`. `Documented.name`
has zero call sites in `core/`, `testbed/` and `integrations/`, and no instance
anywhere overrides it, so it is a trap with no users: it is removed, and
`NameOf` becomes the single identity. Second, the "name + description + schema"
triple that ADR-0013 section 6 sketched as `DocumentedInfo` and never
implemented is shipped for real as a provider-neutral `Descriptor`, whose name
comes from `NameOf` — so a type that cannot route cannot get a descriptor
either, at compile time rather than at runtime.

```yaml spec
issue: issue#801                # issue#NNN or adhoc:<slug>
kind: refactor                  # feature | bug | refactor
touches: [traits, schema]       # capability IDs from codemap/capabilities.yaml (closed list)
breaking: true                  # MUST be true if the contract delta has any `-` line
new-dependency: false           # any new build-depends / flake input
new-capability: false           # this change adds a row to codemap/capabilities.yaml
new-extension-point: false      # this change adds a row to codemap/extension-points.yaml
```

## Contract delta

One removal, one new module. **The constructor function is `Descriptor.describe`,
not `Descriptor.of` as the issue writes it — `of` is a Haskell reserved word
(`case … of`) and cannot be a function name.** `describe` was chosen to read at
the call site (`Descriptor.describe @AddItem`) alongside the existing nullary
type-applied primitives `Schema.toSchema @value` and `TypeName.reflect @value`;
`Descriptor.reflect` and `Descriptor.forType` are the alternatives, and renaming
is a one-line change at this gate.

The signature also carries **`ToJSON value`**, which the issue's sketch omits.
It is not optional: `Documented.examples :: Array value` is type-indexed, and
flattening it to the monomorphic `Array Json.Value` the record holds is exactly
a `ToJSON` call. Every concept type that reaches a descriptor already has a
`ToJSON` instance (the TH markers derive it), so the constraint costs existing
callers nothing.

`Descriptor` is deliberately **not** added to the `Core` re-export list. Its
field labels (`name`, `description`, `schema`) would collide with the
`Documented (..)` and `Schema (..)` re-exports already there; it is a
qualified-import module (`import Descriptor qualified`), and with
`NoFieldSelectors` its fields are reached through `descriptor.name` anyway. That
also settles the import-cycle caveat in the issue: `Descriptor` imports
`Documented`, `Schema` and `Service.Command.Core` directly and never imports
`Core`, so no cycle can form.

```diff signatures
- Documented: name :: Documented value => Text
+ Descriptor: data Descriptor
+ Descriptor: Descriptor :: Text -> Text -> Schema -> Array Value -> Bool -> Descriptor
+ Descriptor: [name] :: Descriptor -> Text
+ Descriptor: [description] :: Descriptor -> Text
+ Descriptor: [schema] :: Descriptor -> Schema
+ Descriptor: [examples] :: Descriptor -> Array Value
+ Descriptor: [deprecated] :: Descriptor -> Bool
+ Descriptor: describe :: forall value (name :: Symbol). (Documented value, ToSchema value, ToJSON value, NameOf value ~ name, KnownSymbol name) => Descriptor
+ Descriptor: instance GHC.Classes.Eq Descriptor.Descriptor
+ Descriptor: instance GHC.Show.Show Descriptor.Descriptor
```

## Criteria

Every criterion is `unit`: nothing here crosses a filesystem, a database or a
socket — the whole change is a typeclass shape and a pure type-directed
function, and the one criterion that *is* about a boundary (C5, the compile-time
rejection) is proven inside GHC by the existing
`Test.CompileTime.shouldNotTypecheck` helper rather than by a runtime effect.

C2 is the criterion that pins the *point* of the issue, and it is written so it
cannot pass by accident: the fixture's `NameOf` (`"AddItem"`) is deliberately
different from its type name (`AddItemCommand`), and the test asserts **both**
sides — that the descriptor answers the routable `NameOf` symbol *and* that
`TypeName.reflect` still answers the type name. An implementation that quietly
kept the old `TypeName.reflect` default would flip exactly this test red.

C5 is the compile-time half of the same guarantee: without it, "the name comes
from `NameOf`" is only true for the types we happened to test. `KnownSymbol
(NameOf value)` on an un-instantiated type family is unsolvable, so
`Descriptor.describe @NoNameCommand` is a type error, and
`shouldNotTypecheck` (already wired into `nhcore` via
`core/testlib/Test/CompileTime.hs` and the `should-not-typecheck` dependency)
turns that into an assertion.

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | `Documented` exposes only `description`, `examples` and `deprecated`: a bare instance still yields the documented defaults (`""`, empty array, `False`), and an instance that overrides `description` is honored — the behavior of the surviving members is unchanged by the removal of `name` | `DocumentedSpec` "the reduced class keeps its defaults and honors overrides" | unit |
| C2 | `Descriptor.describe` takes `name` from `NameOf`, never from a value-level fallback: for a fixture whose `NameOf` is `"AddItem"` but whose type is `AddItemCommand`, the descriptor's `name` is `"AddItem"` while `TypeName.reflect @AddItemCommand` is still `"AddItemCommand"` | `DescriptorSpec` "name comes from NameOf, not from the type name" | unit |
| C3 | `Descriptor.describe` carries `Documented.description` verbatim, including the empty case: a fixture with a description gets it unchanged, and a fixture whose description is `""` gets `""` — the descriptor never substitutes the name, because the empty-description fallback is a rendering decision that belongs to the renderer | `DescriptorSpec` "description is carried verbatim, including empty" | unit |
| C4 | `Descriptor.describe` carries `Schema.toSchema @value` unchanged, flattens `Documented.examples` to `Array Json.Value` through `ToJSON` preserving order, yields the empty array when there are no examples, and carries `deprecated` through in both states | `DescriptorSpec` "schema, examples and deprecated are carried through" | unit |
| C5 | A type with no `NameOf` instance is rejected at the call site, at compile time: `Descriptor.describe @NoNameCommand` does not typecheck even though the type has `Documented`, `ToSchema` and `ToJSON` instances | `DescriptorSpec` "a type without a NameOf instance is rejected at compile time" | unit |
| C6 | The descriptor stays provider-neutral — it holds a `Schema`, not a rendering: the same descriptor feeds `Schema.JsonSchema.toJsonSchema` and `Schema.OpenApi.toOpenApiSchema` downstream, and `toJsonSchema descriptor.schema` equals the `parameters` payload shape that a tool definition needs, with no provider field anywhere in the record | `DescriptorSpec` "the schema field renders through both existing renderers" | unit |

## User impact

**Breaking, for a member with no known users.** `Documented (..)` is re-exported
from `Core`, so any downstream application that wrote

```haskell
instance Documented AddItem where
  name = "AddItem"
  description = "Add an item to a shopping cart"
```

stops compiling with *`name` is not a (visible) method of class `Documented`*.

**Migration.** Identity moves to the type level, and a display string comes from
`TypeName`:

```haskell
-- identity (what routing, dispatch and the command registry use)
type instance NameOf AddItem = "AddItem"

-- a display string for a type that is NOT a concept (no NameOf instance)
TypeName.reflect @AddItem
```

`TypeName.reflect @value` is *exactly* the expression the deleted default ran,
so a caller that relied on the default gets identical text by calling it
directly. A caller that overrode `name` was, by definition, disagreeing with
`NameOf` — which is the bug this change removes — and must move the override to
`type instance NameOf`.

**No call sites are being fixed up in this repo.** `Documented.name` has zero
references across `core/`, `testbed/` and `integrations/`; the only consumer of
`Documented` at all is `Documented.description` at `Integration.Agent:93`, and
`Integration.Agent.commandTool` already sources its name from `NameOf`. Nothing
in the monorepo changes behavior.

**Testbed effect: none.** No testbed command, query or `.hurl` file changes, and
no existing test expectation is touched — the change is additive plus one
removal with no callers.

**What this change deliberately does not do.** `Schema.FieldSchema`,
`Service.Transport.EndpointSchema`, `QueryDefinition` and
`Integration.Agent.CommandTool` are *not* migrated onto `Descriptor`; that is
the follow-up the issue names, and it is only sound once the primitive exists.
`Integration.Agent` is left untouched on purpose: its `CommandTool` carries a
pre-rendered OpenRouter payload, and the boundary that replaces it belongs to
#798. Nothing about `ToSchema` derivation or the `Schema` ADT moves.

## ADR

[ADR-0075](../decisions/0075-nameof-single-type-identity.md) — why `NameOf` is
the single identity of a type rather than one of two, why the value-level
`Documented.name` is removed instead of being redefined in terms of `NameOf`,
why `Descriptor` requires `KnownSymbol (NameOf value)` (scoping the primitive to
concept types, so an unroutable type fails to compile rather than emitting an
undispatchable name), why the record is provider-neutral, and how it supersedes
the unimplemented `DocumentedInfo` sketch in ADR-0013 section 6.
