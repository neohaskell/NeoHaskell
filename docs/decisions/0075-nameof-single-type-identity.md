# ADR-0075: `NameOf` is the single identity of a type; `Descriptor` is the shared name+description+schema primitive

> Implements #801. Supersedes the unimplemented `DocumentedInfo` sketch in
> [ADR-0013](0013-automatic-schema-generation.md) section 6, and narrows the
> `Documented` class that ADR-0013 introduced. Relates to
> [ADR-0045](0045-integration-agent.md) (the Agent integration, the only current
> consumer of `Documented`).

## Status

Proposed

## Context

Two mechanisms in `nhcore` claim to answer "what is this type called".

**`NameOf`** — `type family NameOf (t :: Type) :: Symbol`, declared in
`core/service/Service/Command/Core.hs` and re-exported from `Core`. It is
type-level, so GHC can match on it, and that is what makes it load-bearing:
`emitCommand`, `makeCommandPayload`, `buildHandler`, `withTransport` and the
`KnownHash` command registry all route on it, and the TH concept markers
generate it as the literal type name (`Service/CommandExecutor/TH.hs`), leaving
it hand-overridable.

**`Documented.name`** — a class method on `Documented` (`core/traits/Documented.hs`)
defaulting to `TypeName.reflect @value`, user-overridable, producing a
value-level `Text`.

They are not two views of one fact; they are two facts that can disagree, and in
the repository they already do. `integrations/test/Integration/Agent/TestFixtures.hs`
declares `type instance NameOf AddItemCommand = "AddItem"` for a type named
`AddItemCommand`, so `NameOf` says `"AddItem"` and `Documented.name` says
`"AddItemCommand"`.

The consequence is not hypothetical, it is just currently unexercised.
`Integration.Agent.commandTool` builds an LLM tool definition from a command
type and needs exactly the triple *name + description + schema*. It takes the
name from `NameOf`, the description from `Documented`, and the schema from
`ToSchema` — sidestepping `Documented.name` entirely. It has to: the tool name
it emits is the string the model sends back, `Integration.Agent.Internal.validateToolName`
matches that string against the registered tools, and dispatch then routes on
`NameOf`. A descriptor that innocently read `Documented.name` would emit tools
that fail validation and never dispatch — a runtime failure, invisible at
compile time, occurring only for the subset of types whose `NameOf` differs from
their type name. That is the worst shape a bug can have.

Meanwhile the triple itself exists four times, in four incompatible partial
shapes: `Schema.FieldSchema` (field name, schema, required, description),
`Service.Transport.EndpointSchema` (request/response schema, description,
deprecated), `QueryDefinition` (name as `Text`, schema), and
`Integration.Agent.CommandTool` (tool name, description, pre-rendered
OpenRouter JSON). ADR-0013 section 6 planned the shared version —
`commandSchema :: Schema` plus `commandDocumentation :: Maybe DocumentedInfo`
with `DocumentedInfo { docName, docDescription, docDeprecated }`, captured at
`Service.command` time. `DocumentedInfo` was never implemented; there are zero
occurrences of it in the codebase. `Documented` shipped as the surviving shell
of that plan, and `Documented.name` is the part of the shell that was never
needed: it has **zero call sites** across `core/`, `testbed/` and
`integrations/`, and no instance anywhere overrides it. The only member of
`Documented` anyone calls is `description`, on one line.

## Decision

**1. `NameOf` is the single identity of a type. `Documented.name` is removed.**

`Documented` keeps `description`, `examples` and `deprecated` — human-facing
text only, no identity. Where a *non-concept* type needs a display string, the
caller writes `TypeName.reflect @value`, which is character-for-character the
expression the deleted default ran.

**2. A new provider-neutral primitive `Descriptor` bundles the triple**, in
`core/schema/Descriptor.hs`:

```haskell
data Descriptor = Descriptor
  { name :: Text            -- from NameOf
  , description :: Text     -- from Documented
  , schema :: Schema        -- from ToSchema
  , examples :: Array Json.Value
  , deprecated :: Bool
  }

describe ::
  forall value name.
  ( Documented value
  , ToSchema value
  , Json.ToJSON value
  , NameOf value ~ name
  , GhcSymbol.KnownSymbol name
  ) =>
  Descriptor
```

**3. `KnownSymbol (NameOf value)` is a required constraint, not a convenience.**
It scopes the primitive to concept types — commands, queries, entities,
transports — the types that actually route. A type with no `NameOf` instance
leaves `NameOf value` stuck, `KnownSymbol` unsolvable, and the call a *compile
error*, instead of silently receiving a reflected name that cannot route.

**4. Rendering lives outside and downstream of the descriptor.** The record
holds a `Schema`, never a rendered payload. `Schema.JsonSchema.toJsonSchema`,
`Schema.OpenApi.toOpenApiSchema` and the OpenRouter adapter (#798) are the
renderers; the descriptor knows about none of them.

**5. `Descriptor` supersedes `DocumentedInfo`.** ADR-0013 section 6's sketch is
withdrawn: the shared shape is this record, and it carries the schema and the
examples too, not just the three text fields.

## Rationale

**Why remove `name` rather than redefine it as `NameOf`.** The tempting middle
road is to keep the method and give it the default
`symbolVal (Proxy @(NameOf value))`. It is worse than either end. It would force
`KnownSymbol (NameOf value)` as a superclass of `Documented`, making the *whole*
documentation class unusable for any type that is not a routable concept — the
plain data types that legitimately carry a description and examples. And it
would leave the override in place, so an instance could still declare a
value-level name that disagrees with the type-level one, which is precisely the
divergence being removed. One identity means one mechanism, and the mechanism
has to be the type-level one because that is the one GHC can dispatch on.

**Why the removal is safe to make breaking.** Zero call sites, zero overriding
instances, and the replacement is a mechanical one-liner in both directions
(`type instance NameOf T = "…"` for identity, `TypeName.reflect @T` for a
display string). The alternative — deprecating the method and removing it a
release later — buys a migration window for a member that, as far as the
monorepo can see, nobody uses, at the cost of keeping the divergence trap armed
for that whole window.

**Why `ToJSON` is in the signature even though the issue's sketch omits it.**
`Documented.examples :: Array value` is type-indexed and cannot live in a
monomorphic record; flattening it to `Array Json.Value` *is* a `ToJSON` call.
The flattened form is also what both consumers need — OpenApi example fields and
AI few-shot prompts take JSON, not the original values. Every concept type that
can reach a descriptor already has `ToJSON` (the TH markers derive it), so the
constraint is free at every real call site.

**Why the descriptor does not fall back to the name when the description is
empty.** `Integration.Agent.commandTool` does exactly that today, because
OpenRouter wants a non-empty description. That is a *provider* requirement, and
putting it in the shared primitive would silently give every other consumer —
OpenApi, docs generation — a description that is really a name. The descriptor
carries `Documented.description` verbatim, including `""`, and each renderer
applies its own policy.

**Why the module is not in the `Core` re-export.** `Descriptor`'s field labels
(`name`, `description`, `schema`) would collide with the `Documented (..)` and
`Schema (..)` re-exports already in `Core`. It is a qualified-import module, and
under `NoFieldSelectors` its fields are reached through `descriptor.name`
anyway. This also disposes of the import-cycle caveat raised in the issue:
`Descriptor` imports `Documented`, `Schema` and `Service.Command.Core` directly
and never imports `Core`, so `Core -> Descriptor -> Core` cannot form. (`Core`
already imports `Service.Command` for the `NameOf` re-export, so the
service-layer dependency is not new.)

**Why `describe` and not `of`.** The issue names the constructor
`Descriptor.of`. `of` is a Haskell reserved word (`case … of`) and cannot be a
function name. `describe` reads at the call site — `Descriptor.describe @AddItem` —
alongside the existing nullary type-applied primitives `Schema.toSchema @value`
and `TypeName.reflect @value`.

## Consequences

**Positive.** A type has one name, and it is the one that routes. The
name+description+schema triple has a single shared shape, so the four partial
versions have something to converge on. The compile-time `KnownSymbol` guard
turns "this descriptor's name will not dispatch" from a runtime mystery into a
type error at the call site. ADR-0013's unimplemented sketch stops being a
dangling promise.

**Negative / constraints.** This is a **breaking** change to a `Core`-re-exported
class: a downstream application that overrode `Documented.name` stops compiling
and must move the identity to `type instance NameOf`. `Descriptor` cannot
describe a type that has no `NameOf` instance — by design, but it does mean
"give me the description and schema of this plain data type" is not what this
primitive is for; that caller composes `Documented.description` and
`Schema.toSchema` directly.

**Non-goals.** `Schema.FieldSchema`, `Service.Transport.EndpointSchema`,
`QueryDefinition` and `Integration.Agent.CommandTool` are **not** migrated onto
`Descriptor` here — that is the follow-up the primitive unblocks, and
`Integration.Agent`'s pre-rendered `CommandTool` in particular belongs to the
boundary rework in #798. No change to `ToSchema` derivation or to the `Schema`
ADT.

## Alternatives considered

- **Keep `Documented.name`, default it to `NameOf`.** Rejected: forces
  `KnownSymbol (NameOf value)` onto every `Documented` instance, including plain
  data types that will never route, and leaves the disagreeing override in place.
- **Keep `Documented.name`, and have `Descriptor` read `NameOf` anyway.**
  Rejected: the trap stays armed. The class would advertise a name that the
  framework ignores, and the next consumer to reach for the obvious method gets
  the runtime dispatch failure described above.
- **Deprecate `name` with a `DEPRECATED` pragma and remove it next release.**
  Rejected: no known users to protect, and the cost is keeping a
  compile-time-invisible divergence live for a release cycle.
- **Put `Descriptor` in the service layer (`core/service/`).** Rejected: it is a
  type-metadata primitive built on `Schema` and `Documented`, and its only
  service-layer touchpoint is the `NameOf` type family. `core/schema/` is where
  the schema-side vocabulary already lives, and `nhcore`'s single-library,
  many-`hs-source-dirs` layout makes the dependency legal from there.
- **Implement `DocumentedInfo` as ADR-0013 wrote it** (`docName`,
  `docDescription`, `docDeprecated`, hung off `CommandDefinition`). Rejected: it
  reintroduces a value-level `docName` — the exact divergence being removed —
  carries no schema or examples despite living next to `commandSchema`, and is
  scoped to commands rather than to concepts generally.
