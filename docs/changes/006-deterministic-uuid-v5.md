# Change 006: Add deterministic UUID v5 generation to `Uuid` and the `Decision` monad

Issue #596 asks for a way to derive a **stable** UUID from a known input inside a
command, because today the `Decision` monad offers only `Decider.generateUuid`
(random v4) and `getEntityId` has no UUID-derivation function at all. The bump on
the issue names the concrete consumer: an aggregate whose stream id is derived
from a normalized natural key (a repo path), so that registering the same key
twice routes to the **same** stream and the create command can reject it as a
duplicate — the standard event-sourcing answer to cross-aggregate uniqueness.
This change adds the RFC 4122 v5 primitive at both altitudes it is needed:
`Uuid.generateV5` (pure, usable from `getEntityId`) and
`Decider.generateDeterministicUuid` (the `Decision` combinator the issue
requests), with the pure one as the single implementation. The `uuid` package is
already a direct `nhcore` dependency, so no new dependency is introduced.

```yaml spec
issue: issue#596                # issue#NNN or adhoc:<slug>
kind: feature                   # feature | bug | refactor
touches: [core-primitives, commands]  # capability IDs from codemap/capabilities.yaml (closed list)
breaking: false                 # MUST be true if the contract delta has any `-` line
new-dependency: false           # any new build-depends / flake input
new-capability: false           # this change adds a row to codemap/capabilities.yaml
new-extension-point: false      # this change adds a row to codemap/extension-points.yaml
```

## Contract delta

Three additions, no removals. `Uuid.generateV5` is the whole implementation —
namespace first, name second, matching RFC 4122 and `Data.UUID.V5.generateNamed`.
`Decider.generateDeterministicUuid` is that function lifted into `Decision` via
the existing `Return` constructor, so **no new `Decision` constructor and no new
`runDecision` case** are added. `Bytes.unpack` is the missing inverse of the
existing `Bytes.pack`, added so `generateV5` can reach `generateNamed`'s
`[Word8]` input through dialect vocabulary (`Text.toBytes |> Bytes.unpack`)
rather than a raw `Data.Text.Encoding`/`Data.ByteString` pair inside `Uuid`.

```diff signatures
+ Uuid: generateV5 :: Uuid -> Text -> Uuid
+ Bytes: unpack :: Bytes -> [Word8]
+ Decider: generateDeterministicUuid :: Uuid -> Text -> Decision Uuid
```

## Criteria

C1–C4 pin the pure primitive: determinism is the entire point, so both halves of
it are proven (same inputs ⇒ same output; *different* inputs ⇒ different output,
in the name **and** in the namespace), plus conformance to RFC 4122's version and
variant bits and a published v5 test vector — a hand-rolled implementation that
forgot the bit-fiddling would still pass a determinism-only test. C5 covers the
`Bytes` gap-fill. C6–C7 pin the `Decision` combinator: it agrees with the pure
function, and — the property that distinguishes it from `generateUuid` — it does
**not** draw from the context's random UUID source. C8 is the acceptance
criterion and the motivating use case end-to-end: it crosses the real HTTP
transport and the real event store in the testbed app, which a unit test cannot
do, so it is `acceptance` and names a `.hurl`. It proves the whole point of the
feature — that the same natural key resolves to the same stream, that the
resolved id is the *specific* v5 value (asserted literally, so a cross-process
drift in derivation fails the test), and that the second registration is
therefore rejected as a duplicate.

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | `Uuid.generateV5` is deterministic — the same namespace and name always produce the same `Uuid`, and it matches a published RFC 4122 v5 test vector (namespace DNS, name `python.org`) | `UuidSpec` "generateV5 is deterministic and matches the RFC 4122 v5 test vector" | unit |
| C2 | `Uuid.generateV5` separates inputs — different names under one namespace differ, and the **same** name under different namespaces differs (no global collision domain) | `UuidSpec` "generateV5 distinguishes names and namespaces" | unit |
| C3 | `Uuid.generateV5` output is a well-formed v5 UUID — version nibble is `5` and the RFC 4122 variant bits are set | `UuidSpec` "generateV5 sets the RFC 4122 version and variant bits" | unit |
| C4 | `Uuid.generateV5` encodes the name as UTF-8, so non-ASCII names are handled and a multi-byte name is not truncated or mangled; the empty name is total and still deterministic | `UuidSpec` "generateV5 handles unicode and empty names" | unit |
| C5 | `Bytes.unpack` is the inverse of `Bytes.pack` — round-trips a byte list, and agrees with `Bytes.length` on the resulting list length; the empty `Bytes` unpacks to the empty list | `BytesSpec` "unpack round-trips with pack" | unit |
| C6 | `Decider.generateDeterministicUuid` run through `runDecision` yields exactly `Uuid.generateV5` of the same namespace and name, and repeating the decision yields the same value | `DeciderSpec` "generateDeterministicUuid agrees with Uuid.generateV5 and repeats" | unit |
| C7 | `Decider.generateDeterministicUuid` consumes **no** randomness — a `DecisionContext` whose `genUuid` fails the task still lets a decision using only `generateDeterministicUuid` succeed, while one using `generateUuid` does not | `DeciderSpec` "generateDeterministicUuid does not draw from the random uuid source" | unit |
| C8 | End-to-end natural-key identity in the testbed app: `POST /commands/register-cart-by-key` with a given key creates a cart whose `entityId` is the **literal** v5 UUID derived from the testbed namespace and that key; the derived owner id in the emitted event is likewise deterministic; and a **second** POST with the same key is rejected as an already-registered duplicate rather than creating a second cart | `testbed/tests/commands/register-cart-by-key.hurl` | acceptance |

## User impact

**Not breaking.** Three added signatures, no removals, no behavior change to any
existing function. `Decision`'s constructor set is unchanged, so `runDecision`
and the command executor are untouched — existing commands cannot regress.

**New capability for users.** Natural-key entity identity becomes expressible in
the framework:

```haskell
getEntityId :: RegisterProject -> Maybe Uuid
getEntityId command =
  Uuid.generateV5 projectNamespace command.repoPath |> Just
```

so the same normalized path always routes to the same stream, and `decide` sees
`Just entity` on a repeat and can `Decider.reject "already registered"`. Inside
`decide`, other derived ids come from
`Decider.generateDeterministicUuid namespace key`, which reads alongside
`Decider.generateUuid` without a `pure`/`Task.yield` lift (which the dialect
hook bans at the call site anyway).

**Security constraint, carried in the haddock of both functions.** A v5 UUID is
**not** a secret — anyone who knows the namespace and the name reproduces it
exactly, and names drawn from a small space can be brute-forced from a known
namespace. It must never be used for capability tokens, session ids,
password-reset components, or anything else that is unguessable by design;
`Uuid.generate` (random v4) stays the correct choice there. The division of
labour is now explicit: *random for secrets and fresh identity, deterministic for
derived identity.*

**No default namespace.** The issue floated a `uuidFromText :: Text -> Decision Uuid`
convenience with a built-in namespace; it is deliberately not shipped. A
framework-wide default namespace is a global collision domain, and since the
whole premise of the primitive is that the same input *is* the same stream, such
a collision is a data-integrity bug across unrelated aggregates. Callers build a
namespace with `Uuid.fromText`. RFC 4122's predefined namespaces (DNS/URL/OID/X500)
are likewise not re-exported yet — rule of three, and no consumer has asked.

**Testbed effect.** A new demo command `Testbed.Cart.Commands.RegisterCartByKey`
(registered in `Testbed.Cart.Service`) exercises the feature end-to-end, per the
`new-command-machinery` extension point's rule that framework write-side features
get a demo command plus hurl coverage. It is additive: no existing testbed
command, query, or hurl file changes, and no existing expectation is touched.

**Relationship to the abandoned `feat/uuid-v5-decision` branch.** The issue asks
that the pre-existing branch be merged. It is not merged as-is: alongside the
~130 relevant lines it carries ~5000 lines of unrelated tooling
(`.atomicorch/**`, `docs/designs/**`), duplicate ADR trees (`docs/adr/` and
`docs/decisions/`), and an ADR numbered **0055**, which `main` has since assigned
to *declarative integrations with fakes*. Its `Uuid.generateV5` also reaches for
`Data.Text.Encoding` and `Data.ByteString` directly rather than the dialect's
`Text.toBytes`. This change re-lands the wanted API on a clean branch through the
spec gate, with the ADR renumbered to 0073 and the criteria above; the old branch
should be closed rather than merged.

## ADR

[ADR-0073](../decisions/0073-deterministic-uuid-v5.md) — why the primitive lands
at both altitudes (pure `Uuid.generateV5` for `getEntityId`, `Decision`
combinator for `decide`) with one implementation; why the combinator is `Return`
rather than a new `Decision` effect constructor; and why the namespace is a
required argument with no default-namespace variant.
