# ADR-0073: Deterministic UUID v5 as a Core primitive and a Decision combinator

> Implements #596. Builds on [ADR-0003](0003-command-abstraction-and-flow.md)
> (the command abstraction, `getEntityId` + `decide`, and the `Decision` monad).

## Status

Accepted

## Context

The `Decision` monad — the pure DSL a command's `decide` function runs in —
offers exactly one way to obtain a `Uuid`: `Decider.generateUuid`, which the
executor interprets by drawing a fresh random (v4) UUID from
`DecisionContext.genUuid`. Randomness is the right default for "mint a new
entity", and it is the only thing on offer.

Event-sourced services routinely need the opposite: an identifier that is a
**function of a natural key**. Issue #596 arrived with the general case (derive
an entity id from an OAuth provider's string user id) and was later bumped with
a concrete downstream consumer: a `Project` aggregate whose stream id is derived
from the normalized repo path, so that registering the same repository twice
lands on the *same* stream and the create command can see the entity already
exists and reject it. That is the standard event-sourcing answer to
cross-aggregate uniqueness — there is no other way to make "the same natural key
is the same stream" true without a second store to consult.

Today neither hook in the command flow can express it:

- `getEntityId :: command -> Maybe Uuid` is pure and sees only the command's
  fields, but has no UUID-derivation function available to it.
- `decide` runs in `Decision`, which offers only `generateUuid`.

So the natural key cannot be turned into a stream id anywhere in the framework,
and consumers reach for `Data.UUID.V5` directly — which is both outside the
dialect vocabulary and unavailable to `getEntityId` in any principled form.

RFC 4122 already defines the primitive: a **version 5** UUID is
`SHA-1(namespace ‖ name)` with the version and variant bits overwritten. It is
pure, total, stable across processes and machines, and the `uuid` package
(already a direct dependency of `nhcore`, via `Data.UUID` and `Data.UUID.V4`)
ships it as `Data.UUID.V5.generateNamed`. Nothing needs to be invented; the
question is only where the capability belongs in the NeoHaskell surface.

## Decision

Add the primitive **twice**, at the two altitudes that need it, with the pure
one as the single implementation:

1. **`Uuid.generateV5 :: Uuid -> Text -> Uuid`** — a pure Core primitive, in the
   same module as `generate`, `nil`, `fromText`. Namespace first, name second,
   matching RFC 4122's argument order and the shape of `Data.UUID.V5.generateNamed`.
   Being pure, it is callable from `getEntityId`, from an entity `update`, from a
   query projection — anywhere at all.
2. **`Decider.generateDeterministicUuid :: Uuid -> Text -> Decision Uuid`** — the
   `Decision`-level combinator the issue asks for, defined as
   `Uuid.generateV5 namespace name |> Return`. It adds **no new `Decision`
   constructor** and therefore no new case in `runDecision`: the deterministic
   UUID is data the pure DSL already knows how to carry, not a new effect the
   interpreter must serve.

Supporting this needs one gap filled in the prelude: **`Bytes.unpack :: Bytes -> [Word8]`**,
the missing inverse of the existing `Bytes.pack :: [Word8] -> Bytes`, so that
`generateV5` reaches `generateNamed`'s `[Word8]` input through dialect
vocabulary (`Text.toBytes |> Bytes.unpack`) instead of a raw `Data.Text.Encoding`
/ `Data.ByteString` pair inside `Uuid`.

## Rationale

**Why both, rather than only the `Decision` one.** The issue's own "Alternatives
Considered" section notes that a pure `Uuid`-module function "would also solve
this, and wouldn't need to be in the Decision monad at all". That is correct, and
it is the stronger half — but it is not a substitute for the combinator, because
the two serve different hooks. `getEntityId` is where a natural key becomes a
*stream id*, and it is pure, so it can only ever use the pure function. `decide`
is where a natural key becomes some *other* derived id inside an emitted event,
and there the combinator reads in the same register as its neighbours
(`someId <- Decider.generateDeterministicUuid ns key` next to
`otherId <- Decider.generateUuid`) without forcing a `pure`/`Task.yield` lift at
the call site — a lift the dialect bans outright at the edit hook. Shipping only
the pure function would leave every `decide` that wants a derived id writing
dialect-illegal glue; shipping only the combinator would leave the duplicate-
detection use case — the one the downstream consumer actually needs —
inexpressible. Hence both, with one implementation.

**Why the combinator is `Return`, not a new constructor.** `GenUuid` exists as a
constructor because random generation is an *effect*: the interpreter must supply
entropy, and tests must be able to control it. v5 derivation is a total function
of its inputs; there is nothing for an interpreter to decide. Encoding it as
`Return` keeps `Decision`'s effect algebra honestly minimal and means
`runDecision` needs no change, so the feature cannot regress command execution.

**Why the namespace is an explicit argument, with no default-namespace variant.**
The issue floats a simpler `uuidFromText :: Text -> Decision Uuid` with a
built-in namespace. We decline it. A framework-wide default namespace is a global
collision domain: two unrelated aggregates that both derive from the string
`"default"`, or from a bare user id, would silently land on the same UUID — and
because the whole point of the primitive is that the same input *is* the same
stream, that collision is a cross-aggregate data-integrity bug, not a nuisance.
Requiring the caller to name a namespace makes the collision domain a decision
someone makes on purpose. Callers who want a namespace get one from
`Uuid.fromText`; RFC 4122's predefined namespaces (DNS, URL, OID, X500) are
deliberately not re-exported yet — the rule of three applies, and no consumer has
asked for them.

**Why v5 (SHA-1) rather than v3 (MD5) or a v8 built on SHA-256.** v5 is what RFC
4122 prescribes for name-based UUIDs with a modern hash, it is what the already-
present `uuid` package implements, and it is what interoperating systems produce
for the same namespace and name. The SHA-1 truncation here is used as a
*derivation* function, not as a security primitive; the security-relevant
property — that the output must not be treated as a secret — is a usage
constraint, documented below and in the haddock, not an argument for a different
hash.

## Consequences

**Positive.** Natural-key entity identity becomes expressible in-framework, in
both hooks, using the dialect's own vocabulary; the "idempotent import" and
"stable reference" patterns stop requiring a direct `Data.UUID.V5` dependency in
user apps. `Bytes` gains the inverse of `pack`, which is a plain gap-fill.
`Decision`'s interpreter is untouched.

**Negative / constraints.** A v5 UUID is **not** a secret: anyone who knows the
namespace and the name can reproduce it exactly, and given a UUID plus a guessable
name space, names can be brute-forced. It must never be used as a capability
token, a session id, a password-reset link component, or any other unguessable-by-
design value. The haddock on both functions carries this warning. Where an id
must be unguessable, `Uuid.generate` (v4, random) remains the correct choice, and
that division of labour is now the visible one: *random for secrets and fresh
identity, deterministic for derived identity*.

**Non-goals.** No predefined RFC namespaces, no v3, no default-namespace
convenience wrapper, and no change to how `getEntityId`/`decide` are invoked.

## Alternatives considered

- **Pure `Uuid.generateV5` only.** Rejected: leaves `decide` call sites writing a
  dialect-illegal `pure` lift, which is exactly the ergonomic complaint in #596.
- **`Decider.generateDeterministicUuid` only.** Rejected: `getEntityId` is pure
  and cannot reach into `Decision`, so the duplicate-registration use case — the
  motivating one — would stay unexpressible.
- **A new `GenDeterministicUuid` constructor in `Decision`.** Rejected: it would
  make a total function look like an effect, add a `runDecision` case, and buy
  nothing — there is no interpreter freedom to exercise.
- **`uuidFromText` with a framework default namespace.** Rejected: a global
  collision domain, per the rationale above.
- **Pushing derivation onto the caller (pre-compute the UUID before submitting
  the command).** Rejected: it moves aggregate-identity policy out of the
  aggregate and into every transport client, and clients would need their own v5
  implementation to do it.
