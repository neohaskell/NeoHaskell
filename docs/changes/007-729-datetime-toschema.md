# Change 007: Give `DateTime` a `ToSchema` instance

`DateTime` derives `ToJSON`/`FromJSON` but has no `ToSchema` instance, so the
moment a read model or API record carries a timestamp field — `DateTime` or
`Maybe DateTime` — generic `Schema` derivation stops compiling. Since
`DateTime` is the natural type for timestamps folded onto entities, a timestamp
cannot be surfaced on an HTTP read model today without a workaround (an orphan
instance, or storing epoch `Int` / ISO `Text` instead). This change ships the
instance out of the box, as the other `Core` scalars (`Uuid`, `Text`, `Int`, …)
already do.

```yaml spec
issue: issue#729
kind: bug
touches: [schema, core-primitives]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

One additive instance, defined in `core/schema/Schema.hs` alongside the `Uuid`
instance it mirrors. No signature is removed and no existing instance changes,
so the delta is a single `+` line.

```diff signatures
+ Schema: instance Schema.ToSchema DateTime.DateTime
```

`DateTime` maps to `SText` because that is what `DateTime` already *is* on the
wire: its generic `ToJSON` encodes the wrapped `UTCTime` as an ISO-8601 JSON
string. `SText` is therefore not a convenience choice — it is the schema that
matches the encoder, and C4 below is the criterion that keeps the two honest.

**Deliberately out of scope**: a dedicated `Schema` constructor carrying
OpenAPI's `format: date-time`. The `Schema` ADT has no date-time variant, and
adding one means a new constructor plus its JSON-Schema and OpenAPI lowerings —
a contract change to `Schema` itself, not the blocking gap the issue reports.
C6 pins the current behaviour (`type: string`, no `format`) so that a later
follow-up has to change a test on purpose rather than drift into it.

### Placement decision (so the build step does not re-decide it)

The repo has two precedents: `Uuid`'s instance lives in `core/schema/Schema.hs`,
while `Decimal`'s lives in its own module (`core/decimal/Decimal.hs:219`). This
change follows the `Uuid` precedent — `DateTime` is a prelude-surface scalar
like `Uuid`, and grouping the scalar instances keeps the "Common NeoHaskell
Types" block in `Schema.hs` the one place to look.

No import cycle is created: `Schema.hs` already imports `Uuid`, which itself
imports `Json` and `Task`; `DateTime` imports exactly that same set and never
imports `Schema`. The build step adds `import DateTime (DateTime)` to
`Schema.hs` and nothing else.

## Criteria

C1 is the issue's reproduction and is committed **red in this draft PR**. Note
its shape: a missing instance is a *type* error, so C1 is red as a compile
failure of the `nhcore-test-core` suite (`No instance for (ToSchema DateTime)`),
not as a runtime assertion failure. A draft PR that does not build is the
correct state for this run — heavy CI is skipped on drafts — and the build step
turns it green by adding the instance, changing no assertion.

All criteria are `unit`: the contract is the availability and value of a
typeclass instance plus its two pure lowerings. Nothing crosses the filesystem,
Postgres or HTTP, so nothing here can honestly claim `integration` or
`acceptance`. The end-to-end symptom the issue reports — an HTTP read model
with a timestamp failing to build — is a compile-time consequence of C1 and is
proven by C1, not by a server round-trip.

No property-based qualifier: the contract is a constant (`toSchema @DateTime`
is one fixed value), not an algebraic law, so there is no space to quantify
over.

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | The issue's repro: a record with a `DateTime` field and a `Maybe DateTime` field derives `ToSchema` and compiles; its schema is `SObject` with the `DateTime` field required at `SText` and the `Maybe DateTime` field not required at `SOptional SText` | `Schema.DateTimeSpec` "derives a schema for a record with DateTime fields" | unit |
| C2 | `toSchema @DateTime` is `SText` | `Schema.DateTimeSpec` "generates SText for DateTime" | unit |
| C3 | `toSchema @(Maybe DateTime)` is `SOptional SText` — the container instance composes, which is the second half of the issue's report | `Schema.DateTimeSpec` "generates SOptional SText for Maybe DateTime" | unit |
| C4 | Wire-form honesty: `Json.encodeText` of a `DateTime` is a quoted ISO-8601 string, so the declared `SText` describes what the encoder actually emits | `Schema.DateTimeSpec` "encodes DateTime as an ISO-8601 JSON string, matching SText" | unit |
| C5 | JSON-Schema lowering: `toJsonSchema (toSchema @DateTime)` is `{"type": "string"}` | `Schema.DateTimeSpec` "lowers DateTime to JSON Schema type string" | unit |
| C6 | OpenAPI lowering: `toOpenApiSchema (toSchema @DateTime)` has `type: string` and **no** `format` — pinning the deliberate scope boundary above | `Schema.DateTimeSpec` "lowers DateTime to OpenApiString with no format" | unit |

### Edge cases and failure modes

- **`Maybe DateTime`** — the reported second failure mode; C3.
- **`Array DateTime`** — falls out of the existing `ToSchema element => ToSchema (Array element)` instance with no extra code. Not given its own criterion: it exercises the container instance, which is already covered, not the new one.
- **Nested / record placement** — C1 asserts the whole `SObject`, including the required-vs-optional flag, rather than just the field's schema.
- **Schema/encoder divergence** — the real failure mode of a hand-written scalar instance is claiming a wire shape the encoder does not produce. C4 is the criterion that catches it. Without C4, C2 would be close to tautological (`SText` asserted equals `SText` written), which is exactly what the verify step's anti-tautology check exists to reject.
- **Concurrency** — not applicable, and stated rather than omitted: `toSchema` is a pure nullary class method over an immutable ADT. There is no shared state, no mutation and no parallelism in the contract, so there is nothing for a concurrency stress test to stress.
- **Security** — not applicable, and stated rather than omitted: this instance is output-side only. It participates in *publishing* a schema; no external input crosses a trust boundary through it, and it performs no parsing, decoding or validation. `FromJSON DateTime` (the input side) is untouched.
- **Orphan-instance collision** — the one way this change can break an existing build; see User impact.

## User impact

Not breaking under the mechanical rule: the contract delta removes no
signature, and every type that compiles today still compiles.

One honest caveat, because the issue names the workaround explicitly. Anyone
who worked around the gap with a local orphan `instance ToSchema DateTime` will
now hit a duplicate-instance error. **Migration**: delete the orphan; the
upstream instance is `SText`, identical to the one every known workaround
writes. No behaviour changes for that user — only the declaration site moves.
There is no such orphan anywhere in this repo (`core`, `testbed`,
`integrations`, `neo/starter` all checked), so the monorepo is unaffected.

Testbed effect: none required. `testbed`'s read models
(`Testbed.Cart.Queries.CartSummary`, `Testbed.Stock.Queries.StockLevel`) carry
no timestamp field today, and this change adds no obligation to give them one.

Positive impact: a query read model registered via `deriveQuery` / `withQuery`
can carry a `DateTime` field directly, which is the capability the issue asks
for.

## ADR

Not required — no trigger (breaking / new-dependency / new-capability /
new-extension-point all false). Nor is one warranted on judgement: the only
decision of record is "which existing `Schema` constructor does an existing
wire format map to", the answer is forced by the existing encoder, and both the
reasoning and its scope boundary are written into the Contract delta section
above where a future reader will look first. Adding a date-time `Schema`
constructor later would be an ADR-worthy decision; declining to add one now,
with the current behaviour pinned by C6, is not.
