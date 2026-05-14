# ADR-0057: Fold JSON + `deriving` boilerplate into `command`, `event`, `deriveQuery`

## Status

Proposed

## Context

### Current State

A NeoHaskell command, event, or query file today carries three layers of
boilerplate around its actual data declaration:

1. A `deriving (Generic, Typeable, Show)` clause on the data type
   (events also derive `Eq`, `Ord`).
2. Two empty instance declarations:
   `instance Json.FromJSON X` and `instance Json.ToJSON X`.
3. The concept-marker TH call: `command ''X`, `deriveQuery ''X [''Entity]`.

A real consumer command,
[`Testbed.Cart.Commands.CreateCart`](../../testbed/src/Testbed/Cart/Commands/CreateCart.hs),
looks like this:

```haskell
data CreateCart = CreateCart
  deriving (Generic, Typeable, Show)


instance Json.FromJSON CreateCart


instance Json.ToJSON CreateCart


-- ... getEntityId, decide, type instances ...


command ''CreateCart
```

The first eight lines are ceremony. They are the same shape on every
command, every event, every query, with no per-type variation. They are
the kind of code that Jess can forget — missing `Json.ToJSON` produces a
type-class-not-in-scope error at the dispatch site, several modules away
from the omission.

The concept markers already know the type's `TH.Name` when they run.
`command ''X` could ask, at splice time, "does `Json.ToJSON X` exist in
scope?" via [`reifyInstances`][reifyInstances] and emit one if it does
not. The same holds for `Show`, `Generic`, and (for queries) `ToSchema`.

[reifyInstances]: https://hackage.haskell.org/package/template-haskell-2.20.0.0/docs/Language-Haskell-TH.html#v:reifyInstances

### Use Cases

- A consumer authors a new command. With the change, the file is
  `data + functions + marker`; without it, the file is `data + deriving
  + two empty Json instances + functions + marker`.
- A consumer authors a new event. Today the `event` marker does not
  exist — every event file repeats the same `deriving + FromJSON + ToJSON`
  block. With the change, the file becomes `data + Event-class wiring +
  event ''X`.
- A consumer needs a tagged-sum JSON encoding for a command. With the
  change, the consumer writes `instance Json.ToJSON X where toJSON = ...`
  before the marker call; the marker detects the user-provided instance
  via `reifyInstances` and emits nothing for `ToJSON`.

### Design Goals

1. **Net-negative diff for new feature work.** A command, event, or query
   file written after this lands carries five fewer lines than today.
2. **Idempotency over migration.** Existing files in `testbed/` and in
   out-of-tree consumers (e.g. `evoca-payment-gateway`) keep compiling
   without code change. The change is opt-in by deletion.
3. **Escape hatches preserved.** A consumer who wants a non-default
   `Json.ToJSON` (tagged sum, `omitNothingFields`) writes the instance
   explicitly; the marker emits no duplicate.
4. **Failure mode floor.** Missing `Json.FromJSON` / `Json.ToJSON` on a
   command becomes impossible, not a runtime dispatch error.

### GitHub Issue

- [#630: feat(TH): consolidate JSON + deriving boilerplate into command/query/event markers](https://github.com/neohaskell/NeoHaskell/issues/630)

## Decision

### 1. Make every marker emit JSON + `deriving` boilerplate, idempotently

`command`, `deriveQuery`, and a new `event` TH function each become
responsible for emitting:

| Class | Emitted by `command` | Emitted by `event` | Emitted by `deriveQuery` |
| --- | --- | --- | --- |
| `Show`         | yes | yes | yes |
| `Generic`      | yes | yes | yes |
| `Json.FromJSON` | yes | yes | yes |
| `Json.ToJSON`  | yes | yes | yes |
| `ToSchema`     | already emitted today | no | yes (new) |
| `Typeable`     | no — auto-derived since GHC 7.10, no clause needed | no | no |

Each class is emitted as `deriving stock instance Class X` (for `Show`,
`Generic`) or `instance Json.FromJSON X` / `instance Json.ToJSON X` /
`instance ToSchema X` with empty body (uses the `Generic`-defaulted
implementation).

Before emitting any of these, the marker calls
`TH.reifyInstances <className> [TH.ConT typeName]`. If the result is
non-empty — i.e. an instance for that class is already in scope at the
splice point — the marker emits no declaration for that class. The
escape hatch is therefore "declare the instance before the marker call";
no flag, no opt-out parameter.

### 2. Add `event :: TH.Name -> THLib.DecsQ`

A new TH function lives at
[`core/service/Service/Event/TH.hs`](../../core/service/Service/Event/TH.hs)
and is re-exported from `Service.Event`. The scope of this ADR is
deliberately narrow: `event ''X` emits the same four classes as
`command` (`Show`, `Generic`, `Json.FromJSON`, `Json.ToJSON`), and does
not touch the `Event` typeclass instance, the `EventOf` / `EntityOf`
type-family instances, or the `getEventEntityIdImpl` wiring. Folding
those in is a candidate for a follow-up ADR; this one stops at the
JSON + deriving ceremony.

### 3. Public API

```haskell
-- Service.CommandExecutor.TH (existing, behaviour widened)
command :: TH.Name -> THLib.DecsQ

-- Service.Query.TH (existing, behaviour widened)
deriveQuery :: TH.Name -> [TH.Name] -> THLib.DecsQ

-- Service.Event.TH (new)
event :: TH.Name -> THLib.DecsQ
```

No existing call site changes shape. `command ''X` keeps the same arity
and the same caller-side ergonomics.

### 4. Consumer-side shape after the change

```haskell
data CreateCart = CreateCart


getEntityId :: CreateCart -> Maybe Uuid
getEntityId _ = Nothing


decide :: CreateCart -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide _ entity ctx = do
  ...


type instance EntityOf CreateCart = CartEntity


type instance TransportsOf CreateCart = '[WebTransport]


command ''CreateCart
```

The `deriving (Generic, Typeable, Show)` clause, the empty
`instance Json.FromJSON CreateCart`, and the empty
`instance Json.ToJSON CreateCart` are all gone.

### 5. Considered options

| Candidate                                                                       | Verdict      | Rationale                                                                                                                                                                                                                                       |
| ------------------------------------------------------------------------------- | ------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Idempotent TH (chosen)**                                                      | **Chosen**   | `reifyInstances` at splice time detects user-supplied instances and avoids emitting a duplicate. No migration required, escape hatch preserved, failure mode floor raised.                                                                       |
| Strip inline `deriving` via `reify` and re-emit the data declaration            | Rejected     | TH cannot rewrite a data declaration that has already been spliced. The marker runs after the data declaration in the file order; the inline derives are immutable by then.                                                                      |
| Document the new style, ship a codemod, require migration of every consumer file | Rejected     | Forces every existing consumer (including out-of-tree) to migrate atomically. Breaks `main` on the rollout PR. The benefit (five lines per file) does not pay for the breakage.                                                                  |
| A generic `deriveBoilerplate ''X` helper unattached to `command`/`event`/`deriveQuery` | Rejected | Forgettable. The argument in #630 — "folding it into the existing concept markers makes it impossible to forget" — applies directly.                                                                                                            |
| Emit `deriving stock instance Typeable X` as well                               | Rejected     | Typeable has been auto-derived for every type since GHC 7.10. A standalone clause is at best redundant and at worst conflicts with the auto-derived instance.                                                                                    |

## Consequences

### Positive

- New command, event, or query files lose five lines of ceremony each.
- Missing `Json.FromJSON` / `Json.ToJSON` on a command, event, or query
  is no longer a class of error a consumer can hit.
- Tagged-sum or otherwise-custom JSON encodings remain expressible:
  declare the instance above the marker call.
- The migration is reversible — at any point a consumer can re-add the
  `deriving` clause and the empty instances, and the marker continues
  to behave correctly.

### Negative

- Each marker call performs five extra `reifyInstances` lookups at
  splice time. The cost is negligible — `reifyInstances` is a hash-map
  read inside GHC's instance environment — but it is non-zero, and it
  is paid by every command, event, and query in the codebase.
- Two valid shapes coexist for a transitional period: pre-ADR (explicit
  derives + empty instances) and post-ADR (no derives, marker does it
  all). Documentation and the README ship the post-ADR shape only; a
  follow-up cleanup PR strips the redundancies from `testbed/`.

### Risks

- A consumer might write a non-empty `Json.ToJSON` instance **after**
  the marker call in the file. `reifyInstances` only sees instances
  declared earlier in the splice order, so the marker would emit an
  empty `ToJSON` instance and the consumer's later declaration would
  fail with a duplicate-instance error.
- `StandaloneDeriving` must be enabled in every consumer module. It is
  already in `default-extensions` in `nhcore.cabal` and `testbed.cabal`,
  but out-of-tree consumers may not have it.
- A consumer who wraps a `command` data type in `newtype` and reuses
  the inner type's `Generic` / `Json` instances via `deriving newtype`
  may hit an "instance already in scope" warning. The TH detects this
  via `reifyInstances` and skips emission, so the warning case is
  whatever `deriving newtype` already does today — unchanged by this
  ADR.

### Mitigations

- Document the "marker last" convention in the consumer-facing
  `AGENTS.md` and in the new
  [`core/service/Service/Event/TH.hs`](../../core/service/Service/Event/TH.hs)
  module header. The convention is already followed in every existing
  consumer file in `testbed/`; this ADR makes it load-bearing.
- Out-of-tree consumers needing `StandaloneDeriving` will get a clear
  GHC error pointing at the missing extension; the migration is a
  one-line `default-extensions` add.
- The TH's no-op-on-existing behaviour means out-of-tree consumers
  built against the old API keep compiling against the new API with no
  code change. The cleanup is opt-in.

## References

- [#630: feat(TH): consolidate JSON + deriving boilerplate into command/query/event markers](https://github.com/neohaskell/NeoHaskell/issues/630)
- [`core/service/Service/CommandExecutor/TH.hs`](../../core/service/Service/CommandExecutor/TH.hs) — host of `command`
- [`core/service/Service/Query/TH.hs`](../../core/service/Service/Query/TH.hs) — host of `deriveQuery`
- [`core/service/Service/Event/TH.hs`](../../core/service/Service/Event/TH.hs) — new module for `event` (to be created)
- [`testbed/src/Testbed/Cart/Commands/CreateCart.hs`](../../testbed/src/Testbed/Cart/Commands/CreateCart.hs) — representative consumer
- [`testbed/src/Testbed/Cart/Core.hs`](../../testbed/src/Testbed/Cart/Core.hs) — representative entity + event module
