# ADR-0056: Retype `RequestContext.timestamp` as `DateTime`

## Status

Proposed

## Context

### Current State

`RequestContext` lives in [`core/service/Service/Auth.hs`](../../core/service/Service/Auth.hs)
and is the value every command's `decide` function receives. One of its fields
is `timestamp :: UTCTime`, where `UTCTime` is the type from the upstream `time`
package.

```haskell
data RequestContext = RequestContext
  { user      :: Maybe UserClaims
  , files     :: Map FileRef ResolvedFile
  , requestId :: Uuid
  , timestamp :: UTCTime   -- leaks `Data.Time` into every consumer
  }
```

Inside `nhcore`, there is no reader of `ctx.timestamp` at all: `grep` for
`.timestamp` across `core/` and `testbed/` returns nothing. The field exists
purely so out-of-tree consumers can stamp events with a request time.

Out of the framework, every consumer that reads `ctx.timestamp` and wants to
record it on an event has to convert it to `DateTime` (the `nhcore`
[newtype around `UTCTime`](../../core/core/DateTime.hs)). The only way to do
that today is for the consumer to import `Data.Time.Clock`,
`Data.Time.Clock.POSIX`, and `GHC.Real`, and hand-roll a converter. The stated
invariant for NeoHaskell projects is that _only_ `nhcore` types may appear on
the import surface, so the field signature is a leak with no upside.

### Use Cases

- A consumer command stamps an event with the request time:
  `OrderPlaced { placedAt = ctx.timestamp, ... }`. Today, `placedAt` must
  either be typed as `UTCTime` (forcing the `time` import on the event type)
  or be paired with a hand-rolled `utcTimeAsDateTime` helper.
- A test helper builds a `RequestContext` for a decider unit test. Today, the
  helper imports `Data.Time.Clock` and `Data.Time.Calendar` to construct a
  literal `UTCTime`. With `DateTime`, it calls `DateTime.fromEpochSeconds 0`.
- An anonymous or authenticated request is built from inside a transport. The
  transport calls `Clock.getCurrentTime |> Task.fromIO`. With `DateTime`, it
  calls `DateTime.now`.

### Design Goals

1. **No `Data.Time` on the public import surface.** `Service.Auth` is
   re-exported through the framework's command-handler scaffolding. Anything
   on the type signature of `RequestContext` is on Jess's import surface.
   `UTCTime` is not.
2. **Zero behavioural change.** `DateTime` is a `newtype` over `UTCTime`. The
   wire representation, the JSON encoding, and the value of "now" are all
   bit-identical to today.
3. **Net-negative diff for consumers.** Every consumer file that currently
   defines a `utcTimeAsDateTime` helper plus three `Data.Time*` /
   `GHC.Real` imports gets to delete them. The breakage is the feature.

### GitHub Issue

- [#629: feat(RequestContext): type `timestamp` as `DateTime` to remove `time` leak from consumers](https://github.com/neohaskell/NeoHaskell/issues/629)

## Decision

### 1. Retype the field

`RequestContext.timestamp` is changed from `UTCTime` to `DateTime`. The
re-export and module surface are otherwise unchanged.

```haskell
data RequestContext = RequestContext
  { user      :: Maybe UserClaims
  , files     :: Map FileRef ResolvedFile
  , requestId :: Uuid
  , timestamp :: DateTime   -- was: UTCTime
  }
```

| Candidate                                                                          | Verdict    | Rationale                                                                                                                                                                                                                                                 |
| ---------------------------------------------------------------------------------- | ---------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Add `DateTime.fromUtcTime :: UTCTime -> DateTime` and leave the field as `UTCTime` | Rejected   | Ships a converter every consumer must call. The `UTCTime` stays in the public type signature, so the import-surface leak is unfixed. The original framing of #629 took this path; it was abandoned during the review.                                     |
| Re-export `UTCTime` from `nhcore`                                                  | Rejected   | Removes the import-line leak only. The _type_ on the field is still from an external package, so any change in the upstream `time` API still ripples into consumer code. Treats the symptom, not the leak.                                                |
| **Retype the field directly to `DateTime`**                                        | **Chosen** | `DateTime` is already the `nhcore` representation of an instant; there is no information loss. The field's only function is to carry an instant, so there is no reason for the carrier type to be the upstream one. Net-negative diff for every consumer. |

### 2. Constructor sites switch to `DateTime` primitives

Three constructors inside `Service.Auth` and one test helper in
`core/testlib/Test/Service/Command/Decide/Spec.hs` build a `RequestContext`
today. Each switches to a `DateTime`-flavoured equivalent.

| Constructor                                                           | Today                                                     | After                         |
| --------------------------------------------------------------------- | --------------------------------------------------------- | ----------------------------- |
| `emptyContext`                                                        | `GhcClock.UTCTime (GhcCalendar.fromGregorian 1970 1 1) 0` | `DateTime.fromEpochSeconds 0` |
| `anonymousContext`                                                    | `GhcClock.getCurrentTime \|> Task.fromIO`                 | `DateTime.now`                |
| `authenticatedContext`                                                | `GhcClock.getCurrentTime \|> Task.fromIO`                 | `DateTime.now`                |
| `Test.Service.Command.Decide.Spec.authenticatedContext` (test helper) | `GhcClock.UTCTime (GhcCalendar.fromGregorian 1970 1 1) 0` | `DateTime.fromEpochSeconds 0` |

After the change, neither `Service.Auth` nor the affected test helper imports
`Data.Time`, `Data.Time.Calendar`, or `Data.Time.Clock`.

### 3. Public API

```haskell
-- Service.Auth (unchanged signatures, only the field type moves)
emptyContext         :: RequestContext
anonymousContext     :: Task err RequestContext
authenticatedContext :: UserClaims -> Task err RequestContext
withFiles            :: Map FileRef ResolvedFile -> RequestContext -> RequestContext
```

No new function is introduced. No existing function changes its arity, name,
or `Task`/`Result` signature.

## Consequences

### Positive

- Consumer projects collapse to a single-line cleanup per file: read `ctx.timestamp`
  directly.
- The `time`, `GHC.Real`, and `Data.Time.Calendar` imports leave
  `Service.Auth` and the affected test helper.
- `RequestContext` becomes typeable by reading nothing but `nhcore` modules,
  matching the framework's own invariant.

### Negative

- Breaking change for any out-of-tree consumer that today reads
  `ctx.timestamp` as `UTCTime`. The migration is _deletion_ — drop the local
  converter and the foreign imports — but it is still a recompile.

### Risks

- A consumer might pattern-match on the `UTCTime` constructor today (i.e.
  `case ctx.timestamp of UTCTime day diff -> ...`). After the change, that
  match no longer typechecks. `DateTime` does not export its data
  constructor, so the consumer cannot reach the underlying `UTCTime`.

### Mitigations

- The migration shape is documented in the PR body and in the ADR's
  "Constructor sites" table.
- If a consumer genuinely needs to inspect the underlying time value, the
  framework already exposes `DateTime.toEpochSeconds` and
  `DateTime.fromEpochSeconds`, which round-trip through `Int64`. Adding a
  `DateTime.toUtcTime` accessor is _not_ part of this ADR — it would
  re-introduce the leak this ADR removes. If a real use case appears, it is
  its own ADR.

## References

- [#629: feat(RequestContext): type `timestamp` as `DateTime` to remove `time` leak from consumers](https://github.com/neohaskell/NeoHaskell/issues/629)
- [`core/service/Service/Auth.hs`](../../core/service/Service/Auth.hs)
- [`core/core/DateTime.hs`](../../core/core/DateTime.hs)
- [`core/testlib/Test/Service/Command/Decide/Spec.hs`](../../core/testlib/Test/Service/Command/Decide/Spec.hs)
