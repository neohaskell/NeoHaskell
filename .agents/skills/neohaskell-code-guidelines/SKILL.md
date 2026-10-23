---
name: neohaskell-code-guidelines
description: NeoHaskell code style guidelines for writing and reviewing .hs code in this repository.
---

| Use | Never |
|---|---|
| `x \|> foo \|> bar` | `bar $ foo x`, `$` |
| `do let y = expr` | `let..in`, `where` |
| `case x of` | patterns in function head |
| `if cond then a else b` | `case cond of True -> …` |
| Named helpers with one abstraction level; orchestration reads top-to-bottom | nested control-flow pyramids, orchestration mixed with branch mechanics |
| Comments that explain constraints or intent | comments narrating what code does; extract a named function instead |
| Early-exit sentinel guards in `Task` validation | nested if/case pyramids |
| `[fmt\|Hello #{name}!\|]` | `<>` / `++` for strings |
| `Result err val` | `Either` |
| `Task err val` | `IO` |
| `Task.yield v` | `pure`, `return` |
| `forall element result.` | single-letter type params |
| `import Foo (Foo); import Foo qualified` | unqualified imports |
| nhcore Core modules (`Text`, `Array`, `Char`, `File`, `Path`…) | raw `Data.*` / `System.*` / `Ghc*` imports |

## Readable service code

Read [Name for the reader](../principle-name-for-the-reader/SKILL.md) before choosing module, type, or operation names. For service orchestration, exception boundaries, outcome types, serialization, or test resources, read [service design](../neohaskell-best-practices/references/readable-services.md).

Run the [static checks](references/static-checks.md) after changing Haskell or tracked application and test code. These checks own the executable subset of the rules. Review the remaining design decisions against their owning skills.

## Application examples

Write generated starters, testbed modules, documentation examples and fixtures
that represent user applications in NeoHaskell's application dialect. Use the
framework markers for the instances they generate. Use `newtype` when it models
the domain. Never use explicit `stock` or `via` deriving strategies in these
application examples.

Keep each deriving clause only for a concrete framework constraint or an
operation the example actually performs. Remove unused `Eq`, `Ord` and `Show`
instances. GHC supplies `Typeable` automatically, so omit explicit `Typeable`
deriving. Keep `Generic` and serialization instances when an actual codec needs
them and a framework marker does not supply them. Apply
[Minimize Reader Load](../principle-minimize-reader-load/SKILL.md) to declarations,
imports and annotations as well as control flow.

Framework implementation and compiler fixtures that test GHC integration can
use the GHC machinery their internal contracts require. Keep that machinery
inside the owning framework boundary.
