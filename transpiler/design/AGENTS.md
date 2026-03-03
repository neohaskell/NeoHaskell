# AGENTS.md — NeoHaskell Transpiler

## Project Overview

The NeoHaskell transpiler converts `.nh` files to standard `.hs` files, then delegates to GHC for compilation.

```
.nh source → Parser (Megaparsec) → AST → Code Gen (prettyprinter) → .hs + LINE pragmas → GHC
```

## Key Documents

- **compiler-strategy.md** — Architecture decisions, what NOT to do, risks
- **syntax.md** — Language syntax specification
- **event-modeling.md** — Domain modeling approach (TBD)

## Design Principles

### Compiler
1. **No GHC library dependency** — Text in, text out
2. **LINE pragmas for errors** — GHC reports errors in `.nh` files
3. **Hackage compatible** — Standard `.hs` output
4. **Maintainable** — One person can maintain this

### Syntax
1. **Elm as baseline** — Proven simplicity
2. **Rust naming** — `trait`, `impl`
3. **Braces over layout** — Simpler parsing
4. **Domain keywords** — `record`, `entity`, `command`, `query`, `agent`

## Current Status

### Validated ✅
- Transpiler architecture (source-to-source)
- Megaparsec + prettyprinter tooling
- LINE pragmas for error reporting
- LSP proxy approach for IDE
- Must NOT do: GHC plugins for syntax, custom driver, fork GHC

### In Progress 🔄
- Syntax specification (feature by feature)
- PoC implementation planning

### To Investigate ⚠️
- Hidden `.neo/` directory for build artifacts
- Cabal integration with non-root `.cabal` file
- HLS configuration for custom project structure

## Working on Syntax

When defining syntax features:

1. **Start with Elm** — Does Elm have this? If yes, start there
2. **Add NeoHaskell flavor** — Domain keywords, Rust naming
3. **Question complexity** — Does this NEED to exist?
4. **Document in syntax.md** — Open questions → closed decisions

### Feature Status

| Feature | Status |
|---------|--------|
| Records | ✅ Defined |
| Attributes | ✅ Defined |
| Generics | ✅ Defined |
| Type annotations | ✅ Defined |
| Monadic bind (`let!`) | ✅ Defined |
| Blocks (`{ }`) | ✅ Defined |
| Tuples (`#()`) | ✅ Defined |
| Pipe (`\|>`) | ✅ Defined |
| Domain keywords | ✅ Defined |
| Functions | ✅ Defined |
| Pattern matching | ✅ Defined |
| Traits/impl | ❌ Pending |
| Enums | ✅ Defined |
| Brands | ✅ Defined |
| Lambdas | ✅ Defined |

## References

- Elm language (simplicity baseline)
- Rust (naming conventions)
- F# (let!, computation expressions)
- TypeScript (source-to-source success story)

---

_Last updated: 2026-02-25_
