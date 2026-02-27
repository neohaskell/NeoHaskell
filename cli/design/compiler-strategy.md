# NeoHaskell Compiler Strategy

**Status:** Validated  
**Last updated:** 2026-02-25

---

## Core Strategy: Source-to-Source Transpiler

NeoHaskell compiles `.nh` files to standard `.hs` files, then delegates to GHC.

```
.nh source file
    ↓ Megaparsec parser
NeoHaskell AST (with source locations)
    ↓ Code generator (prettyprinter library)
.hs file + {-# LINE #-} pragmas
    ↓ GHC (via Cabal)
Compiled binary (errors point to .nh via LINE pragmas)
```

### Components

| Component | Tool | Why |
|-----------|------|-----|
| Parser | **Megaparsec** | Excellent error messages, `makeExprParser` for operators |
| Code Gen | **prettyprinter** | No GHC dependency, inspectable output |
| Build | **Cabal Hooks** (3.14+) | Native integration, incremental builds |
| IDE | **LSP proxy over HLS** | Leverage existing tooling |

### Key Decisions

1. **No GHC library dependency** — Essential for maintenance
2. **LINE pragmas for errors** — GHC reports errors in `.nh` files
3. **Standard `.hs` output** — Readable, debuggable, Hackage compatible
4. **Mixed `.hs`/`.nh` projects** — Natural coexistence

---

_More sections to be added as we validate the research._

---

## Must NOT Do

These approaches have been evaluated and rejected:

### ❌ GHC Plugins for Custom Syntax

- `parsedResultAction` runs AFTER GHC parses → `.nh` files fail before plugin sees them
- `--frontend` plugins don't integrate with `ghc --make` → breaks Cabal/HLS
- **Plugins ARE useful as complement** (post-parse error enhancement) but cannot replace the parser

### ❌ Custom GHC Driver (Clash model)

- Requires per-GHC-version directories (`src-bin-9.0`, `src-bin-9.4`, etc.)
- Multi-month upgrade effort per GHC release
- QBayLogic (funded team, multiple engineers) says it's "more involved than average"
- Liquid Haskell migrated FROM standalone TO plugin because maintaining parallel pipeline was unsustainable
- Only supports one GHC version at a time

### ❌ Fork GHC

- Eta tried this → project is dead
- Unsustainable maintenance burden

---

## Why Transpiler Wins

**Transpiler advantages:**
- **GHC version coupling:** None (vs High/Very High in alternatives)
- **Maintenance:** Minimal (vs Medium/Extreme per GHC release)
- **Mixed .hs/.nh:** Natural coexistence (vs Impossible/Complex)
- **Debugging:** Readable .hs output (vs Opaque internals)

**Equal across all approaches:**
- Hackage compatibility: Full
- IDE support: Requires work, but transpiler + LSP proxy is most viable path

---

## Parser & Code Generation

### Parser: Megaparsec

- Excellent error messages with source positions
- `makeExprParser` for operator precedence (pipes, etc.)
- `withRecovery` for reporting multiple errors
- Braces instead of layout → simpler parsing than standard Haskell

### Code Generator: prettyprinter

- No `ghc` library dependency
- No CPP guards per GHC version
- Inspectable output for debugging
- Same approach as Agda's GHC backend

### Future: nhcore Integration

Both Megaparsec and prettyprinter will be integrated into NeoHaskell's stdlib (nhcore) as first-class citizens. This will happen incrementally as features are needed.

---

## Code Generation Best Practices

- Always explicit `{-# LANGUAGE ... #-}` pragmas
- Fully qualified names or explicit imports
- Explicit braces and semicolons (avoid layout bugs)
- Comment header indicating auto-generated
- **Keep useful warnings** (e.g., incomplete case branches) — do NOT use `-Wno-all`

---

## Build Integration: Cabal Hooks

**Approach:** Cabal 3.14+ `build-type: Hooks` (replaces deprecated `build-type: Custom`)

- `SetupHooks.hs` declares pre-build rules
- Cabal only re-transpiles changed `.nh` files
- Mixed `.hs`/`.nh` natural
- User can use `cabal build` directly — not a walled garden
- Uses haskell.nix under the hood (which uses Cabal)

### ⚠️ TO INVESTIGATE: Hidden .neo Directory

**Goal:** All Haskell tooling artifacts should live in `.neo/`, hidden from user.

Questions to answer:
- How to run `cabal build` when `.cabal` file is in `.neo/` not root?
- Does `--project-file` or `cabal.project` support this?
- How does haskell.nix handle non-root cabal files?
- How to configure HLS/LSP proxy to find the project in `.neo/`?

User-facing structure should be clean:
```
my-project/
├── neo.toml        # User config
├── src/
│   └── *.nh        # User code
└── .neo/           # Hidden: generated .hs, .cabal, build artifacts
```

---

## Error Reporting: LINE Pragmas

```haskell
{-# LINE 15 "src/Cart.nh" #-}
```

- GHC reports errors pointing to `.nh` source files
- GHC doesn't verify file exists (just stores as FastString)
- Works for ALL errors: parse, type, warnings
- Standard approach used by: alex, happy, hsc2hs, c2hs, Agda
- Works in GHCi, ghcid, cabal, stack

---

## Error Enhancement Roadmap

### Tiers

1. **Day 1:** LINE pragmas (errors point to .nh)
2. **Week 2-3:** Wrapper post-processes stderr, shows .nh context
3. **Month 2+:** GHC plugin with Diagnostic API (9.4+)
4. **Long-term:** Custom TypeError in stdlib

### Error Design Principles

- **Structured ADT with fields** — Not strings, enables i18n and tooling
- **Lovable and understandable** — Elm and Rust are the gold standard
- **i18n ready** — Error type holds data, rendering is separate

Example error ADT:
```haskell
data NeoError
  = WhereBlockNotSupported
      { location :: SrcSpan
      , suggestion :: Text  -- "Use 'do' with 'let' instead"
      }
  | IncompleteCaseMatch
      { location :: SrcSpan
      , missingPatterns :: [Pattern]
      }
  | ...
```

Rendering happens at the edge, supports multiple languages.

---

## IDE Support: LSP Proxy over HLS

```
Editor ←→ neohaskell-lsp (.nh) ←→ HLS (.hs)
               ↕                      ↕
          transpiler            GHC compilation
          source maps
```

### Why NOT Other Options

- **HLS plugins:** Cannot intercept/transform before GHC
- **FUSE filesystem:** Linux-only, problematic on macOS
- **Fork ghcide:** Maintenance hell

### Feature Difficulty

- **Easy:** Diagnostics, completions, hover
- **Medium:** Go-to-definition, find-references
- **Hard:** Rename, code actions (require reverse transpilation)
- **Native:** Formatting → `neo fmt`

---

## Lessons from Prior Art

### Pattern 1: Minimize Host-Compiler Coupling

- **Eta** (fork GHC) → 💀 Dead
- **Clash** (GHC as library) → 😰 Months of upgrade per GHC
- **Agda** (text → external ghc) → ✅ Minimal coupling
- **TypeScript** (output standard JS) → ✅ Winner

### Pattern 2: Maintain Ecosystem Compatibility

- **Frege** (no Hackage) → 😢 "Where are the libraries?"
- **Kotlin** (same bytecode as Java) → ✅ Seamless interop

### Pattern 3: Ship Source Maps + IDE from Day 1

- **CoffeeScript** launched without source maps → debugging pain → hurt adoption
- **TypeScript** launched with tsserver → superior IDE from start

### Anti-pattern: "Just Syntax Sugar"

CoffeeScript died when ES6 added the same features natively.

**NeoHaskell must provide REAL value:**
- Better error messages
- Semantic keywords (record/entity/command/query)
- Boilerplate generation
- Curated stdlib experience

---

## PoC Plan

### Week 1: Core Transpiler + Cabal

- Day 1-2: AST types with Located nodes + Megaparsec parser
- Day 3-4: Code generator with prettyprinter + LINE pragmas
- Day 5-7: `nh-transpile` CLI + Cabal Hooks integration

### Week 2: Error Processing + IDE

- Day 8-10: `neo build/run` wrapper + error post-processing
- Day 11-14: VS Code extension + file watcher for HLS

### Dependencies

- `megaparsec` (parser)
- `prettyprinter` (code gen)
- `fsnotify` (file watching)
- `optparse-applicative` (CLI)
- `toml-reader` (project config)
- **NO `ghc` library dependency** — essential

### ⚠️ PENDING: Syntax Definition

The exact syntax for the smallest compilable `.nh` file is not yet defined. This must be completed before PoC implementation begins.

See: syntax design documents (TBD)

---

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Cabal Hooks too new (3.14) | Fallback to `build-type: Custom` with `hookedPreProcessors` |
| HLS doesn't fully respect LINE pragmas | LSP proxy remaps all responses |
| Generated .hs becomes unreadable | Keep direct mapping, design syntax with expansion in mind |
| GHC changes break generated code | Explicit pragmas, explicit imports, don't rely on defaults |
| Solo maintainer burnout | Minimal architecture: text in → text out, no GHC API |

### Escape Hatch

If preprocessor isn't enough for future features:
- Gradually migrate to **transpiler + complementary GHC plugin**
- Plugin does post-parse transformations, custom type checking, enhanced errors
- Core transpiler architecture unchanged
