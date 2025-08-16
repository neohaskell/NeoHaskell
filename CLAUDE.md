# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

NeoHaskell is a dialect of Haskell focused on newcomer-friendliness and productivity. This is a monorepo containing:

- **cli/**: Command line tool (`nhcli`) with executable `neo`
- **core/**: Core library (`nhcore`) with NeoHaskell's standard library modules
- **website/**: Documentation website built with Astro/Starlight

## Development Setup

Required tools:

- Install [Nix](https://nixos.org/download/)
- Run `nix-shell` to enter development environment
- Run `cabal update && cabal build all` to build everything

The project uses VS Code with Haskell Language Server for development.

## Build and Test Commands

### Core Commands

- `cabal build all` - Build all packages
- `cabal test` - Run all tests
- `cabal test nhcore-test` - Run core library tests only
- `cabal test nhcli-test` - Run CLI tests only
- `hlint .` - Run linter

### Documentation Tests

- `./scripts/run-doctest` - Run doctest (installs doctest if needed)

### Formatting

- Code auto-formats on save in VS Code using fourmolu
- Configuration in `fourmolu.yaml` (2-space indent, 120 char limit)

## Architecture

### Core Library Structure

The core library (`nhcore`) provides NeoHaskell's standard library organized into:

- **Core modules**: Basic types (Text, Int, Array, Maybe, Result, etc.)
- **System modules**: File, Directory, Path, Environment, Time, Subprocess
- **Concurrency**: AsyncTask, Channel, Lock, ConcurrentVar
- **Service layer**: Event sourcing with EventStore (InMemory implementation)
- **HTTP**: Basic HTTP client functionality
- **Traits**: Type classes (Mappable, Appendable, Combinable, etc.)
- **Testing**: Comprehensive EventStore test suites with property-based testing

### CLI Tool

The CLI (`nhcli`) provides project management commands through the `neo` executable.

## NeoHaskell Code Style

NeoHaskell follows specific coding conventions (from `.cursor/rules/neohaskell-style.mdc`):

### Import Style

Always import types explicitly and modules qualified:

```haskell
import Service.Event (Event(..))
import Service.Event qualified as Event
```

Use `Ghc` prefix for base library imports:

```haskell
import Data.List qualified as GhcList
```

### Code Structure

- No point-free style - always explicit function application
- Prefer pipe operator `|>` over nested `$`
- Use `do` blocks for intermediate bindings (even non-monadic), no `let..in` or `where`
- Pattern matching only in `case..of` expressions
- Use `forall` for type parameters with descriptive names (not single letters)
- Use `Result` instead of `Either`
- Use `[fmt|Hello {name}!|]` for string concatenation
- Use `<TypeName>.yield` instead of `pure` or `return`

### Forbidden Patterns

- Never import Haskell ecosystem modules unless explicitly requested
- Use nhcore modules instead of base/ecosystem equivalents
- No `let..in` or `where` clauses
- No function definition pattern matching
- No short type parameter names (use `value` not `a`)

## Common Tasks

- Adding new core modules: Add to `core/nhcore.cabal` exposed-modules
- Adding CLI commands: Extend modules in `cli/src/Neo/`
- Running specific tests: Use `cabal test <package-name>-test`
- Code formatting: Automatic in VS Code, or `fourmolu --mode inplace **/*.hs`
