# NEO CLI (nhcli)

NeoHaskell CLI tool for project scaffolding, building, and execution.

## ARCHITECTURE

```
cli/
├── app/Main.hs           # Thin wrapper: Task.runMain Neo.run
├── src/Neo.hs            # Command router + options parsing
├── src/Neo/Build.hs      # Build orchestration (generates .cabal, nix, runs nix-build)
├── src/Neo/New.hs        # Project scaffolding (creates dir, files, git init)
├── src/Neo/Run.hs        # Project execution
├── src/Neo/Shell.hs      # Virtual environment entry
└── src/Neo/Core/         # ProjectConfiguration type (neo.json schema)
```

## COMMAND FLOW

1. `Main.hs` → `Task.runMain Neo.run`
2. `Neo.run` → parses args via `Command.parseHandler` (nhcore)
3. Routes to `Build.handle`, `New.handle`, `Run.handle`, or `Shell.handle`
4. Each handler reads `neo.json` → `ProjectConfiguration`

## TEMPLATE SYSTEM

Templates are pure functions returning `Text` via `[fmt|...|]` interpolation:

| Template | Purpose |
|----------|---------|
| `Neo.Build.Templates.AppMain` | Generated `.launcher/Main.hs` |
| `Neo.Build.Templates.Cabal` | Generated `{name}.cabal` |
| `Neo.Build.Templates.CabalProject` | Static `cabal.project` |
| `Neo.Build.Templates.Nix` | Nix build expression |
| `Neo.New.Templates.MainModule` | Initial `src/{Name}.hs` |
| `Neo.New.Templates.NeoJson` | Initial `neo.json` |
| `Neo.New.Templates.GitIgnore` | Initial `.gitignore` |

Pattern: `template :: ProjectConfiguration -> Text` or `template :: Text`

## WHERE TO LOOK

| Task | Location |
|------|----------|
| Add new CLI command | `Neo.hs` (parser + handler routing) |
| Modify build process | `Neo/Build.hs` |
| Change generated files | `Neo/Build/Templates/*.hs` |
| Modify scaffolding | `Neo/New.hs` + `Neo/New/Templates/*.hs` |
| Change neo.json schema | `Neo/Core/ProjectConfiguration.hs` |

## KEY TYPES

- `ProjectConfiguration` - Parsed `neo.json` (name, version, deps, etc.)
- `NeoCommand` - ADT for parsed commands: `Build | Run | New | Shell`
- `CommonFlags` - Shared flags (e.g., `--projectConfig` path)

## CONVENTIONS

- Library-first: All logic in `src/`, executable is 3-line wrapper
- Templates as modules: One template per file, exports `template` function
- Uses nhcore's `Command` module for options parsing (not optparse-applicative directly)
- All errors wrapped in command-specific `Error` ADT, converted to user-friendly text
