# ADR-0026: Neo CLI and Project Configuration (`neo.json`)

## Status

Proposed

## Context

Every NeoHaskell project currently requires **11+ nearly-identical boilerplate files** that only differ in project name, dependencies, and module list. The real pain isn't file count — it's **triple-synchronization**: the same NeoHaskell commit SHA must be manually kept in sync across `cabal.project` (tag field), `flake.nix` (input URL), and `flake.lock` (`nix flake lock`).

### Per-Project Boilerplate (11 files)

| File | What Varies | Everything Else |
|------|-------------|-----------------|
| `*.cabal` | name, deps, exposed-modules | 23 extensions, GHC options, default-language, common\_cfg stanza |
| `cabal.project` | NeoHaskell commit SHA, constraints | source-repository-package structure, subdir patterns |
| `flake.nix` | project name, NeoHaskell commit SHA | haskell.nix overlay, inputMap, supported systems, nixConfig caches |
| `nix/hix.nix` | project name | compiler-nix-name, shell.tools list, shell.buildInputs |
| `docker-compose.yml` | credentials (always `neohaskell`) | postgres:16-alpine, ports, volumes, healthcheck |
| `launcher/Launcher.hs` | — (always identical) | `app \|> Application.run \|> Task.runOrPanic` |
| `.hlint.yaml` | — | standard config |
| `fourmolu.yaml` | — | standard config |
| `.envrc` | — | `use flake` |
| `.gitignore` | — | standard Haskell + Nix + project ignores |
| `.dockerignore` | — | standard |

**Evidence**: Compared `CIOS/datalake/`, `neohaskell-template/`, and `NeoHaskell/testbed/` — all three follow the exact same pattern with only the variables above differing.

### Framework Constants (NEVER vary)

These are baked into NeoHaskell's identity and should never be project-configurable:

**23 GHC Extensions** (identical in every `.cabal`):

```
ApplicativeDo, BlockArguments, DataKinds, DeriveDataTypeable,
DuplicateRecordFields, ImportQualifiedPost, ImpredicativeTypes,
NamedFieldPuns, NoFieldSelectors, NoImplicitPrelude, OverloadedLabels,
OverloadedLists, OverloadedRecordDot, OverloadedStrings, PackageImports,
PartialTypeSignatures, QualifiedDo, QuasiQuotes, Strict, TemplateHaskell,
TypeApplications, TypeFamilies, TypeOperators
```

**GHC Options**: `-Wall -Wno-orphans -threaded -fno-warn-partial-type-signatures -fno-warn-name-shadowing -Werror`

**Default Language**: `GHC2021`

**Compiler**: `ghc98` (via `nix/hix.nix`, managed by NeoHaskell internally)

**Nix Binary Caches**: `cache.iog.io` + `neohaskell.cachix.org`

**Shell Tools**: `cabal`, `hlint`, `fourmolu`, `hspec-discover`, `haskell-language-server`, `hurl` (included in the nix environment automatically)

### What's Already Solved: Runtime Config (Layer 2)

The `Config` module in nhcore (see [ADR-0021](0021-declarative-config-dsl.md)) handles runtime configuration:

```haskell
defineConfig "DatalakeConfig"
  [ Config.field @Text "dbHost"
      |> Config.doc "PostgreSQL host"
      |> Config.defaultsTo ("localhost" :: Text)
      |> Config.envVar "DB_HOST"
  , Config.field @Text "dbPassword"
      |> Config.doc "PostgreSQL password"
      |> Config.required
      |> Config.envVar "DB_PASSWORD"
      |> Config.secret
  ]
```

**This layer is complete. No changes needed.**

### What's Truly User-Authored

Only these files contain actual application logic:

| File/Dir | Purpose | Template-able? |
|----------|---------|----------------|
| `src/App.hs` | Application wiring | Partially (starter template) |
| `src/MyApp/**` | Business logic (Core, Commands, Queries, Integrations) | No |
| `tests/**` | Hurl acceptance tests | No |
| `AGENTS.md` | AI knowledge base | Yes (template + project-specific learnings) |
| `.skills/` | AI skills | Yes (shipped with framework) |

### The Core Problem: SHA Triple-Synchronization

The deepest pain point is that **the same information must be manually synchronized across three files in three different formats**:

1. **`cabal.project`** — as a `tag:` field in `source-repository-package`:

   ```
   tag: 1f7e19e06fc1cc6a6d40cd4ac4f7f91a77b8b66d
   ```

2. **`flake.nix`** — as part of the `inputMap` key and the `neohaskellCommit` variable:

   ```nix
   neohaskellCommit = "1f7e19e06fc1cc6a6d40cd4ac4f7f91a77b8b66d";
   inputMap = {
     "https://github.com/neohaskell/neohaskell.git/${neohaskellCommit}" = neohaskell;
   };
   ```

3. **`flake.lock`** — as the locked revision (updated by `nix flake lock --update-input neohaskell`):

   ```json
   "neohaskell": { "locked": { "rev": "1f7e19e..." } }
   ```

**Current update procedure** (manual, error-prone):

1. Run `nix flake lock --update-input neohaskell`
2. Extract new SHA: `nix eval .#neohaskell.rev --raw`
3. Update `cabal.project` tag field
4. Update `flake.nix` neohaskellCommit variable
5. Pray you didn't make a typo

**This is the #1 problem `neo.json` + CLI should solve.**

### The Three Config Layers

| Layer | Purpose | Format | Status |
|-------|---------|--------|--------|
| Layer 1: Project Config | Build-time metadata, deps, generation | `neo.json` | **Missing** |
| Layer 2: Runtime Config | Env vars, CLI args, config files | `defineConfig` TH | **Complete** (ADR-0021) |
| Layer 3: Infrastructure Config | Package-declared infra requirements | Package metadata | **Future** (tracked as GitHub issue) |

### Why Not Hpack?

[Hpack](https://github.com/sol/hpack) (`package.yaml`) already generates `.cabal` from YAML. It solves 1/7th of the problem:

| Aspect | Hpack | neo.json |
|--------|-------|----------|
| Generates | `.cabal` only | `.cabal` + `cabal.project` + `flake.nix` + `nix/hix.nix` + `docker-compose.yml` + launcher + more |
| NeoHaskell extensions | Must list manually | Baked in (framework constant) |
| NeoHaskell dependency | Must configure `source-repository-package` manually | Automatic from `neohaskellVersion` |
| Nix integration | None | Full flake generation |
| Module detection | Auto-detect | Auto-detect |

## Decision

We will introduce a `neo.json` project configuration file and a `neo` CLI tool that together eliminate boilerplate, solve SHA triple-synchronization, and provide a unified developer experience for NeoHaskell projects.

**Core Philosophy**: `neo` is the **mandatory developer interface** for NeoHaskell. Users never touch nix or cabal directly. They build via `neo build`, enter shell via `neo shell`, add dependencies via `neo add`, etc. Generated files are implementation details (.gitignored). The NeoHaskell package registry is real (a JSON file in a GitHub repo mapping package names to repo URLs).

### 1. `neo.json` Schema

Single source of truth for project metadata and dependencies:

```jsonc
{
  // Project identity
  "name": "datalake",
  "version": "0.1.0",
  "description": "Hosq Datalake - Proposal evaluation and funding recommendation system",
  "license": "Apache-2.0",
  "author": "Your Name",
  "maintainer": "your.email@example.com",

  // NeoHaskell version pin (THE source of truth)
  "neohaskellVersion": "main",  // Can be SHA, version tag, or branch

  // Dependencies (unified record, npm-style)
  "dependencies": {
    // Default (no prefix) = NeoHaskell package registry
    "nhcore": "^1.0.0",
    "nhintegrations": "^1.0.0",
    
    // Hackage packages
    "hackage://aeson": "^2.2",
    "hackage://bytestring": "^0.12",
    
    // GitHub repos
    "gh://user/my-lib": "main",
    
    // Arbitrary git repos
    "git://https://example.com/repo.git": "v1.0.0",
    
    // Local packages
    "file://../my-local-lib": "*"
  },

  // Shell tools (expandable nix environment)
  "shell-tools": ["postgresql", "poppler_utils"],

  // Optional: escape hatches for edge cases
  "extra-extensions": ["AllowAmbiguousTypes"],
  "extra-ghc-options": ["-Wno-unused-imports"]
}
```

**Key Changes from Original Design**:

1. **No lock file** — locking is delegated to nix (`flake.lock`) and cabal. NeoHaskell doesn't reinvent dependency resolution.
2. **Unified dependencies field** — single record with prefix-based resolution (`hackage://`, `gh://`, `git://`, `file://`). No separate `hackage-dependencies`, `git-dependencies`, or `constraints` fields.
3. **Simple NeoHaskell version pin** — top-level `neohaskellVersion` string, not a nested object.
4. **No infrastructure section** — runtime infrastructure config is already handled by `defineConfig` TH (ADR-0021). Build-time infra (docker-compose) is future work.
5. **No slices/AI workflow section** — deferred to future work, not part of MVP.
6. **No overrides catch-all** — escape hatches are standardized fields (`extra-extensions`, `extra-ghc-options`), not a generic `overrides` object.

### 2. Package Registry (Real Implementation)

The NeoHaskell package registry is a **GitHub repository** containing a single JSON file that maps package names to repo URLs and versions:

**Registry structure** (`https://github.com/neohaskell/registry`):

```json
{
  "packages": {
    "nhcore": {
      "repo": "github:neohaskell/neohaskell",
      "subdir": "core/",
      "versions": {
        "1.0.0": "abc123...",
        "1.1.0": "def456..."
      }
    },
    "nhintegrations": {
      "repo": "github:neohaskell/neohaskell",
      "subdir": "integrations/",
      "versions": {
        "1.0.0": "abc123..."
      }
    }
  }
}
```

**Resolution rules**:

- **No prefix** (e.g., `"nhcore": "^1.0.0"`) → look up in NeoHaskell registry
- **`hackage://`** (e.g., `"hackage://aeson": "^2.2"`) → pass through to cabal
- **`gh://`** (e.g., `"gh://user/repo": "main"`) → GitHub repo
- **`git://`** (e.g., `"git://https://example.com/repo.git": "v1.0.0"`) → arbitrary git repo
- **`file://`** (e.g., `"file://../my-lib": "*"`) → local filesystem path

### 3. Generated vs Committed: Generate and Gitignore

**Decision**: Generated files are **implementation details** and must be **.gitignored**.

| Strategy | Pros | Cons |
|----------|------|------|
| **Generate & Gitignore** | Clean git history, single source of truth, enforces neo as mandatory interface | Requires `neo` CLI installed |
| **Generate & Commit** | Works without neo CLI | Defeats the purpose of neo as mandatory interface, creates drift risk |

**Rationale**: NeoHaskell is an opinionated framework. Users interact with the framework via `neo`, not via raw nix/cabal. This is closer to Rails or Cargo than to Hpack. Generated files are build artifacts, not source code.

**`.gitignore` additions**:

```
# NeoHaskell generated files (DO NOT COMMIT)
*.cabal
cabal.project
flake.nix
flake.lock
nix/
launcher/
docker-compose.yml
.envrc
```

**Committed files**:

- `neo.json` (source of truth)
- `src/` (application code)
- `tests/` (tests)
- `AGENTS.md`, `.skills/` (AI workflow)
- Standard configs (`.hlint.yaml`, `fourmolu.yaml`, `.gitignore`, `.dockerignore`)

### 4. Module Auto-Detection

The `.cabal` file currently requires manually listing `exposed-modules`. The CLI will:

1. **Scan `src/` recursively** for `.hs` files
2. **Derive module names** from paths (`src/Datalake/Proposal/Core.hs` -> `Datalake.Proposal.Core`)
3. **Populate `exposed-modules`** automatically

**NeoHaskell convention**: All modules are public. Everything is exposed. No internal modules, no hiding implementation details.

### 5. CLI Command Surface (Phase 1 Only)

**Core commands** (MVP):

| Command | Description | Generated Files |
|---------|-------------|-----------------|
| `neo init` | Create new project from template | All generated files + starter `src/App.hs` |
| `neo shell` | Enter nix develop shell | — |
| `neo build` | Regenerate if needed, then `cabal build all` | (checks generated files are current) |
| `neo run` | Build and run the application | — |
| `neo test` | Run hurl tests + cabal tests | — |
| `neo dev` | Dev mode with file watching | — |
| `neo add <package>` | Add dependency to neo.json (defaults to registry) | Regenerates `.cabal` + `cabal.project` |
| `neo update` | Update NeoHaskell to latest, regenerate all | All generated files |
| `neo generate` | Regenerate all files from neo.json | All generated files |
| `neo doctor` | Verify generated files match neo.json | — |
| `neo clean` | Clean build artifacts | — |

**Dependency resolution examples**:

```bash
# Add from NeoHaskell registry (default)
neo add nhcore

# Add from Hackage
neo add hackage://aeson

# Add from GitHub
neo add gh://user/my-lib

# Add from arbitrary git repo
neo add git://https://example.com/repo.git

# Add local package
neo add file://../my-local-lib
```

**Removed from original design**:

- `neo init --from-existing` — only 2 existing projects, handled manually by creator
- `neo lock` — no lock file, nix and cabal handle locking
- Phase 2 (AI workflow) — future work
- Phase 3 (Infrastructure) — future work

### 6. File Generation Matrix

What `neo generate` produces from `neo.json`:

| Generated File | Source Data | Template Complexity |
|----------------|------------|-------------------|
| `*.cabal` | name, deps, `src/` scan for modules | Medium — must handle lib + exe stanzas, common\_cfg |
| `cabal.project` | NeoHaskell SHA from neohaskellVersion, dependencies | Low — template with variable substitution |
| `flake.nix` | name, NeoHaskell SHA, shell-tools | Medium — must match haskell.nix overlay pattern |
| `nix/hix.nix` | name, shell-tools | Low — simple template |
| `docker-compose.yml` | (future: infrastructure config) | Low — but deferred to future work |
| `launcher/Launcher.hs` | — (always identical) | Trivial |
| `.gitignore` | — (standard template) | Trivial |
| `.hlint.yaml` | — (standard) | Trivial |
| `fourmolu.yaml` | — (standard) | Trivial |
| `.envrc` | — (`use flake`) | Trivial |
| `AGENTS.md` | project structure, patterns | Medium — must scan codebase |
| `.env.example` | Runtime Config definition (from defineConfig TH) | Hard — requires parsing TH output or reading source |

### 7. Escape Hatches (Standardized Fields)

Every successful config system has escape hatches. NeoHaskell provides standardized fields for edge cases:

```jsonc
{
  // Extra GHC extensions (beyond framework defaults)
  "extra-extensions": ["AllowAmbiguousTypes"],
  
  // Extra GHC options
  "extra-ghc-options": ["-Wno-unused-imports"],
  
  // Extra nix shell tools
  "shell-tools": ["postgresql", "poppler_utils"]
}
```

**Removed from original design**:

- Generic `overrides` catch-all object
- `extra-modules` / `exclude-modules` — everything is public, no exceptions

**Without escape hatches, the first project that needs something neo.json doesn't support will abandon it entirely.**

### 8. The Bootstrapping Problem

The CLI is written in Haskell (dogfooding nhcore). But to build Haskell, you need the Haskell toolchain, which is provided by `nix develop`, which requires the `flake.nix` that `neo` generates.

**Solution**: Nix is mandatory. Use `nix run github:neohaskell/neo` exclusively.

```bash
# Zero-install experience (uses Nix cache)
nix run github:neohaskell/neo -- init my-project
cd my-project
neo shell  # Enter nix develop
neo build  # Build the project
```

**Removed from original design**:

- Pre-built binaries
- Shell script bootstrap
- Non-Nix installation options

**Rationale**: NeoHaskell already requires Nix for the build system. Don't fragment the installation story.

### 9. Comparison with Similar Ecosystems

| Feature | Cargo (Rust) | Stack (Haskell) | neo (NeoHaskell) |
|---------|-------------|-----------------|------------------|
| Config file | `Cargo.toml` | `stack.yaml` + `.cabal` | `neo.json` |
| Lock file | `Cargo.lock` | `stack.yaml.lock` | Delegates to `flake.lock` + cabal |
| Generates build files | No (Cargo IS the build) | No (uses Cabal underneath) | Yes (generates `.cabal`, `flake.nix`, etc.) |
| Package registry | crates.io | Stackage + Hackage | NeoHaskell registry + Hackage |
| Escape hatches | `[patch]` section | `extra-deps` | Standardized fields (`extra-extensions`, `shell-tools`) |
| Mandatory CLI | Yes | Yes | Yes |
| Framework opinions | Minimal | Moderate | **Strong** (extensions, style, patterns) |

### 10. File Generation Templates

#### Generated `flake.nix`

```nix
{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.neohaskell.url = "git+https://github.com/neohaskell/neohaskell.git?ref={{neohaskellVersion}}";
  inputs.neohaskell.flake = false;

  outputs = { self, nixpkgs, flake-utils, haskellNix, neohaskell, ... }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      neohaskellCommit = builtins.readFile (neohaskell + "/.git/refs/heads/main");
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            hixProject = final.haskell-nix.hix.project {
              src = ./.;
              inputMap = {
                "https://github.com/neohaskell/neohaskell.git/${neohaskellCommit}" = neohaskell;
              };
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.hixProject.flake { };
      in flake // { legacyPackages = pkgs; });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://neohaskell.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "neohaskell.cachix.org-1:mo2cLaGbwqbrxs9xhqKK8jeNsn3osi7t6XoAmxSZssc="
    ];
    allow-import-from-derivation = "true";
  };
}
```

#### Generated `.cabal`

```cabal
cabal-version: 3.4
name: {{name}}
version: {{version}}
synopsis: {{description}}
homepage: https://neohaskell.org
license: {{license}}
license-file: LICENSE
author: {{author}}
maintainer: {{maintainer}}
category: Development
build-type: Simple

common common_cfg
  ghc-options:
    -Wall -Wno-orphans -threaded
    -fno-warn-partial-type-signatures
    -fno-warn-name-shadowing -Werror
  default-extensions:
    ApplicativeDo BlockArguments DataKinds DeriveDataTypeable
    DuplicateRecordFields ImportQualifiedPost ImpredicativeTypes
    NamedFieldPuns NoFieldSelectors NoImplicitPrelude OverloadedLabels
    OverloadedLists OverloadedRecordDot OverloadedStrings PackageImports
    PartialTypeSignatures QualifiedDo QuasiQuotes Strict TemplateHaskell
    TypeApplications TypeFamilies TypeOperators
    {{#each extra-extensions}}
    {{this}}
    {{/each}}
  build-depends:
    base,
    {{#each dependencies}}
    {{@key}},
    {{/each}}

library
  import: common_cfg
  exposed-modules:
    {{#each auto-detected-modules}}
    {{this}}
    {{/each}}
  hs-source-dirs: src
  default-language: GHC2021

executable {{name}}
  import: common_cfg
  main-is: Launcher.hs
  build-depends: base, {{name}}
  hs-source-dirs: launcher
  default-language: GHC2021
```

#### Generated `launcher/Launcher.hs`

```haskell
module Main where

import App (app)
import Core
import Service.Application qualified as Application
import Task qualified

main :: IO ()
main = do
  let runApp = app |> Application.run
  runApp |> Task.runOrPanic
```

**Note**: `Application.run` already handles line buffering internally. No need for explicit `hSetBuffering` calls.

### 11. Edge Cases

| Edge Case | Impact | Mitigation |
|-----------|--------|------------|
| Project needs additional GHC extensions | Breaks "framework constant" assumption | `extra-extensions` field |
| Custom Nix overlays | `flake.nix` needs extra overlays | Future: `extra-flake-inputs` field |
| No-postgres projects | docker-compose shouldn't have postgres | Only generate infra that's configured (future work) |
| `.envrc` depends on `flake.nix` | If flake.nix regeneration fails, direnv breaks | Regenerate atomically |
| First generation (no flake.lock) | `nix develop` fails without lock | `neo init` runs `nix flake lock` |

**Removed from original design**:

- Multiple executables — single executable for now
- Additional git dependencies — handled by `gh://` or `git://` in unified dependencies field
- Monorepo with multiple NH projects — one repo = one NeoHaskell project
- GHC version upgrade — GHC version is NOT user-managed, NeoHaskell manages it internally
- Module with non-standard path — not allowed, everything is public
- Test suites beyond hurl — neo auto-generates a single test suite in `tests/`, no multiple suites
- Custom shell.buildInputs — handled by `shell-tools` field

## Consequences

### Positive

1. **Single source of truth**: `neo.json` replaces manual synchronization of the same SHA across three files. The #1 developer pain point is eliminated.

2. **Zero-boilerplate project creation**: `neo init` generates all 11 boilerplate files from a single config file. New projects start in seconds, not hours.

3. **Strong framework opinions**: `neo` is the mandatory interface. Users never touch nix or cabal directly. This creates consistency across all NeoHaskell projects.

4. **Clean git history**: Generated files are .gitignored, so diffs only show meaningful changes to `neo.json` and application code.

5. **Real package registry**: The NeoHaskell registry is a simple JSON file in a GitHub repo, making it trivial to add new packages.

6. **Safe updates**: `neo update` atomically updates the NeoHaskell version across all generated files — no more partial updates or typos.

7. **Escape hatches prevent lock-in**: Standardized fields (`extra-extensions`, `extra-ghc-options`, `shell-tools`) ensure projects can always express requirements that `neo.json` doesn't natively support.

### Negative

1. **Mandatory tooling**: Developers must install `neo` (via `nix run`) to work on NeoHaskell projects. This is a hard requirement, not optional.

2. **Bootstrapping dependency**: Requires Nix installed. Non-Nix users cannot use NeoHaskell.

3. **Template maintenance**: As NeoHaskell evolves (new GHC versions, new extensions, new Nix patterns), the generation templates must be updated in lockstep.

4. **Registry maintenance**: The package registry JSON file must be manually updated when new packages are released. Future: automate this with CI.

### Risks

1. **Adoption friction**: Developers accustomed to editing `.cabal` files directly may resist the mandatory CLI. Mitigation: clear documentation explaining the benefits.

2. **Debugging complexity**: If `neo generate` produces broken files, developers can't easily fix them (they're gitignored). Mitigation: `neo doctor` validates generated files, clear error messages.

3. **Registry single point of failure**: If the registry repo is down, `neo add` fails. Mitigation: cache registry locally, fallback to Hackage.

### Implementation Priority

| Phase | Scope | Priority |
|-------|-------|----------|
| Phase 0 | Package registry (JSON file in GitHub repo) | Critical |
| Phase 1 | `neo.json` schema, file generation, module auto-detection, core CLI (`init`, `shell`, `generate`, `build`, `run`, `add`, `update`, `doctor`) | Critical |

**Removed from original design**:

- Phase 2 (AI workflow) — future work
- Phase 3 (Infrastructure) — future work
- Phase 4 (Registry) — now part of Phase 0

## References

- [ADR-0021: Declarative Config DSL](0021-declarative-config-dsl.md) — Runtime config (Layer 2), which this ADR complements at the build-time layer
- [Hpack](https://github.com/sol/hpack) — Prior art for `.cabal` generation from simpler config
- [Cargo](https://doc.rust-lang.org/cargo/) — Inspiration for unified project management CLI
- [haskell.nix](https://input-output-hk.github.io/haskell.nix/) — Underlying Nix infrastructure for Haskell builds
