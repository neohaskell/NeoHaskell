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

**Compiler**: `ghc98` (via `nix/hix.nix`)

**Nix Binary Caches**: `cache.iog.io` + `neohaskell.cachix.org`

**Shell Tools**: `cabal`, `hlint`, `fourmolu`, `hspec-discover`, `haskell-language-server`, `hurl`

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
| `slices/**` | Event model specifications | No (user-authored specs) |
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
| Layer 3: Infrastructure Config | Package-declared infra requirements | Package metadata | **Future** |

### Why Not Hpack?

[Hpack](https://github.com/sol/hpack) (`package.yaml`) already generates `.cabal` from YAML. It solves 1/7th of the problem:

| Aspect | Hpack | neo.json |
|--------|-------|----------|
| Generates | `.cabal` only | `.cabal` + `cabal.project` + `flake.nix` + `nix/hix.nix` + `docker-compose.yml` + launcher + more |
| NeoHaskell extensions | Must list manually | Baked in (framework constant) |
| NeoHaskell dependency | Must configure `source-repository-package` manually | Automatic from `neohaskell.ref` |
| Nix integration | None | Full flake generation |
| Module detection | Auto-detect | Auto-detect |
| AI workflow | None | Full slice/RALPH management |
| Lock file | None | `neo.lock` for reproducible builds |

## Decision

We will introduce a `neo.json` project configuration file and a `neo` CLI tool that together eliminate boilerplate, solve SHA triple-synchronization, and provide a unified developer experience for NeoHaskell projects.

### 1. `neo.json` Schema

Single source of truth for project metadata, dependencies, and generation:

```jsonc
{
  // Project identity
  "name": "datalake",
  "version": "0.1.0",
  "description": "Hosq Datalake - Proposal evaluation and funding recommendation system",
  "license": "Apache-2.0",
  "author": "Your Name",
  "maintainer": "your.email@example.com",

  // Dependencies
  "dependencies": {
    // NeoHaskell packages (from future NH registry)
    "nhcore": "neohaskell://core",
    "nhintegrations": "neohaskell://integrations"
  },

  // Hackage dependencies (beyond what nhcore re-exports)
  "hackage-dependencies": [
    "aeson",
    "bytestring",
    "text"
  ],

  // Version constraints
  "constraints": {
    "hasql": "< 1.10"
  },

  // Additional git dependencies (for non-registry packages)
  "git-dependencies": {
    // "my-lib": { "url": "https://github.com/...", "ref": "main", "subdir": "lib" }
  },

  // NeoHaskell version pin (THE source of truth)
  "neohaskell": {
    "ref": "main"
    // SHA is resolved and locked in neo.lock, not here
  },

  // Optional: override generated file content
  "overrides": {
    "cabal.project": {
      "append": ""
    },
    "flake.nix": {
      "extra-inputs": {},
      "extra-build-inputs": []
    },
    "hix.nix": {
      "extra-shell-tools": {},
      "extra-build-inputs": []
    }
  },

  // Infrastructure (MVP: just postgres config)
  "infrastructure": {
    "postgres": {
      "user": "neohaskell",
      "password": "neohaskell",
      "database": "neohaskell",
      "port": 5432
    }
  },

  // AI workflow configuration
  "slices": {
    "directory": "slices",
    "contexts": ["Proposal", "ProposalMetricEvaluation"]
  }
}
```

### 2. The Lock File (`neo.lock`)

Every successful package manager has a lock file. `neo.lock` locks:

- **NeoHaskell exact SHA** (resolved from `neohaskell.ref`)
- **Hackage package versions** (resolved from latest at lock time)
- **Git dependency SHAs** (resolved from refs)

```jsonc
{
  "lockVersion": 1,
  "neohaskell": {
    "sha": "1f7e19e06fc1cc6a6d40cd4ac4f7f91a77b8b66d",
    "resolved-from": "main"
  },
  "hackage": {
    "aeson": "2.2.1.0",
    "bytestring": "0.12.1.0",
    "text": "2.1"
  }
}
```

**`neo.lock` should be committed to git** — ecosystem consensus is clear (Cargo.lock, go.sum, package-lock.json, flake.lock all committed).

### 3. Generated vs Committed: Generate and Commit

Ecosystem evidence strongly favors **"generate and commit"** over **"generate and gitignore"**:

| Strategy | Pros | Cons |
|----------|------|------|
| **Generate & Gitignore** | Clean git history, single source of truth | IDE breaks after clone, CI needs `neo` installed, `envrc` chain breaks |
| **Generate & Commit** | IDE works after clone, CI uses standard tools, no bootstrap needed | Diff noise, potential drift, two files to "look at" |

**Decision**: Generate on `neo init` and `neo update`, **commit the results**. Use `neo generate` as a tidy/regeneration command (like `go mod tidy`). Add a CI check that verifies generated files match `neo.json` (like `go mod verify`).

**Rationale**: After `git clone`, a developer should be able to run `nix develop && cabal build` without installing any NeoHaskell-specific tooling. The `neo` CLI is a convenience, not a hard requirement.

### 4. Module Auto-Detection

The `.cabal` file currently requires manually listing `exposed-modules`. The CLI will:

1. **Scan `src/` recursively** for `.hs` files
2. **Derive module names** from paths (`src/Datalake/Proposal/Core.hs` -> `Datalake.Proposal.Core`)
3. **Populate `exposed-modules`** automatically

**Caveat**: Auto-detection loses intentional structure — you can't express "this module is internal" vs "this is public API." For now, NeoHaskell projects expose everything (no internal modules), so this is acceptable. Future: add `exclude-modules` in `neo.json`.

### 5. CLI Command Surface (Phased)

#### Phase 1: Project Management (Core)

| Command | Description | Generated Files |
|---------|-------------|-----------------|
| `neo init` | Create new project from template | All generated files + starter `src/App.hs` |
| `neo init --from-existing` | Convert existing project to neo.json-managed | `neo.json` from existing files |
| `neo build` | Regenerate if needed, then `cabal build all` | (checks generated files are current) |
| `neo run` | Build and run the application | — |
| `neo test` | Run hurl tests + cabal tests | — |
| `neo dev` | Dev mode with file watching | — |
| `neo add <package>` | Add hackage dependency to neo.json | Regenerates `.cabal` |
| `neo add neohaskell://<pkg>` | Add NeoHaskell package | Regenerates `.cabal` + `cabal.project` |
| `neo update` | Update NeoHaskell to latest, regenerate all | All generated files + `neo.lock` |
| `neo lock` | Resolve and lock dependencies | `neo.lock` |
| `neo generate` | Regenerate all files from neo.json | All generated files |
| `neo doctor` | Verify generated files match neo.json | — |
| `neo clean` | Clean build artifacts | — |

#### Phase 2: AI Workflow

| Command | Description |
|---------|-------------|
| `neo slice list` | List all slices and their status |
| `neo slice next` | Show next planned slice |
| `neo slice add <name>` | Create new slice spec |
| `neo slice done <id>` | Mark slice as done |
| `neo ralph` | Run RALPH loop (AI implements next slice) |
| `neo agents init` | Generate AGENTS.md from project structure |
| `neo agents update` | Update AGENTS.md learnings section |
| `neo skills list` | List available .skills/ |
| `neo skills add <name>` | Add a skill to the project |

#### Phase 3: Infrastructure & Deployment

| Command | Description |
|---------|-------------|
| `neo infra up` | Start infrastructure (docker-compose up) |
| `neo infra down` | Stop infrastructure |
| `neo deploy <target>` | Generate deployment config for target |

### 6. File Generation Matrix

What `neo generate` produces from `neo.json` + `neo.lock`:

| Generated File | Source Data | Template Complexity |
|----------------|------------|-------------------|
| `*.cabal` | name, deps, `src/` scan for modules | Medium — must handle lib + exe stanzas, common\_cfg |
| `cabal.project` | NeoHaskell SHA from neo.lock, constraints, git-deps | Low — template with variable substitution |
| `flake.nix` | name, NeoHaskell SHA, extra inputs | Medium — must match haskell.nix overlay pattern |
| `nix/hix.nix` | name, extra tools, extra build inputs | Low — simple template |
| `docker-compose.yml` | infrastructure config | Low — but must merge package-declared infra (future) |
| `launcher/Launcher.hs` | — (always identical) | Trivial |
| `.gitignore` | — (standard template) | Trivial |
| `.hlint.yaml` | — (standard) | Trivial |
| `fourmolu.yaml` | — (standard) | Trivial |
| `.envrc` | — (`use flake`) | Trivial |
| `AGENTS.md` | project structure, slices, patterns | Medium — must scan codebase |
| `.env.example` | Runtime Config definition (from defineConfig TH) | Hard — requires parsing TH output or reading source |

### 7. Escape Hatches (Non-Negotiable)

Every successful config system has escape hatches:

```jsonc
{
  "overrides": {
    // Append raw content to generated cabal.project
    "cabal-project-append": "constraints: hasql < 1.10",
    // Extra nix flake inputs
    "extra-flake-inputs": {
      "my-lib": { "url": "github:user/lib" }
    },
    // Extra nix shell tools
    "extra-shell-tools": ["postgresql", "poppler_utils"],
    // Extra GHC extensions (beyond framework defaults)
    "extra-extensions": ["AllowAmbiguousTypes"],
    // Extra GHC options
    "extra-ghc-options": ["-Wno-unused-imports"],
    // Extra modules not auto-detected
    "extra-modules": [],
    // Modules to exclude from auto-detection
    "exclude-modules": []
  }
}
```

**Without escape hatches, the first project that needs something neo.json doesn't support will abandon it entirely.**

### 8. Package Registry (`neohaskell://`) — MVP Strategy

For MVP, skip the full registry. Instead:

1. **NeoHaskell packages** = GitHub monorepo subdirectories (already the case)
   - `neohaskell://core` -> `github:neohaskell/neohaskell` subdir `core/`
   - `neohaskell://integrations` -> `github:neohaskell/neohaskell` subdir `integrations/`

2. **Version = NeoHaskell monorepo ref** (branch, tag, or SHA)
   - All NH packages share the same version (monorepo versioning)
   - `neo.lock` locks the exact SHA

3. **Hackage dependencies** = passed through to `.cabal` `build-depends`
   - Cabal + Nix handle Hackage resolution (don't reinvent)

4. **Future**: Extract packages into separate repos with a proper registry when the ecosystem grows.

### 9. Infrastructure Declarations (Layer 3) — Future

Packages will declare their infrastructure needs:

```haskell
-- In nhcore's Service.EventStore.Postgres module metadata:
-- "I need a PostgreSQL database"
```

When the CLI sees a project using these modules, it auto-generates `docker-compose.yml` entries and `.env.example` variables.

**MVP scope**: `neo.json` has an explicit `infrastructure` section. The CLI generates `docker-compose.yml` from it. No package-level declarations yet.

### 10. The Bootstrapping Problem

The CLI is written in Haskell (dogfooding nhcore). But to build Haskell, you need the Haskell toolchain, which is provided by `nix develop`, which requires the `flake.nix` that `neo` generates.

**Solutions** (in order of pragmatism):

1. **Pre-built binaries** — Distribute `neo` as a static binary via GitHub Releases / Cachix. User installs once, then `neo init` generates everything.
2. **Nix package** — `neo` itself is a Nix package: `nix run github:neohaskell/neo -- init`
3. **Shell script bootstrap** — `curl -sSL https://neohaskell.org/install | sh`
4. **`npx`-style** — `nix run github:neohaskell/neo` (no install needed, uses Nix cache)

**Recommended**: Option 4 (`nix run`) for zero-install experience, with Option 1 as fallback for non-Nix users.

### 11. Migration Path

#### Existing Projects -> neo.json

`neo init --from-existing` will:

1. Read existing `*.cabal` -> extract name, version, deps, modules
2. Read existing `cabal.project` -> extract NeoHaskell SHA, constraints
3. Read existing `flake.nix` -> extract any custom inputs
4. Read existing `nix/hix.nix` -> extract any custom tools
5. Read existing `docker-compose.yml` -> extract infrastructure config
6. Generate `neo.json` + `neo.lock`
7. Verify: `neo generate` produces files identical to existing ones

#### neo.json -> Existing Tools

A project managed by `neo.json` must **always** be buildable with standard tools:

```bash
# These must ALWAYS work (no neo CLI required):
nix develop       # Enter dev shell
cabal build all   # Build
cabal run myapp   # Run
hurl --test ...   # Test
```

The `neo` CLI is a **convenience layer**, not a hard dependency. This is critical for adoption.

### 12. Comparison with Similar Ecosystems

| Feature | Cargo (Rust) | Go Modules | Stack (Haskell) | neo (NeoHaskell) |
|---------|-------------|------------|-----------------|------------------|
| Config file | `Cargo.toml` | `go.mod` | `stack.yaml` + `.cabal` | `neo.json` |
| Lock file | `Cargo.lock` | `go.sum` | `stack.yaml.lock` | `neo.lock` |
| Generates build files | No (Cargo IS the build) | No (Go IS the build) | No (uses Cabal underneath) | Yes (generates `.cabal`, `flake.nix`, etc.) |
| Package registry | crates.io | Go Module Proxy | Stackage + Hackage | `neohaskell://` (future) + Hackage |
| Escape hatches | `[patch]` section | `replace` directive | `extra-deps` | `overrides` section |
| IDE works after clone | Yes | Yes | Yes (mostly) | Yes (generated files committed) |
| Framework opinions | Minimal | Minimal | Moderate | **Strong** (extensions, style, patterns) |
| AI workflow | None | None | None | **Built-in** (RALPH, slices, skills) |

### 13. File Generation Templates

#### Generated `flake.nix`

```nix
{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.neohaskell.url = "git+https://github.com/neohaskell/neohaskell.git?ref={{neohaskell.ref}}";
  inputs.neohaskell.flake = false;
  {{#each overrides.extra-flake-inputs}}
  inputs.{{@key}}.url = "{{this.url}}";
  {{/each}}

  outputs = { self, nixpkgs, flake-utils, haskellNix, neohaskell, ... }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      neohaskellCommit = "{{neo.lock.neohaskell.sha}}";
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
  build-depends:
    base,
    {{#each dependencies}}
    {{@key}},
    {{/each}}
    {{#each hackage-dependencies}}
    {{this}},
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
import System.IO qualified as GhcIO
import Task qualified

main :: IO ()
main = do
  GhcIO.hSetBuffering GhcIO.stdout GhcIO.LineBuffering
  GhcIO.hSetBuffering GhcIO.stderr GhcIO.LineBuffering
  let runApp = app |> Application.run
  runApp |> Task.runOrPanic
```

(Always identical. Every NeoHaskell application.)

### 14. Edge Cases

| Edge Case | Impact | Mitigation |
|-----------|--------|------------|
| Project needs additional GHC extensions | Breaks "framework constant" assumption | `overrides.extra-extensions` field |
| Multiple executables (e.g., CLI + server) | `.cabal` generation must handle multiple exe stanzas | `neo.json` has `executables` array |
| Additional git dependencies (not NH) | `cabal.project` needs more `source-repository-package` | `neo.json` has `git-dependencies` |
| Monorepo with multiple NH projects | Shared `flake.nix`, separate `.cabal` files | Future: `neo.json` workspace support |
| Custom Nix overlays | `flake.nix` needs extra overlays | `overrides.extra-flake-inputs` |
| GHC version upgrade | All projects need coordinated update | NeoHaskell monorepo pins GHC version |
| No-postgres projects | docker-compose shouldn't have postgres | Only generate infra that's configured |
| `.envrc` depends on `flake.nix` | If flake.nix regeneration fails, direnv breaks | Regenerate atomically |
| Module with non-standard path | Auto-detection misses it | `overrides.extra-modules` |
| Test suites beyond hurl | Project uses HSpec unit tests too | `neo.json` has `test-suites` config |
| Custom shell.buildInputs | Project needs extra nix packages | `overrides.extra-build-inputs` |
| First generation (no flake.lock) | `nix develop` fails without lock | `neo init` runs `nix flake lock` |

## Consequences

### Positive

1. **Single source of truth**: `neo.json` + `neo.lock` replace manual synchronization of the same SHA across three files. The #1 developer pain point is eliminated.

2. **Zero-boilerplate project creation**: `neo init` generates all 11 boilerplate files from a single config file. New projects start in seconds, not hours.

3. **Standard tools still work**: Generated files are committed, so `nix develop && cabal build` works without the `neo` CLI. The CLI is a convenience, not a hard dependency.

4. **Ecosystem consistency**: All NeoHaskell projects share the same structure, making it trivial to onboard to any project.

5. **AI workflow integration**: Built-in slice management, RALPH orchestration, and AGENTS.md generation are first-class features, not afterthoughts.

6. **Safe updates**: `neo update` atomically updates the NeoHaskell version across all generated files — no more partial updates or typos.

7. **Escape hatches prevent lock-in**: The `overrides` section ensures projects can always express requirements that `neo.json` doesn't natively support.

### Negative

1. **Two-file mental model**: Developers must understand the relationship between `neo.json` (source) and generated files (output). This is a conceptual overhead similar to Hpack or protobuf.

2. **Drift risk**: If developers manually edit generated files, they'll be overwritten by `neo generate`. Mitigated by CI check (`neo doctor`) and clear `-- GENERATED FILE, DO NOT EDIT` headers.

3. **Bootstrapping complexity**: The CLI itself needs a distribution mechanism. Mitigated by `nix run` zero-install approach.

4. **Template maintenance**: As NeoHaskell evolves (new GHC versions, new extensions, new Nix patterns), the generation templates must be updated in lockstep.

5. **Module auto-detection limitations**: Cannot express internal vs public API boundaries. Acceptable for now; addressable with `exclude-modules` later.

### Risks

1. **Adoption friction**: Existing projects must run `neo init --from-existing` to migrate. If the migration tool produces files that don't match existing ones, trust is broken immediately. Mitigation: verify round-trip fidelity in migration tool.

2. **Over-abstraction**: If `neo.json` tries to abstract too much, it becomes harder to debug build issues because the actual build files are generated. Mitigation: generated files are readable and committed, so developers can always inspect them.

3. **Registry complexity**: The `neohaskell://` protocol adds a custom package resolution layer on top of Cabal. MVP avoids this by mapping directly to GitHub monorepo subdirectories. Full registry deferred to Phase 4.

### Implementation Priority

| Phase | Scope | Priority |
|-------|-------|----------|
| Phase 0 | `neo.json` schema, file generation, module auto-detection | Critical |
| Phase 1 | Core CLI (`init`, `generate`, `build`, `run`, `update`, `doctor`) | Critical |
| Phase 2 | AI workflow (`slice`, `ralph`, `agents`, `skills`) | High |
| Phase 3 | Infrastructure (`docker-compose`, `.env.example`, `infra up/down`) | Medium |
| Phase 4 | Package registry (`neohaskell://`, version resolution, dependency solving) | Medium |

## References

- [ADR-0021: Declarative Config DSL](0021-declarative-config-dsl.md) — Runtime config (Layer 2), which this ADR complements at the build-time layer
- [Hpack](https://github.com/sol/hpack) — Prior art for `.cabal` generation from simpler config
- [Cargo](https://doc.rust-lang.org/cargo/) — Inspiration for unified project management CLI
- [Go Modules](https://go.dev/ref/mod) — Inspiration for lock file and `go mod tidy`-style regeneration
- [haskell.nix](https://input-output-hk.github.io/haskell.nix/) — Underlying Nix infrastructure for Haskell builds
