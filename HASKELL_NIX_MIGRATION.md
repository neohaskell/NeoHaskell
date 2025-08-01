# NeoHaskell User Project Nix Infrastructure Migration to haskell.nix

## Overview

Migrate the NeoHaskell nix libraries (used by `neo build` and `neo shell` for user projects) from nixpkgs-based infrastructure to IOHK's haskell.nix. This fixes macOS ARM64 compatibility issues and enables cross-compilation for user projects.

## Current Architecture

When users run `neo build` or `neo shell`, the CLI generates a nix expression that:
1. Imports NeoHaskell's `nix/lib.nix`
2. Calls `lib.buildNeoProject` or `lib.shellForNeoProject`
3. These functions use `developPackage` from nixpkgs

## Key Changes Needed

### 1. Update `nix/lib.nix`

Add haskell.nix support alongside the existing nixpkgs infrastructure:

```nix
{ pkgs ? import ./nixpkgs.nix }:

let
  # Import haskell.nix
  haskellNix = import (builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz";
  }) {};
  
  nixpkgs = import haskellNix.sources.nixpkgs {
    overlays = [ haskellNix.overlay ];
    system = builtins.currentSystem;
  };
  
  pkgsWithHaskellNix = nixpkgs;
  
  # Rest of the imports remain the same
  common = import ./common.nix { pkgs = pkgsWithHaskellNix; };
  
in {
  # Keep existing functions for backwards compatibility
  buildNeoProject = import ./project-builder.nix { pkgs = pkgsWithHaskellNix; };
  shellForNeoProject = import ./project-shell.nix { pkgs = pkgsWithHaskellNix; };
  
  # Add new haskell.nix versions
  buildNeoProjectHaskellNix = import ./project-builder-haskellnix.nix { 
    pkgs = pkgsWithHaskellNix; 
    haskell-nix = pkgsWithHaskellNix.haskell-nix;
  };
  shellForNeoProjectHaskellNix = import ./project-shell-haskellnix.nix { 
    pkgs = pkgsWithHaskellNix;
    haskell-nix = pkgsWithHaskellNix.haskell-nix;
  };
  
  generators = common.generators;
  utils = import ./utils { inherit pkgs; };
}
```

### 2. Create New haskell.nix-based Project Builder

Create `nix/project-builder-haskellnix.nix`:

```nix
{ pkgs, haskell-nix }:
{ neoJsonPath, neoHaskellSource, srcPath ? null }:

let
  common = import ./common.nix { inherit pkgs; };
  project = common.setupProject { inherit neoJsonPath neoHaskellSource srcPath; };
  generatedFiles = common.generateProjectFiles {
    inherit (project) neoConfig srcPathValue modules generators;
  };
  
  haskellProject = haskell-nix.cabalProject {
    src = generatedFiles;
    compiler-nix-name = "ghc984";
    
    # Handle nhcore dependency via source-repository-package in cabal file
    modules = [{
      packages.${project.neoConfig.name} = {
        ghcOptions = common.ghcFlags;
        doCheck = false;
        doHaddock = false;
      };
    }];
  };
  
in haskellProject.${project.neoConfig.name}.components.exes.${project.neoConfig.name}
```

### 3. Create New haskell.nix-based Shell

Create `nix/project-shell-haskellnix.nix`:

```nix
{ pkgs, haskell-nix }:
{ neoJsonPath, neoHaskellSource, srcPath ? null }:

let
  common = import ./common.nix { inherit pkgs; };
  project = common.setupProject { inherit neoJsonPath neoHaskellSource srcPath; };
  generatedFiles = common.generateProjectFiles {
    inherit (project) neoConfig srcPathValue modules generators;
  };
  
  haskellProject = haskell-nix.cabalProject {
    src = generatedFiles;
    compiler-nix-name = "ghc984";
  };
  
in haskellProject.shellFor {
  packages = p: [ p.${project.neoConfig.name} ];
  
  tools = {
    cabal = "latest";
    haskell-language-server = "latest";
    fourmolu = "latest";
    hlint = "latest";
  };
  
  shellHook = ''
    echo "NeoHaskell development environment ready!"
    echo "Project: ${project.neoConfig.name}"
    echo "Source: ${project.actualSrcPath}"
  '';
}
```

### 4. Update Cabal Generator for haskell.nix Compatibility

Modify `nix/generators/cabal.nix` to add source-repository-package stanza:

```nix
# In the cabal file generation, add this section before the library stanza:
''
cabal-version: 3.0
name: ${project.name}
version: ${project.version}
description: ${project.description}
author: ${project.author}
license: ${project.license}

-- Source repository for NeoHaskell core dependency
source-repository-package
  type: git
  location: ${neoHaskellSource}
  subdir: core
  tag: HEAD

library
  exposed-modules: ${lib.concatStringsSep "\n    " modules}
  hs-source-dirs: src
  build-depends: 
    base,
    nhcore,
    ${lib.concatStringsSep ",\n    " (lib.mapAttrsToList formatDependency deps)}
  
  default-language: GHC2021
  default-extensions: ${lib.concatStringsSep ", " common.extensions}
  ghc-options: ${lib.concatStringsSep " " common.ghcFlags}

executable ${project.name}
  main-is: Main.hs
  hs-source-dirs: app
  build-depends: 
    base,
    ${project.name}
  default-language: GHC2021
  ghc-options: ${lib.concatStringsSep " " common.ghcFlags}
''
```

### 5. Update CLI Commands

Change the nix expression generation in `cli/src/Neo/Build.hs`:

```haskell
-- Change the function call from lib.buildNeoProject to lib.buildNeoProjectHaskellNix
let buildExpression =
      [fmt|
let
  nhroot = (#{neoHaskellSource});
  lib = import (nhroot + "/nix/lib.nix") {};
in
  lib.buildNeoProjectHaskellNix {  -- Changed from buildNeoProject
    neoJsonPath = "#{neoPathText}";
    neoHaskellSource = #{neoHaskellSource};
    srcPath = "#{rootPathText}/src";
  }
|]
```

Similarly update `cli/src/Neo/Shell.hs`:

```haskell
-- Change from lib.shellForNeoProject to lib.shellForNeoProjectHaskellNix
let shellExpression =
      [fmt|
let
  nhroot = (#{neoHaskellSource});
  lib = import (nhroot + "/nix/lib.nix") {};
in
  lib.shellForNeoProjectHaskellNix {  -- Changed from shellForNeoProject
    neoJsonPath = "#{neoPathText}";
    neoHaskellSource = #{neoHaskellSource};
    srcPath = "#{rootPathText}/src";
  }
|]
```

### 6. Optional: Cross-Compilation Support

Add to `nix/lib.nix` for future cross-compilation features:

```nix
{
  # Existing functions
  buildNeoProjectHaskellNix = import ./project-builder-haskellnix.nix { ... };
  shellForNeoProjectHaskellNix = import ./project-shell-haskellnix.nix { ... };
  
  # Cross-compilation variants
  buildNeoProjectWindows = { neoJsonPath, neoHaskellSource, srcPath ? null }:
    let
      builder = import ./project-builder-haskellnix.nix { 
        pkgs = pkgs.pkgsCross.mingwW64;
        haskell-nix = pkgs.pkgsCross.mingwW64.haskell-nix;
      };
    in builder { inherit neoJsonPath neoHaskellSource srcPath; };
    
  buildNeoProjectStatic = { neoJsonPath, neoHaskellSource, srcPath ? null }:
    let
      builder = import ./project-builder-haskellnix.nix { 
        pkgs = pkgs.pkgsMusl;
        haskell-nix = pkgs.pkgsMusl.haskell-nix;
      };
    in builder { inherit neoJsonPath neoHaskellSource srcPath; };
}
```

## Summary

This migration:

1. **Keeps the existing user interface** - Users still run `neo build` and `neo shell`
2. **Only changes the nix backend** - From `developPackage` to `haskell-nix.cabalProject`
3. **Fixes macOS ARM64** - The main goal
4. **Enables cross-compilation** - As a bonus for future features
5. **Minimal code changes** - Just new nix files and updated function calls in CLI

The key insight is that we're not changing how users interact with NeoHaskell, just the underlying nix implementation that was causing issues on macOS ARM64.

## Performance Optimization: Binary Cache Setup

**IMPORTANT**: The first build with haskell.nix will be extremely slow (potentially hours) because it builds GHC and all dependencies from source. To avoid this, configure IOHK's binary cache.

### Setup Binary Cache

Add these settings to your Nix configuration to use IOHK's pre-built binaries:

#### Option 1: User-level configuration
Create/edit `~/.config/nix/nix.conf`:
```
extra-trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
extra-substituters = https://cache.iog.io
```

#### Option 2: System-level configuration
Edit `/etc/nix/nix.conf`:
```
extra-trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
extra-substituters = https://cache.iog.io
```

#### Option 3: NixOS configuration
Add to `/etc/nixos/configuration.nix`:
```nix
{
  nix.settings.trusted-public-keys = [
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];
  nix.settings.substituters = [
    "https://cache.iog.io"
  ];
}
```

### Why This Matters

- **Without cache**: First build takes hours (building GHC from source)
- **With cache**: First build takes minutes (downloading pre-built binaries)
- **Critical**: This is the #1 issue people have with haskell.nix - always configure the cache first

### Verification

After configuration, restart the Nix daemon:
```bash
# macOS
sudo launchctl kickstart -k system/org.nixos.nix-daemon

# Linux
sudo systemctl restart nix-daemon
```

Then test that cache is working by checking if builds download instead of compile.