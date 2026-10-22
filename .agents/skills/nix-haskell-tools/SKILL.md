---
name: nix-haskell-tools
description: Run Haskell compiler, Cabal, test, doctest, formatter, and other Haskell toolchain commands in NeoHaskell or a generated consumer project.
---

# Nix Haskell tools

Run Haskell toolchain commands through the development shell for the project under test. Use `nix develop --command <tool> <args>` from that project's root. Do not invoke `ghc`, `ghci`, `runghc`, `cabal`, `doctest`, `fourmolu`, `hlint`, or similar tools directly from the host shell.

Run a repository-owned helper that calls Haskell tools through `nix develop --command <path-to-helper> <args>` too. A helper's host-tool fallback does not authorize running it outside Nix.

Examples from the NeoHaskell repository root:

```sh
nix develop --command ghc --version
nix develop --command cabal test
```

For a generated app, change to the generated app's root before running the command. Its generated Cabal and Nix files must exist first. If a task needs another flake, pass that flake to `nix develop` while keeping the tool after `--command`.

If the development shell fails before the tool starts, report the Nix failure as an environment or project-shell failure. Do not retry the Haskell tool outside Nix.
