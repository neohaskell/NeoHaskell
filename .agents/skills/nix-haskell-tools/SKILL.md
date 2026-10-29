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

## Keep a component REPL open while editing

From the repository root, start the component that owns the code or Hspec example. The development project uses optimization level 0. These commands keep one GHCi process open across source edits:

```sh
nix develop --command cabal repl nhcore-test-core --project-file=cabal.project.dev -v0
nix develop --command cabal repl nhcore-test-service --project-file=cabal.project.dev -v0
nix develop --command cabal repl nhintegrations-test --project-file=cabal.project.dev -v0
nix develop --command cabal repl nhtestbed:nhtestbed-spec-demo --project-file=cabal.project.dev -v0
```

In the test component's GHCi prompt, run a focused Hspec example, edit a loaded module, then reload and rerun it:

```text
:main --match "the example name"
:reload
:main --match "the example name"
:quit
```

For a one-shot check, run `nix develop --command .agents/skills/tdd/scripts/test-match "the example name" nhcore-test-core`. That helper starts a new component REPL on each invocation. A persistent REPL avoids that startup on repeated edits; it does not require a separate `cabal build` between reloads.

If you add a module, change a Cabal stanza, dependency, project file, Template Haskell marker, or generated source, quit and restart the component REPL with its current configuration. `:reload` checks the modules loaded by the current session. It does not replace compiler rejection fixtures or generated consumer checks for Template Haskell and code generation.

Before delivery, run the affected integration and PostgreSQL checks through [NeoHaskell tests](../neohaskell-test/SKILL.md). Run [generated consumer verification](../neohaskell-consumer-verification/SKILL.md) when framework exports, Template Haskell markers, or generated APIs change. Every PR still needs the generated-project contract and full required CI gate; a focused REPL result does not cover them.
