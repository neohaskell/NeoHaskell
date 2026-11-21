---
name: neohaskell-language-extensions
description: Keep established Haskell language-extension defaults consistent across Cabal components and their tools. Use before adding, removing, or reviewing LANGUAGE pragmas, Cabal extensions, or compiler/linter extension mismatches.
---

# Keep language extensions with their owner

Keep established project-wide extensions in the Cabal configuration. Synchronize
those shared defaults across the components that use the same dialect. Preserve
intentional component-specific settings. Synchronizing defaults does not authorize
adding a novel extension globally.

1. Inventory the repository's Cabal files, imported common stanzas, component
   overrides, conditionals, source pragmas, and effective compiler flags. Include
   maintained starter and testbed packages. Record the effective settings for
   each affected module and build configuration before editing.
2. Identify which settings are established shared defaults and which serve a
   particular component or compiler contract. Correct drift in shared Cabal
   defaults. Check actual uses before moving or removing a component-specific
   extension. Do not make every extension list identical by unioning them.
3. Remove a redundant source pragma only after confirming that every supported
   build path for that module supplies the same setting. Retain a source pragma
   for a justified module-specific setting, a deliberate opt-out, or a standalone
   compiler fixture's required tool contract. Record that reason in verification.
   A lint workaround alone does not justify repeating a Cabal default in source.
4. Compare compiler settings with the parser settings used by HLint and other
   affected tools. Correct mismatches at the owning tool configuration or
   invocation. Use [static-check verification](../neohaskell-code-guidelines/references/static-checks.md)
   to prove valid code passes and a meaningful invalid control still fails.
   Never enable `Strict`, remove a necessary lazy pattern, or weaken a lint gate
   merely to silence a parser mismatch.
5. Compile affected components with the maintained toolchain and run behavior
   checks for demand, exceptions, or generated code when those contracts can
   change. Record the old and new effective settings and any unavailable build
   configurations. Use [consumer verification](../neohaskell-consumer-verification/SKILL.md)
   when starter inputs or generated app contracts change.

In an illustrative library, a shared Cabal stanza enables `Strict` for its
library and tests. Adding `{-# LANGUAGE Strict #-}` to every module duplicates
that policy. A standalone rejection fixture compiled outside Cabal can instead
need an explicit extension setting to reproduce its intended diagnostic. Check
that fixture's invocation before removing the setting.
