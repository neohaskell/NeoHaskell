---
name: principle-foundational-thinking
description: "Apply before writing logic: choosing core types and data structures, sequencing scaffold-vs-feature work, asking what concurrent actors share. Get the data structures right so downstream code becomes obvious."
---

# Foundational Thinking

Apply this principle when developing NeoHaskell's language, framework, and toolchain internals.

**Structural decisions** protect option value. **Code-level decisions** protect simplicity.

**Data structures first.** Get the data shape right before writing logic. Define core types early, trace every access pattern, and choose structures that match the dominant paths. For framework APIs, define the semantic contract alongside the types, including evaluation and representation behavior when relevant. Typechecking alone does not establish that contract.

At code level, DRY the structure, not every line. Types and data models should converge. Three similar statements still beat a premature abstraction. Prefer explicit over clever. Test behavior and edge cases, not line counts.

**Concurrency corollary.** Before sharing mutable state, identify the invariant concurrent operations must preserve. Choose the existing NeoHaskell concurrency module whose ownership, blocking, atomicity, communication, and lifetime guarantees fit the operation. Read the relevant implementations in `core/concurrency/` before choosing their APIs. Isolate ownership or provide synchronization that preserves the invariant. A read followed by a dependent write may need one atomic operation even when each individual access is safe.

**Scaffold first.** If something helps every later phase, do it first. Ask "does every subsequent phase benefit from this existing?" CI, linting, test infrastructure, and shared types are scaffold. Sequence for option value: setup before features, tests before fixes. Keep commits small and single-purpose.

Each increment should land a coherent abstraction or deepen one that exists. Updating a core type or API can legitimately require coordinated changes to pure consumers, class instances, and code generators. Use compiler feedback to guide those changes, then check the semantic contract. Do not make callers duplicate a framework rule or depend on an undocumented effect order. Put that rule or ordering in the abstraction that owns it.

Consider subtraction before scaffolding. Remove proven dead code, accounting for exported and generated uses before declaring framework code dead.
