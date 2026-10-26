---
name: neohaskell-best-practices
description: Implement and review NeoHaskell framework, language, and toolchain internals. Use for Haskell API design, boundary parsing, service records, effects, concurrency, derivation, and their tests in this repository; not as an application-building guide.
---

# NeoHaskell best practices

Apply [NeoHaskell code guidelines](../neohaskell-code-guidelines/SKILL.md) for the dialect and [Type System Discipline](../principle-type-system-discipline/SKILL.md) for type design. Ground API choices in the maintained module and its callers. Follow AGENTS.md for the shared workflow.

| Rule | Summary |
|---|---|
| Data last | Design transformation APIs with configuration, predicates, and callbacks first and the primary transformed value last, so callers can pipe the value through stages. Read existing signatures; adapt explicitly instead of assuming every current API has this order. |
| Pipelines for composition | Compose transformations with `\|>` and read them in execution order. Use it instead of `(.)`, `$`, `<<`, `>>`, or nested transformation calls. Ordinary application, constructors, record field access, and Task sequencing retain their own meaning. |
| Explicit function parameters | No point-free definitions or aliases: name every declared input, including the final piped value. Name anonymous callback inputs explicitly too. Passing a named helper as a callback and partially applying leading arguments in a pipeline stage are valid. |
| Named, readable steps | Give each helper one clear job and name it by intent. Give top-level helpers explicit type signatures. Split a pipeline when its steps stop describing one coherent transformation; use named intermediates when branches or several inputs need context. Use explicit lambdas instead of clever operator sections or `flip`. |
| Boolean conditions and ADT cases | Use ordinary `if condition then ... else ...` for Boolean checks and `case` for constructors of `Maybe`, `Result`, and other ADTs. Use regular `else if` chains when needed. Do not use `MultiWayIf` or `case` on `True`/`False`. |
| Algebraic data types | Put each valid case and its required data in a constructor. If fields become present or absent together, encode those states with constructors; keep genuinely independent absence as `Maybe`. Use named ADT modes when Boolean parameters hide domain meaning; retain `Bool` for genuine predicates and clearly named Boolean fields. Match constructors explicitly when adding a case should force a caller decision; preserve the build's incomplete-pattern checks. |
| Module-qualified API names | Design the module, type, and operation names together. The module supplies the subject: `Array.first`, `Array.append`, and `Map.get`; avoid repeating that subject in function names. Match a type-centered public module and its principal type, enabling paired imports such as `import Array (Array)` and `import Array qualified`. |
| Intentional module APIs | Group helpers around the owning type or subsystem and export its intended contract. Keep implementation helpers private. Restrict constructor access when it protects a concrete invariant; expose constructors when caller pattern matching belongs to the contract. |
| Semantic newtypes | Prefer `newtype` over `type` for an abstraction with its own meaning, operations, or ownership. Follow [Type System Discipline](../principle-type-system-discipline/SKILL.md) for module ownership, transparent-alias exceptions, and construction guarantees. |
| Constructive, total representations | Prefer a shape that carries the required guarantee, such as a head plus rest or pairs. Keep ordinary collections when the contract is total; `Array.first` and `LinkedList.head` already represent absence with `Maybe`. A start plus duration still needs a nonnegative-duration invariant. |
| Foreign results | Convert a GHC or foreign `Either` with `Result.fromEither` before branching, then match `Ok` and `Err`. Keep raw `Left`/`Right` matching inside the owning Result conversion implementation. Preserve the original error value and evaluation behavior. |
| Boundary parsing and errors | Decode external `Text`, `Bytes`, or `Json.Value` into the owning type, then establish semantic constraints the decoder does not check. Use `Result` for explained pure failure and `Task` for effects; preserve typed errors until an adapter owns their presentation. Follow [Boundary Discipline](../principle-boundary-discipline/SKILL.md). |
| Authoritative shapes | Reuse owning types, type families, and existing Generic/Template Haskell derivation. `ToSchema` derives a schema from a Haskell type; it is not an input validator. Keep custom decoding when the contract requires it, and check what a marker already generates before adding instances. |
| Necessary declarations | Follow [Minimize Reader Load](../principle-minimize-reader-load/SKILL.md). Keep derived capabilities, imports and annotations for demonstrated constraints or operations. Remove automatically supplied and unused boilerplate from consumer examples. |
| Checked conversions | Establish the semantic precondition as well as representation compatibility. Read conversion behavior: `Int.fromInt64` clamps, so it does not prove an exact conversion. Do not replace an unexplained partial path with `panic` or an unsafe conversion. |
| Records and service ownership | Use named records for coherent configuration or easily confused arguments; retain simple positional and pipeline APIs where clear. Services are records of functions. Their constructors own captured dependencies, mutable state, and resource lifetimes; pure callers should not carry synchronization machinery. |
| Pure logic and deferred effects | Keep transformations and decision construction separate from effect execution. Constructing or showing a `Task` does not run it; constructing a `Decision` does not execute its capabilities. Compose with the existing interpreters and runners at their owning boundaries. |
| Concurrency at the owner | Identify the shared invariant, then choose the existing module in `core/concurrency/` for its atomicity, blocking, communication, and lifetime semantics. Put dependent reads and writes in one suitable atomic operation. Use [Foundational Thinking](../principle-foundational-thinking/SKILL.md). |
| Evaluation is part of behavior | Check the wrapper's argument order and demand behavior before substituting a fold, collection, or stream operation. Preserve required laziness and bounded resource use. Execute effects and consume the result required by the caller; use [Benchmark Checklist](../benchmark-checklist/SKILL.md) for performance claims. |
| Behavioral and compiler evidence | Exercise real service operations and assert observable results. Use meaningful QuickCheck properties in the owning Hspec suite. Compile consumer fixtures for generated APIs and type guarantees; run generated behavior when the claim is behavioral. Follow [Test Behavior](../principle-test-behavior-not-implementation/SKILL.md) and [Prove It Works](../principle-prove-it-works/SKILL.md). |
| Framework diagnostics | Use `Log.debug`, `Log.info`, `Log.warn`, or `Log.critical` for runtime diagnostics. Framework internals can use `Log.withScope` for structured context. Include useful operation identifiers, preserve redaction, and make compiler/generator failures explain the expected contract and repair. |

Run compiler and test commands through `scripts/with-toolchain <command> <arguments...>`. The wrapper uses the checked shell witness, enters the project Nix shell when needed, and labels an unpinned host fallback. It preserves argument boundaries and command exit status.

Use [NeoHaskell tests](../neohaskell-test/SKILL.md) for current suite, doctest and testbed entrypoints. Capture runtime or compiler contracts with [Regression evidence](../regression-evidence/SKILL.md). For event-store behavior, select relevant [controlled-fault regression recipes](../event-store-regression/SKILL.md) alongside the broader maintained tests.

For framework export, TH marker or generated-API changes, follow
[NeoHaskell consumer verification](../neohaskell-consumer-verification/SKILL.md)
before implementation and verification. It owns the starter and testbed migration
contract. Apply the [application example rules](../neohaskell-code-guidelines/SKILL.md#application-examples)
to consumer code, including fixtures that represent an app.

For new modules, follow [module documentation](../neohaskell-module-docs/SKILL.md). For extension changes or compiler/linter mismatches, follow [language extensions](../neohaskell-language-extensions/SKILL.md).

Read [readable services](references/readable-services.md) when changing service orchestration, internal outcomes, exception ownership, JSON codecs, or temporary test resources.

Read [patterns](references/patterns.md) when designing module APIs, functions, and pipelines or working on boundary invariants, service ownership, deferred execution, concurrency, generated consumers, or diagnostics.
