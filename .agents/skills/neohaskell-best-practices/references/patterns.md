# NeoHaskell implementation patterns

Paths below are relative to the repository root. Read the maintained implementation before applying a pattern; its exact contract matters.

## Data-last functions, pipelines, and explicit parameters

Put configuration and callbacks before the value being transformed. The existing `Array.map`, `Array.takeIf`, `Array.foldl`, `Map.set`, `Result.map`, and `Task.andThen` follow this outer data-last shape. The callback's own argument order is a separate contract: NeoHaskell's `Array.foldl` callback receives the element before the accumulator.

Name the final value in the function definition and thread it explicitly through `|>`:

```haskell
adjustedTotal :: Int -> Array Int -> Int
adjustedTotal adjustment values =
  values
    |> Array.map (\value -> value + adjustment)
    |> Array.foldl (\value total -> total + value) 0
```

`adjustment` configures the operation; `values` is its final data argument. Each callback names its inputs. Partial application inside a pipeline stage supplies leading arguments while the pipeline supplies the final value. It does not justify omitting parameters from the function definition.

Use `|>` for composition, including inside a callback when several transformations are needed. Do not define functions as compositions, aliases to other functions, or partially applied expressions with hidden parameters. Ordinary use of a named helper as a callback remains clear; it does not need an identity lambda around it. Task `do` sequencing is effect sequencing, and record field selection selects an operation; neither is a replacement composition operator.

An existing implementation boundary may have a different argument order. For example, `AtomicVar.modifyWithResult` currently takes its variable before its transition function. Respect that signature. If it needs to be a pipeline stage, use a small named data-last adapter with explicit parameters rather than `flip`; changing the original API requires a coherent caller migration within the task.

Keep a function at one abstraction level. A pipeline should tell one readable transformation; a long chain of unrelated stages or deeply nested lambdas needs named helpers. Use `do` with named `let` bindings when several inputs or intermediate results need to remain visible. Follow the approved dialect skill for explicit `case`/`if`, imports, formatting expressions, and Core vocabulary.

## Boolean predicates and ADT matching

Use ordinary `if`/`then`/`else` when a condition answers yes or no. Use `case` to inspect the constructors of `Maybe`, `Result`, or a domain ADT. Do not use `MultiWayIf` or turn a Boolean predicate into a `case` over `True` and `False`. Regular `else if` chains are available; extract named decisions if the branching becomes hard to follow.

An illustrative boundary helper uses both forms according to what it inspects:

```haskell
validateCount :: Maybe Int -> Result Text Int
validateCount maybeCount =
  case maybeCount of
    Nothing -> Err "Missing count"
    Just count ->
      if count < 0
        then Err "Count must be nonnegative"
        else Ok count
```

The constructor determines whether there is a count. The Boolean predicate checks the count's numeric constraint. Give each kind of decision its corresponding syntax.

## Module names are part of the API

Design the module name, principal type, and exported operation names as one interface. The qualified module supplies the subject; the function supplies the operation. Use names such as `Array.first`, `Array.append`, and `Map.get`, rather than repeating the subject as in `arrayHead` or `appendList`. Read an API at its call sites, including its signatures and pipelines, when choosing those names.

For a type-centered public module, the principal type should match the module's subject name: `Array` exports `Array`, `Map` exports `Map`, and `Text` exports `Text`. Import the type explicitly for signatures and the module qualified for operations:

```haskell
import Array (Array)
import Array qualified
```

Signatures can then use `Array value`, and transformations read as `values |> Array.first` or `values |> Array.map transform`. The same pairing appears in `core/core/Task.hs`, `core/core/Map.hs`, and `core/core/Log.hs`. The subject lives in the module qualifier instead of being repeated in every function name; this naming works together with data-last function design.

## Intentional module APIs and valid states

Organize operations around their owning type or subsystem. Keep one-use helpers near the code that uses them, and export the intended API rather than every representation helper. Extract shared logic when the responsibility is genuinely shared; similar-looking cases can have different boundary contracts.

Constructor visibility is part of an invariant's enforcement. `core/core/Redacted.hs` exports `Redacted` without its constructors and exposes named operations for wrapping and explicit unwrapping. `core/service/Service/Event/StreamId.hs` exports its constructor, so its smart constructor alone cannot protect every construction path. Choose visibility according to the public contract; an ADT intended for caller pattern matching can legitimately expose constructors.

When several fields must become present or absent together, independent `Maybe` fields permit combinations that do not represent a real state. Put the associated fields in one constructor of an ADT instead. Keep `Maybe` for actual independent absence and `Result` when a caller needs a failure reason or a recovery choice. Do not collapse a useful typed failure into `Nothing` or display text before the owning boundary.

Name a mode when an unexplained Boolean would make the caller remember what `True` means. A small ADT can expose the alternatives directly. The existing `MultiTenancyMode = MustHaveUuid | MustNotHaveUuid` in `core/service/Service/CommandExecutor/TH.hs` names two compiler-validation requirements. Choose similarly meaningful names when a framework API has distinct modes. Keep genuine yes/no predicates and clear Boolean fields as `Bool`; do not wrap every Boolean automatically. Boolean checks use `if`/`then`/`else`, while named alternatives use `case`.

## Boundary invariants and shape ownership

`core/service/Service/Event/StreamId.hs` distinguishes an identifier from arbitrary text. Its `fromText` returns `Result Text StreamId` and checks the maximum length. The module also exports the constructor and declares a Generic-backed `Json.FromJSON` instance. Consequently, merely receiving a `StreamId` does not prove that every construction path enforced the length check. When an API promises a constrained value, establish that promise across all ingress paths; do not infer it from the wrapper's name.

The helpers in `core/json/Json.hs` decode text, bytes, and JSON values using the target's `FromJSON` instance. Successful decoding proves what that instance checks. For a new constrained type, make its decoding path use the same semantic validation as its smart constructor. Preserve the established wire format when changing an existing decoder.

`core/schema/Schema.hs` derives `ToSchema` through `Generic`. Its `SInt`, for example, describes a shape without establishing a positive-integer constraint. Keep a type's schema, codec, and promised invariants consistent without treating schema description as validation.

Likewise, `core/core/Int.hs` implements `Int.fromInt64` by clamping out-of-range values. Reuse it for a saturating contract. If exactness is required, check the bounds and return an appropriate failure before converting. A function named “safe” is not evidence that it preserves every source value.

## Services own dependencies; pure logic stays pure

The cache contract in `core/service/Service/SnapshotCache/Core.hs` is a record of operations:

```haskell
data SnapshotCache state = SnapshotCache
  { get :: EntityName -> StreamId -> Task Error (Maybe (Snapshot state)),
    set :: Snapshot state -> Task Error Unit,
    delete :: EntityName -> StreamId -> Task Error Unit,
    clear :: Task Error Unit
  }
```

`Service.SnapshotCache.InMemory.new` allocates a `ConcurrentMap` once and closes each operation over its private store. Its clients use the service record. `Service.EntityFetcher.Core.newWithCache` receives an `EventStore`, a `SnapshotCache`, initial state, and a pure reducer; its returned fetch operation owns that wiring. Follow this division when adding a capability: identify which constructor owns state and dependencies, which operation owns effects, and which transform can stay pure. Where a service acquires resources, include their release in the owning lifecycle, as `EventStore.close` does.

`core/service/Service/Command/Core.hs` expresses command signatures using `EntityOf`, `EventOf`, and `EntityIdType`. Reuse those relationships when extending framework APIs instead of declaring another unrelated record with the same fields. For an intentionally distinct transport shape, define an explicit conversion owned by that boundary.

## Execution, evaluation, and concurrency

`core/core/Task.hs` wraps deferred effects. `Task.asResult` still returns a `Task`; it handles the typed error channel when executed. It does not catch underlying synchronous exceptions. `Task.asResultSafe` additionally catches those exceptions and converts errors to text, while rethrowing asynchronous cancellation. Choose it at a boundary that owns that error conversion, such as a worker loop, rather than erasing typed errors throughout the call chain. `Task.finally cleanup work` installs cleanup around execution; its typed cleanup errors are discarded, so handle them explicitly if the lifecycle contract needs to report them.

`core/service/Decider.hs` separates `Decision` construction from `runDecision`, which uses a `DecisionContext` to execute capabilities. A test of resulting events must run the interpreter. A persistence claim additionally needs the storage operation and an observable read or notification at the claimed completion boundary.

Read the relevant existing concurrency implementation before selecting an operation:

| Need | Existing behavior to account for |
|---|---|
| A dependent update to one variable | `AtomicVar.modifyWithResult` computes the new value and returned result inside one transaction. Separate `peek` and `set` calls do not provide that combined guarantee. |
| A blocking handoff or protected value | `ConcurrentVar.get` takes the value; `peek` reads it without taking it. `set` blocks when full; replacement and modification have separate APIs. |
| Producer/consumer communication | `Channel.newBounded` rejects nonpositive capacity and blocks writers when full. `Channel.new` is unbounded. Choose capacity and completion behavior at the communication owner. |
| A scoped background task | `AsyncTask.process` scopes the worker with `withAsync`. `AsyncTask.run` returns a handle; its owner must arrange observation and shutdown. `race` cancels the losing task. |
| A stream result | `Stream.readNext` blocks until a message, distinguishes end from error, and consumes a message. `Stream.consume` and `toArray` consume through completion; a stream handle alone is not evidence that its output was processed. |

For pure collection changes, inspect the wrapper too. `Array.foldl` and `LinkedList.foldl` pass the element before the accumulator and use a strict left fold underneath. That does not recursively force every field of an accumulator. Check the evaluation depth the real consumer requires; do not add full forcing indiscriminately or assume a renamed fold preserves demand behavior.

## Generated interfaces and useful tests

`core/service/Service/TH/Boilerplate.hs` checks for existing instances before generating its Show, Generic, FromJSON, and ToJSON boilerplate. Each marker still owns its additional declarations: inspect the marker rather than assuming every generated instance has the same duplicate-handling behavior.

For a generator change, compile a consumer that uses the affected generated interface. Include an existing custom instance when preservation of that instance is the claim. `core/test/Service/CommandExecutor/TH/AlreadyDerivedCommandFixture.hs` is an existing consumer fixture. Successful compilation can establish that declarations are available without conflicting instances; runtime codec or schema behavior needs its own assertions through those interfaces. A positive fixture alone does not establish rejection of invalid declarations.

`core/testlib/Test/CompileTime.hs` exposes `shouldNotTypecheck` for deliberately deferred type-error tests and `shouldTypecheck`, which forces its argument. Keep deferred errors scoped to the intended negative fixture. Do not use a crashing placeholder as evidence of successful typechecking, and distinguish a compiler rejection from an unrelated runtime exception.

For service behavior, `core/testlib/Test/Service/SnapshotCache/InMemory/Spec.hs` performs a real cache write and checks the state and position read back through the service. Use this observable boundary when testing a refactor; checking only a constructed snapshot never exercises storage.

For properties, follow [Test Behavior](../../principle-test-behavior-not-implementation/SKILL.md): register QuickCheck properties with the owning Hspec suite and keep counterexamples reproducible. Examples worth considering are collection laws, preservation of a validated invariant under operations, or a service operation sequence checked against a simple independent model. Cover deliberate rejection cases as well as valid inputs. Pair codec round trips with independently specified wire examples so matching encoder/decoder mistakes cannot pass together. `QuickCheck.generate` alone only creates data; it does not run a property.

## Framework diagnostics

`core/core/Log.hs` writes JSON records with time, level, message, and available call-site/context fields. Its framework-internal scope API takes `Array (Text, Text)` and a `Task`. The command executor uses it for component, stream, entity, and command context:

```haskell
Log.withScope [("component", "CommandExecutor"), ("streamId", streamIdText)] do
  Log.debug "Executing command"
```

The scope is keyed by thread identity and restores the prior context through `Task.finally`. Do not assume a spawned thread inherits it; establish the needed context at the worker's own boundary. Keep identifiers useful and avoid placing secrets or raw payloads in the context. `core/core/Redacted.hs` hides wrapped values in `Show` and deliberately has no `ToJSON` instance; unwrapping a value for a diagnostic defeats that protection.

For compile-time diagnostics, `Service.CommandExecutor.TH.buildErrorMessage` names the offending function and presents expected/current signatures and a repair hint. Preserve this ability to act on an error when changing framework validation.
