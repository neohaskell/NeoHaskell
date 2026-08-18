# Executable Slices and Constrained Local Generation

Status: design proposal

Last updated: 2026-08-18

Tracking epic: [#824](https://github.com/neohaskell/NeoHaskell/issues/824)

## Implementation roadmap

Implement this architecture as a sequence of vertical, independently reviewable changes. Do not begin with a horizontal rewrite of either the framework or `neo`.

1. **Reconcile the existing Event Model and tooling backlog with this document.**
   - Treat [#573](https://github.com/neohaskell/NeoHaskell/issues/573), [#578](https://github.com/neohaskell/NeoHaskell/issues/578), [#579](https://github.com/neohaskell/NeoHaskell/issues/579), [#580](https://github.com/neohaskell/NeoHaskell/issues/580), [#581](https://github.com/neohaskell/NeoHaskell/issues/581), [#582](https://github.com/neohaskell/NeoHaskell/issues/582), [#583](https://github.com/neohaskell/NeoHaskell/issues/583), [#584](https://github.com/neohaskell/NeoHaskell/issues/584), and [#585](https://github.com/neohaskell/NeoHaskell/issues/585) as prior design material, not an implementation-ready plan.
   - Preserve their useful work on structural declarations, type relationships, integrations, rules, wiring, test generation, visual export, and reference-application migration.
   - Remove assumptions that require a central `EventModel.hs`, an editable `event-model.json`, public `Service` registration, type-only behavioral rules, or one generated technical shape at a time.
   - Close [#819](https://github.com/neohaskell/NeoHaskell/issues/819) through deletion of the shelved full-port plan. The new Haskell semantic-engine issue [#827](https://github.com/neohaskell/NeoHaskell/issues/827) is deliberately narrower than that shelved rewrite: the production native launcher and operational CLI remain while semantic capabilities move behind a proven protocol.
   - Reconcile the broad missing-code idea in [#49](https://github.com/neohaskell/NeoHaskell/issues/49) with the restricted synthesis contract in [#831](https://github.com/neohaskell/NeoHaskell/issues/831); do not ship unrestricted `implement` behavior.
   - Coordinate the semantic engine with the VSCode/HLS work tracked by [#493](https://github.com/neohaskell/NeoHaskell/issues/493) so both clients can consume the same typed project service rather than creating competing compiler sessions.
   - Keep [#807](https://github.com/neohaskell/NeoHaskell/issues/807) and [#785](https://github.com/neohaskell/NeoHaskell/issues/785) as continuous CLI-quality and onboarding gates.
2. **Stabilize semantic identity.** Complete [#801](https://github.com/neohaskell/NeoHaskell/issues/801) so slices and their nodes have one versionable identity independent of source position or display text.
3. **Define executable local slices.** Implement [#825](https://github.com/neohaskell/NeoHaskell/issues/825), reconciling [#573](https://github.com/neohaskell/NeoHaskell/issues/573), [#578](https://github.com/neohaskell/NeoHaskell/issues/578), and [#580](https://github.com/neohaskell/NeoHaskell/issues/580). Prove state-change, state-view, translation, multi-entity-query, incomplete-implementation, example, counterexample, and property declarations.
4. **Prove one runtime vertical slice without public `Service`.** Implement [#826](https://github.com/neohaskell/NeoHaskell/issues/826) together with the applicable compile-time validation from [#581](https://github.com/neohaskell/NeoHaskell/issues/581). One behavior must run from `Application.withSlices`, derive its entity runtime, and pass an acceptance test before broad migration begins.
5. **Introduce the Haskell semantic engine through a strangler boundary.** Implement [#827](https://github.com/neohaskell/NeoHaskell/issues/827). Keep the production native launcher and route one read-only command through a version-matched Haskell engine with golden parity.
6. **Make the source model inspectable while the application is red.** Implement [#828](https://github.com/neohaskell/NeoHaskell/issues/828). The closed Slice DSL, not implementation heuristics or cached JSON, must provide authoritative topology and source-span diagnostics.
7. **Add typed enrichment and exact source edits.** Implement [#829](https://github.com/neohaskell/NeoHaskell/issues/829), extending [#581](https://github.com/neohaskell/NeoHaskell/issues/581) with a warm GHC session, resolved identities, `.hie`-level relationships where useful, and exact-printing code actions.
8. **Establish independent behavioral oracles before model-assisted implementation.** Implement [#833](https://github.com/neohaskell/NeoHaskell/issues/833), refining [#583](https://github.com/neohaskell/NeoHaskell/issues/583). Concrete outcomes come from approved contracts; boundary properties must reject declared mutants.
9. **Move generation to approved slices and transactional writes.** Implement [#830](https://github.com/neohaskell/NeoHaskell/issues/830), revising [#809](https://github.com/neohaskell/NeoHaskell/issues/809). Deterministic command, event, query, and integration scaffolds remain internal building blocks rather than the primary user workflow.
10. **Implement atomic typed-hole synthesis.** Implement [#831](https://github.com/neohaskell/NeoHaskell/issues/831). Start with preconditions and field mappings, then earn support for decider branches, projections, fixtures, generators, shrinkers, and property bodies independently.
11. **Integrate and evaluate local inference.** Implement [#832](https://github.com/neohaskell/NeoHaskell/issues/832). Benchmark MiniCPM5-1B as one baseline behind a provider-neutral interface; do not make the model name part of the architecture or grant autonomy without per-module evidence.
12. **Render the same source-derived model.** Complete the export work in [#584](https://github.com/neohaskell/NeoHaskell/issues/584), then implement [#834](https://github.com/neohaskell/NeoHaskell/issues/834) so the IDE shows source, typed, implementation, test, and drift states without becoming another model editor.
13. **Migrate and prove the complete consumer workflow.** Implement [#835](https://github.com/neohaskell/NeoHaskell/issues/835), revising or superseding [#585](https://github.com/neohaskell/NeoHaskell/issues/585). Preserve acceptance behavior, deployed identities, CLI compatibility, packaging, and the clean-machine 600-second onboarding SLO.

The dependency spine is:

```text
#801
  -> #825
  -> #826
  -> #827
  -> #828
  -> #829
  -> #833
  -> #830
  -> #831
  -> #832
  -> #584
  -> #834
  -> #835
```

Some work can overlap after interfaces are fixed: the native launcher protocol can begin after the Slice DSL boundary is known; visual export can begin after the source-model schema is stable; deterministic generation primitives can be developed before local inference. The vertical proof and compatibility gates cannot be skipped.

## Executive summary

NeoHaskell should make the application source itself the Event Model. A user should not maintain an implementation, a separate model file, and a separate runtime registry that can disagree. A local Haskell Slice declaration should simultaneously describe the intended behavior, compose the runtime components, provide an inspectable model for `neo`, and define the boundary for generation and testing.

The slice is the primary unit of:

- product behavior;
- Event Modeling;
- source locality;
- runtime composition;
- generation;
- tests and properties;
- progress and readiness;
- inspection and visualization;
- review and delivery.

Commands, events, entities, queries, and integrations remain first-class typed concepts, but they are components of a behavior rather than the primary public generation surface.

`Service` should cease to be a user-facing modeling concept. It was introduced as a grouping around entity command execution, but its name suggests DDD services, application services, deployment units, or bounded contexts. The framework still needs runtime grouping by entity and event stream; it should derive that grouping internally from slices and expose entities explicitly when state is folded from events.

`neo` should use a Haskell semantic engine because its central work increasingly depends on the real Haskell syntax tree, type relationships, source spans, exact edits, and a warm GHC session. A small native launcher should remain responsible for bootstrap, distribution, compatibility selection, process supervision, and stable terminal behavior. This is a strangler migration, not a rewrite flag day.

Generation should follow hole-driven development. Deterministic scaffolding creates explicit typed holes. Type-directed search, local API indexes, nearby examples, and bounded expression enumeration reduce each hole to a small synthesis problem. A small local model may rank or compose candidates using an English rule, examples, counterexamples, and a closed function vocabulary. GHC, tests, properties, and mutation checks decide whether the proposal is accepted. The model never receives unrestricted repository access and never changes product semantics.

## Goals

- Make the application source the sole authoritative Event Model.
- Provide code locality comparable to a well-structured Spring Boot application while retaining typed Event Modeling and event-sourced semantics.
- Allow slices to be declared before every implementation type or function is complete.
- Keep meaningful model inspection available while unrelated application code does not compile.
- Generate and complete behavior in small, measurable, atomic units.
- Make deterministic mechanisms contribute most of the generation capability.
- Use local inference only where semantic ranking or bounded expression composition is required.
- Keep the workflow useful offline and on a low-end 16 GB development machine.
- Preserve CLI, packaging, release, compatibility, and onboarding contracts throughout migration.

## Non-goals

- A general autonomous coding agent inside `neo`.
- A model that explores or edits arbitrary repository files.
- A second authoritative Event Model in JSON, a database, GHCi state, or a generated package.
- One model call that generates a complete slice, decider, query, test suite, or application.
- A promise that one specific local model is sufficient before measuring it on NeoHaskell holes.
- Replacing frontier reasoning for product semantics before a smaller model proves equivalent quality.
- Requiring every tooling operation to become an event-sourced domain workflow merely because the engine is written in NeoHaskell.
- Requiring one slice to contain exactly one command.
- Requiring a query to project exactly one entity.

## Architectural principles

### The application is the model

The source declaration used for runtime composition is also the declaration parsed by tooling. There is no synchronization protocol between “the model” and “the implementation” because they are not separate editable artifacts.

Derived artifacts are allowed when they have one-way authority:

- diagram JSON;
- IDE layout;
- caches;
- generated import registries;
- documentation exports;
- compatibility manifests.

They may be regenerated or discarded. Editing them cannot change domain semantics.

### Locality over central registries

A slice declaration belongs next to the behavior it composes. A bounded context may expose a deterministic collection of slices, but the semantic declarations remain local.

```text
src/Application/Orders/
  Slices/PlaceOrder.hs
  Slices/ViewFulfilment.hs
  Commands/PlaceOrder.hs
  Events/OrderPlaced.hs
  Queries/FulfilmentStatus.hs
  Integrations/ReserveStock.hs
```

This layout is illustrative, not mandatory. The invariant is semantic locality, not a fixed folder spelling.

### Slice scope over technical shape scope

The public request is “add or complete this behavior,” not “generate an event file.” A slice may contain:

- a state-changing command and its possible events;
- a state view fed by several entities;
- an inbound translation from an external fact to a command;
- an outbound reaction from an event to an external operation or another command;
- several nodes needed to deliver one coherent behavior.

Technical generators remain useful internal compilers for deterministic boilerplate.

### Source inspection and typed validation are separate states

A syntax-valid Slice declaration should remain inspectable when a decider or projection is broken. Typed validation adds stronger evidence when GHC can resolve the relevant modules. Tooling reports both instead of collapsing them into one green/red state.

### Semantic reasoning and mechanical compilation are separate

Frontier reasoning owns high-impact semantic decisions:

- the user desire and PRD;
- slice boundaries;
- stream ownership;
- invariants and preconditions;
- information completeness;
- integration direction;
- examples, counterexamples, boundaries, and expected outcomes.

Deterministic tooling owns:

- stable IDs and references;
- structural validation;
- scaffolds and imports;
- typed candidate retrieval;
- registration derivation;
- test skeletons and assertions from approved contracts;
- compilation, execution, and mutation gates.

A small local model operates only between those layers on bounded holes.

## Composition model

```text
Application
  -> bounded contexts
  -> chapters
  -> slices
  -> commands, events, entities, queries, integrations, translations
```

Bounded contexts and chapters organize behavior. Slices deliver behavior. Entities own event-stream state where needed. Queries project read models from zero, one, or multiple entities. Integrations connect slices to external systems or to other commands without erasing causal direction.

### State-change slice

A possible API shape is:

```haskell
slice :: Slice
slice =
  Slice.stateChange @"PlaceOrder"
    |> Slice.entity @OrderEntity
    |> Slice.command @PlaceOrder
    |> Slice.produces @OrderPlaced
    |> Slice.triggers @ReserveStock
    |> Slice.precondition @"customer-is-active"
      "The customer must be active when the order is placed"
```

The exact combinator names are not fixed by this document. The required properties are:

- the declaration is valid Haskell;
- the vocabulary is deliberately closed;
- semantic relationships are explicit;
- source parsing does not require evaluating arbitrary functions;
- GHC can prove typed relationships when implementation is available.

### State-view slice and multi-entity queries

A query is not owned by one entity. It may combine several event streams:

```haskell
slice :: Slice
slice =
  Slice.stateView @"FulfilmentStatus"
    |> Slice.query @FulfilmentStatus
    |> Slice.projects @OrderEntity
    |> Slice.projects @ShipmentEntity
    |> Slice.projects @PaymentEntity
    |> Slice.fedBy @OrderPlaced
    |> Slice.fedBy @ShipmentDispatched
    |> Slice.fedBy @PaymentCaptured
```

Its type relationship is a list:

```haskell
type instance EntitiesOf FulfilmentStatus =
  '[OrderEntity, ShipmentEntity, PaymentEntity]
```

The runtime must subscribe the projection to every declared entity/event source. Source inspection and the IDE must display every source. Validation must compare the Slice declaration against `EntitiesOf query`, not invent a singular `EntityOf query` relationship.

Every `fedBy @Event` declaration must resolve the event's owning entity through `EventOf`/`EventVariantOf` and prove that the owner is present in the complete `EntitiesOf query` list. Source validation rejects invalid Slice syntax and unresolved closed-vocabulary references without claiming typed ownership. Typed validation resolves `EventOf`/`EventVariantOf` owners and proves `EntitiesOf` membership. Runtime subscription derivation is allowed only for `typed-valid` slices; `typed-validation-blocked` slices remain inspectable but cannot run. Valid feeds must remain complete and visible to runtime wiring, source inspection, and IDE rendering.

### Translation slice

Cross-boundary behavior should preserve the distinction between an observed external fact and an application command:

```haskell
slice :: Slice
slice =
  Slice.translation @"RecordPaymentResult"
    |> Slice.triggeredBy @PaymentProviderWebhook
    |> Slice.command @RecordPaymentResult
    |> Slice.entity @PaymentEntity
    |> Slice.produces @PaymentCaptured
    |> Slice.produces @PaymentRejected
```

### Incomplete slices

A slice may be declared while its implementation contains holes or while some target declarations have not been completed. Tooling must represent at least:

```text
declared
source-valid
source-invalid
implementation-missing
typed-validation-blocked
typed-valid
typed-invalid
tests-missing
tests-red
ready
```

The source declaration is still useful for planning, graph inspection, next-slice selection, and generation.

## Replacing public `Service`

`Service` currently groups commands that execute against a shared entity/event type and wires the corresponding runtime. That grouping remains operationally necessary, but it is not a useful public domain abstraction.

The public application API should move toward:

```haskell
app :: Application
app =
  Application.new
    |> Application.withConfig @ApplicationConfig
    |> Application.withEventStore makeEventStore
    |> Application.withTransport WebTransport.server
    |> Application.withSlices ApplicationSlices.all
```

`Application.withSlices` derives:

- command handlers and routes;
- entity reconstruction;
- event-store bindings;
- query subscriptions and stores;
- outbound integration subscriptions;
- inbound integration workers;
- internal command dispatch;
- transport exposure;
- lifecycle ownership.

Internally, the framework may construct an `EntityRuntime`, `CommandRegistry`, or equivalent. It groups commands by relationships such as:

```haskell
EntityOf PlaceOrder ~ OrderEntity
EventOf OrderEntity ~ OrderEvent
```

The name of this internal structure is an implementation choice. It must not reappear as a public “service” that users manually keep synchronized with slices.

Migration must be staged:

1. introduce slices beside existing registrations;
2. compare derived and manual registrations;
3. run both through contract tests where possible;
4. switch application composition to slices;
5. deprecate manual/public service registration;
6. remove it only after reference and generated applications migrate.

## One source of truth and derived exports

An editable `event-model.json` must not remain canonical. It creates the possibility of three divergent truths:

```text
model JSON
application registration
implementation types/functions
```

The Slice source replaces the first two. JSON remains useful only as an export format:

```text
Slice source
  -> source model
  -> typed enrichment when available
  -> diagram/export JSON
```

The export should carry stable semantic IDs and source references. It should state whether each relationship is declared, type-verified, implemented, and tested. Consumers must not write structural changes back into JSON.

IDE layout is presentation state. It may store positions, collapsed groups, colors, or viewport information keyed by stable IDs. It cannot create commands, events, edges, or rules.

### Migrating current JSON mutation paths

The existing `workspace/healEventModel` and direct-write IDE paths mutate `event-model.json`. They cannot coexist indefinitely with the export-only contract. During migration they must be treated as legacy compatibility behavior and must not become an input to Slice runtime composition.

Before JSON is declared export-only in a released workflow:

1. move healing and write operations to exact Slice source spans through semantic-engine code actions;
2. make diagram JSON generation one-way and read-only;
3. reject direct JSON writes with an actionable migration diagnostic;
4. include the source revision or relevant file hashes in every source-span code-action request, revalidate them under the project write lock, and reject stale actions with an actionable conflict diagnostic;
5. add parity tests proving that the source edit produces the same intended model change and regenerated export, including an intervening-edit conflict case;
6. remove the legacy JSON mutation routes only after supported clients use the source-edit flow.

### Project write coordination

Every writer, including IDE actions, CLI generation, `workspace/healEventModel` compatibility behavior, and semantic-engine code actions, must coordinate through one OS-visible advisory lock rooted at the project, such as `.neo/locks/project-write.lock`. In-process mutexes are insufficient because writers may run in separate processes.

The lock contract must define:

- exclusive ownership for the complete revalidate-and-commit critical section;
- owner metadata containing process identity, operation ID, start time, and engine/CLI version for diagnostics only;
- a bounded acquisition timeout with cancellation and an actionable report of the current owner;
- automatic kernel release when the owning process exits;
- stale metadata cleanup only after successfully acquiring the OS lock, never by deleting a lock held by another process;
- one implementation or protocol shared by all writer surfaces rather than independent lock files;
- revalidation of requested revision or file hashes after lock acquisition and before any write.

Concurrency tests must start two real processes, force an intervening edit while one waits, and prove that only one writer commits while the stale action is rejected without overwriting either change.

## `neo` architecture

### Why the semantic engine should be Haskell

The tool must understand:

- real Haskell parsing and extensions;
- imports, aliases, qualified names, and type applications;
- source spans and exact edits;
- type families such as `EntityOf`, `EventOf`, and `EntitiesOf`;
- compiler diagnostics;
- `.hie` symbol/reference data where useful;
- the same GHC version used by the application.

Reimplementing that semantics in Rust would preserve the current heuristic gap or require a second Haskell tool behind the Rust implementation anyway. The semantic center therefore belongs in Haskell.

### Why keep a native launcher

The first executable must work before a project compiles. It must reliably:

- find the project root;
- read the compatibility revision;
- select or install the matching engine;
- verify the engine executable against a signed manifest or approved cryptographic digest from a configured trusted source before first execution;
- reject missing, untrusted, or mismatched engine artifacts and roll a failed installation back to the previous known-good engine;
- start or reconnect to a persistent engine;
- supervise process groups and signals;
- forward args, stdin, stdout, stderr, and exit codes;
- preserve interactive and CI behavior;
- run bootstrap and recovery commands;
- report missing toolchains or incompatible protocols clearly.

The launcher may remain Rust. It should become small and semantically boring rather than immediately disappearing.

### Persistent engine

Repeated commands should reuse a project-scoped engine:

```text
native neo launcher
  -> project engine socket
  -> version-matched Haskell engine
       -> source model
       -> warm GHC session
       -> symbol and API indexes
       -> generators and code actions
       -> IDE transport
```

A persistent engine amortizes package-graph loading, parsing, renaming, typechecking, and index construction. It must have explicit lifecycle, protocol-version, invalidation, cancellation, and cleanup contracts.

Tooling commands do not automatically need event-sourced persistence. Use NeoHaskell transports and application patterns when they fit, but do not force bounded filesystem operations, compiler invocations, or daemon supervision into fake domain entities merely for dogfooding.

## Two-pass inspection

### Pass 1: source model

The first pass uses the real version-matched Haskell parser over the closed Slice DSL. It should work without typechecking the full application.

It produces:

- bounded contexts, chapters, and slices;
- declared commands, events, entities, queries, integrations, and translations;
- examples, counterexamples, rules, and properties;
- stable identities and dependencies;
- exact source spans;
- syntax and source-model diagnostics.

It validates relationships that require no type information:

- duplicate IDs;
- illegal combinations;
- missing mandatory nodes;
- contradictory slice forms;
- dependency cycles;
- identity collisions;
- malformed examples and rules.

An error in one implementation module must not erase unaffected source declarations.

### Pass 2: typed model

When relevant modules can be loaded, a warm GHC session enriches the source model:

- resolve symbols through imports and aliases;
- prove command/entity/event relationships;
- prove query/entity-list relationships;
- prove integration targets and payload compatibility;
- inspect expected hole types;
- produce candidate symbols from real APIs;
- support exact source edits and impact analysis.

`neo inspect` should report distinct evidence:

```text
source model: valid
contract types: blocked by error in Orders.Decide:42
implementation: failing
last tests: red
```

It must not report “no model” merely because typed enrichment is blocked.

## `neo generate`

The primary workflows should be:

```bash
neo generate "Allow an order to be cancelled before dispatch"
neo generate --slice cancel-order
neo generate --next
```

### Wish to semantic proposal

A frontier model may transform a user desire into a proposed PRD and slice-contract change. This step decides semantics and therefore requires explicit human approval before source mutation.

The proposal should contain:

- affected bounded context/chapter;
- slice kind and stable ID;
- commands and expected events;
- entity/stream ownership;
- query and integration effects;
- preconditions and invariants;
- examples, counterexamples, and boundaries;
- dependencies on existing slices;
- open semantic questions.

The frontier should not hand-author repetitive source IDs, imports, registries, or JSON serialization. `neo` compiles the approved proposal into the Slice DSL deterministically.

### Approved slice to implementation

`neo generate --slice <id>` should:

1. parse the authoritative source model;
2. validate slice readiness and dependencies;
3. create a temporary workspace;
4. generate deterministic local scaffolds;
5. create explicit typed holes;
6. derive test structures from approved contracts;
7. retrieve and synthesize hole candidates independently;
8. compile and run bounded repair loops;
9. run structural, example, property, mutation, lint, and relevant acceptance gates;
10. present semantic and textual diffs;
11. write atomically only after success and approval policy allow it.

`--next` may choose the next implementable slice deterministically from dependency state. It should not reprioritize product work through hidden model judgment.

### Required modes

- deterministic-only;
- fully offline;
- dry-run;
- CI/non-interactive JSON;
- explanation/provenance;
- bounded model-assisted completion;
- resume an existing partial slice.

A failure must state the operation, rejected candidate, expected type, failed examples/properties, final compiler diagnostic, concrete repair options, and confirmation that the real workspace was unchanged.

## Hole-driven development

The unit of local generation is a typed expression hole, not a file or conversational task.

A universal problem shape is:

```haskell
data SynthesisProblem input output = SynthesisProblem
  { name :: Text
  , intent :: Text
  , expectedType :: Type
  , allowedSymbols :: Array Symbol
  , examples :: Array (Example input output)
  , counterexamples :: Array (Example input output)
  , laws :: Array Law
  , maximumExpressionDepth :: Int
  , searchBudget :: SearchBudget
  }
```

Successful synthesis returns either one expression or a ranked candidate set. Failure is explicit and carries a diagnostic:

```haskell
data SynthesisResult
  = Expression HaskellExpression
  | RankedCandidates (Array HaskellExpression)
  | Unresolved Diagnostic
```

The synthesizer cannot add imports, declarations, modules, dependencies, or arbitrary edits. Those are separate typed code actions with separate validators.

### Relationship to Djinn-style synthesis

Type inhabitation answers:

```text
expected type
  -> expressions that inhabit the type
```

Business logic needs another signal because many incorrect expressions are well typed. The additional evidence is:

```text
English intent
+ allowed vocabulary
+ examples
+ counterexamples
+ boundaries/laws
```

The local model provides semantic search guidance or ranking. It does not replace type search, GHC, or executable examples.

### Deterministic first

For each hole:

```text
generate/retrieve type-correct candidates
  -> reject forbidden symbols
  -> run examples
  -> run counterexamples
  -> apply laws and bounds
  -> ask the local model only when candidates remain ambiguous or search explodes
  -> compile
  -> run bounded semantic verification
```

Simple preconditions and mappings may be solved entirely by bounded enumeration. The system should report that no model was needed rather than invoke one for branding.

## Atomic synthesis modules

Each module has its own input schema, output type, corpus, validator, metrics, and autonomy threshold:

- `PreconditionSynthesizer`;
- `DecisionBranchSynthesizer`;
- `EventMappingSynthesizer`;
- `ProjectionSynthesizer`;
- `IntegrationMappingSynthesizer`;
- `FixtureSynthesizer`;
- `GeneratorSynthesizer`;
- `ShrinkerSynthesizer`;
- `PropertyBodySynthesizer`.

Success in one module does not authorize another. A model may be production-ready for field mappings and unacceptable for properties.

### Preconditions

A precondition problem includes:

- entity/state type;
- command/context types;
- English rule;
- closed predicate/function vocabulary;
- at least two positive examples;
- at least one counterexample;
- explicit boundary cases;
- expected Boolean or rejection-result type.

Example hole:

```haskell
canCancel :: OrderEntity -> Timestamp -> Bool
canCancel order now =
  _hole
```

Problem evidence:

```text
Intent:
An order can be cancelled only while it has not been dispatched and
before the cancellation deadline.

Allowed symbols:
- Order.isDispatched :: OrderEntity -> Bool
- Order.cancellationDeadline :: OrderEntity -> Maybe Timestamp
- Time.isBefore :: Timestamp -> Timestamp -> Bool
- Maybe.map :: (a -> b) -> Maybe a -> Maybe b
- Maybe.withDefault :: a -> Maybe a -> a
- not
- (&&)

Positive examples:
- pending order, one hour before deadline -> True
- pending order, one minute before deadline -> True

Counterexamples:
- dispatched order before deadline -> False
- pending order at deadline -> False
- pending order without deadline -> False
```

A possible accepted result is:

```haskell
\order now ->
  not (Order.isDispatched order)
    && order.cancellationDeadline
      |> Maybe.map (\deadline -> Time.isBefore now deadline)
      |> Maybe.withDefault False
```

The expression must parse, use only allowed symbols, typecheck, satisfy every example and counterexample, and survive the relevant property and mutation checks.

### Deciders

Do not ask a model to “implement the decider.” Decompose it into:

- precondition expressions;
- branch ordering;
- rejection mapping;
- event payload mapping;
- accept-existing versus accept-new choice;
- individual transition cases.

An implementation problem consumes already approved preconditions and expected decision tables. It cannot reinterpret or weaken them.

### Event and integration mappings

Typed record mappings are strong early candidates for local synthesis because target fields, source fields, allowed transformations, and examples tightly bound the search. Compile-time field/type errors and example mismatches provide clear feedback.

### Projections

Synthesize each event/entity update branch independently. A multi-entity query does not become one monolithic synthesis task. Each branch has its own source event/entity, previous query state, expected update, and laws such as idempotence or monotonicity where appropriate.

## Test generation and independence

Tests and implementation must not be freely generated from the same unconstrained prompt. That creates correlated errors where a wrong interpretation makes both code and tests green.

The authority chain should be:

```text
frontier proposal + human approval
  -> concrete Slice contract
  -> deterministic test assertions and typed holes
  -> independent atomic implementation synthesis
  -> GHC, examples, properties, and mutations
```

### Example-based tests

The Slice contract contains concrete Given/When/Then or Given/When/Reject scenarios. `neo` deterministically generates test structure and expected outcomes.

The model may synthesize bounded fixtures or adapters, but it may not change:

- the command under test;
- expected event/rejection;
- boundary value;
- assertion polarity.

### Property-based tests

A property contract should declare:

- quantified variables and ranges;
- the law in natural language and structured form where possible;
- boundary values;
- generators and valid-state constraints;
- mutants that the property must reject.

For a deadline rule, declared mutants may include:

```text
< replaced with <=
required state check removed
&& replaced with ||
missing timestamp defaults to True
```

A property is insufficient if it remains green against the relevant wrong implementations. Mutation score is part of the property acceptance contract, not a later optional quality metric.

Generic-derived arbitrary values alone are not enough. They often generate invalid domain histories and can make properties vacuous. Generators must respect the Slice contract and event-stream invariants.

## Local model boundary

MiniCPM5-1B is a candidate baseline, not an architectural dependency or an assumed capability. The hypothesis to test is:

> Given an expected type, a one- or two-sentence English rule, a closed symbol vocabulary, positive examples, counterexamples, and compiler feedback, a small local model can rank or construct the correct NeoHaskell expression within a bounded repair budget.

The model receives only:

- one hole;
- its expected type;
- local semantic intent;
- allowed symbols and signatures;
- selected nearby examples;
- example/counterexample data;
- a concise compiler or validator rejection when repairing.

It does not receive a repository shell, arbitrary tools, broad write access, or an instruction to generate several artifacts.

The inference boundary must be provider-neutral. Model installation is explicit, provenance- and checksum-verified, and never triggered as a surprise by a daily command. Deterministic-only and no-model operation remain supported.

## Evaluation

Evaluate real NeoHaskell synthesis problems, not HumanEval or generic agent benchmarks.

Measure per module:

- exact accepted solution rate;
- first-pass parse and compile rate;
- convergence within two or three repairs;
- invented or forbidden symbol rate;
- positive-example pass rate;
- counterexample and boundary pass rate;
- property pass and mutation kill rate;
- candidates explored;
- model invocations and tokens;
- wall-clock latency;
- peak memory;
- thermal behavior on the low-end baseline;
- offline completion rate;
- semantic agreement with reviewed implementations.

Compare:

```text
deterministic enumeration only
model only
retrieval + model
retrieval + model + compiler
retrieval + enumeration + model ranking + compiler
```

The deterministic environment may contribute more effective capability than a larger model. Autonomy is granted per synthesis module only after measured thresholds are met.

## Transactionality and safety

Every generation operation must be staged:

```text
real workspace
  -> capture base revision and relevant file hashes
  -> isolated temporary workspace
  -> deterministic edits
  -> atomic hole completions
  -> sandboxed parse/type/test/property/mutation/lint gates
  -> semantic diff
  -> acquire project commit lock
  -> revalidate base revision and file hashes
  -> approved atomic write, or abort/rebase on mismatch
```

Generation must never overwrite edits made after it began. The final commit phase holds a project-scoped lock, compares the current revision or relevant file hashes with the captured base, and aborts with a reproducible conflict diagnostic when they differ. Automatic rebasing is allowed only when a deterministic three-way merge and all validation gates succeed again.

The temporary workspace is not an execution sandbox. Generated code, compiler plugins, build hooks, tests, properties, and mutation runners must execute with:

- network access denied by default;
- filesystem access restricted to the isolated workspace and explicit read-only toolchain paths;
- a scrubbed environment containing no ambient credentials or unrelated configuration;
- validated real paths and rejected symlink escapes before execution and commit;
- CPU, memory, process-count, output-size, and wall-clock limits;
- process-group supervision and descendant cleanup on success, failure, cancellation, and timeout.

Unrestricted execution is an explicit trusted mode with a visible warning and must never be selected automatically by model output or project content.

On failure:

- the real workspace is unchanged;
- temporary evidence is retained or summarized reproducibly;
- secrets and unrelated source are not included in traces;
- diagnostics identify the failed hole and validator;
- a manual repair path is provided.

Model output is untrusted input. Parse and validate it before any source insertion. Enforce expression size/depth, symbol allowlists, timeouts, token budgets, compiler-repair budgets, and process cleanup.

## IDE and progress model

The IDE consumes source-model events from the semantic engine and renders:

- bounded contexts and chapters;
- slice columns or timelines;
- entity swim lanes;
- commands and produced events;
- multi-entity query feeds;
- inbound/outbound translations;
- slice dependencies;
- preconditions and behavioral contracts;
- source, typed, implementation, test, and deployment status.

Every node navigates to source. Drift is evidence, not an alternate editing model:

```text
declared, not implemented
implemented, not declared
declared relationship contradicts type family
source-valid, typed validation blocked
implementation green, required property missing
```

Layout data may be stored separately because position is not domain semantics.

## Migration strategy

Use a strangler migration across both framework and CLI.

### Framework

1. Add Slice declarations without removing current registration.
2. Build source and typed validators.
3. Derive runtime registration for one behavior.
4. Differentially compare derived and manual wiring.
5. Migrate bounded contexts incrementally.
6. Deprecate public `Service` composition.
7. Remove central editable model artifacts after parity.

### CLI

1. Define the launcher/engine protocol.
2. Route a pure read-only command through Haskell.
3. Add source inspection.
4. Add typed validation and exact edits.
5. Add deterministic slice generation.
6. Add behavioral oracles and atomic synthesis.
7. Move interactive/watch/IDE surfaces after lifecycle parity.
8. Remove Rust semantic implementations only when contract tests prove parity.

### Compatibility gates

Continuously verify:

- CLI arguments, aliases, output channels, exit codes, and JSON contracts;
- signal, cancellation, timeout, and terminal restoration;
- embedded starter and immutable compatibility revision;
- generated application build/test/run/health;
- source inspection in green and red states;
- package and release artifacts;
- clean-machine onboarding within 600 seconds;
- offline deterministic workflow;
- local-model installation and checksum behavior.

## Decision consequences

### Benefits

- One truthful model instead of synchronized artifacts.
- Strong code locality for humans and agents.
- Runtime, tooling, tests, and diagrams consume the same declarations.
- Broken implementation does not destroy architectural visibility.
- Generation becomes observable, reproducible, and benchmarkable.
- Small local models can contribute without receiving dangerous autonomy.
- GHC and executable contracts remain the acceptance authority.

### Costs

- The Slice DSL must remain deliberately closed and source-parseable.
- GHC API/version management becomes a first-class toolchain responsibility.
- The native-launcher/Haskell-engine protocol adds a distribution boundary.
- Public `Service` migration is a breaking API program, not a rename.
- High-quality examples, counterexamples, boundaries, and mutation operators require product work.
- Local inference may fail the baseline; deterministic and frontier fallbacks must remain first-class.

## Open design questions

These should be resolved by vertical proofs rather than speculative framework-wide implementation:

- Exact Slice DSL combinator names and how incomplete references are represented.
- Whether bounded-context/chapter aggregation is explicit, generated from imports, or both.
- Stable identity/versioning rules for deployed slices and renamed nodes.
- The smallest parser surface that supports source-valid inspection without evaluating arbitrary Haskell.
- Exact GHC API versus `ghc-lib` packaging strategy across supported framework versions.
- Engine process/socket lifecycle and cache invalidation across branches and worktrees.
- Which synthesis modules meet autonomy thresholds on the low-end baseline.
- How much property structure can be lowered deterministically before model assistance is needed.
- How source-derived model changes are reviewed and approved in interactive and CI workflows.

The first vertical slice should answer enough of these to prove the architecture before broader migration:

```text
local Slice declaration
  -> source inspection while red
  -> typed relationship validation
  -> Application.withSlices
  -> derived entity runtime
  -> deterministic contract test
  -> one atomic precondition hole
  -> transactional acceptance
```
