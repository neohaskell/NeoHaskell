# Framework verification map example

This illustrates a verification map for NeoHaskell internals using maintained consumer fixtures. It is a teaching example, not a generated or executed verification skill. A generator must discover and prove literal setup, driving, evidence and cleanup recipes for the selected checkout before delivering its output.

## Baseline preconditions

- Identify the exact revision, toolchain, build configuration, selected fixture and required dependencies from current declarations and source.
- Own the runtime instance and its disposable database/upload state. The current HTTP recipes assume port 8080; prove ownership or report the unmet prerequisite rather than driving another instance.
- Use the generated skill's Launch and Doctor recipes. A ready query subsystem still does not establish completion of a later integration command.
- Put run state under `.agents-state/verification/<run>/scratch/` and retained proof under `.agents-state/verification/<run>/evidence/`. These example paths must become concrete paths in generated output.
- Establish each feature's baseline predicates before mutations and record the returned identifiers.

## Driving conventions

- Use the real compiler, consumer or owning framework entrypoint for the contract being proved.
- Reuse existing fixture and harness commands after inspecting what they exercise; inspect a full generated recipe before running it.
- Capture identifiers at creation and query exact records. Wait for observable predicates with bounded retries, recording deadlines and last values on failure.
- Repeat mutating commands only when the recipe explicitly calls for another action; a read retry is not permission to resubmit a mutation.
- Clean only owned processes and scratch state. Keep proof and diagnostic evidence after failed iterations and final teardown.

## Proof and skip reporting

- Compiler evidence identifies the consumer source, toolchain, command and result; successful compilation alone does not establish runtime behavior.
- Runtime evidence includes actual inputs/actions, observed results and any claimed stored or downstream effects. Expected values come from the contract, not a copy of the implementation.
- Record feature ID, entrypoint, exact entity/stream identifiers and environment identity with proof artifacts.
- Report unmet prerequisites and attempted paths; do not count another entrypoint as proof of a skipped one.
- Separate normal-path observations from stronger correlation, delivery, replay or durability claims that require additional evidence.

## Feature entry contract

Each feature starts with an H1 title and a paragraph naming the developer/caller-visible contract, then exactly four H2 sections in order:

1. `Sub-features` lists short IDs and their behavior.
2. `How to get to it (user POV)` lists the real caller entrypoints.
3. `Driving it with <harness>` starts with `Preconditions:` and pairs each action with its observable proof and actual recipe source.
4. `Gotchas` records unmet prerequisites, harness gaps and traps that can invalidate proof.

Keep the map about reachable contracts, inputs, stable handles, required state and proof. Include the interaction boundaries necessary to distinguish completion, without copying implementation internals into every recipe.

## Features

- [Generated entity API](generated-entity-api.md) separates consuming compilation from generated runtime behavior.
- [Command and query state](command-query-state.md) checks the exact entity created through the command entrypoint.
- [Indirect command propagation](indirect-command-propagation.md) follows an event through an outbound integration, an internal command and independent projections.
