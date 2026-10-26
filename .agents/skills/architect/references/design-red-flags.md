# Design red flags

Screen every candidate before synthesis. A red flag is a reason to revise or reject the shape.

## Shallow module

A shallow module exposes a large interface while hiding little complexity. Judge depth by the capability and policy hidden behind the public surface relative to the size of that surface. Prefer a simple interface backed by substantial behavior.

Do not confuse a deep module with a deep call chain. A deep call chain scatters understanding across layers. A deep module concentrates capability behind one interface.

Look for these signs:

- Callers coordinate several methods to complete one operation.
- Public options expose internal stages or implementation choices.
- Learning the interface does not save the caller from learning the implementation.

## Information leakage

Information leakage makes multiple modules depend on the same internal decision. A representation, policy, or protocol detail appears in more than one place, so changing it requires coordinated edits.

Exposing a boundary's private transport, storage, or wire representation is leakage. Parse external data into domain types behind the owning interface. Framework types and intentional interoperability types may be public contracts; evaluate them by their promised semantics and ownership, per **boundary-discipline**.

## Temporal decomposition

Temporal decomposition organizes modules by execution order instead of the knowledge they own. Separate load, validate, transform, and save stages often repeat one representation and its invariants across several boundaries.

Group code around domain knowledge and ownership. Operations that run at different times can still belong to one module when they protect the same decisions. Pure transformations may remain useful compositional functions; distinguish data dependencies from effect sequencing when judging a Haskell design.

## Pass-through method

A pass-through method forwards the same arguments to another method with the same shape. It adds a layer without hiding complexity.

Remove it or move responsibility to the module that can complete the operation. Keep a forwarding boundary when it adds policy, adaptation, or a useful framework facade, service contract, type boundary, or class abstraction. Direct delegation alone does not establish redundancy.

## Split ownership

More than one module writes the same state or keeps its own copy of it. An agent that edits one writer can't see the others, so their rules diverge.

Give each shared-state invariant and worker lifetime an owning subsystem. Other modules use its operations. Keep required atomic transitions or coordination at that boundary, using the appropriate existing NeoHaskell concurrency primitives, per **separate-before-serializing-shared-state**.

## Two ways to do one task

The design supports more than one way to do the same task. An agent copies whichever way it finds first, so every way keeps gaining callers.

For redundant internal APIs where no external compatibility obligation exists and coordinated breaking changes are feasible, migrate callers and remove the old path in the same refactor wave, per **migrate-callers-then-delete-legacy-apis**. Distinct operations or implementations with different contracts are not duplicate paths merely because their shapes resemble each other.

## Importable internals

A caller can import a module's internals. An agent takes the shortest path that compiles, so it imports them directly and they become part of the interface.

Define the intended caller boundary. In Haskell, export lists control visible names, constructors and fields; package exposed-module declarations control module imports by external package consumers. Use the appropriate boundary to enforce the public contract and verify it from the relevant caller context.

## Hand-synced list

Two or more places list the same items, and adding an item means editing every list. An agent that sees one list updates only that one.

Keep one list and derive the others from it. If a list can't be derived, make the build fail when the lists disagree.
