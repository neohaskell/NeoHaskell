### Feature

**You own the design. Plan, review, verify.** Delegate implementation. Stay in the lead.

Framework export, TH marker and generated-API changes also follow
[NeoHaskell consumer verification](../../skills/neohaskell-consumer-verification/SKILL.md)
for the same-PR starter and testbed migration contract.
Every public NeoHaskell feature follows that skill's working testbed example
contract, even when no existing consumer needs migration. State why it does not
apply only when the change develops no public feature.

1. `how` over the affected subsystem.
2. `architect` for parallel design exploration only; pass the chosen design to the implementation owner below. Apply [NeoHaskell best practices](../../skills/neohaskell-best-practices/SKILL.md) and [code guidelines](../../skills/neohaskell-code-guidelines/SKILL.md) as design constraints.
3. Write the throughput checkpoint as four todo items. A dimension that genuinely does not apply (single file, no fan-out) keeps its item with `n/a: <reason>` rather than being dropped:
   - **Blocking first steps.** Gates run before fan-out.
   - **Independent workstreams.** Disjoint files, services, or layers are candidates for parallel work; shared public types, instances, generators or service contracts can still couple them. Shared writes serialize.
   - **Shared mutable state.** Default to splitting genuinely independent state (the **separate-before-serializing-shared-state** principle skill). Keep each shared invariant at its existing owner and choose the appropriate NeoHaskell concurrency module when coordination is needed; pure callers do not acquire synchronization plumbing. Serialize only for real invariants.
   - **Smallest safe decomposition.** If one worker is best, name why.
4. Delegate code-writing through the available implementer roles in `AGENTS.md`, within actual host capacity, with a specific scope (file paths, named data shape and its organizing structure per **principle-model-the-domain**, a state machine over scattered booleans, a table/registry over branching, a typed model over repeated shape assumptions, chosen before the delegate writes logic, and success criteria). When the implementation admits multiple valid shapes (error handling, abstraction layer, test structure), delegate via the **arena** skill instead so the runners surface the alternatives and the cross-judge guards the pick. Mandatory: no skip-with-reason escape, and Minimal change does not override it (the gain is review separation, not lines saved). A subagent forbidden to spawn satisfies this by owning the diff directly with the same review separation. No "standing by" reply that waits on a nested agent. Comments follow `AGENTS.md` and the accepted **no-comments** skill. Surgical edits, re-ground against the source for upstream-derived files. Port shared-primitive improvements to all consumers and verify each. Commit liberally.
5. Verify on the matching surface: compile representative consumers for public/generated API contracts; demand pure results and execute Tasks, decisions or affected service flows for runtime claims. For a public feature, also run its testbed example and maintained automated assertion through the public API, with concrete run and test commands. Use meaningful Hspec examples and QuickCheck properties with independent expectations, following TDD when its accepted trigger applies. Use **benchmark-checklist** for relevant runtime or consumer-compilation performance claims. "Inconclusive" or wrong-surface is not a pass. Flag it.
6. Rebase into small, ordered commits. Stack follow-ups.
   Use the **sequence-verifiable-units** principle skill, building, verifying, and committing each coherent unit before the next, including the types, instances, generators and affected consumers required for its Haskell contract.
7. If the design is contested, `interrogate` before shipping.
8. Run **Opening a PR**.

Code-coupled work (one feature, one migration) goes to a single owner with the checkpoint inline. That owner fans out internally after the blocking phase. Parent-level fan-out is for slices that produce independent artifacts (audits, cross-subsystem investigations, competing experiments). Rewrite the checkpoint at phase boundaries. Spawn a fresh owner rather than chaining interrupts.

**Reply:** what you built, what you chose and why, the throughput checkpoint, open decisions. Tables for design alternatives.
