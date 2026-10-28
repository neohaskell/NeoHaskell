# Review Rubric

Review through whichever lenses are relevant. Not every lens applies to every change.

## Correctness

Does the code actually do what the intent says it should?

- Edge cases: empty collections, `Maybe`/`Result` cases, constructor coverage, boundary values and concurrent access
- Error handling: are errors caught, propagated, or silently swallowed?
- Off-by-one, type coercion, integer overflow, string encoding
- Haskell semantics where relevant: totality, evaluation/strictness, pure transformations versus deferred `Task` effects, and changed instances or generated declarations
- Service contracts where relevant: do record operations and their injected implementations honor the stated semantic guarantees?
- State management: race conditions, retained state or resources, and worker lifetimes
- Do both the happy path and the sad path work?
- Idempotency: what happens if this operation runs twice, or if a previous run crashed halfway? If the answer is "it depends on what state was left behind," a reconciliation step is missing.
- Concurrency: if multiple actors can touch the same mutable state (files, branches, shared data), is access serialized structurally (locks, sequential phases, exclusive ownership), or by conventions that won't hold?

When you find a potential bug, trace the relevant data dependencies or effect path. Don't just flag "this could be Nothing". Show the reachable input or state that exposes the problem.

## Root Causes vs. Symptoms

Is the code fixing the actual problem or papering over a symptom?

Answering this often requires looking beyond the changed files. Read the surrounding code (callers, callees, type definitions, sibling modules) and understand the architecture the change lives in. Use the available source-reading and text-search capabilities to follow the call chain, read the types, and understand why the code exists before judging whether the change addresses the right layer.

- Guard clauses that mask a deeper invariant violation
- Retry logic that hides a broken contract
- Unchecked coercions that silence a type-design error
- A workaround: why is it needed? What would a proper fix look like?
- A fix in module A that should really be a fix in module B's contract
- Instructions where structure would be better: if the fix is a comment saying "don't do X" or a convention someone has to remember, ask whether it could instead be a type constraint, a lint rule, or a runtime check that makes the wrong thing impossible

## Structural Integrity

Does the code fit well into the system it's part of?

- Boundary discipline: is validation at system boundaries, or scattered through business logic? Validate data once where it enters the system, then trust it internally.
- Abstraction level: is the code mixing high-level orchestration with low-level detail?
- Coupling: does this change introduce dependencies that will make future changes harder?
- Data structure fit: do the data structures match the actual access patterns?
- Bolted-on vs. integrated: if the new requirement had been known from the start, would the code look like this, or does the change feel patched onto the existing design?
- Legacy dual-paths: does the change introduce a new API while keeping the old one alive? If there are no external consumers, migrate callers and delete the old path in the same wave. Don't leave compatibility layers that will become permanent.

Don't penalize simple code for lacking abstraction. Premature abstraction is worse than duplication.

## Verification

Can you tell that this code works from reading it?

- Are there tests? Do they test behavior or implementation details? Where laws, broad input spaces or state transitions matter, inspect relevant property-based coverage and whether generators, shrinking and assertions can expose a defect.
- Are there assertions/invariants that would catch regressions?
- If this is a bug fix: is there a test for the bug?
- If this touches an integration boundary: is the full path tested?
- Check the real thing, not a proxy. If the code checks liveness via file mtime or cached state instead of reading the actual value, that's a verification gap.
- For delegated or async work: does the code verify actual output artifacts, or does it trust self-reports and summaries?
- For a developed public NeoHaskell feature: does the testbed application exercise its public API, does a maintained automated scenario assert observable behavior, and are the commands and run results supplied? Unit or compiler tests and documentation examples do not fill a missing testbed example. For internal-only work, check the stated applicability against the diff.

## Complexity Budget

Is the complexity justified by what the code accomplishes?

- Code that could be simpler without losing correctness or clarity
- Abstractions that serve only one call site
- Configuration or parameterization for cases that don't exist yet
- Dead code, unused imports, vestigial parameters
- Over-engineering: "just in case" code paths with no current callers
- Obsolete compatibility paths kept alive for transitional stability that's no longer needed. If the migration is done, delete the scaffolding
- Does the user experience justify the complexity? Every feature, control, and option should earn its place. Half-finished features are worse than missing ones.

Simpler is better unless simpler is wrong.

## Security

Apply [Security by Construction](../../principle-security-by-construction/SKILL.md). Trace each finding's reachable input and effect path, affected guarantee and concrete impact; distinguish demonstrated defects from design risks or unknowns. Inspect actual parsing/construction, authority and query visibility, secret output paths, storage/input sinks, data isolation and resource-abuse controls where relevant. Prefer remedies at the owning framework boundary over repeated caller obligations.

## Performance and scaling

Apply [Design for Throughput](../../principle-design-for-throughput/SKILL.md). Assess the changed structure against the relevant workload, whole-flow costs, contention domain and bounded resource behavior, including actual evaluation/codec work and downstream compilation costs. Distinguish a reasoned design concern from a measured regression. Demand evidence appropriate to an empirical claim; do not impose a full target-load benchmark merely to review an architectural choice.
