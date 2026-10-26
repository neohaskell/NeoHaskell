---
name: principle-boundary-discipline
description: "Apply when wiring validation, error handling, or framework adapters. Concentrate guards at their owning boundaries, including public framework APIs. Trust established invariants, retain necessary runtime checks, and keep core logic in pure functions."
---

# Boundary Discipline

Place validation, type narrowing, and error handling at the boundaries that own their contracts, including public framework APIs. Trust invariants established by types or earlier checks. Preserve operation-specific checks required by the contract when types or earlier checks do not establish the condition. Core logic lives in pure functions. The shell is thin and mechanical.

**Why:** Repeating established validation is noisy and redundant. Keep core rules separate from effectful adapter wiring so they can be tested independently.

**The pattern:**
- **At boundaries** (CLI args, config files, external APIs, network protocols, public framework APIs): validate and handle errors according to the contract.
- **Inside the system:** typed data and error propagation. Do not repeat validation of established invariants. Preserve operation-specific checks required by the contract when the types or earlier checks do not establish the condition.
- **Across the boundary.** Expose domain concepts, not the boundary's private representation. Keep general-purpose mechanism inside and special-purpose policy at the edge.

**Applications:**

Validation and error handling:
- Validate config at parse time (the boundary), not inside business logic
- Parse raw data into domain types at the boundary
- Do not leak private transport, storage, or wire representations through the public API. Framework types and intentional interoperability types may be part of the public contract.
- No redundant nil checks deep in call chains if the boundary already validated

Code organization:
- Core rules in pure functions using stable framework types, without dependencies on effectful adapter wiring
- Parse functions: pure transforms from raw bytes to typed state
- Prompt construction: structured state in, string out
- Scoring and assessment: pure transforms from state to results

**The tests:**
- "Is this data crossing a boundary right now?" If not, distinguish repeated validation of an established invariant from an operation-specific check required by the contract.
- "Can this be a pure function that the shell just calls?" If yes, extract it.
