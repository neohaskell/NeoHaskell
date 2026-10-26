---
name: principle-separate-before-serializing-shared-state
description: "Apply when changing shared mutable state, worker coordination, or task lifetimes in NeoHaskell framework internals. Reuse the owning subsystem, separate genuinely independent state, and use existing concurrency primitives where synchronization is required."
---

# Separate Before Serializing Shared State

Start with the framework subsystem that owns the state and its lifecycle, such as a dispatcher, store, or cache. Keep concurrency decisions there, behind operations callers can use. Keep pure transformations pure.

**Why:** Coordination belongs with the invariant it protects. Spreading mutable state or synchronization across callers makes the invariant harder to enforce. Instructions and conventions are not concurrency control.

**Pattern:**
1. **Find the owner and invariant.** Identify the subsystem that owns the affected state or worker lifetime and read its existing concurrency design before changing it.
2. **Reuse or extend the owner's operations.** If a new capability needs an atomic transition or coordinated lifecycle step, implement it at that boundary and have callers use it.
3. **Separate genuinely independent state.** Prefer independent ownership when the state does not need to be shared. Preserve a canonical store or coordinated worker registry when its invariant requires one.
4. **Use NeoHaskell concurrency primitives when needed.** Within the owning subsystem, read the relevant implementations in `core/concurrency/` and choose the module whose ownership, blocking, atomicity, communication, and lifetime guarantees fit the operation. Preserve the whole transition: separately safe reads and writes may not form an atomic update. Follow the [Foundational Thinking](../principle-foundational-thinking/SKILL.md) concurrency guidance.
