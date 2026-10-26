---
name: principle-experience-first
description: "Apply when product, UX, or feature-scope tradeoffs come up. Choose user delight over implementation convenience; ship fewer polished features over more rough ones."
---

# Experience First

When implementation convenience conflicts with user delight, choose delight.

- Every feature, control, and option must be justified
- Ship less, ship better (polished experience with three features beats rough one with ten)
- Prototype before committing (design decisions are cheaper in a throwaway prototype suited to the work than production code)
- Get the details right (clear API names, predictable behavior, actionable errors, documentation, and visual details where relevant)
- Tighten the core loop (every feature should serve the central workflow or get out of the way)

The user is whoever consumes the work. For a UI that is the end user. For a library or an internal API it is the colleague who imports it. The engineer who maintains the code next is a user too. For NeoHaskell's framework, language, and toolchain, prioritize Jess: a junior TypeScript developer with limited time who wants to build a working project without deep prerequisite learning. Consider maintainers' experience too, and explain the impact on both Jess and maintainers.

Foundations should serve the experience. Foundational thinking governs the *sequence* of work. This principle governs the *target*.
