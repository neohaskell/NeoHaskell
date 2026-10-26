---
name: principle-encode-lessons-in-structure
description: "Apply when you catch yourself writing the same instruction a second time, or notice a recurring correction. For NeoHaskell internals, encode the rule through a Haskell type, the API that owns the invariant, existing Generic/Template Haskell derivation, a lint, metadata flag, runtime check, or script instead of more text."
---

# Encode Lessons in Structure

If the lesson needs judgment and cannot be encoded, make the instruction more prominent and add an example of the failure mode.

**Corollary:** If the fix is structural, only use the structural fix. The instruction is the symptom.
