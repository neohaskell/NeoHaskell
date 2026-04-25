# ADR-0055: UUID v5 Support in Decision Monad

**Status:** Proposed  
**Date:** 2026-04-25  
**Deciders:** NeoHaskell Architecture Team  

## Context

NeoHaskell currently provides `generateUuid` (v4, random) in the `Decision` monad for generating unique identifiers in command handlers. UUID v5 (namespace-based, deterministic, RFC 4122) is needed for consistent ID generation from inputs such as external OAuth user IDs, merchant IDs, or other domain identifiers.

The question is whether UUID v5 should be:
1. **Monadic** - placed in `Decision` alongside `generateUuid` for discoverability in command handlers
2. **Pure** - exposed as a library function outside the monad for simplicity and purity

## Decision

We will implement UUID v5 as a **monadic function** in the `Decision` monad:

```haskell
generateDeterministicUuid :: Uuid -> Text -> Decision Uuid
```

This decision prioritizes **discoverability and consistency** for command handler authors.

## Options Considered

| Aspect | A: Monadic | B: Pure Function |
|--------|-----------|-----------------|
| **API** | `generateDeterministicUuid :: Uuid -> Text -> Decision Uuid` | `Uuid.fromNamespace :: Uuid -> Text -> Uuid` |
| **Location** | `Decider` module (alongside `generateUuid`) | Utility module / library function |
| **Discoverability** | ⭐⭐⭐ High—handlers find it next to `generateUuid` | ⭐ Low—requires knowledge of utility APIs |
| **Purity** | ⭐ Impure wrapper around pure logic | ⭐⭐⭐ Pure; deterministic semantics visible |
| **Type Clarity** | Mixed with other `Decision` effects | Clear: deterministic input → deterministic output |
| **Usage in Handlers** | `uuid <- generateDeterministicUuid ns name` | `let uuid = Uuid.fromNamespace ns name` |
| **Consistency** | ✓ Mirrors `generateUuid` call pattern | ✗ Different pattern (no `<-`) |
| **Testing** | Standard `Decision` test utilities apply | Simple property testing on pure function |
| **Cognitive Load** | Lower for handler authors (everything in `Decision`) | Higher (must know to look outside monad) |

## Rationale

**Why Monadic:**

1. **Discoverability** – Command handlers authored in the `Decision` monad should find UUID generation as a cohesive API. Placing v5 alongside v4 (`generateUuid`) ensures authors discover it naturally.

2. **Consistency** – The `Decision` monad is the contract for command-side effects. Even though v5 is deterministic, co-locating it with v4 reinforces that "all ID generation happens here" for handler authors.

3. **Future Extensibility** – If we later need versioned UUIDs or additional deterministic ID schemes, they belong in the same API surface.

4. **Handler Ergonomics** – The `<- generateDeterministicUuid ns name` pattern is familiar to handlers already using `generateUuid`.

**Trade-offs Accepted:**

- We introduce a monadic wrapper around pure logic. This is semantically safe (the function is deterministic and has no side effects), but the monad wrapping is a lie about effects. This is acceptable because:
  - It's documented (naming: "Deterministic")
  - The cost is minimal (a single binding)
  - The benefit (discoverability) outweighs the purity trade-off

## Implementation Details

### Signature

```haskell
-- | Generate a UUID v5 from a namespace and name.
-- Deterministic: same inputs always produce the same UUID.
generateDeterministicUuid :: Uuid -> Text -> Decision Uuid
```

### Placement

- **Module:** `Decider` (or `Decider.Uuid` if namespace concerns arise)
- **Near:** `generateUuid` to emphasize the relationship

### Constraints

- **Namespace:** Must be a valid UUID (caller provides domain-specific namespace)
- **Name:** Arbitrary Text; will be UTF-8 encoded for hashing
- **Hash:** SHA-1 (RFC 4122 v5 standard)

## Consequences

### Positive

- UUID v5 is discoverable by command handler authors without external documentation
- Consistent API for all UUID generation in command handlers
- Aligns with the `Decision` monad as the "effects API" for commands

### Negative

- Introduces a monadic wrapper around deterministic logic (philosophical purity concern, not practical)
- Slightly larger `Decision` API surface

### Neutral

- No performance impact (SHA-1 hashing is negligible)
- No dependency changes required (use existing UUID library)

## Related Decisions

- **ADR-0001** (if exists): `Decision` monad design  
- **Issue #596:** UUID v5 requirement for OAuth integration

## References

- RFC 4122 §4.3 - UUID Version 5 (SHA-1)
- [Data.UUID.V5](https://hackage.haskell.org/package/uuid-1.3.15/docs/Data-UUID-V5.html) (typical Haskell library)

## Approval

- [ ] Architecture Team
- [ ] Engineering Lead
